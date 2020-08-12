source("../WERM_Heuristic.R")

mylossfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )
  return(mean(weightval * pred.loss) + sqrt(LW))
}

mygradfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 ))
  return(mean(pred.loss) + (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel))
}

multiGlobal = function(OBS,D){
  W = OBS[,1:D] 
  X = OBS[,(D+1)]; X0 = rep(0,length(X)); X1 = rep(1,length(X))
  Z = OBS[,(D+2)] 
  Y = OBS[,(D+3)]
  DATA = data.frame(W,Z,X,Y)
  
  ################################################################
  # Evaluate the weight hat{W}
  # See Eq. (A.5)
  # W = P(x)(\sum_{x'} P(y | z,x',w)P(x'|w))/(P(x|w)P(y|z,x,w) 
  # = P(x)(P^{Wd}(y|w,z)P(w))/(P(x|w)P(y|z,x,w) 
  ################################################################
  # Learn P(x)
  Prob.X = X*mean(X) + (1-X)*(1-mean(X))
  
  # Learn P(x|w)
  model.X.w = learnXG_mediator(DATA,c(1:D),X,rep(0,length(X)))
  pred.X.w = predict(model.X.w, newdata=data.matrix(W),type='response')
  Prob.X.w = X*pred.X.w + (1-X)*(1-pred.X.w)
  Prob.X0.w = (1-pred.X.w) # P(X=1 | w)
  Prob.X1.w = pred.X.w # P(X=0 | w)
  
  # Learn P(z|x,w)
  model.Z.xw = learnXG_mediator(DATA,c(1:(D+1)),Z,rep(0,length(X)))
  pred.Z.xw = predict(model.Z.xw, newdata=data.matrix(data.frame(W,X)),type='response')
  Prob.Z.xw = Z*pred.Z.xw + (1-Z)*(1-pred.Z.xw)
  Prob.Z = Z * mean(Z) + (1-Z)*(1-mean(Z))
  
  # Learn P^{Wd}(y|w,z)
  Ybox = rep(0,nrow(DATA))
  bootstrap_iter = 20
  for (idx in 1:bootstrap_iter){
    sampled_df = WERM_Sampler(DATA,Prob.Z/Prob.Z.xw)
    # Learn Pw(y|w,z)
    model.Yw.wz = learnXG_mediator(sampled_df,c(1:D,(D+2)),Y,rep(0,length(X)))
    pred.Yw.wz = predict(model.Yw.wz, newdata=data.matrix(data.frame(W,Z)),type='response')
    Prob.Yw.wz = Y*pred.Yw.wz + (1-Y)*(1-pred.Yw.wz)
    Ybox = Ybox + Prob.Yw.wz
  }
  Prob.Yw.wz = Ybox/bootstrap_iter
  
  # Learn P(y|w,x,z)
  model.Y.wxz = learnXG_mediator(DATA,c(1:(D+2)),Y,rep(0,length(Y)))
  pred.Y.wxz = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X,Z)),type='response')
  pred.Y.wx0z = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X=X0,Z)),type='response')
  pred.Y.wx1z = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X=X1,Z)),type='response')
  Prob.Y.wxz = Y*pred.Y.wxz + (1-Y)*(1-pred.Y.wxz)
  
  # Compute \hat{W}
  W_importance = (Prob.Yw.wz * Prob.X)/(Prob.X.w * Prob.Y.wxz)
  
  ################################################################
  # Learn h and W.
  ################################################################
  # regvallist = c(1:100)*(nrow(OBS))/20
  regvallist = seq(0,10,by=0.2)
  lambda_W = learnHyperParam(regvallist,data.matrix(DATA),W_importance,0)
  learned_W = learnWdash(W_importance,data.matrix(data.frame(W=W,Z=Z)),lambda_W)
  if (sd(learned_W) == 0){
    learned_W = learned_W + rnorm(n=length(learned_W),mean = 0, sd=1e-2)
  }
  ################################
  # Predict Pw(y | x)
  ################################
  lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=X)),Y,1)
  mygradW = WERMGradient(N=nrow(OBS),inputMat = data.matrix(data.frame(X)),labelVal = Y, evalMat =  data.matrix(data.frame(X)), lambda_h = lambda_h,  lambda_W = lambda_W, iterMax = 1000000, init_W=W_importance, LossFun=mylossfun, GradFun=mygradfun)
  
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X), inVar_eval = data.frame(X=rep(0,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= mygradW)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X), inVar_eval = data.frame(X=rep(1,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= mygradW)
  GLOBALanswer = c(Yx0,Yx1)
  return(GLOBALanswer)
}

