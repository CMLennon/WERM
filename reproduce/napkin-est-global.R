source("WERM_Heuristic.R")

napkin_lossfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )
  return(mean(weightval * pred.loss) + sqrt(LW))
}

napkin_gradfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )) + 1e-4
  return(mean(pred.loss) + (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel))
}


multiGlobal = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  Z = OBS[,D+1] 
  X = OBS[,D+2]  
  Y = OBS[,D+3]
  DATA = data.frame(W,Z,X,Y)
  
  ################################
  # Compute SW 
  ################################
  # Importance sampling based 
  SW_importance_sampling  = ComputeSW.lowhigh(Z,W,'Z')
  
  ################################
  # Learn the Hyperparameter 
  ################################
  # Re-learn with the regularization
  # regvallist = c(1:100)*(nrow(OBS))/20
  regvallist = seq(0,10,by=0.2)
  lambda_W = learnHyperParam(regvallist,data.matrix(data.frame(W=W,Z=Z)),SW_importance_sampling,0)
  # lambda_W = lambda_W/nrow(OBS)
  learned_W = learnWdash(SW_importance_sampling,data.matrix(data.frame(W=W,Z=Z)),lambda_W)
  
  if (sd(learned_W) == 0){
    learned_W = learned_W + rnorm(n=length(learned_W),mean = 0, sd=1e-2)
  }
  
  ################################
  # Predict Pw(y | x)
  ################################
  ##### OPT Initial  
  lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=X)),Y,1)/nrow(OBS)
  mygradW = WERMGradient(N=nrow(OBS),inputMat = data.matrix(data.frame(X,Z)),labelVal = Y, evalMat =  data.matrix(data.frame(X,Z)), lambda_h = lambda_h,  lambda_W = lambda_W, iterMax = 1000000, init_W=SW_importance_sampling, LossFun=napkin_lossfun, GradFun=napkin_gradfun)
  
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= mygradW)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= mygradW)
  GLOBALanswer = c(Yx0,Yx1)
  return(GLOBALanswer)
}


