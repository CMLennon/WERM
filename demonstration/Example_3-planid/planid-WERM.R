library(xgboost)
library(boot)
source('../WERM_Heuristic.R')

WERMEstimator = function(OBS,D){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]; X0 = rep(0,length(X)); X1 = rep(1,length(X));
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)]  
  Y = OBS[,(D+4)]
  DATA = data.frame(W,X,R,Z,Y)
  
  ################################################################
  # Evaluate the weight hat{W}
  # See Eq. (A.5)
  # W = P^{W}(y|w,r,z)P(x)P(r)P(z|w,x)/(P(x|w)P(r|w)P(z|w,x,r)P(y|w,x,r,z)
  ################################################################
  # Learn P(x)
  Prob.X = X*mean(X) + (1-X)*(1-mean(X))
  
  # Learn P(r)
  Prob.R = R*mean(R) + (1-R)*(1-mean(R))
  
  # Learn P(x|w)
  model.X.w = learnXG_Planid(DATA,c(1:D),X,rep(0,length(X)))
  pred.X.w = predict(model.X.w, newdata=data.matrix(W),type='response')
  Prob.X.w = X*pred.X.w + (1-X)*(1-pred.X.w)
  Prob.X0.w = (1-pred.X.w)
  Prob.X1.w = pred.X.w
  
  # Learn P(r|w)
  model.R.w = learnXG_Planid(DATA,c(1:D),R,rep(0,length(R)))
  pred.R.w = predict(model.R.w, newdata=data.matrix(W),type='response')
  Prob.R.w = R*pred.R.w + (1-R)*(1-pred.R.w)
  
  # Learn P(z|w,x,r)
  model.Z.wxr = learnXG_Planid(DATA,c(1:(D+2)),Z,rep(0,length(Z)))
  pred.Z.wxr = predict(model.Z.wxr, newdata=as.matrix(cbind(W,X,R)),type='response')
  Prob.Z.wxr = Z*pred.Z.wxr + (1-Z)*(1-pred.Z.wxr)
  
  # Learn P(z)
  Prob.Z = Z * mean(Z) + (1-Z)*(1-mean(Z))
  
  # Learn P^{Wd}(y|w,r,z)
  Ybox = rep(0,nrow(DATA))
  bootstrap_iter = 10
  for (idx in 1:bootstrap_iter){
    sampled_df = WERM_Sampler(DATA,(Prob.R*Prob.Z)/(Prob.R.w*Prob.Z.wxr))
    # Learn Pw(y|w,z)
    model.Yw.wzr = learnXG_Planid(sampled_df,c(1:D,(D+2),(D+3)),Y,rep(0,length(X)))
    pred.Yw.wzr = predict(model.Yw.wzr, newdata=data.matrix(data.frame(W,R,Z)),type='response')
    Prob.Yw.wzr = Y*pred.Yw.wzr + (1-Y)*(1-pred.Yw.wzr)
    Ybox = Ybox + Prob.Yw.wzr
  }
  Prob.Yw.wzr = Ybox/bootstrap_iter
  
  # Learn P(z|w,x)
  model.Z.wx = learnXG_Planid(DATA,c(1:(D+1)),Z,rep(0,length(Z)))
  pred.Z.wx = predict(model.Z.wx, newdata=data.matrix(DATA[,c(1:(D+1))]),type='response')
  Prob.Z.wx = Z*pred.Z.wx + (1-Z)*(1-pred.Z.wx)
  
  # Learn P(y|w,x,r,z)
  model.Y.wxrz = learnXG_Planid(DATA,c(1:(D+3)),Y,rep(0,length(Y)))
  pred.Y.wxrz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X,R,Z)),type='response')
  pred.Y.wx0rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X=X0,R,Z)),type='response')
  pred.Y.wx1rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X=X1,R,Z)),type='response')
  Prob.Y.wxrz = Y*pred.Y.wxrz + (1-Y)*(1-pred.Y.wxrz)
  
  # Compute \hat{W}
  W_importance = (Prob.X* Prob.R * Prob.Z.wx *  (Prob.X0.w*pred.Y.wx0rz + Prob.X1.w*pred.Y.wx1rz))/(Prob.X.w * Prob.R.w * Prob.Z.wxr * Prob.Y.wxrz)
  
  ################################################################
  # Learn h and W.
  ################################################################
  # regvallist = c(1:100)*(nrow(OBS))/20
  regvallist = seq(0,10,by=0.2)
  lambda_W = learnHyperParam(regvallist,data.matrix(DATA),W_importance,0)
  learned_W = learnWdash(W_importance,data.matrix(DATA),lambda_W)
  lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=X, R=R)),Y,1)
  
  xgbMatrix = xgb.DMatrix(data.matrix(data.frame(X=DATA$X,R=DATA$R)), label=DATA$Y)
  modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,objective = "binary:logistic",weight = learned_W)
  
  X0R0 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(0,nrow(DATA))))
  X0R1 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(1,nrow(DATA))))
  X1R0 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(0,nrow(DATA))))
  X1R1 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(1,nrow(DATA))))
  
  Yx0r0 = mean(predict(modelY_xgboost,newdata=X0R0,type='response'))
  Yx0r1 = mean(predict(modelY_xgboost,newdata=X0R1,type='response'))
  Yx1r0 = mean(predict(modelY_xgboost,newdata=X1R0,type='response'))
  Yx1r1 = mean(predict(modelY_xgboost,newdata=X1R1,type='response'))
  
  # Yx0r0_results[iteridx] = Yx0r0
  # Yx0r1_results[iteridx] = Yx0r1
  # Yx1r0_results[iteridx] = Yx1r0
  # Yx1r1_results[iteridx] = Yx1r1
  # myans = c(mean(Yx0r0_results),mean(Yx0r1_results),mean(Yx1r0_results),mean(Yx1r1_results))
  myans = c(Yx0r0,Yx0r1,Yx1r0,Yx1r1)
  
  Yxr00 = WERM_Heuristic(inVar_train = data.frame(X=X,R=R), inVar_eval = data.frame(X=rep(0,nrow(OBS)),R=rep(0,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yxr01 = WERM_Heuristic(inVar_train = data.frame(X=X,R=R), inVar_eval = data.frame(X=rep(0,nrow(OBS)),R=rep(1,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yxr10 = WERM_Heuristic(inVar_train = data.frame(X=X,R=R), inVar_eval = data.frame(X=rep(1,nrow(OBS)),R=rep(0,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yxr11 = WERM_Heuristic(inVar_train = data.frame(X=X,R=R), inVar_eval = data.frame(X=rep(1,nrow(OBS)),R=rep(1,nrow(OBS))), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  WERManswer = c(Yxr00,Yxr01,Yxr10,Yxr11)
  return(WERManswer)
}




