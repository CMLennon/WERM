library(xgboost)
library(boot)
source('WERM_Heuristic.R')

multiHeuristic = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]; X0 = rep(0,length(X)); X1 = rep(1,length(X));
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)]  
  Y = OBS[,(D+4)]
  DATA = data.frame(W,X,R,Z,Y)
  myYbinary = 0
  
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
  model.X.w = learnXG(inVar = data.matrix(data.frame(W=W)), labelval = X, regval = rep(0,length(X)))
  # model.X.w = learnXG_Planid(DATA,c(1:D),X,rep(0,length(X)))
  pred.X.w = predict(model.X.w, newdata=data.matrix(data.frame(W=W)),type='response')
  Prob.X.w = X*pred.X.w + (1-X)*(1-pred.X.w)
  Prob.X0.w = (1-pred.X.w)
  Prob.X1.w = pred.X.w
  
  # Learn P(r|w)
  model.R.w = learnXG(inVar = data.matrix(data.frame(W=W)), labelval = R, regval = rep(0,length(R)))
  # model.R.w = learnXG_Planid(DATA,c(1:D),R,rep(0,length(R)))
  pred.R.w = predict(model.R.w, newdata=data.matrix(data.frame(W=W)),type='response')
  Prob.R.w = R*pred.R.w + (1-R)*(1-pred.R.w)
  
  # Learn P(z|w,x,r)
  model.Z.wxr = learnXG(inVar = data.matrix(data.frame(W=W,X=X,R=R)), labelval = Z, regval = rep(0,length(Z)))
  # model.Z.wxr = learnXG_Planid(DATA,c(1:(D+2)),Z,rep(0,length(Z)))
  pred.Z.wxr = predict(model.Z.wxr, newdata=as.matrix(data.frame(W=W,X=X,R=R)),type='response')
  Prob.Z.wxr = Z*pred.Z.wxr + (1-Z)*(1-pred.Z.wxr)
  
  # Learn P(z)
  Prob.Z = Z * mean(Z) + (1-Z)*(1-mean(Z))
  
  # Learn P^{Wd}(y|w,r,z)
  # Ybox = rep(0,nrow(DATA))
  # bootstrap_iter = 10
  # for (idx in 1:bootstrap_iter){
  #   sampled_df = WERM_Sampler(DATA,(Prob.R*Prob.Z)/(Prob.R.w*Prob.Z.wxr))
  #   # Learn Pw(y|w,z)
  #   model.Yw.wzr = learnXG_Planid(sampled_df,c(1:D,(D+2),(D+3)),Y,rep(0,length(X)))
  #   pred.Yw.wzr = predict(model.Yw.wzr, newdata=data.matrix(data.frame(W,R,Z)),type='response')
  #   Prob.Yw.wzr = Y*pred.Yw.wzr + (1-Y)*(1-pred.Yw.wzr)
  #   Ybox = Ybox + Prob.Yw.wzr
  # }
  # Prob.Yw.wzr = Ybox/bootstrap_iter
  
  # Learn P(z|w,x)
  model.Z.wx = learnXG(inVar = data.matrix(data.frame(W=W,X=X)), labelval = Z, regval = rep(0,length(Z)))
  # model.Z.wx = learnXG_Planid(DATA,c(1:(D+1)),Z,rep(0,length(Z)))
  # pred.Z.wx = predict(model.Z.wx, newdata=data.matrix(DATA[,c(1:(D+1))]),type='response')
  pred.Z.wx = predict(model.Z.wx, newdata=as.matrix(data.frame(W=W,X=X)),type='response')
  Prob.Z.wx = Z*pred.Z.wx + (1-Z)*(1-pred.Z.wx)
  
  # Learn P(y|w,x,r,z)
  lambda_y = rep(0,nrow(OBS))
  model.Y.wxrz = learnXG(inVar = data.matrix(data.frame(W=W,X=X,R=R,Z=Z)), labelval = Y, regval = lambda_y)
  # model.Y.wxrz = learnXG_Planid(DATA,c(1:(D+3)),Y,rep(0,length(Y)))
  pred.Y.wxrz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X,R=R,Z=Z)),type='response')
  pred.Y.wx0rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X0,R=R,Z=Z)),type='response')
  pred.Y.wx1rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X1,R=R,Z=Z)),type='response')
  # Prob.Y.wxrz = Y*pred.Y.wxrz + (1-Y)*(1-pred.Y.wxrz)
  Prob.Y.wxrz = pred.Y.wx1rz
  
  # Compute \hat{W}
  W_importance = (Prob.X* Prob.R * Prob.Z.wx *  (Prob.X0.w*pred.Y.wx0rz + Prob.X1.w*pred.Y.wx1rz))/(Prob.X.w * Prob.R.w * Prob.Z.wxr * Prob.Y.wxrz)
  learned_W = W_importance
  lambda_h = rep(0.5,nrow(OBS))
  
  
  X0R0 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(0,nrow(DATA))))
  X0R1 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(1,nrow(DATA))))
  X1R0 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(0,nrow(DATA))))
  X1R1 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(1,nrow(DATA))))
  
  Yx0r0 = WERM_Heuristic(inVar_train = data.frame(X=X, R=R), inVar_eval = data.frame(X=rep(0,nrow(DATA)),R=rep(0,nrow(DATA))), Y = Y, Ybinary = myYbinary, lambda_h = lambda_h, learned_W= learned_W)  
  Yx0r1 = WERM_Heuristic(inVar_train = data.frame(X=X, R=R), inVar_eval = data.frame(X=rep(0,nrow(DATA)),R=rep(1,nrow(DATA))), Y = Y, Ybinary = myYbinary, lambda_h = lambda_h, learned_W= learned_W)  
  Yx1r0 = WERM_Heuristic(inVar_train = data.frame(X=X, R=R), inVar_eval = data.frame(X=rep(1,nrow(DATA)),R=rep(0,nrow(DATA))), Y = Y, Ybinary = myYbinary, lambda_h = lambda_h, learned_W= learned_W)  
  Yx1r1 = WERM_Heuristic(inVar_train = data.frame(X=X, R=R), inVar_eval = data.frame(X=rep(1,nrow(DATA)),R=rep(1,nrow(DATA))), Y = Y, Ybinary = myYbinary, lambda_h = lambda_h, learned_W= learned_W)  
  
  WERManswer = c(Yx0r0,Yx0r1,Yx1r0,Yx1r1)
  return(WERManswer)
}




