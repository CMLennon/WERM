library(xgboost)
library(boot)
source('WERM_Heuristic.R')

multiID = function(OBS,D,numCate){
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
  # Ybox = rep(0,nrow(DATA))
  # bootstrap_iter = 20
  # for (idx in 1:bootstrap_iter){
  #   sampled_df = WERM_Sampler(DATA,Prob.Z/Prob.Z.xw)
  #   # Learn Pw(y|w,z)
  #   model.Yw.wz = learnXG_mediator(sampled_df,c(1:D,(D+2)),Y,rep(0,length(X)))
  #   pred.Yw.wz = predict(model.Yw.wz, newdata=data.matrix(data.frame(W,Z)),type='response')
  #   Prob.Yw.wz = Y*pred.Yw.wz + (1-Y)*(1-pred.Yw.wz)
  #   Ybox = Ybox + Prob.Yw.wz
  # }
  
  
  # Learn P(y|w,x,z)
  model.Y.wxz = learnXG_mediator(DATA,c(1:(D+2)),Y,rep(0,length(Y)))
  pred.Y.wxz = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X,Z)),type='response')
  pred.Y.wx0z = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X=X0,Z)),type='response')
  pred.Y.wx1z = predict(model.Y.wxz, newdata=data.matrix(data.frame(W,X=X1,Z)),type='response')
  Prob.Y.wxz = Y*pred.Y.wxz + (1-Y)*(1-pred.Y.wxz)
  Prob.Yw.wz = pred.Y.wx0z*Prob.X0.w + pred.Y.wx1z*Prob.X1.w
  
  # Compute \hat{W}
  W_importance = (Prob.Yw.wz * Prob.X)/(Prob.X.w * Prob.Y.wxz)
  
  ################################################################
  # Learn h
  ################################################################
  learned_W = W_importance
  lambda_h = rep(0,nrow(DATA))
  
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  IDanswer = c(Yx0,Yx1)
  return(IDanswer)
}
  


