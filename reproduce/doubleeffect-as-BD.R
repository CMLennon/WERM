source('WERM_Heuristic.R')

asBDEstimator = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]  
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)] 
  Y = OBS[,(D+4)]
  DATA = data.frame(W,X,R,Z,Y)
  
  ################################################################
  # Compute E[Y|x,z,w]
  ################################################################
  model.Y = learnXG(inVar = data.matrix(data.frame(W=W, X=X, R=R, Z=Z)),labelval = Y, regval = rep(0,nrow(DATA)))
  # model.Y = learnXG_Planid(sampled_df=DATA,inVarCol=c(1:(D+3)),labelval=Y,regval=rep(0,nrow(DATA)))
  vector0 = rep(0,nrow(DATA)); vector1 = rep(1,nrow(DATA))
  DATA.X0R0 = data.frame(W=W, X=vector0, R=vector0, Z=Z)
  DATA.X0R1 = data.frame(W=W, X=vector0, R=vector1, Z=Z)
  DATA.X1R0 = data.frame(W=W, X=vector1, R=vector0, Z=Z)
  DATA.X1R1 = data.frame(W=W, X=vector1, R=vector1, Z=Z)
  
  ################################################################
  # Predict
  ################################################################
  pred.X0R0 = predict(model.Y, newdata=data.matrix(DATA.X0R0),type='response')
  pred.X0R1 = predict(model.Y, newdata=data.matrix(DATA.X0R1),type='response')
  pred.X1R0 = predict(model.Y, newdata=data.matrix(DATA.X1R0),type='response')
  pred.X1R1 = predict(model.Y, newdata=data.matrix(DATA.X1R1),type='response')
  
  
  return(c(mean(pred.X0R0),mean(pred.X0R1),
           mean(pred.X1R0),mean(pred.X1R1)
           ))
}





