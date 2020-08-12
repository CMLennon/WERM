source('WERM_Heuristic.R')

asBDEstimator = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]
  Z = OBS[,(D+2)] 
  Y = OBS[,(D+3)]
  DATA = data.frame(W,Z,X,Y)
  
  ################################################################
  # Compute E[Y|x,z,w]
  ################################################################
  model.Y = learnXG(inVar = data.matrix(data.frame(W=W, X=X,Z=Z)),labelval = Y, regval = rep(0,nrow(DATA)))
  # model.Y = learnXG_Planid(sampled_df=DATA,inVarCol=c(1:(D+2)),labelval=Y,regval=rep(0,nrow(DATA)))
  vector0 = rep(0,nrow(DATA)); vector1 = rep(1,nrow(DATA))
  DATA.X0 = data.frame(W=W, X=vector0, Z=Z)
  DATA.X1 = data.frame(W=W, X=vector1, Z=Z)
  
  ################################################################
  # Predict
  ################################################################
  pred.X0 = predict(model.Y, newdata=data.matrix(DATA.X0),type='response')
  pred.X1 = predict(model.Y, newdata=data.matrix(DATA.X1),type='response')

  return(c(mean(pred.X0),mean(pred.X1)))
}





