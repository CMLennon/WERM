source('WERM_Heuristic.R')

fix_answer = function(quantity){
  quantity = min(quantity,1) 
  quantity = max(0,quantity)
  return(quantity)
}

asIPWEstimator = function(OBS,D,numCate){
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
  model.X = learnXG(inVar = data.matrix(data.frame(W=W, Z=Z)),labelval = X, regval = rep(0,nrow(DATA)))
  pred.X = predict(model.X, newdata = data.matrix(data.frame(W=W, Z=Z)),type='response')
  prob.X = mean(X)*pred.X + (1-mean(X))*pred.X
  
  Ix0 = ((X==0)*1)
  Ix1 = ((X==1)*1)
  
  ################################################################
  # Predict
  ################################################################
  pred.X0 = fix_answer(mean((Y*Ix0)/prob.X,na.rm=T))
  pred.X1 = fix_answer(mean((Y*Ix1)/prob.X,na.rm=T))
  
  
  return(c(mean(pred.X0),mean(pred.X1)))
}





