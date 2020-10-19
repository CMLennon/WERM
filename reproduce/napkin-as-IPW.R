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
  Z = OBS[,(D+1)] 
  X = OBS[,(D+2)]  
  Y = OBS[,(D+3)]
  DATA = data.frame(W,Z,X,Y)
  
  ################################################################
  # Compute P(X | W,Z)
  ################################################################
  model.X = learnXG(inVar = data.matrix(data.frame(W=W, Z=Z)),labelval = X, regval = rep(0,nrow(DATA)))
  prob.X1 = predict(model.X, newdata = data.matrix(data.frame(W=W, Z=Z)),type='response')
  prob.X0 = 1-prob.X1
  Ix0 = (X==0)*1
  Ix1 = (X==1)*1
  
  ################################################################
  # Predict
  ################################################################
  # pred.X0R0 = fix_answer(mean((Y*Ix0r0)/(prob.X*prob.R),na.rm=T))
  # pred.X0R1 = fix_answer(mean((Y*Ix0r1)/(prob.X*prob.R),na.rm=T))
  # pred.X1R0 = fix_answer(mean((Y*Ix1r0)/(prob.X*prob.R),na.rm=T))
  # pred.X1R1 = fix_answer(mean((Y*Ix1r1)/(prob.X*prob.R),na.rm=T))
  
  pred.X0 = mean((Y*Ix0)/(prob.X0),na.rm=T)
  pred.X1 = mean((Y*Ix1)/(prob.X1),na.rm=T)
  
  return(c(mean(pred.X0),mean(pred.X1)))
}





