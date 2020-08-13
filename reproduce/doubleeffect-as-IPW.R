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
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)] 
  Y = OBS[,(D+4)]
  DATA = data.frame(W,X,R,Z,Y)
  
  ################################################################
  # Compute E[Y|x,z,w]
  ################################################################
  model.X = learnXG(inVar = data.matrix(data.frame(W=W, Z=Z)),labelval = X, regval = rep(0,nrow(DATA)))
  model.R = learnXG(inVar = data.matrix(data.frame(W=W, Z=Z, X=X)),labelval = R, regval = rep(0,nrow(DATA)))
  prob.X1 = predict(model.X, newdata = data.matrix(data.frame(W=W, Z=Z)),type='response')
  prob.X0 = 1-prob.X1
  prob.R1 = predict(model.R, newdata = data.matrix(data.frame(W=W, Z=Z, X=X)),type='response')
  prob.R0 = 1-prob.R1
  # prob.X = mean(X)*pred.X + (1-mean(X))*pred.X
  # prob.R = mean(R)*pred.R + (1-mean(R))*pred.R
  
  Ix0r0 = ((X==0)*1)*((R==0)*1)
  Ix0r1 = ((X==0)*1)*((R==1)*1)
  Ix1r0 = ((X==1)*1)*((R==0)*1)
  Ix1r1 = ((X==1)*1)*((R==1)*1)
  
  ################################################################
  # Predict
  ################################################################
  # pred.X0R0 = fix_answer(mean((Y*Ix0r0)/(prob.X*prob.R),na.rm=T))
  # pred.X0R1 = fix_answer(mean((Y*Ix0r1)/(prob.X*prob.R),na.rm=T))
  # pred.X1R0 = fix_answer(mean((Y*Ix1r0)/(prob.X*prob.R),na.rm=T))
  # pred.X1R1 = fix_answer(mean((Y*Ix1r1)/(prob.X*prob.R),na.rm=T))
  
  pred.X0R0 = mean((Y*Ix0r0)/(prob.X0*prob.R0),na.rm=T)
  pred.X0R1 = mean((Y*Ix0r1)/(prob.X0*prob.R1),na.rm=T)
  pred.X1R0 = mean((Y*Ix1r0)/(prob.X1*prob.R0),na.rm=T)
  pred.X1R1 = mean((Y*Ix1r1)/(prob.X1*prob.R1),na.rm=T)
  
  
  return(c(mean(pred.X0R0),mean(pred.X0R1),
           mean(pred.X1R0),mean(pred.X1R1)
  ))
}





