source('../WERM_Heuristic.R')

# A function for estimating \hat{W*}. This function is dependent on the graph. 
myComputeSW.lowhigh = function(outVar.train, inVar.train, outVar.eval, inVar.eval, outputname){
  # Compute P(outputVal|inputVal1,...,inputValD)
  inVar_margin = data.matrix(rep(1,length(outVar.train)))
  model_xgboost = xgboost(verbose = 0, data = data.matrix(inVar.train), label = outVar.train, nrounds = 20,max.depth=10,lambda=1/length(outVar.train),alpha=1/length(outVar.train), objective = "binary:logistic")
  model_xgboost_margin = xgboost(verbose = 0, data = inVar_margin, label = outVar.eval, nrounds = 20,max.depth=10,lambda=1/length(outVar.train),alpha=1/length(outVar.train), objective = "binary:logistic")
  
  Prob.outVar.1 = predict(model_xgboost_margin,newdata=inVar_margin,type='response')
  Prob.outVar.0 = 1 - Prob.outVar.1
  Prob.outVar = diag(Prob.outVar.1) %*% as.matrix(outVar.eval) + diag(Prob.outVar.0) %*% as.matrix(1-outVar.eval)
  
  Prob.outVar.1.giveninVar = predict(model_xgboost,newdata=data.matrix(inVar.eval),type="response")
  Prob.outVar.0.giveninVar = 1-Prob.outVar.1.giveninVar
  Prob.outVar.inVar = diag(Prob.outVar.1.giveninVar) %*% as.matrix(outVar.eval) + diag(Prob.outVar.0.giveninVar) %*% as.matrix(1-outVar.eval)
  SWXX = Prob.outVar / (Prob.outVar.inVar)
  return(SWXX)
}

SplitDataset = function(DATA){
  all_idx = c(1:nrow(DATA))
  train_idx = sample(c(1:nrow(DATA)),nrow(DATA)/2)
  eval_idx = setdiff(all_idx,train_idx)
  DATA.train = DATA[train_idx,]
  rownames(DATA.train) = c(1:nrow(DATA.train))
  DATA.eval = DATA[eval_idx,]
  rownames(DATA.eval) = c(1:nrow(DATA.eval))
  return(list(DATA.train,DATA.eval))
}

asBDEstimator = function(OBS,D){
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





