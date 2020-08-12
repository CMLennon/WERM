source('WERM_Heuristic.R')

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

asBDEstimator = function(OBS,D,numCate){
  W = OBS[,1:D] 
  Z = OBS[,(D+1)] 
  X = OBS[,(D+2)]  
  Y = OBS[,(D+3)]
  DATA = data.frame(W,Z,X,Y)
  
  ################################################################
  # Compute E[Y|x,z,w]
  ################################################################
  model.Y = learnXG(inVar = data.matrix(data.frame(W=W,Z=Z,X=X)),labelval = Y, regval = rep(0,nrow(DATA)))
  DATA.X0 = data.frame(W=W,Z=Z,X=rep(0,nrow(DATA)))
  DATA.X1 = data.frame(W=W,Z=Z,X=rep(1,nrow(DATA)))
  
  ################################################################
  # Predict
  ################################################################
  pred.X0 = predict(model.Y, newdata=data.matrix(DATA.X0),type='response')
  pred.X1 = predict(model.Y, newdata=data.matrix(DATA.X1),type='response')
  
  return(c(mean(pred.X0),mean(pred.X1)))
}





