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

WERMEstimator = function(OBS,D){
    W = OBS[,1:D] 
    Z = OBS[,(D+1)] 
    X = OBS[,(D+2)]  
    Y = OBS[,(D+3)]
    DATA = data.frame(W,Z,X,Y)
    
    tmp = SplitDataset(DATA)
    DATA.train = tmp[[1]]; DATA.eval = tmp[[2]]
    Wtrain = DATA.train[,1:D]; Weval = DATA.eval[,1:D]
    Ztrain = DATA.train[,(D+1)]; Zeval = DATA.eval[,(D+1)]

    ################################################################
    # \hat{W*}. Suppose we assume this is given. 
    ################################################################
    SW_importance_sampling  = myComputeSW.lowhigh(outVar.train = Ztrain,
                                                inVar.train = Wtrain,
                                                outVar.eval = Zeval,
                                                inVar.eval = Weval, outputname = 'Z') 

    ################################################################
    # Learn h and W.
    ################################################################
    regvallist = seq(0,10,by=0.2)
    tmp = SplitDataset(DATA)
    DATA.train = tmp[[1]]; DATA.eval = tmp[[2]]
    Wtrain = DATA.train[,1:D]; Weval = DATA.eval[,1:D]
    Ztrain = DATA.train[,(D+1)]; Zeval = DATA.eval[,(D+1)]
    Xtrain = DATA.train[,(D+2)]; Xeval = DATA.eval[,(D+2)]
    Ytrain = DATA.train[,(D+3)]; Yeval = DATA.eval[,(D+3)]
    
    lambda_W = learnHyperParam(regvallist,data.matrix(data.frame(W=Wtrain,Z=Ztrain)),SW_importance_sampling,0)
    learned_W = learnWdash(SW_importance_sampling,data.matrix(data.frame(W=Weval,Z=Zeval)),lambda_W)
    lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=Xtrain)),Ytrain,1)

    Yx0 = WERM_Heuristic(inVar_train = data.frame(X=Xtrain,Z=Ztrain), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Zeval), Y = Ytrain, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
    Yx1 = WERM_Heuristic(inVar_train = data.frame(X=Xtrain,Z=Ztrain), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Zeval), Y = Ytrain, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
    WERManswer = c(Yx0,Yx1)

    return(WERManswer)
}





