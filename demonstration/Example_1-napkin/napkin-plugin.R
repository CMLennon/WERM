  library(xgboost)
  library(boot)

  ################################
  # Dataset Generation 
  ################################
  source('napkin-data.R') # Napkin dataset generation code 
  source('napkin-WERM.R')
  source('../WERM_Heuristic.R')

  # N = 1000
  # Nintv = 1000000
  # D = 10 # Cardinality of W 
  # numCate = 2
  # C = numCate - 1
  # 
  # seednum = sample(1:1000000,1)
  # mytmp = dataGen(seednum,N,Nintv,D,C)
  # OBS = mytmp[[1]] # Observational dataset 
  # INTV = mytmp[[2]]


highdim_reg_xgboost = function(OBS, outVarVector){
  ### P(W), P(W|X)
  for (d in 1:D){
    outVar = data.matrix(outVarVector[,d])
    if (d > 1){
      # P(r)
      inVar = data.matrix(outVarVector[,c(1:(d-1))])
      MyModel = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=0,alpha=0, objective = "binary:logistic")
      list.Pr = c(list.Pr, list(MyModel))
    }else{
      inVar = data.matrix(rep(1,nrow(OBS)))
      MyModel = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=0,alpha=0, objective = "binary:logistic")
      list.Pr = list(MyModel)
    }
  }
  return(list.Pr)
}


PlugInEstimator = function(OBS,D){
  W = OBS[,1:D] 
  R = OBS[,(D+1)] 
  X = OBS[,(D+2)]  
  Y = OBS[,(D+3)]
  DATA = data.frame(W,R,X,Y)
  
  Wunique = unique(W[,1])[order(unique(W[,1]))]
  Runique = unique(R)[order(unique(R))]
  Xunique = unique(X)[order(unique(X))]
  Yunique = unique(Y)[order(unique(Y))]

  ################################################################################ 
  # Enumerate all possible values of column
  ################################################################################
  tmp = c()
  for (d in 1:D){
    tmp = append(tmp,list(Wunique)) # W 
  }
  Wname = paste("W",1:D,sep="")
  tmp = append(tmp,list(Runique)) # R 
  tmp = append(tmp,list(Xunique)) # X 
  allpossible = expand.grid(tmp)
  colnames(allpossible) = c(Wname,'R','X')
  ################################################################################

  ################################################################################ 
  # Learn prob Y 
  ################################################################################
  inVarMat_Y = as.matrix(DATA[,1:(ncol(DATA)-1)])
  evalMat_Y = as.matrix(allpossible[,1:(ncol(allpossible))])
  model_Y = learnXG(inVar=inVarMat_Y,labelval=Y,regval=rep(0,nrow(DATA)))
  pred_Y = predict(model_Y,newdata=evalMat_Y,type='response')
  allpossible[,'prob.Y'] = pred_Y
  # Ytable = allpossible 
  # Ytable[,'prob'] = pred_Y

  ################################################################################
  # Learn P(x|z,w)
  ################################################################################
  inVarMat_X = as.matrix(data.frame(W,R))
  evalMat_X = as.matrix(allpossible[,c(1:(D+1))])
  model_X = learnXG(inVar=inVarMat_X,labelval=X,regval=rep(0,nrow(DATA)))
  pred_X = predict(model_X,newdata=evalMat_X,type='response')
  prob_X = pred_X*allpossible$X + (1-pred_X)*(1-allpossible$X)
  allpossible[,'prob.X.ZW'] = prob_X
  # Xtable = allpossible 
  # Xtable[,'prob'] = prob_X

  ################################################################################
  # Learn P(w)
  ################################################################################
  model.W = highdim_reg_xgboost(OBS,W)
  tmp = rep(1,nrow(allpossible))
  for (d in 1:D){
    if (d == 1){
      predInVar = data.matrix(rep(1,nrow(allpossible)))
    }else{
      predInVar = data.matrix(allpossible[,c(1:(d-1))])
    }
    predval = predict(model.W[[d]],newdata=predInVar,type='response') # P(Zd =1 | Z(d-1),Z(d-2),...,Z(1),X)
    # predval[predval < 0] = 1e-8
    # predval[predval > 1] = 1 - 1e-8
    resultval = predval * allpossible[d] + (1-predval) * (1-allpossible[d])
    tmp = tmp * resultval
  }
  allpossible[,'prob.W'] = tmp
  # Wtable = allpossible
  # Wtable[,(ncol(allpossible)+1)] = tmp
  # colnames(Wtable)[ncol(Wtable)] = 'prob'
    
  ################################################################################
  # Evaluate 
  ################################################################################
  allpossibleOrig = allpossible

  ComputeVal = allpossible
  ComputeVal$val1 = Ytable$prob * Xtable$prob * Wtable$prob
  ComputeVal$val2 = Xtable$prob * Wtable$prob
  
  ## Marginalizing over W
  tmp = c()
  tmp = append(tmp, list(c(0:1))) # R
  tmp = append(tmp,list(c(0,1))) # X
  allpossible = expand.grid(tmp)
  colnames(allpossible) = c('R','X')
  ComputeVal.MarginW = allpossible
  ComputeVal.MarginW$val1 = 0
  ComputeVal.MarginW$val2 = 0
  
  for (rval in (0:1)){
    for(xval in (0:1)){
      ComputeVal.MarginW[ComputeVal.MarginW$R==rval & ComputeVal.MarginW$X==xval,'val1'] = sum(ComputeVal[ComputeVal$X==xval & ComputeVal$R==rval,'val1'],na.rm=T)
      ComputeVal.MarginW[ComputeVal.MarginW$R==rval & ComputeVal.MarginW$X==xval,'val2'] = sum(ComputeVal[ComputeVal$X==xval & ComputeVal$R==rval,'val2'],na.rm=T)
    }
  }
  ComputeVal.MarginW$val3 = exp(log(ComputeVal.MarginW$val1) - log(ComputeVal.MarginW$val2))
  
  Yx0 = mean(ComputeVal.MarginW[ComputeVal.MarginW$X==0,'val3'],na.rm = T)
  Yx1 = mean(ComputeVal.MarginW[ComputeVal.MarginW$X==1,'val3'],na.rm = T)

  return(c(Yx0,Yx1))
}

