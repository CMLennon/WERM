library(xgboost)
library(boot)

ExpYParam_xg  = function(allpossible,DATA){
  inVar = data.matrix(DATA[,c(1:(ncol(DATA)-1))])
  predInVar = data.matrix(allpossible[,c(1:(ncol(allpossible)))])
  outVar = DATA$Y
  model_xgboost = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=0,alpha=0, objective = "binary:logistic")
  predval = predict(model_xgboost,newdata=predInVar,type='response')
  Ytable = allpossible
  Ytable$Y = predval
  return(Ytable)
}

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

PlugInEstimator = function(OBS,D,numCate){
  W = OBS[,1:D] 
  X = OBS[,(D+1)]
  Z = OBS[,(D+2)] 
  Y = OBS[,(D+3)]
  DATA = data.frame(W,X,Z,Y)
  
  ################################################################################ 
  # Enumerate all possible values of column
  ################################################################################
  C = 1 
  tmp = c()
  for (d in 1:D){
    tmp = append(tmp,list(c(0:C))) # W
  }
  Wname = paste("W",1:D,sep="")
  tmp = append(tmp,list(c(0,1))) # X
  tmp = append(tmp, list(c(0:1))) # Z
  allpossible = expand.grid(tmp)
  colnames(allpossible) = c(Wname,'X','Z')
  
  ################################################################################ 
  ################################################################################
  # Conditional Probability Table 
  ################################################################################
  ################################################################################
  # E[Y|X,Z,W]
  Ytable = ExpYParam_xg(allpossible,DATA)  
  
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
  PwTable = allpossible
  PwTable[,(ncol(allpossible)+1)] = tmp
  colnames(PwTable)[ncol(PwTable)] = 'prob'
  ################################################################################
  
  ################################################################################
  # Learn P(x|w)
  ################################################################################
  PxTable = allpossible 
  inVar = data.matrix(W)
  outVar = X 
  predInVar = data.matrix(allpossible[,c(1:D)])
  model.X = xgboost(verbose=0, data=inVar,label=outVar, nrounds = 20,max.depth=10,lambda=0,alpha=0, objective = "binary:logistic")
  predX = predict(model.X,newdata = predInVar,type="response")
  tmp = predX*allpossible$X + (1-predX)*(1-allpossible$X)
  PxTable[,(ncol(allpossible)+1)] = tmp
  colnames(PxTable)[ncol(PxTable)] = 'prob'
  ################################################################################
  
  ################################################################################
  # Learn P(z|x,w)
  ################################################################################
  PzTable = allpossible 
  inVar = data.matrix(data.frame(W,X))
  outVar = Z 
  predInVar = data.matrix(allpossible[,c(1:(D+1))])
  model.Z = xgboost(verbose=0, data=inVar,label=outVar, nrounds = 20,max.depth=10,lambda=0,alpha=0, objective = "binary:logistic")
  predZ = predict(model.Z,newdata = predInVar,type="response")
  tmp = predZ*allpossible$Z + (1-predZ)*(1-allpossible$Z)
  PzTable[,(ncol(allpossible)+1)] = tmp
  colnames(PzTable)[ncol(PzTable)] = 'prob'
  ################################################################################
  
  
  ################################################################################ 
  # Compute! 
  ################################################################################
  # Store the original table 
  allpossibleOrig = allpossible
  ComputeVal = allpossible
  
  # Compute E[Y|W,X,Z] * P(X|W)
  ComputeVal$YX = Ytable$Y * PxTable$prob
  # Compute P(Z|X,W) * P(W)
  ComputeVal$ZW = PzTable$prob * PwTable$prob
  # Marginalize for X 
  Margin.over.x.YX = ComputeVal[ComputeVal$X==0,'YX'] + ComputeVal[ComputeVal$X==1,'YX']
  # Fix X=x for P(Z|x,W) * P(W)
  ZW.X0 = ComputeVal[ComputeVal$X==0,'ZW']
  ZW.X1 = ComputeVal[ComputeVal$X==1,'ZW']
  # Compute Causal effect 
  Yx0 = sum(Margin.over.x.YX * ZW.X0)
  Yx1 = sum(Margin.over.x.YX * ZW.X1)
  myans = c(Yx0,Yx1)
  return(myans)
}


