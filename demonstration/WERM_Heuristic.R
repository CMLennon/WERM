mylossfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )
  return(mean(weightval * pred.loss) + sqrt(LW))
}

mygradfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 ))
  return(mean(pred.loss) + (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel))
}

learnHyperParam = function(regvallist,invar,mylabel,learningbinary){
  ############################
  # Objective
    # Learn the hyperparameter \lambda_W, \lambda_h
  # Input 
    # regvallist: A candidate list of the hyperparameters; i.e., We are choosing \lambda_W, \lambda_h from this list. 
    # invar: An input covariate R 
    # mylabel: An output Y 
    # learningbinary: 1 if the output is binary. 0 Otherwise. 
  # Output 
    # Chosen hyperparameter
  # Idea: 
    # We split the input covariate (inVar) into training/testing samples. 
    # For each hyperparameter in regvallist, we train the model using the training sample and record the performance on the test samples. 
    # Then, we output the hyperparameter returning the best performance on the test samples. 
  ############################
  
  ############################
  # Randomly split the covariates invar into two groups 
    # (1/3) of covariates are used for traning 
    # the rest (2/3) of covariates are used for testing  
  ############################
  trainidx = sample(1:nrow(invar),nrow(invar)/3) # Randomly split the data index (1/3, 2/3). 
  testidx = setdiff(c(1:nrow(invar)),trainidx)
  DATAtrain = invar[trainidx,] # An input covariate used for training  
  DATAtest = invar[testidx,] # An input covariate used for testing 
  
  ############################
  # For each hyperparameter in regvallist, we train the model using the training sample and record the performance on the test samples. 
  ############################
  performancerecord = rep(0,length(regvallist)) # A list for storing the performance 
  for (idx in 1:length(regvallist)){ 
    regval = regvallist[idx]/nrow(invar) # For each hyperparameter
    # Split the output vector (mylabel) into training/test 
    if (is.null(dim(mylabel)) == TRUE){
      trainlabel = mylabel[trainidx]
      testlabel = mylabel[testidx]
    }else{
      trainlabel = mylabel[trainidx,]
      testlabel = mylabel[testidx,]  
    }
    
    if (learningbinary == 0){ # If the output value is non-binary
      model_XG = xgboost(verbose = 0, data = as.matrix(DATAtrain), label = trainlabel, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
      predval = predict(model_XG,newdata=as.matrix(DATAtest))
      performancerecord[idx] = sum(abs(predval - testlabel))  
    }else{ # If the output value is binary
      model_XG = xgboost(verbose = 0, data = as.matrix(DATAtrain), label = trainlabel, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2,objective = "binary:logistic")
      predval = predict(model_XG,newdata=as.matrix(DATAtest),type='response')
      predval = (predval >= 0.5)*1
      performancerecord[idx] = sum((predval != testlabel ))
    }
  }
  minimum_error = performancerecord[which.min(performancerecord)]
  regvalcand = regvallist[performancerecord == minimum_error]
  regval = median(regvalcand)
  return(regval)
}

learnXG = function(inVar,labelval,regval){
  ############################
  # Objective
    # Learn XGBoost model 
  # Input 
    # sampled df: data 
    # inVarCol: Column index for the input 
    # labelval: Label
    # regval: Hyperparameter lambda h
  # Output 
    # learnt XGboost model 
  ############################
  # regval = ((max(labelval) - min(labelval))**1)/nrow(data_sampled)
  model_XG = xgboost(verbose = 0, data = inVar, label = labelval, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  return(model_XG)
}

learnXG_Planid = function(sampled_df,inVarCol,labelval,regval){
  ############################
  # Objective
    # Learn XGBoost model 
  # Input 
    # sampled df: data 
    # inVarCol: Column index for the input 
    # labelval: Label
    # regval: Hyperparameter lambda h
  # Output 
    # learnt XGboost model 
  ############################

  W = sampled_df[,1:D] 
  X = sampled_df[,(D+1)]
  R = sampled_df[,(D+2)]  
  Z = sampled_df[,(D+3)]  
  Y = sampled_df[,(D+4)]
  data_sampled = data.frame(W,X,R,Z,Y)
  
  inVar = as.matrix(data_sampled[,inVarCol])
  # regval = ((max(labelval) - min(labelval))**1)/nrow(data_sampled)
  model_XG = xgboost(verbose = 0, data = inVar, label = labelval, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  return(model_XG)
}


learnXG_mediator = function(sampled_df,inVarCol,labelval,regval){
  ############################
  # Objective
    # Learn XGBoost model 
  # Input 
    # sampled df: data 
    # inVarCol: Column index for the input 
    # labelval: Label
    # regval: Hyperparameter lambda h
  # Output 
    # learnt XGboost model 
  ############################

  W = sampled_df[,1:D] 
  X = sampled_df[,(D+1)]  
  Z = sampled_df[,(D+2)] 
  Y = sampled_df[,(D+3)]
  data_sampled = data.frame(W,X,Z,Y)
  
  inVar = as.matrix(data_sampled[,inVarCol])
  # regval = ((max(labelval) - min(labelval))**1)/nrow(data_sampled)
  model_XG = xgboost(verbose = 0, data = inVar, label = labelval, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  return(model_XG)
}


learnWdash = function(Wdash1_importance,inVar,regval){
  ############################
  # Objective
    # Given the weight \hat{W^*}, we want to approximate W. 
  # Input 
    # Wdash1_importance: \hat{W^*}
    # inVar: Input covariates. The approximated W is a function of inVar; i.e., W(inVar)
    # regval: Hyperparameter \lambda_W
  # Output 
    # W, an approximated W*
  ############################
  
  # Construct the xgboost regression model 
  model_Wdash1 = xgboost(verbose = 0, data = inVar, label = Wdash1_importance, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  Wdash1 = predict(model_Wdash1,newdata = inVar, type='response')
  
  # Handling if W contains the negative value. 
  if (length(Wdash1[Wdash1 < 0 ]) > 0){
    Wdash1[Wdash1 < 0 ] = runif(n=length(Wdash1[Wdash1 < 0 ]),min=0,max=min(abs(Wdash1)))  
  }
  return(Wdash1)
}

WERM_Sampler = function(DATA, myWeight){
  ################################
  # Objective 
    # Given the dataset D (samples of the distribution P) and myWeight W, generate the dataset D^W that could play as samples drawn from P^W. 
  # Input 
    # DATA: Samples from P 
    # myWeight: Weight. 
  ################################
  numidx = 0
  numsampled = 0
  norm_SW = myWeight/max(myWeight) # Normed weight ranging [0,1]
  sampled_df = data.frame() # Sampled data.frame
  num_draw = 1*nrow(DATA)
  mediator_itermax = 2*max(round(max(myWeight)),1)
  ################################
  # Sampling! 
  ################################
  while(1){
    numidx = numidx + 1
    # taking_idx = (colMeans(sapply(norm_SW,function(p) rbinom(10,1,p))) > 0.5)

    # Draw (0,1) samples from the Bernoulli distribution. The number of samples matches with the length of the dataset. 
    taking_idx = sapply(norm_SW,function(p) rbinom(1,1,p)) > 0.5 
    # Take samples from DATA whenever taking_idx = 1 
    resampled_DATA =  DATA[sample(nrow(DATA),replace = TRUE), ]
    rownames(resampled_DATA) = rownames(DATA)
    sampled_DATA = resampled_DATA[taking_idx,]
    sampled_df = rbind(sampled_df,sampled_DATA)
    # Count the number of subsamples. 
    numsampled = numsampled + sum(taking_idx)
    # break 
    if (numsampled >= num_draw){
      break
    }
    if (numidx >= mediator_itermax){
      sampled_df =  DATA
      break 
    }
  }
  rownames(sampled_df) = c(1:nrow(sampled_df))
  sampled_df = sampled_df[1:num_draw,]
  return(sampled_df)
}

ComputeSW.lowhigh = function(outVar, inVar, outputname){
  # Compute P(outputVal|inputVal1,...,inputValD)
  inVar_margin = data.matrix(rep(1,length(outVar)))
  model_xgboost = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  model_xgboost_margin = xgboost(verbose = 0, data = inVar_margin, label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  
  Prob.outVar.1 = predict(model_xgboost_margin,newdata=inVar_margin,type='response')
  Prob.outVar.0 = 1 - Prob.outVar.1
  # Prob.outVar = diag(Prob.outVar.1) %*% as.matrix(outVar) + diag(Prob.outVar.0) %*% as.matrix(1-outVar)
  Prob.outVar = outVar * Prob.outVar.1 + (1-outVar) * Prob.outVar.0
  
  Prob.outVar.1.giveninVar = predict(model_xgboost,newdata=data.matrix(inVar),type="response")
  Prob.outVar.0.giveninVar = 1-Prob.outVar.1.giveninVar
  Prob.outVar.inVar = outVar * Prob.outVar.1.giveninVar + (1-outVar) * Prob.outVar.0.giveninVar
  # Prob.outVar.inVar = diag(Prob.outVar.1.giveninVar) %*% as.matrix(outVar) + diag(Prob.outVar.0.giveninVar) %*% as.matrix(1-outVar)
  SWXX = Prob.outVar / (Prob.outVar.inVar)
  return(SWXX)
}


WERM_Heuristic = function(inVar_train,inVar_eval, Y, Ybinary, lambda_h, learned_W){
  ############################
  # Objective
    # Given the input for the train, input for the evaluate and weight, learn the weighted regression function and evaluate the output. 
  # Input 
    # inVar_train: Input for the training. Example: (X,Z)
    # inVar_train: Input for the evaluating. Example: (X=0, Z) 
    # Y: Output label 
    # Ybinary: 1 if binary; 0 otherwise. 
    # lambda_h: Hyperparameter for the regression function 
    # lambda_W: Hyperparameter for the weight function 
  # Output 
    # estimated E_{P^W(y|x)}[Y|x]. 
  ############################
  xgbMatrix = xgb.DMatrix(data.matrix(inVar_train), label=Y) 
  if (Ybinary == 0){
    modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,weight = learned_W)
  }else{
    modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,objective = "binary:logistic",weight = learned_W)
  }
  Yx = mean(predict(modelY_xgboost,newdata=data.matrix(inVar_eval),type='response'))
  return(Yx)
}

WERMGradient = function(N,inputMat, labelVal, evalMat, lambda_h, lambda_W, iterMax, init_W, LossFun, GradFun){
  xgbMatrix = xgb.DMatrix(data.matrix(data.frame(inputMat)), label=labelVal)
  modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h, weight = learned_W)
  pred.Y = predict(modelY_xgboost,newdata=data.matrix(evalMat),type='response')
  # pred.Y = (pred.Y >= 0.5)*1
  # new_pred_loss = sum((pred.Y != labelVal)*1)/N
  # pred.Y = (pred.Y >= 0.5)*1
  new_pred_loss = sum((pred.Y-labelVal)^2)
  
  learned_W = init_W
  lossval = LossFun(new_pred_loss,learned_W,init_W,lambda_W)
  gradval = GradFun(new_pred_loss,learned_W,init_W,lambda_W)
  
  learning_rate_eta = 0.01
  cvg_criteria = 1 
  for (myiter in 1:iterMax){
    iteridx = 0
    old_pred_loss = new_pred_loss
    while(1){
      iteridx = iteridx +1 
      if (iteridx > iterMax){
        break
      }
      old_W = learned_W 
      # new_W = learned_W - invhessval %*% gradval
      new_W = learned_W - learning_rate_eta * gradval
      if (sum(abs(old_W - new_W),na.rm=T) < cvg_criteria){
        break 
      }
      if (length(new_W[new_W < 0]) > 0){
        break 
      }
      learned_W = new_W
      gradval = GradFun(old_pred_loss,learned_W,init_W,lambda_W)
    }
    
    # modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,objective = "binary:logistic",weight = learned_W)
    modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,weight = learned_W)
    pred.Y = predict(modelY_xgboost,newdata=data.matrix(evalMat),type='response')
    # pred.Y = (pred.Y >= 0.5)*1
    # new_pred.loss = sum((pred.Y != labelVal)*1)/N
    new_pred_loss = sum((pred.Y-labelVal)^2)
    if (abs(old_pred_loss - new_pred_loss) < 0.01){
      break 
    }
  }
  return(learned_W)
}



