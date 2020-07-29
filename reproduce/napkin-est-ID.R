remove_outliers <- function(x, lcf, ucf) {
  medianX = median(x)
  qnt <- quantile(x, probs=c(lcf, ucf), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] = medianX
  y[x > (qnt[2] + H)] = medianX
  y
}

removeOutlier = function(x,lcf,ucf){
  if (max(x) < 2){
    return(x)
  }
  
  if((max(x) >= 2) & (max(x) < 5)){
    x = remove_outliers(x,lcf,ucf)
    return(x)
  }else{
    x[x>5] = NA
    medX = median(x,na.rm=T)
    x[is.na(x)] = medX
    x = remove_outliers(x,lcf,ucf)
    return(x)
  }
}

timeoutFun = function(Fun, mytime){
  result = withTimeout({
    Fun
  }, timeout = mytime, onTimeout = "silent")
  return(result)
}

MakeModel.highlow = function(W,X,D,OBS,outputname,inputname){
  # Q[Y | Z]
  ## Compute weight
  
  ### P(W), P(W|X)
  for (d in 1:D){
    # Compute Wi ~ W(i-1) + ... + W(1) + X 
    output.name = paste(outputname,d,sep="")
    if (d > 1){
      # P(r)
      input.name = paste(outputname,1:(d-1),sep="")
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = c(list.Pr, list(MyModel))
      
      #P(r|v)
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = c(list.PrX, list(MyModel.X))
    }else{
      input.name = "1"
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = list(MyModel)
      
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = list(MyModel.X)
    }
  }
  return(list(list.Pr,list.PrX))
}


# Weight for Xa 
ComputeSW.highlow = function(W,X,D, OBS, outputname, inputname){
  # Q[Y | Z]
  ## Compute weight
  
  ### P(W), P(W|X)
  for (d in 1:D){
    # Compute Wi ~ W(i-1) + ... + W(1) + X 
    output.name = paste(outputname,d,sep="")
    if (d > 1){
      # P(r)
      input.name = paste(outputname,1:(d-1),sep="")
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = c(list.Pr, list(MyModel))
      
      #P(r|v)
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = c(list.PrX, list(MyModel.X))
    }else{
      input.name = "1"
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = list(MyModel)
      
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = list(MyModel.X)
    }
  }
  
  # Compute SWr 
  ## Compute P(w) mean 
  Prob.W1 = rep(0,D)
  Prob.W0 = rep(0,D)
  for (d in 1:D){
    model.pw = list.Pr[[d]]
    Prob.W1[d] = mean(predict.glm(model.pw,newdata=OBS,type='response'))
    Prob.W0[d] = 1-Prob.W1[d]
  }
  
  ## Compute P(w|x) mean 
  Prob.W1X = matrix(0,nrow=numCate,ncol=D) # row: possible wval // col: z0,z1...
  Prob.W0X = matrix(0,nrow=numCate,ncol=D)
  for (d in 1:D){
    model.pwx = list.PrX[[d]]
    for (xval in 0:1){
      rowidx = xval+1
      if (nrow(OBS[OBS$X==xval,]) == 0){
        Prob.W1X[rowidx,d] = 0
        Prob.W0X[rowidx,d] = 1-Prob.W1X[rowidx,d]
      }else{
        Prob.W1X[rowidx,d] = mean(predict.glm(model.pwx,newdata=OBS[OBS$X==xval,],type='response'))  
        Prob.W0X[rowidx,d] = 1-Prob.W1X[rowidx,d]
      }
    }
  }
  # Compute P(w) vector 
  ProbW = as.matrix(W) %*% diag(Prob.W1) + as.matrix(1-W)%*%diag(Prob.W0)
  # Compute P(w|x) vector 
  WX = cbind(W,X)
  tempWX = cbind(W,X)
  for (xval in 0:1){
    WXval = WX[WX$X== xval,][,1:D]
    if (nrow(WXval) == 0){
      next
    }else{
      rowidx = xval + 1 
      Prob.W1Xval = Prob.W1X[rowidx,]
      Prob.W0Xval = Prob.W0X[rowidx,]
      tempWX[tempWX$X == xval,1:D] = as.matrix(WXval) %*% diag(Prob.W1Xval) + as.matrix(1-WXval)%*%diag(Prob.W0Xval)  
    }
  }
  ProbWX = tempWX[,1:D]
  SWw = ProbW / (ProbWX + 1e-6)
  SWw = apply(SWw,1,prod)
  return(SWw)
}

ComputeSW.lowhigh = function(outVar, inVar, outputname){
  # Compute P(outputVal|inputVal1,...,inputValD)
  inVar_margin = data.matrix(rep(1,length(outVar)))
  model_xgboost = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  model_xgboost_margin = xgboost(verbose = 0, data = inVar_margin, label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  
  Prob.outVar.1 = predict(model_xgboost_margin,newdata=inVar_margin,type='response')
  Prob.outVar.0 = 1 - Prob.outVar.1
  Prob.outVar = diag(Prob.outVar.1) %*% as.matrix(outVar) + diag(Prob.outVar.0) %*% as.matrix(1-outVar)
  
  Prob.outVar.1.giveninVar = predict(model_xgboost,newdata=data.matrix(inVar),type="response")
  Prob.outVar.0.giveninVar = 1-Prob.outVar.1.giveninVar
  Prob.outVar.inVar = diag(Prob.outVar.1.giveninVar) %*% as.matrix(outVar) + diag(Prob.outVar.0.giveninVar) %*% as.matrix(1-outVar)
  SWXX = Prob.outVar / (Prob.outVar.inVar)
  # SWXX = 1 / (Prob.outVar.inVar)
  # SWXX[SWXX < 0] = runif(length(SWXX[SWXX < 0]),min=0,max=min(abs(SWXX)))
  return(SWXX)
  
  # formula.XX = as.formula(paste(paste(paste(outputname,'~',sep=""),paste(colnames(inVar),collapse="+")),"+1",sep=""))
  # formula.marginXX = as.formula(paste(outputname,'~ 1'))
  # modelXX = glm(formula.XX, data=OBS, family= binomial())
  # model_marginXX = glm(formula.marginXX, data=OBS, family= binomial())
  # # modelXX =  svyglm(formula.XX, design = svydesign(~1, weights=1, data=OBS,family=quasibinomial()))
  # # model_marginXX =svyglm(formula.marginXX, design = svydesign(~1, weights=~SWw, data=OBS,family=quasibinomial()))
  # 
  # ## Compute P(outVar) mean
  # Prob.outVar.1 = predict.glm(model_marginXX,newdata=OBS,type='response')
  # Prob.outVar.0 = 1 - Prob.outVar.1
  # Prob.outVar = diag(Prob.outVar.1) %*% as.matrix(outVar) + diag(Prob.outVar.0) %*% as.matrix(1-outVar)
  # 
  # ## Compute P(outVar|inVar1,...,inVarD) mean
  # Prob.outVar.1.giveninVar = predict.glm(modelXX,newdata=OBS,type="response")
  # Prob.outVar.0.giveninVar = 1-Prob.outVar.1.giveninVar
  # Prob.outVar.inVar = diag(Prob.outVar.1.giveninVar) %*% as.matrix(outVar) + diag(Prob.outVar.0.giveninVar) %*% as.matrix(1-outVar)
  # SWXX = Prob.outVar / (Prob.outVar.inVar)
  # return(SWXX)
}

highdim_reg = function(OBS, outputname, inputname){
  ### P(W), P(W|X)
  
  for (d in 1:D){
    # Compute Wi ~ W(i-1) + ... + W(1) + X 
    output.name = paste(outputname,d,sep="")
    if (d > 1){
      # P(r)
      input.name = paste(outputname,1:(d-1),sep="")
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = c(list.Pr, list(MyModel))
      
      #P(r|v)
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = c(list.PrX, list(MyModel.X))
    }else{
      input.name = "1"
      tmpModel = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name,collapse="+")))
      MyModel = glm(tmpModel,family=binomial(),data=OBS)
      list.Pr = list(MyModel)
      
      input.name.X = c(input.name,inputname)
      tmpModel.X = as.formula(paste(paste(output.name,'~',sep=""),paste(input.name.X,collapse="+")))
      MyModel.X = glm(tmpModel.X,family=binomial(),data=OBS)
      list.PrX = list(MyModel.X)
    }
  }
  return(list.PrX)
}

UnivRegress = function(OBS, XX, output, outputname){
  myformula = as.formula(paste(paste(outputname,'~',sep=""),paste(colnames(XX),collapse="+")))
  mymodel = glm(myformula, data=OBS, family= quasibinomial())
  myprob1 = predict.glm(mymodel,newdata=OBS,type="response",control=list(maxit=100))
  myprob0 = 1-myprob1
  myprob = diag(myprob1) %*% as.matrix(output) + diag(myprob0) %*% as.matrix(1-output)
  return(myprob)
}

addSmallNoise = function(dat){
  for (colidx in 1:dim(dat)[2]){
    dat[,colidx] = dat[,colidx] + 1e-4*rnorm(length(dim(dat)[1]),0,1)
  }
  return(dat)
}

sampler = function(data, SW_zw){
  ################################
  # Sampler
  ################################
  numidx = 0
  numsampled = 0
  norm_SW = SW_zw/max(SW_zw)
  sampled_df = data.frame()
  num_draw = 1*nrow(data)
  while(1){
    numidx = numidx + 1
    taking_idx = (colMeans(sapply(norm_SW,function(p) rbinom(10,1,p))) > 0.5)
    resampled_data =  data[sample(nrow(data),replace = TRUE), ]
    rownames(resampled_data) = rownames(data)
    sampled_data = resampled_data[taking_idx,]
    sampled_df = rbind(sampled_df,sampled_data)
    numsampled = numsampled + nrow(sampled_data)
    if (numsampled >= num_draw){
      break
    }
  }
  rownames(sampled_df) = c(1:nrow(sampled_df))
  sampled_df = sampled_df[1:num_draw,]
  return(sampled_df)
}

multiID = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  Z = OBS[,D+1] 
  X = OBS[,D+2]  
  Y = OBS[,D+3]
  DATA = data.frame(W,Z,X,Y)
  
  ################################
  # Compute SW 
  ################################
  # Importance sampling based 
  SW_importance_sampling  = ComputeSW.lowhigh(Z,W,'Z')
  learned_W = SW_importance_sampling
  lambda_h = rep(0,nrow(OBS))
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  myans = c(Yx0,Yx1)
  
  return(myans)
}
# 
# source('napkin-data.R')
# source('napkin-naive.R')
# source('napkin-param.R')
# 
# require(stats)
# 
# N = 1000
# Nintv = 1000000
# 
# D = 10
# numCate = 2
# C = numCate - 1
# 
# seednum = sample(1:1000000,1)
# # seednum = 987
# mytmp = dataGen(seednum,N,Nintv,D,C)
# OBS = mytmp[[1]]
# INTV = mytmp[[2]]
# 
# answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
# obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))
# 
# naive_start = Sys.time()
# # naiveanswer = naiveAdj(OBS,D,numCate)
# # naiveperformance = mean(abs(answer-naiveanswer),na.rm=T)
# naiveperformance = 0.5
# naive_end = Sys.time()
# naive_runtime = naive_end - naive_start
# print(paste("Naive Runtime:",round(naive_runtime,2),sep=""))
# 
# param_start = Sys.time()
# paramanswer = paramAdj(OBS,D,numCate)
# paramperformance = mean(abs(answer-paramanswer),na.rm=T)
# param_end = Sys.time()
# param_runtime = param_end - param_start
# print(paste("Param Runtime:",round(param_runtime,2),sep=""))
# 
# multi_start = Sys.time()
# multianswer = multiAdj(OBS,D,numCate)
# multiperformance = mean(abs(answer-multianswer),na.rm=T)
# multi_end = Sys.time()
# multi_runtime = multi_end - multi_start
# print(paste("Multi Runtime:",round(multi_runtime,2),sep=""))
# 
# print(paste("OBS: ",round(mean(abs(answer-obsans)),2),sep=""))
# 
# # print(c(paste("Naive: ",round(naiveperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
# print(c(paste("Naive: ",round(naiveperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
# modelname = c("Naive","Param","Multi")
# modelperformance = c(naiveperformance,paramperformance,multiperformance)
# print(paste("Winner: ",modelname[which.min(modelperformance)],sep=""))




