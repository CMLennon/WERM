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
  ## Compute P(outputVal|inputVal1,...,inputValD)
  formula.XX = as.formula(paste(paste(paste(outputname,'~',sep=""),paste(colnames(inVar),collapse="+")),"+1",sep=""))
  formula.marginXX = as.formula(paste(outputname,'~ 1'))
  modelXX = glm(formula.XX, data=OBS, family= binomial())
  model_marginXX = glm(formula.marginXX, data=OBS, family= binomial())
  # modelXX =  svyglm(formula.XX, design = svydesign(~1, weights=1, data=OBS,family=quasibinomial()))
  # model_marginXX =svyglm(formula.marginXX, design = svydesign(~1, weights=~SWw, data=OBS,family=quasibinomial()))
  
  ## Compute P(outVar) mean 
  Prob.outVar.1 = predict.glm(model_marginXX,newdata=OBS,type='response')
  Prob.outVar.0 = 1 - Prob.outVar.1
  Prob.outVar = diag(Prob.outVar.1) %*% as.matrix(outVar) + diag(Prob.outVar.0) %*% as.matrix(1-outVar)
  
  ## Compute P(outVar|inVar1,...,inVarD) mean 
  Prob.outVar.1.giveninVar = predict.glm(modelXX,newdata=OBS,type="response")
  Prob.outVar.0.giveninVar = 1-Prob.outVar.1.giveninVar
  Prob.outVar.inVar = diag(Prob.outVar.1.giveninVar) %*% as.matrix(outVar) + diag(Prob.outVar.0.giveninVar) %*% as.matrix(1-outVar)
  SWXX = Prob.outVar / (Prob.outVar.inVar)
}

ComputeSW = function(DATA){
  ################################################################
  # Compute P(r,z)/(P(r|w)P(z|w,x))
  ################################################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]  
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)] 
  Y = OBS[,(D+4)]
  
  ################################################################
  # Compute P(z)
  ################################################################
  Prob.Z.1 = mean(Z)
  Prob.Z.0 = 1-Prob.Z.1
  Prob.Z = Z*Prob.Z.1 + (1-Z)*Prob.Z.0
  
  ################################################################
  # Compute P(r|z)
  ################################################################
  model_xgboost_RZ = xgboost(verbose = 0, data = data.matrix(Z), label = R, nrounds = 20,max.depth=10,lambda=1/length(R),alpha=1/length(R), objective = "binary:logistic")
  pred.R.Z = predict(model_xgboost_RZ,newdata=data.matrix(Z),type='response')
  Prob.R.Z = R*pred.R.Z + (1-R)*(1-pred.R.Z)
  
  # formula.RZ = as.formula('R~Z')
  # model.RZ = glm(formula.RZ, data=DATA, family= binomial())
  # pred.R.Z = predict.glm(model.RZ,newdata = DATA,type='response')
  # Prob.R.Z = R*pred.R.Z + (1-R)*(1-pred.R.Z)
  
  ################################################################
  # Compute P(r,z)
  ################################################################
  Prob.numer = Prob.Z*Prob.R.Z
  
  ################################################################
  # Compute P(r|w)
  ################################################################
  # inVar_margin = data.matrix(rep(1,length(outVar)))
  # model_xgboost = xgboost(verbose = 0, data = data.matrix(inVar), label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  # model_xgboost_margin = xgboost(verbose = 0, data = inVar_margin, label = outVar, nrounds = 20,max.depth=10,lambda=1/length(outVar),alpha=1/length(outVar), objective = "binary:logistic")
  # Prob.outVar.1 = predict(model_xgboost_margin,newdata=inVar_margin,type='response')
  model_xgboost_RW = xgboost(verbose = 0, data = data.matrix(W), label = R, nrounds = 20,max.depth=10,lambda=1/length(R),alpha=1/length(R), objective = "binary:logistic")
  pred.R.W = predict(model_xgboost_RW,newdata=data.matrix(W),type='response')
  Prob.R.W = R*pred.R.W + (1-R)*(1-pred.R.W)
  
  # formula.RW = as.formula(paste(paste('R','~',sep=""),paste(paste('W',1:ncol(W),sep=""),collapse="+")))
  # model.RW = glm(formula.RW, data=DATA, family= binomial())
  # pred.R.W = predict.glm(model.RZ,newdata = DATA,type='response')
  # Prob.R.W = R*pred.R.W + (1-R)*(1-pred.R.W)
  
  ################################################################
  # Compute P(z|r,x,w)
  ################################################################
  model_xgboost_ZRW = xgboost(verbose = 0, data = data.matrix(cbind(R,X,W)), label = Z, nrounds = 20,max.depth=10,lambda=1/length(R),alpha=1/length(R), objective = "binary:logistic")
  pred.Z.RXW = predict(model_xgboost_ZRW,newdata=data.matrix(cbind(R,X,W)),type='response')
  Prob.Z.RXW = Z*pred.Z.RXW + (1-Z)*(1-pred.Z.RXW)
  
  # formula.ZRXW = as.formula(paste(paste('Z','~',sep=""),paste(c(paste('W',1:ncol(W),sep=""),'X','R'),collapse="+")))
  # model.ZRXW = glm(formula.ZRXW, data=DATA, family= binomial())
  # pred.Z.RXW = predict.glm(model.ZRXW,newdata = DATA,type='response')
  # Prob.Z.RXW = Z*pred.Z.RXW + (1-Z)*(1-pred.Z.RXW)
  
  ################################################################
  ################################################################
  Prob.deno = Prob.R.W*Prob.Z.RXW
  SW = Prob.numer/Prob.deno
  return(SW)
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

double_sampler = function(DATA, SW_zw){
  ################################
  # Sampler
  ################################
  numidx = 0
  numsampled = 0
  norm_SW = SW_zw/max(SW_zw)
  sampled_df = data.frame()
  num_draw = 1*nrow(DATA)
  mediator_itermax = 2*max(round(max(SW_zw)),1)
  while(1){
    numidx = numidx + 1
    # taking_idx = (colMeans(sapply(norm_SW,function(p) rbinom(10,1,p))) > 0.5)
    taking_idx = sapply(norm_SW,function(p) rbinom(1,1,p)) > 0.5
    resampled_DATA =  DATA[sample(nrow(DATA),replace = TRUE), ]
    rownames(resampled_DATA) = rownames(DATA)
    sampled_DATA = resampled_DATA[taking_idx,]
    sampled_df = rbind(sampled_df,sampled_DATA)
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

learnWdash = function(Wdash1_importance,inVar,regval){
  ################################
  # Compute Wdash1 P(z)/P(z | w,x)
  ################################
  # Importance sampling based 
  # Re-learn with the regularization
  # regval = ((max(Wdash1_importance) - min(Wdash1_importance))**1)/nrow(OBS)
  model_Wdash1 = xgboost(verbose = 0, data = inVar, label = Wdash1_importance, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  Wdash1 = predict(model_Wdash1,newdata = inVar, type='response')
  if (length(Wdash1[Wdash1 < 0 ]) > 0){
    Wdash1[Wdash1 < 0 ] = runif(n=length(Wdash1[Wdash1 < 0 ]),min=0,max=min(abs(Wdash1)))  
  }
  return(Wdash1)
}

mylossfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )
  return(mean(weightval * pred.loss) + sqrt(LW))
}

mygradfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 ))
  return(mean(pred.loss) + (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel))
}

hessfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 ))
  vec1 = (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel)
  vec2 = (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel)
  hessmat = -0.5*vec1 %*% t(vec2) * LW^(-3/2)
  return(hessmat)
}


learnXG = function(sampled_df,inVarCol,labelval,regval){
  ################################
  # Data Setup 
  ################################
  W = sampled_df[,1:D] 
  X = sampled_df[,(D+1)]  
  R = sampled_df[,(D+2)]  
  Z = sampled_df[,(D+3)] 
  Y = sampled_df[,(D+4)]
  data_sampled = data.frame(W,X,R,Z,Y)
  
  inVar = as.matrix(data_sampled[,inVarCol])
  regval = ((max(labelval) - min(labelval))**1)/nrow(data_sampled)
  model_XG = xgboost(verbose = 0, data = inVar, label = labelval, nrounds = 20,max.depth=10,lambda=regval,alpha=regval/2)
  return(model_XG)
}

multiID = function(OBS,D,numCate){
  ################################
  # Data Setup 
  ################################
  W = OBS[,1:D] 
  X = OBS[,(D+1)]; X0 = rep(0,length(X)); X1 = rep(1,length(X));
  R = OBS[,(D+2)]  
  Z = OBS[,(D+3)] 
  Y = OBS[,(D+4)]
  
  DATA = data.frame(W,X,R,Z,Y)
  
  
  ################################
  # Learn P(x)
  Prob.X = X*mean(X) + (1-X)*(1-mean(X))
  
  # Learn P(r)
  Prob.R = R*mean(R) + (1-R)*(1-mean(R))
  
  # Learn P(z)
  Prob.Z = Z*mean(Z) + (1-Z)*(1-mean(Z))
  
  
  
  # Learn P(x|w)
  model.X.w = learnXG(DATA,c(1:D),X,rep(0,length(X)))
  pred.X.w = predict(model.X.w, newdata=data.matrix(W),type='response')
  Prob.X.w = X*pred.X.w + (1-X)*(1-pred.X.w)
  Prob.X0.w = (1-pred.X.w)
  Prob.X1.w = pred.X.w
  
  # Learn P(r|w)
  model.R.w = learnXG(DATA,c(1:D),R,rep(0,length(R)))
  pred.R.w = predict(model.R.w, newdata=data.matrix(W),type='response')
  Prob.R.w = R*pred.R.w + (1-R)*(1-pred.R.w)
  
  # Learn P(z|w,x,r)
  model.Z.wxr = learnXG(DATA,c(1:(D+2)),Z,rep(0,length(Z)))
  pred.Z.wxr = predict(model.Z.wxr, newdata=data.matrix(DATA[,c(1:(D+2))]),type='response')
  Prob.Z.wxr = Z*pred.Z.wxr + (1-Z)*(1-pred.Z.wxr)
  
  
  Ybox = rep(0,nrow(DATA))
  for (idx in 1:20){
    sampled_df = double_sampler(DATA,(Prob.R*Prob.Z)/(Prob.R.w*Prob.Z.wxr))
    # Learn Pw(y|w,z)
    model.Yw.wzr = learnXG(sampled_df,c(1:D,(D+2),(D+3)),Y,rep(0,length(X)))
    pred.Yw.wzr = predict(model.Yw.wzr, newdata=data.matrix(data.frame(W,R,Z)),type='response')
    Prob.Yw.wzr = Y*pred.Yw.wzr + (1-Y)*(1-pred.Yw.wzr)  
    Ybox = Ybox + Prob.Yw.wzr
  }
  Prob.Yw.wzr = Ybox/20
  
  # Learn P(z|w,x)
  model.Z.wx = learnXG(DATA,c(1:(D+1)),Z,rep(0,length(Z)))
  pred.Z.wx = predict(model.Z.wx, newdata=data.matrix(DATA[,c(1:(D+1))]),type='response')
  Prob.Z.wx = Z*pred.Z.wx + (1-Z)*(1-pred.Z.wx)
  
  # Learn P(y|w,x,r,z)
  model.Y.wxrz = learnXG(DATA,c(1:(D+3)),Y,rep(0,length(Y)))
  pred.Y.wxrz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X,R,Z)),type='response')
  pred.Y.wx0rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X=X0,R,Z)),type='response')
  pred.Y.wx1rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W,X=X1,R,Z)),type='response')
  
  Prob.Y.wxrz = Y*pred.Y.wxrz + (1-Y)*(1-pred.Y.wxrz)
  Prob.Y.wx0rz = Y*pred.Y.wx0rz + (1-Y)*(1-pred.Y.wx0rz)
  Prob.Y.wx1rz = Y*pred.Y.wx1rz + (1-Y)*(1-pred.Y.wx1rz)
  ###############################
  W_importance = (Prob.X* Prob.R * Prob.Z.wx *  (Prob.X0.w*pred.Y.wx0rz + Prob.X1.w*pred.Y.wx1rz))/(Prob.X.w * Prob.R.w * Prob.Z.wxr * Prob.Y.wxrz)
  W_importance[W_importance<0] = runif(length(W_importance[W_importance<0]),min=0,max=min(abs(W_importance)))
  # W_importance = (Prob.X* Prob.R * Prob.Z.wx *  Prob.Yw.wzr )/(Prob.X.w * Prob.R.w * Prob.Z.wxr * Prob.Y.wxrz)
  
  learned_W = W_importance
  ################################
  

  ################################
  # Predict Pw1(y | x)
  ################################
  lambda_h = 0
  xgbMatrix = xgb.DMatrix(data.matrix(data.frame(X=DATA$X,R=DATA$R)), label=DATA$Y)
  modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=lambda_h,alpha=lambda_h,objective = "binary:logistic",weight = learned_W)
  
  
  X0R0 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(0,nrow(DATA))))
  X0R1 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(1,nrow(DATA))))
  X1R0 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(0,nrow(DATA))))
  X1R1 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(1,nrow(DATA))))

  Yx0r0 = mean(predict(modelY_xgboost,newdata=X0R0,type='response'))
  Yx0r1 = mean(predict(modelY_xgboost,newdata=X0R1,type='response'))
  Yx1r0 = mean(predict(modelY_xgboost,newdata=X1R0,type='response'))
  Yx1r1 = mean(predict(modelY_xgboost,newdata=X1R1,type='response'))

  # Yx0r0_results[iteridx] = Yx0r0
  # Yx0r1_results[iteridx] = Yx0r1
  # Yx1r0_results[iteridx] = Yx1r0
  # Yx1r1_results[iteridx] = Yx1r1
  # myans = c(mean(Yx0r0_results),mean(Yx0r1_results),mean(Yx1r0_results),mean(Yx1r1_results))
  myans = c(Yx0r0,Yx0r1,Yx1r0,Yx1r1)
  
  
  
  # ################################
  # # Compute Wdash1 P(z)/P(z | w,x)
  # ################################
  # Wdash1_importance = ComputeSW(DATA)
  # regval = ((max(Wdash1_importance) - min(Wdash1_importance))**1)/nrow(OBS)
  # Wdash1 = learnWdash(Wdash1_importance,data.matrix(DATA),regval)
  # 
  # ################################
  # # Gen Ddash1 
  # ################################
  # sampled_df = sampler2(DATA,Wdash1)
  # 
  # ################################
  # # On wdash1, Compute W1= P(x)/Pw'(x |w,r,z,y)
  # ################################
  # Px = X*mean(X) + (1-X)*(1-mean(X))
  # inVarCol = c(c(1:D),(D+2),(D+3),(D+4))
  # model_Pw1X.wrzy = learnXG(sampled_df,inVarCol,sampled_df$X)
  # pred_Pw1X.wrzy = predict(model_Pw1X.wrzy,newdata = as.matrix(DATA[,inVarCol]),type='response')
  # Prob.X.wrzy = DATA$X * pred_Pw1X.wrzy + (1-DATA$X)*(1-pred_Pw1X.wrzy)
  # W1importance = Px/Prob.X.wrzy
  # regval = ((max(W1importance) - min(W1importance))**2)/nrow(OBS)
  # W1 = learnWdash(W1importance,as.matrix(DATA),regval)
  # 
  # ################################
  # # Gen D1 
  # ################################
  # sampled_D1 = sampler2(DATA,W1)
  # model_Pw1R.x = learnXG(sampled_df,(D+1),sampled_D1$R)
  # pred_Pw1R.x = predict(model_Pw1R.x,newdata = as.matrix(DATA[,(D+1)]),type='response')
  # prob_Pw1R.x = DATA$R*pred_Pw1R.x + (1-DATA$R)*(1-pred_Pw1R.x)
  # model_Pw1R.zwx = learnXG(sampled_df,c(c(1:D),(D+1),(D+3)),sampled_D1$R)
  # pred_Pw1R.zwx = predict(model_Pw1R.zwx,newdata = as.matrix(DATA[,c(c(1:D),(D+1),(D+3))]),type='response')
  # prob_Pw1R.zwx = DATA$R*pred_Pw1R.zwx + (1-DATA$R)*(1-pred_Pw1R.zwx)
  # W2importance = prob_Pw1R.x/prob_Pw1R.zwx
  # regval = ((max(W2importance) - min(W2importance))**1)/nrow(OBS)
  # W2 = learnWdash(W2importance,as.matrix(DATA),regval)
  # 
  # regval = ((max(W1*W2) - min(W1*W2))**1)/nrow(OBS)
  # myW = learnWdash(W1*W2,as.matrix(DATA),regval)
  # 
  # ################################
  # # Gen D2
  # ################################
  # sampled_D2 = sampler2(DATA,myW)
  # ################################
  # # Predict Pw1(y | x,r)
  # ################################
  # xgbMatrix = xgb.DMatrix(data.matrix(data.frame(X=sampled_D2$X,R=sampled_D2$R)), label=sampled_D2$Y)
  # modelY_xgboost = xgboost(verbose=0, data=xgbMatrix,nrounds = 20,max.depth=20,lambda=1/nrow(DATA),alpha=1/nrow(DATA),objective = "binary:logistic")
  # 
  # X0R0 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(0,nrow(DATA))))
  # X0R1 = data.matrix(data.frame(X=rep(0,nrow(DATA)),R=rep(1,nrow(DATA))))
  # X1R0 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(0,nrow(DATA))))
  # X1R1 = data.matrix(data.frame(X=rep(1,nrow(DATA)),R=rep(1,nrow(DATA))))
  # 
  # Yx0r0 = mean(predict(modelY_xgboost,newdata=X0R0,type='response'))
  # Yx0r1 = mean(predict(modelY_xgboost,newdata=X0R1,type='response'))
  # Yx1r0 = mean(predict(modelY_xgboost,newdata=X1R0,type='response'))
  # Yx1r1 = mean(predict(modelY_xgboost,newdata=X1R1,type='response'))
  # 
  # myans = c(Yx0r0,Yx0r1,Yx1r0,Yx1r1)
  
  return(myans)
}


# source('doubleeffect-data.R')
# source('doubleeffect-naive.R')
# source('doubleeffect-param.R')
# # source('doubleeffect-est.R')
# source('double-est-exact.R')
# require(stats)
# 
# N = 1000
# Nintv = 1000000
# 
# D = 15
# numCate = 2
# C = numCate - 1
# 
# numSim = 20
# naiveList = rep(0,numSim)
# paramList = rep(0,numSim)
# multiList = rep(0,numSim)
# multi2List = rep(0,numSim)
# 
# for (iterSim in (1:numSim)){
#   seednum = sample(1:1000000,1)
#   # seednum = 4321
#   mytmp = dataGen(seednum,N,Nintv,D,C)
#   OBS = mytmp[[1]]
#   INTV = mytmp[[2]]
# 
#   answer = c(mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==1),'Y.intv']),
#              mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==1),'Y.intv']))
#   obsans = c(mean(OBS[(OBS$X == 0)&(OBS$R==0),'Y']),mean(OBS[(OBS$X == 0)&(OBS$R==1),'Y']),
#              mean(OBS[(OBS$X == 1)&(OBS$R==0),'Y']),mean(OBS[(OBS$X == 1)&(OBS$R==1),'Y']))
#   # print(paste("OBS performance:",round(mean(abs(answer-obsans),na.rm=T),4)))
# 
#     naive_start = Sys.time()
#     # naiveanswer = naiveAdj(OBS,D,numCate)
#     # naiveperformance = mean(abs(answer-naiveanswer),na.rm=T)
#     naiveperformance = 0.5
#     naive_end = Sys.time()
#     naive_runtime = naive_end - naive_start
#     print(paste("Naive Runtime:",round(naive_runtime,2),sep=""))
# 
#     param_start = Sys.time()
#     paramanswer = paramAdj(OBS,D,numCate)
#     paramperformance = mean(abs(answer-paramanswer),na.rm=T)
#     # paramperformance = 0.5
#     param_end = Sys.time()
#     param_runtime = param_end - param_start
#     print(paste("Param Runtime:",round(param_runtime,2),sep=""))
# 
#     multi_start = Sys.time()
#     multianswer = multiAdj(OBS,D,numCate)
#     multiperformance = mean(abs(answer-multianswer),na.rm=T)
#     # multiperformance = 0.5
#     multi_end = Sys.time()
#     multi_runtime = multi_end - multi_start
#     print(paste("Multi Runtime:",round(multi_runtime,2),sep=""))
# 
#     multi_exact_start = Sys.time()
#     multi_exact_answer = multiExactAdj(OBS,D,numCate)
#     multi_exact_performance = mean(abs(answer-multi_exact_answer),na.rm=T)
#     # multi_exact_performance = 0.5
#     multi_exact_end = Sys.time()
#     multi_exact_runtime = multi_exact_end - multi_exact_start
#     print(paste("Multi_exact Runtime:",round(multi_exact_runtime,2),sep=""))
# 
#     print(paste("OBS: ",round(max(abs(answer-obsans)),2),sep=""))
# 
#     # print(c(paste("Naive: ",round(naiveperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
#     print(c(paste("Naive: ",round(naiveperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("MultiExact: ",round(multi_exact_performance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
#     modelname = c("Naive","Param","MultiExact","Multi")
#     modelperformance = c(naiveperformance,paramperformance,multi_exact_performance,multiperformance)
#     print(paste("Winner: ",modelname[which.min(modelperformance)],sep=""))
# 
#     naiveList[iterSim] = naiveperformance
#     paramList[iterSim] = paramperformance
#     multiList[iterSim] = multiperformance
#     multi2List[iterSim] = multi_exact_performance
#   }
#   # print(summary(naiveList))
#   # print(summary(paramList))
#   # print(summary(multi2List))
#   # print(summary(multiList))
# 
#   tmp_mat = round(matrix(c(median(paramList),median(multi2List),median(multiList)),ncol=3),5)
#   colnames(tmp_mat) = c("Param","Exact","Multi")
#   rownames(tmp_mat) = "Error"
#   print(tmp_mat)
#   print(paste("Winner: ",colnames(tmp_mat)[which.min(tmp_mat)],sep=""))




