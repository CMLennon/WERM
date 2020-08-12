library(xgboost)
library(boot)
source('WERM_Heuristic.R')

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
  
  
  ################################################################
  # Evaluate the weight hat{W}
  # See Eq. (A.5)
  # W = P^{W}(y|w,r,z)P(x)P(r)P(z|w,x)/(P(x|w)P(r|w)P(z|w,x,r)P(y|w,x,r,z)
  ################################################################
  # Learn P(x)
  Prob.X = X*mean(X) + (1-X)*(1-mean(X))
  
  # Learn P(r)
  Prob.R = R*mean(R) + (1-R)*(1-mean(R))
  
  # Learn P(x|w)
  model.X.w = learnXG(inVar = data.matrix(data.frame(W=W)), labelval = X, regval = rep(0,length(X)))
  # model.X.w = learnXG_Planid(DATA,c(1:D),X,rep(0,length(X)))
  pred.X.w = predict(model.X.w, newdata=data.matrix(data.frame(W=W)),type='response')
  Prob.X.w = X*pred.X.w + (1-X)*(1-pred.X.w)
  Prob.X0.w = (1-pred.X.w)
  Prob.X1.w = pred.X.w
  
  # Learn P(r|w)
  model.R.w = learnXG(inVar = data.matrix(data.frame(W=W)), labelval = R, regval = rep(0,length(R)))
  # model.R.w = learnXG_Planid(DATA,c(1:D),R,rep(0,length(R)))
  pred.R.w = predict(model.R.w, newdata=data.matrix(data.frame(W=W)),type='response')
  Prob.R.w = R*pred.R.w + (1-R)*(1-pred.R.w)
  
  # Learn P(z|w,x,r)
  model.Z.wxr = learnXG(inVar = data.matrix(data.frame(W=W,X=X,R=R)), labelval = Z, regval = rep(0,length(Z)))
  # model.Z.wxr = learnXG_Planid(DATA,c(1:(D+2)),Z,rep(0,length(Z)))
  pred.Z.wxr = predict(model.Z.wxr, newdata=as.matrix(data.frame(W=W,X=X,R=R)),type='response')
  Prob.Z.wxr = Z*pred.Z.wxr + (1-Z)*(1-pred.Z.wxr)
  
  # Learn P(z)
  Prob.Z = Z * mean(Z) + (1-Z)*(1-mean(Z))
  
  # Learn P^{Wd}(y|w,r,z)
  # Ybox = rep(0,nrow(DATA))
  # bootstrap_iter = 10
  # for (idx in 1:bootstrap_iter){
  #   sampled_df = WERM_Sampler(DATA,(Prob.R*Prob.Z)/(Prob.R.w*Prob.Z.wxr))
  #   # Learn Pw(y|w,z)
  #   model.Yw.wzr = learnXG_Planid(sampled_df,c(1:D,(D+2),(D+3)),Y,rep(0,length(X)))
  #   pred.Yw.wzr = predict(model.Yw.wzr, newdata=data.matrix(data.frame(W,R,Z)),type='response')
  #   Prob.Yw.wzr = Y*pred.Yw.wzr + (1-Y)*(1-pred.Yw.wzr)
  #   Ybox = Ybox + Prob.Yw.wzr
  # }
  # Prob.Yw.wzr = Ybox/bootstrap_iter
  
  # Learn P(z|w,x)
  model.Z.wx = learnXG(inVar = data.matrix(data.frame(W=W,X=X)), labelval = Z, regval = rep(0,length(Z)))
  # model.Z.wx = learnXG_Planid(DATA,c(1:(D+1)),Z,rep(0,length(Z)))
  # pred.Z.wx = predict(model.Z.wx, newdata=data.matrix(DATA[,c(1:(D+1))]),type='response')
  pred.Z.wx = predict(model.Z.wx, newdata=as.matrix(data.frame(W=W,X=X)),type='response')
  Prob.Z.wx = Z*pred.Z.wx + (1-Z)*(1-pred.Z.wx)
  
  # Learn P(y|w,x,r,z)
  model.Y.wxrz = learnXG(inVar = data.matrix(data.frame(W=W,X=X,R=R,Z=Z)), labelval = Y, regval = rep(0,length(Y)))
  # model.Y.wxrz = learnXG_Planid(DATA,c(1:(D+3)),Y,rep(0,length(Y)))
  pred.Y.wxrz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X,R=R,Z=Z)),type='response')
  pred.Y.wx0rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X0,R=R,Z=Z)),type='response')
  pred.Y.wx1rz = predict(model.Y.wxrz, newdata=data.matrix(data.frame(W=W,X=X1,R=R,Z=Z)),type='response')
  Prob.Y.wxrz = Y*pred.Y.wxrz + (1-Y)*(1-pred.Y.wxrz)
  
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




