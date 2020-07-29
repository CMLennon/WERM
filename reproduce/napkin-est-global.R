source("WERM_Heuristic.R")

napkin_lossfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = mean((weightval - weightlabel)^2 + lambda_W * weightval^2 )
  return(mean(weightval * pred.loss) + sqrt(LW))
}

napkin_gradfun = function(pred.loss, weightval, weightlabel,lambda_W){
  LW = sqrt(mean((weightval - weightlabel)^2 + lambda_W * weightval^2 ))
  return(mean(pred.loss) + (1/sqrt(LW))*((1+lambda_W)*weightval - weightlabel)/length(weightlabel))
}


multiGlobal = function(OBS,D,numCate){
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
  
  ################################
  # Learn the Hyperparameter 
  ################################
  # Re-learn with the regularization
  # regvallist = c(1:100)*(nrow(OBS))/20
  regvallist = seq(0,10,by=0.2)
  lambda_W = learnHyperParam(regvallist,data.matrix(data.frame(W=W,Z=Z)),SW_importance_sampling,0)/nrow(OBS)
  lambda_W = lambda_W/nrow(OBS)
  learned_W = learnWdash(SW_importance_sampling,data.matrix(data.frame(W=W,Z=Z)),lambda_W)
  
  orig_W = learned_W
  
  ################################
  # Predict Pw(y | x)
  ################################
  ##### OPT Initial  
  lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=X)),Y,1)/nrow(OBS)
  mygradW = WERMGradient(N=nrow(OBS),inputMat = data.matrix(data.frame(X,Z)),labelVal = Y, evalMat =  data.matrix(data.frame(X,Z)), lambda_h = lambda_h, lambda_W = lambda_W,iterMax = 1000000,init_W=SW_importance_sampling, LossFun=napkin_lossfun, GradFun=napkin_gradfun)
  
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  GLOBALanswer = c(Yx0,Yx1)
  return(GLOBALanswer)
}

source('napkin-data.R')
# source('napkin-naive.R')
source('napkin-param.R')
# source('napkin-est.R')
source('napkin-est-heuristic.R')
# source('napkin-est3-opt.R')
require(stats)

N = 1000
Nintv = 1000000

D = 15
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))

WERManswer = multiHeuristic(OBS,D,numCate)
GLOBALanswer = multiGlobal(OBS,D,numCate)
PARAManswer = PlugInEstimator(OBS,D,numCate)

# 
# numSim = 10
# paramList = rep(0,numSim)
# multiList = rep(0,numSim)
# multi2List = rep(0,numSim)
# optList = rep(0,numSim)
# 
# for (iterSim in (1:numSim)){
#   seednum = sample(1:1000000,1)
# #   # seednum = 123
#   mytmp = dataGen(seednum,N,Nintv,D,C)
#   OBS = mytmp[[1]]
#   INTV = mytmp[[2]]
# 
#   answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
#   obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))
# 
#   opt_start = Sys.time()
#   optanswer = multiOpt(OBS,D,numCate)
#   optperformance = mean(abs(answer-optanswer),na.rm=T)
#   # naiveperformance = 0.5
#   opt_end = Sys.time()
#   opt_runtime = opt_end - opt_start
#   print(paste("OPT Runtime:",round(opt_runtime,2),sep=""))
# 
#   param_start = Sys.time()
#   paramanswer = paramAdj(OBS,D,numCate)
#   paramperformance = mean(abs(answer-paramanswer),na.rm=T)
#   # paramperformance = 0.5
#   param_end = Sys.time()
#   param_runtime = param_end - param_start
#   print(paste("Param Runtime:",round(param_runtime,2),sep=""))
# 
#   multi_start = Sys.time()
#   multianswer = multiAdj(OBS,D,numCate)
#   multiperformance = mean(abs(answer-multianswer),na.rm=T)
#   # multiperformance = 0.5
#   multi_end = Sys.time()
#   multi_runtime = multi_end - multi_start
#   print(paste("Multi Runtime:",round(multi_runtime,2),sep=""))
# 
#   multi_exact_start = Sys.time()
#   multi_exact_answer = multiExactAdj(OBS,D,numCate)
#   multi_exact_performance = mean(abs(answer-multi_exact_answer),na.rm=T)
#   # multi_exact_performance = 0.5
#   multi_exact_end = Sys.time()
#   multi_exact_runtime = multi_exact_end - multi_exact_start
#   print(paste("Multi_exact Runtime:",round(multi_exact_runtime,2),sep=""))
# 
#   print(paste("OBS: ",round(max(abs(answer-obsans)),2),sep=""))
# 
#   # print(c(paste("Naive: ",round(naiveperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
#   print(c(paste("OPT: ",round(optperformance,4),sep=""),paste("Param: ",round(paramperformance,4),sep=""),paste("MultiExact: ",round(multi_exact_performance,4),sep=""),paste("Multi: ",round(multiperformance,4),sep="")))
#   modelname = c("OPT","Param","MultiExact","Multi")
#   modelperformance = c(optperformance,paramperformance,multi_exact_performance,multiperformance)
#   print(paste("Winner: ",modelname[which.min(modelperformance)],sep=""))
# 
#   optList[iterSim] = optperformance
#   paramList[iterSim] = paramperformance
#   multiList[iterSim] = multiperformance
#   multi2List[iterSim] = multi_exact_performance
# }
# 
# print(summary(optList))
# print(summary(paramList))
# print(summary(multi2List))
# print(summary(multiList))
# 
# tmp_mat = round(matrix(c(median(optList),median(paramList),median(multi2List),median(multiList)),ncol=4),5)
# colnames(tmp_mat) = c("OPT","Param","Exact","Multi")
# rownames(tmp_mat) = "Error"
# print(tmp_mat)
# print(paste("Winner: ",colnames(tmp_mat)[which.min(tmp_mat)],sep=""))
# 
# #
# # # Compare Mean
# # t.test(multiList,multi2List,paired=TRUE,alternative = "two.sided")
# # t.test(multiList,paramList,paired=TRUE,alternative = "less")
# #
# # # Compare Variance
# # var.test(multiList,multi2List,alternative="two.sided")
# # var.test(multiList,paramList,alternative="two.sided")
# 
# # df.result.wrm = as.matrix(read.csv('tmp-multi.csv')); colnames(df.result.wrm) = NULL
# # df.result.exact = as.matrix(read.csv('tmp-exact.csv')); colnames(df.result.exact) = NULL
# # t.test(df.result.wrm[1,],df.result.exact[1,],alternative = "two.sided")
# # var.test(df.result.wrm[1,],df.result.exact[1,],alternative = "two.sided")
# # for (idx in 1:10){

