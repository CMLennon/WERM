library(survey)
library(xgboost)
library(cowplot)
library(boot)
library(ipw)
library(Hmisc)
library(ggplot2)
library(foreach)
library(doParallel)
library(R.utils)
library(dplyr)
library(doSNOW)
library(arm)
library(mgcv)
library(mise)
library(tictoc)

# Log Example
## Rscript simulator.R 'napkin' 20 2 30 20 1 20 5 50 'napkin_0730_2300_D20'
## Rscript simulator.R 'napkin' 5 2 10 20 1 20 5 50 'napkin_tmp'
## nohup taskset -c 0-25 Rscript simulator.R 'napkin' 5 2 10 20 1 20 30 50 'napkin_tmp' >log-napkin-tmp.txt & 

args = commandArgs(trailingOnly = TRUE)
cores = detectCores()
timeoutLim = 9999

probleminstance = args[1]
D = as.numeric(args[2])
numCate = as.numeric(args[3])
simRound = as.numeric(args[4])
totalN = as.numeric(args[5])
nidx.start = as.numeric(args[6])
nidx.end = as.numeric(args[7])
corenum = as.numeric(args[8])
NumUnit = as.numeric(args[9])
filetitle = args[10]

# Example 
# probleminstance = 'doubleeffect'
# D = 5
# numCate = 2
# simRound = 5
# totalN = 20
# nidx.start = 1
# nidx.end = totalN
# corenum = 4
# NumUnit = 50
# filetitle = 'double_temp'

Nintv = 10^7

if(probleminstance == 'napkin'){
  source('napkin-data.R')
  source('napkin-est-global.R')
  source('napkin-param.R')
  source('napkin-est-heuristic.R')
  source('napkin-est-ID.R')
}


if(probleminstance == 'mediator'){
  source('mediator-data.R')
  source('mediator-est-global.R')
  source('mediator-param.R')
  source('mediator-est-heuristic.R')
  source('mediator-est-ID.R')
}

if(probleminstance == 'doubleeffect'){
  source('doubleeffect-data.R')
  source('doubleeffect-est-global.R')
  source('doubleeffect-param.R')
  source('doubleeffect-est-heuristic.R')
  source('doubleeffect-est-ID.R')
}

timeoutFun = function(Fun, mytime){
  result = withTimeout({
    Fun
  }, timeout = mytime, onTimeout = "silent")
  return(result)
}

RunFunWithTime = function(TimeFUN, EstFUN, OBS,D,numCate,timelim){
  tic()
  estval = TimeFUN(EstFUN(OBS,D,numCate),timelim)
  if(is.null(estval)==T){
    estval = NA
    esttime = NA 
  }else{
    esttime = toc()
    esttime = unname(esttime$toc - esttime$tic)  
  }
  return(c(estval,esttime))
}

print(probleminstance)
cl = makeCluster(corenum,outfile='log.txt')
registerDoSNOW(cl)  

C = numCate -1 
Nlist = c(1:totalN)*NumUnit


mat.intv = matrix(0,nrow=totalN,ncol=6)
mat.global = matrix(0,nrow=totalN,ncol=6)
mat.naive = matrix(0,nrow=totalN,ncol=6)
mat.heuristic = matrix(0,nrow=totalN,ncol=6)
mat.id = matrix(0,nrow=totalN,ncol=6)
mat.summary.time.global = matrix(0,nrow=totalN,ncol=6)
mat.summary.time.naive = matrix(0,nrow=totalN,ncol=6)
mat.summary.time.heuristic = matrix(0,nrow=totalN,ncol=6)
mat.summary.time.id = matrix(0,nrow=totalN,ncol=6)

mat.total.intv = matrix(0,nrow=totalN,ncol=simRound)
mat.total.global = matrix(0,nrow=totalN,ncol=simRound)
mat.total.global.time = matrix(0,nrow=totalN,ncol=simRound)
mat.total.naive = matrix(0,nrow=totalN,ncol=simRound)
mat.total.naive.time = matrix(0,nrow=totalN,ncol=simRound)
mat.total.heuristic = matrix(0,nrow=totalN,ncol=simRound)
mat.total.heuristic.time = matrix(0,nrow=totalN,ncol=simRound)
mat.total.id = matrix(0,nrow=totalN,ncol=simRound)
mat.total.id.time = matrix(0,nrow=totalN,ncol=simRound)

for (nidx in nidx.start:nidx.end){
  N = Nlist[nidx]
  print(N)
  
  pb <- txtProgressBar(max = simRound, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  val.total = foreach(idx= 1:simRound, .combine = 'rbind',
                      .packages = c('survey', 'boot', 'ipw', 'Hmisc','R.utils','dplyr','arm','xgboost','tictoc'),.options.snow = opts) %dopar% {
                        
                        if(probleminstance == 'doubleeffect'){
                          seednum = sample(1:1000000,1)
                          mytmp = dataGen(seednum,N,Nintv,D,C)
                          OBS = mytmp[[1]]
                          INTV = mytmp[[2]]
                          answer = c(mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==1),'Y.intv']),
                                     mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==1),'Y.intv']))
                          
                          PLUGINresult = RunFunWithTime(timeoutFun,PlugInEstimator,OBS,D,numCate,timeoutLim)
                          PLUGINanswer = c(PLUGINresult[1],PLUGINresult[2],PLUGINresult[3],PLUGINresult[4])
                          PLUGINtime = PLUGINresult[5]
                          
                          GLOBALresult = RunFunWithTime(timeoutFun,multiGlobal,OBS,D,numCate,timeoutLim)
                          GLOBALanswer = c(GLOBALresult[1],GLOBALresult[2],GLOBALresult[3],GLOBALresult[4])
                          GLOBALtime = GLOBALresult[5]
                          
                          HEURISTICresult = RunFunWithTime(timeoutFun,multiHeuristic,OBS,D,numCate,timeoutLim)
                          HEURISTICanswer = c(HEURISTICresult[1],HEURISTICresult[2],HEURISTICresult[3],HEURISTICresult[4])
                          HEURISTICtime = HEURISTICresult[5]
                          
                          IDresult = RunFunWithTime(timeoutFun,multiID,OBS,D,numCate,timeoutLim)
                          IDanswer = c(IDresult[1],IDresult[2],IDresult[3],IDresult[4])
                          IDtime = IDresult[5]
                          
                          PLUGINperformance = mean(abs(answer-PLUGINanswer),na.rm=T)
                          GLOBALperformance = mean(abs(answer-GLOBALanswer),na.rm=T)
                          HEURISTICperformance = mean(abs(answer-HEURISTICanswer),na.rm=T)
                          IDperformance = mean(abs(answer-IDanswer),na.rm=T)
                          
                          
                        }else{
                          seednum = sample(1:1000000,1)
                          tmp = dataGen(seednum,N,Nintv,D,C)
                          OBS = tmp[[1]]
                          INTV = tmp[[2]]
                          answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))  
                          
                          PLUGINresult = RunFunWithTime(timeoutFun,PlugInEstimator,OBS,D,numCate,timeoutLim)
                          PLUGINanswer = c(PLUGINresult[1],PLUGINresult[2])
                          PLUGINtime = PLUGINresult[3]
                          
                          GLOBALresult = RunFunWithTime(timeoutFun,multiGlobal,OBS,D,numCate,timeoutLim)
                          GLOBALanswer = c(GLOBALresult[1],GLOBALresult[2])
                          GLOBALtime = GLOBALresult[3]
                          
                          HEURISTICresult = RunFunWithTime(timeoutFun,multiHeuristic,OBS,D,numCate,timeoutLim)
                          HEURISTICanswer = c(HEURISTICresult[1],HEURISTICresult[2])
                          HEURISTICtime = HEURISTICresult[3]
                          
                          IDresult = RunFunWithTime(timeoutFun,multiID,OBS,D,numCate,timeoutLim)
                          IDanswer = c(IDresult[1],IDresult[2])
                          IDtime = IDresult[3]
                          
                          PLUGINperformance = mean(abs(answer-PLUGINanswer),na.rm=T)
                          GLOBALperformance = mean(abs(answer-GLOBALanswer),na.rm=T)
                          HEURISTICperformance = mean(abs(answer-HEURISTICanswer),na.rm=T)
                          IDperformance = mean(abs(answer-IDanswer),na.rm=T)
                        }
                        answerperformance = 0 
                        return(c(answerperformance,PLUGINperformance,GLOBALperformance,HEURISTICperformance,IDperformance,
                                 PLUGINtime,GLOBALtime,HEURISTICtime,IDtime))
                      }
  close(pb)
  
  
  val.intv = val.total[,1]
  val.naive = val.total[,2]
  val.multi = val.total[,3]
  val.exact = val.total[,4]
  val.id = val.total[,5]
  val.time.plugin = val.total[,6]
  val.time.global = val.total[,7]
  val.time.heuristic = val.total[,8]
  val.time.id = val.total[,9]
  
  mat.total.intv[nidx,] = val.intv
  mat.total.naive[nidx,] = val.naive
  mat.total.naive.time[nidx,] = val.time.plugin
  mat.total.global[nidx,] = val.multi
  mat.total.global.time[nidx,] = val.time.global
  mat.total.heuristic[nidx,] = val.exact
  mat.total.heuristic.time[nidx,] = val.time.heuristic
  mat.total.id[nidx,] = val.id
  mat.total.id.time[nidx,] = val.time.id
  
  dev.intv = abs((val.intv - val.intv))
  qt.intv = quantile(dev.intv,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm = T)
  mean.intv = mean(dev.intv)
  mat.intv[nidx,] = c(as.numeric(qt.intv),mean.intv)
  
  dev.naive = abs((val.intv - val.naive))
  qt.naive = quantile(dev.naive,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.naive = mean(dev.naive)
  mat.naive[nidx,] = c(as.numeric(qt.naive),mean.naive)
  
  dev.multi = abs((val.intv - val.multi))
  qt.multi = quantile(dev.multi,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.multi = mean(dev.multi)
  mat.global[nidx,] = c(as.numeric(qt.multi),mean.multi)
  
  dev.exact = abs((val.intv - val.exact))
  qt.exact = quantile(dev.exact,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.exact = mean(dev.exact)
  mat.heuristic[nidx,] = c(as.numeric(qt.exact),mean.exact)
  
  dev.id = abs((val.intv - val.id))
  qt.id = quantile(dev.id,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.id = mean(dev.id)
  mat.id[nidx,] = c(as.numeric(qt.id),mean.id)
  
  dev.time.naive = val.time.plugin
  qt.time.naive = quantile(dev.time.naive,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.time.naive = mean(qt.time.naive)
  mat.summary.time.naive[nidx,] = c(as.numeric(qt.time.naive),mean.time.naive)
  
  dev.time.heuristic = val.time.heuristic
  qt.time.heuristic = quantile(dev.time.heuristic,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.time.heuristic = mean(qt.time.heuristic)
  mat.summary.time.heuristic[nidx,] = c(as.numeric(qt.time.heuristic),mean.time.heuristic)
  
  dev.time.id = val.time.id
  qt.time.id = quantile(dev.time.id,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.time.id = mean(qt.time.id)
  mat.summary.time.id[nidx,] = c(as.numeric(qt.time.id),mean.time.id)
  
  dev.time.global = val.time.global
  qt.time.global = quantile(dev.time.global,probs=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
  mean.time.global = mean(qt.time.global)
  mat.summary.time.global[nidx,] = c(as.numeric(qt.time.global),mean.time.global)
}

conflist = c(5,25,50,75,95,'mean')
for (idx in 1:length(conflist)){
  confval = conflist[idx]
  assign(paste('Y.intv.',confval,sep=""),mat.intv[,idx])
  assign(paste('Y.global.',confval,sep=""),mat.global[,idx])
  assign(paste('Y.plugin.',confval,sep=""),mat.naive[,idx])
  assign(paste('Y.heuristic.',confval,sep=""),mat.heuristic[,idx])
  assign(paste('Y.id.',confval,sep=""),mat.id[,idx])
  
  assign(paste('Y.time.global.',confval,sep=""),mat.summary.time.global[,idx])
  assign(paste('Y.time.heuristic.',confval,sep=""),mat.summary.time.heuristic[,idx])
  assign(paste('Y.time.id.',confval,sep=""),mat.summary.time.id[,idx])
  assign(paste('Y.time.naive.',confval,sep=""),mat.summary.time.naive[,idx])
  
}

df.result = data.frame(Nlist, Y.intv.5,Y.intv.25,Y.intv.50,Y.intv.75,Y.intv.95,Y.intv.mean,
                       Y.global.5,Y.global.25,Y.global.50,Y.global.75,Y.global.95,Y.global.mean, # Global
                       Y.plugin.5,Y.plugin.25,Y.plugin.50,Y.plugin.75,Y.plugin.95,Y.plugin.mean, # Plugin 
                       Y.heuristic.5,Y.heuristic.25,Y.heuristic.50,Y.heuristic.75,Y.heuristic.95,Y.heuristic.mean, # Heuristic 
                       Y.id.5,Y.id.25,Y.id.50,Y.id.75,Y.id.95,Y.id.mean, # WERM-ID
                       
                       Y.time.global.5,Y.time.global.25,Y.time.global.50,Y.time.global.75,Y.time.global.95,Y.time.global.mean,
                       Y.time.heuristic.5,Y.time.heuristic.25,Y.time.heuristic.50,Y.time.heuristic.75,Y.time.heuristic.95,Y.time.heuristic.mean,
                       Y.time.id.5,Y.time.id.25,Y.time.id.50,Y.time.id.75,Y.time.id.95,Y.time.id.mean,
                       Y.time.naive.5,Y.time.naive.25,Y.time.naive.50,Y.time.naive.75,Y.time.naive.95,Y.time.naive.mean
)
write.csv(df.result,paste("Result/",filetitle,"-summary.csv",sep=""))
write.csv(mat.total.intv,paste("Result/",filetitle,"-intv.csv",sep=""))
write.csv(mat.total.naive,paste("Result/",filetitle,"-param.csv",sep=""))
write.csv(mat.total.heuristic,paste("Result/",filetitle,"-heuristic.csv",sep=""))
write.csv(mat.total.global,paste("Result/",filetitle,"-global.csv",sep=""))
write.csv(mat.total.id,paste("Result/",filetitle,"-id.csv",sep=""))

write.csv(mat.total.naive.time,paste("Result/",filetitle,"-time-param.csv",sep=""))
write.csv(mat.total.heuristic.time,paste("Result/",filetitle,"-time-heuristic.csv",sep=""))
write.csv(mat.total.global.time,paste("Result/",filetitle,"-time-global.csv",sep=""))
write.csv(mat.total.id.time,paste("Result/",filetitle,"-time-id.csv",sep=""))

stopCluster(cl)  





