require(stats)
library(mise)
library(tictoc)
mise()
source('napkin-data.R')
source('napkin-est-global.R')
source('napkin-param.R')
source('napkin-est-heuristic.R')
source('napkin-est-ID.R')

N = 1000
Nintv = 1000000

D = 20
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))

tic()
WERManswer = multiHeuristic(OBS,D,numCate); toc()
tic()
GLOBALanswer = multiGlobal(OBS,D,numCate); toc()
tic()
PLUGINanswer = PlugInEstimator(OBS,D,numCate); toc()
IDanswer = multiID(OBS,D,numCate)

WERMperformance = mean(abs(WERManswer-answer))
GLOBALperformance = mean(abs(GLOBALanswer-answer))
PLUGINperformance = mean(abs(PLUGINanswer-answer))
IDperformance = mean(abs(IDanswer-answer))

resultArray = c(PLUGINperformance,GLOBALperformance,WERMperformance,IDperformance)
resultTbl = matrix(round(resultArray,3),ncol=4)
colnames(resultTbl) = c("PlugIn","Global","WERM","ID")
rownames(resultTbl) = "Error"
print(resultTbl)
print(paste("Winner:",colnames(resultTbl)[which.min(resultArray)]))


