require(stats)
library(mise)
library(tictoc)
# mise()
source('napkin-data.R')
source('napkin-est-global.R')
source('napkin-param.R')
source('napkin-as-BD.R')
source('napkin-as-IPW.R')
source('napkin-est-heuristic.R')
source('napkin-est-ID.R')

# Rscript doubleeffect-main.R 100000 2 
args = commandArgs(trailingOnly = TRUE)

N = as.numeric(args[1])
Nintv = 5000000

D = as.numeric(args[2])
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))

WERManswer = multiHeuristic(OBS,D,numCate)
# GLOBALanswer = multiGlobal(OBS,D,numCate)
GLOBALanswer = WERManswer
BDanswer = asBDEstimator(OBS,D,numCate)
IPWanswer = asIPWEstimator(OBS,D,numCate)

WERMperformance = mean(abs(WERManswer - answer))
GLOBALperformance = mean(abs(GLOBALanswer - answer))
BDperformance = mean(abs(BDanswer - answer))
IPWperformance = mean(abs(IPWanswer - answer))

 
# # resultArray = c(PLUGINperformance,GLOBALperformance,WERMperformance,IDperformance,asBDperformance,asIPWperformance)
resultArray = c(GLOBALperformance,WERMperformance,BDperformance,IPWperformance)
resultTbl = matrix(round(resultArray,3),ncol=4)
colnames(resultTbl) = c("Global","WERM","BD","IPW")
rownames(resultTbl) = "Error"
print(resultTbl)
print(paste("Winner:",colnames(resultTbl)[which.min(resultArray)]))

