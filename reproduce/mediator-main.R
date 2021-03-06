require(stats)
library(mise)
mise()
source('mediator-data.R') 
source('mediator-est-global.R')
source('mediator-param.R')
source('mediator-est-heuristic.R')
source('mediator-est-ID.R')
source('mediator-as-BD.R')
source('mediator-as-IPW.R')

N = 1000
Nintv = 1000000

D = 10
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))

WERManswer = multiHeuristic(OBS,D,numCate)
PLUGINanswer = PlugInEstimator(OBS,D,numCate)
IDanswer = multiID(OBS,D,numCate)
GLOBALanswer = multiGlobal(OBS,D,numCate)
asBDanswer = asBDEstimator(OBS,D,numCate)
asIPWanswer = asIPWEstimator(OBS,D,numCate)

WERMperformance = mean(abs(WERManswer-answer))
GLOBALperformance = mean(abs(GLOBALanswer-answer))
PLUGINperformance = mean(abs(PLUGINanswer-answer))
IDperformance = mean(abs(IDanswer-answer))
asBDperofrmance = mean(abs(asBDanswer-answer))
asIPWperformance = mean(abs(asIPWanswer-answer))

resultArray = c(PLUGINperformance,GLOBALperformance,WERMperformance,IDperformance,asBDperofrmance,asIPWperformance)
resultTbl = matrix(round(resultArray,3),ncol=6)
colnames(resultTbl) = c("PlugIn","Global","WERM","ID","BD","IPW")
rownames(resultTbl) = "Error"
print(resultTbl)
print(paste("Winner:",colnames(resultTbl)[which.min(resultArray)]))
