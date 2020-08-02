require(stats)
library(mise)
mise()
source('doubleeffect-data.R')
source('doubleeffect-est-global.R')
source('doubleeffect-param.R')
source('doubleeffect-est-heuristic.R')
source('doubleeffect-est-ID.R')

N = 1000
Nintv = 1000000

D = 10
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

Xunique = unique(OBS$X)[order(unique(OBS$X))]
Runique = unique(OBS$R)[order(unique(OBS$R))]
answer = c(0,0,0,0)
obsans = c(0,0,0,0)
idx = 1 
for (xval in Xunique){
  for (rval in Runique){
    INTVFiltered = subset(INTV,X.intv == xval & R.intv == rval)
    OBSFiltered = subset(OBS,X == xval & R == rval)
    answer[idx] = mean(OBSFiltered$Y)
    obsans[idx] = mean(INTVFiltered$Y.intv)
    idx = idx + 1 
  }
}

WERManswer = multiHeuristic(OBS,D,numCate)
PLUGINanswer = PlugInEstimator(OBS,D,numCate)
IDanswer = multiID(OBS,D,numCate)
GLOBALanswer = multiGlobal(OBS,D,numCate)

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
