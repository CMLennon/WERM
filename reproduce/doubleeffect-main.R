require(stats)
library(mise)
mise()
source('doubleeffect-data.R')
source('doubleeffect-est-global.R')
source('doubleeffect-as-BD.R')
source('doubleeffect-param.R')
source('doubleeffect-est-heuristic.R')
source('doubleeffect-est-ID.R')
source('doubleeffect-as-IPW.R')

computePerformance_Double = function(OBS,answer,prediction){
  Xunique = unique(OBS$X)[order(unique(OBS$X))]
  Runique = unique(OBS$R)[order(unique(OBS$R))]
  idx = 1 
  proportion_X = rep(0,length(Xunique)*length(Runique))
  for (xval in Xunique){
    for (rval in Runique){
      proportion_X[idx] = nrow(subset(OBS,X==xval & R==rval))/nrow(OBS)
      idx = idx + 1 
    }
  }
  return(sum(abs(answer-prediction)*proportion_X,na.rm=T))
}

N = 1000
Nintv = 1000000

D = 2
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
# seednum = 123
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]]
INTV = mytmp[[2]]

Xunique = unique(OBS$X)[order(unique(OBS$X))]
Runique = unique(OBS$R)[order(unique(OBS$R))]
answer = c(0,0,0,0)
# obsans = c(0,0,0,0)
idx = 1 
for (xval in Xunique){
  for (rval in Runique){
    INTVFiltered = subset(INTV,X.intv == xval & R.intv == rval)
    # OBSFiltered = subset(OBS,X == xval & R == rval)
    answer[idx] = mean(INTVFiltered$Y.intv)
    # obsans[idx] = mean(INTVFiltered$Y.intv)
    idx = idx + 1 
  }
}

WERManswer = multiHeuristic(OBS,D,numCate)
GLOBALanswer = multiGlobal(OBS,D,numCate)
BDanswer = asBDEstimator(OBS,D,numCate)
# asIPWanswer = asIPWEstimator(OBS,D,numCate)

WERMperformance = mean(abs(WERManswer - answer))
GLOBALperformance = mean(abs(GLOBALanswer - answer))
BDperformance = mean(abs(BDanswer - answer))

# WERMperformance = computePerformance_Double(OBS,answer,WERManswer)
# GLOBALperformance = computePerformance_Double(OBS,answer,GLOBALanswer)
# asBDperformance = computePerformance_Double(OBS,answer,asBDanswer)
# 
# # resultArray = c(PLUGINperformance,GLOBALperformance,WERMperformance,IDperformance,asBDperformance,asIPWperformance)
resultArray = c(GLOBALperformance,WERMperformance,BDperformance)
resultTbl = matrix(round(resultArray,3),ncol=3)
colnames(resultTbl) = c("Global","WERM","BD")
rownames(resultTbl) = "Error"
print(resultTbl)
print(paste("Winner:",colnames(resultTbl)[which.min(resultArray)]))
