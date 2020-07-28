# setwd("Example_1-napkin")
library(xgboost)
library(boot)
library(mise)
mise()

################################
# Dataset Generation 
################################
source('napkin-data.R') # Napkin dataset generation code 
source('napkin-WERM.R')
source('napkin-plugin.R')

N = 1000
Nintv = 100000
D = 10 # Cardinality of W 
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]] # Observational dataset 
INTV = mytmp[[2]]

Xunique = unique(OBS$X)[order(unique(OBS$X))]
obsans = rep(0,length(Xunique))
answer = rep(0,length(Xunique))
idx = 1
yval = 1 
for (xval in Xunique){
  obsans[idx] = nrow(subset(OBS,X==xval & Y==yval))/nrow(subset(OBS,X==xval))
  answer[idx] = nrow(subset(INTV,X.intv==xval & Y.intv==yval))/nrow(subset(INTV,X.intv==xval))
  idx = idx + 1 
}

# answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv'])); answerATE = answer[2]-answer[1]
# obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y'])); obsATE = obsans[2]-obsans[1]
WERManswer = WERMEstimator(OBS,D); WERMATE = WERManswer[2]-WERManswer[1]
PlugInanswer = PlugInEstimator(OBS,D); PlugInATE = PlugInanswer[2]-PlugInanswer[1]

obs_xy = c(nrow(subset(OBS,X==0&Y==1))/nrow(OBS),nrow(subset(OBS,X==1&Y==1))/nrow(OBS))
obs_x = c(nrow(subset(OBS,X==0))/nrow(OBS),nrow(subset(OBS,X==1))/nrow(OBS))
intv_xy = c(nrow(subset(INTV,X.intv==0&Y.intv==1))/nrow(INTV),nrow(subset(INTV,X.intv==1&Y.intv==1))/nrow(INTV))
intv_x = c(nrow(subset(INTV,X.intv==0))/nrow(INTV),nrow(subset(INTV,X.intv==1))/nrow(INTV))
obs_y.x = obs_xy/obs_x
intv_y.x = intv_xy/intv_x

print(c("Answer:",round(answer,4)))
print(c("PlugIn:",round(PlugInanswer,4)))
print(c("OBS:",round(obsans,4)))
print(c("WERM:",round(WERManswer,4)))


WERMperformance = mean(abs(answer-WERManswer))
PlugInperformance = mean(abs(answer-PlugInanswer))
OBSperformance = mean(abs(answer-obsans))

performance_mat = matrix(round(c(PlugInperformance,OBSperformance,WERMperformance),4),ncol=3)
colnames(performance_mat) = c('PlugIn','OBS','WERM')
rownames(performance_mat) = 'Error'
print(performance_mat)
