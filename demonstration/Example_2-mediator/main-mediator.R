# setwd("Example_2-mediator")
library(xgboost)
library(boot)

################################
# Dataset Generation 
################################
source('mediator-data.R') # Napkin dataset generation code 
source('mediator-WERM.R')
source('mediator-plugin.R')

N = 1000
Nintv = 1000000
D = 15 # Cardinality
numCate = 2
C = numCate - 1

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]] # Observational dataset 
INTV = mytmp[[2]]

answer = c(mean(INTV[INTV$X.intv==0,'Y.intv']),mean(INTV[INTV$X.intv==1,'Y.intv']))
obsans = c(mean(OBS[OBS$X==0,'Y']),mean(OBS[OBS$X==1,'Y']))
WERManswer = WERMEstimator(OBS,D)
PlugInanswer = PlugInEstimator(OBS,D)

print(round(answer,4))
print(round(WERManswer,4))
print(round(PlugInanswer,4))

WERMperformance = mean(abs(answer-WERManswer))
PlugInperformance = mean(abs(answer-PlugInanswer))
OBSperformance = mean(abs(answer-obsans))

performance_mat = matrix(round(c(PlugInperformance,OBSperformance,WERMperformance),4),ncol=3)
colnames(performance_mat) = c('PlugIn','OBS','WERM')
rownames(performance_mat) = 'Error'
print(performance_mat)
