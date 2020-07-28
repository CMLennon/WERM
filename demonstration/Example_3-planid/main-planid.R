
source('planid-data.R') # Napkin dataset generation code 
source('planid-WERM.R')
source('planid-plugin.R')
source('../WERM_Heuristic.R')
N = 1000
Nintv = 1000000
D = 15 # Cardinality of W 

seednum = sample(1:1000000,1)
mytmp = dataGen(seednum,N,Nintv,D,C)
OBS = mytmp[[1]] # Observational dataset 
INTV = mytmp[[2]]

answer = c(mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 0)&(INTV$R.intv==1),'Y.intv']),
             mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==0),'Y.intv']),mean(INTV[(INTV$X.intv == 1)&(INTV$R.intv==1),'Y.intv']))
obsans = c(mean(OBS[(OBS$X == 0)&(OBS$R==0),'Y']),mean(OBS[(OBS$X == 0)&(OBS$R==1),'Y']),
             mean(OBS[(OBS$X == 1)&(OBS$R==0),'Y']),mean(OBS[(OBS$X == 1)&(OBS$R==1),'Y']))
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


