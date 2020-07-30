library(xgboost)
library(boot)
source('WERM_Heuristic.R')

multiHeuristic = function(OBS,D,numCate){
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
  
  ################################################################
  # Learn h and W.
  ################################################################
  # regvallist = c(1:100)*(nrow(OBS))/20
  regvallist = seq(0,10,by=0.2)
  lambda_W = learnHyperParam(regvallist,data.matrix(data.frame(W=W,Z=Z)),SW_importance_sampling,0)/nrow(OBS)
  lambda_W = lambda_W/nrow(OBS)
  learned_W = learnWdash(SW_importance_sampling,data.matrix(data.frame(W=W,Z=Z)),lambda_W)
  lambda_h = learnHyperParam(regvallist,data.matrix(data.frame(X=X)),Y,1)/nrow(OBS)
  
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  WERManswer = c(Yx0,Yx1)
  return(WERManswer)
}


