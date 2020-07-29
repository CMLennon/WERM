source('WERM_Heuristic.R')
multiID = function(OBS,D,numCate){
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
  learned_W = SW_importance_sampling
  lambda_h = rep(0,nrow(OBS))
  Yx0 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(0,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  Yx1 = WERM_Heuristic(inVar_train = data.frame(X=X,Z=Z), inVar_eval = data.frame(X=rep(1,nrow(OBS)),Z=Z), Y = Y, Ybinary = 1, lambda_h = lambda_h, learned_W= learned_W)
  myans = c(Yx0,Yx1)
  
  return(myans)
}





