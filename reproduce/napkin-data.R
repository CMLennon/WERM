myXOR = function(W){
  myval = xor(W[,2],W[,1])
  for(idx in 3:(ncol(W))){
    myval = xor(myval,W[,idx])
  }
  return(myval)
}

dataGen = function(seednum,N,Nintv,D,C){
  # W should be high dim 
  
  set.seed(seednum)
  varval = 2
  
  c1 = rnorm(D,1,1)
  c2 = rnorm(D,-2,1)
  cz = rnorm(D,2,1)
  
  U1mean = -8; U1Var = 10
  U1 = rnorm(N,U1mean,U1Var)
  U1.intv = rnorm(Nintv,U1mean,U1Var)
  
  U2mean = 6; U2Var = 8
  U2 = rnorm(N,U2mean,U2Var)
  U2.intv = rnorm(Nintv,U2mean,U2Var)
  
  
  fW = function(N,U1,U2){
    Uw = rnorm(N,0,0.5)
    W = matrix(0,ncol=D,nrow=N)
    for (idx in 1:D){
      W[,idx] = rbinom(N,size=1,prob=inv.logit(c1[idx]*U1+c2[idx]*U2))
    }
    W = data.frame(W)
    colnames(W) = paste('W',1:D,sep="")
    return(W)
  }
  fZ = function(N,W){
    Uz = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    czmat = as.matrix(cz)
    Zval = inv.logit(Wmat %*% czmat)
    Z = round(inv.logit(-1*Zval + Uz-1 ))
    # Z = rbinom(N,size=1,inv.logit(-1*log(abs(Zval)+1) + Uz - 1  ))
    return(Z)
  }
  fX = function(N,U1,Z){
    Ux = rnorm(N,1,2)
    # X = rbinom(N,size=1,inv.logit(log(abs(1*(U1*Z))+1)* 2*(2*Z-1)*Ux - 4*Ux*exp(U1-2) -3))
    X = rbinom(N,size=1,inv.logit(1*U1 - 2*Z + Ux - 5  ))
    return(X)
  }
  fY = function(N,U2,X){
    Uy = rnorm(N,-2,1)
    ind.X = 2*X - 1 
    # Y =1*(2*U2 + ind.X- Uy )
    # Y = inv.logit(Y)
    
    Y = rbinom(N,size=1,inv.logit(0.5*U2 - 2*ind.X + Uy))
    # Y = rbinom(N,size=1,inv.logit(-1*ind.X* U2*log(abs(U2*ind.X)+1) + 0.1*U2-Uy))
    return(Y)
  }
  
  # OBS construction 
  W = fW(N,U1,U2)
  Z = fZ(N,W)
  X = fX(N,U1,Z)
  Y = fY(N,U2,X)
  OBS = data.frame(W,Z,X,Y)
  
  # INTV construction 
  # X.intv = seq(from=min(X),to=max(X),length.out=N)
  W.intv = fW(Nintv,U1.intv,U2.intv)
  Z.intv = fZ(Nintv,W.intv)
  X.intv = c(rep(0,Nintv/2),rep(1,Nintv/2))
  Y.intv = fY(Nintv,U2.intv,X.intv)
  INTV = data.frame(W.intv,Z.intv,X.intv,Y.intv)
  
  return(list(OBS,INTV))
}

numStrata = function(D,numCate){
  return(2 * (numCate^D) * 2)
}