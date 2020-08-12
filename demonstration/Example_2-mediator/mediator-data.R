# library(boot)
# library(ipw)

dataGen = function(seednum,N,Nintv,D,C){
  # W should be high dim 
  
  set.seed(seednum)
  varval = 1
  
  c1 = rnorm(D,-2,0.5)
  c2 = rnorm(D,1,0.5)
  
  cx = rnorm(D,2,0.5)
  cz = rnorm(D,-0.8,0.5)
  cy = rnorm(D,1.5,0.5)
  
  U1 = rnorm(N,0,varval)
  U2 = rnorm(N,0,varval)
  U3 = rnorm(N,0,varval)
  U1intv = rnorm(Nintv,0,varval)
  U2intv = rnorm(Nintv,0,varval)
  U3intv = rnorm(Nintv,0,varval)
  
  fW = function(N,U1,U2){
    Uw = rnorm(N,0,0.5)
    W = matrix(0,ncol=D,nrow=N)
    for (idx in 1:D){
      W[,idx] = rbinom(N,size=1,prob=inv.logit(c1[idx]*U1+c2[idx]*U2 +Uw))
    }
    W = data.frame(W)
    colnames(W) = paste('W',1:D,sep="")
    return(W)
  }
  fX = function(N,W,U1,U3){
    Ux = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    cxmat = as.matrix(cx)
    Wval = inv.logit(Wmat %*% cxmat)
    # X = round(inv.logit(-1*Wval + Ux - U1 + U3 ))
    X = rbinom(N,size=1,inv.logit(-1*Wval - 2*U1 + 0.5*U3*Wval + Ux - 2*U1*U3 ))
    return(X)
  }
  fZ = function(N,W,X){
    Uz = rnorm(N,0,1)
    Wmat = as.matrix(2*W-1)
    czmat = as.matrix(cz)
    Wval = inv.logit(Wmat %*% czmat)
    # Z = rbinom(N,size=1,inv.logit(1*Wval - 2*(2*X-1) + Uz ))
    # X = rbinom(N,size=1,inv.logit(1*U1 - 2*Z + Ux ))
    Z = rbinom(N,size=1,inv.logit((1*Wval*(2*X-1) - 2*(2*X-1) )**2+ Uz -2  ))
    return(Z)
  }
  fY = function(N,U2,U3,Z,W){
    Uy = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    cymat = as.matrix(cy)
    Wval = inv.logit(Wmat %*% cymat)
    Y = rbinom(N,size=1,inv.logit(-(0.5*U2 -0.8*U3)**2 + 2*log(abs((2*Z-1)*Wval)+1)- U3*U2))
    # Y = rbinom(N,size=1,inv.logit(-U3-U2+Z-Wval+1))
    return(Y)
  }
  
  # OBS construction 
  W = fW(N,U1,U2)
  X = fX(N,W,U1,U3)
  Z = fZ(N,W,X)
  Y = fY(N,U2,U3,Z,W)
  OBS = data.frame(W,X,Z,Y)
  
  # INTV construction 
  Wintv = fW(Nintv,U1intv,U2intv)
  X.intv = c(rep(0,Nintv/2),rep(1,Nintv/2))
  Zintv = fZ(Nintv,Wintv,X.intv)
  Y.intv = fY(Nintv,U2intv,U3intv,Zintv,Wintv)
  INTV = data.frame(Wintv,X.intv,Zintv,Y.intv)
  
  return(list(OBS,INTV))
}

