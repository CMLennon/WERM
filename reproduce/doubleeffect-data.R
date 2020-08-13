# library(boot)
# library(ipw)

myXOR = function(W){
  xorval = rep(1,nrow(W))
  for (idx in 1:(D-1)){
    xorval = xor(xorval,W[,(idx+1)])
  }
  return(xorval)
}

dataGen = function(seednum,N,Nintv,D,C){
  # W should be high dim 
  
  set.seed(seednum)
  varval = 1
  
  c.z.1 = rnorm(D,-2,0.5)
  c.z.2 = rnorm(D,1,0.5)
  c.z.3 = rnorm(D,0,1)
  
  c.w.1 = rnorm(D,2,0.5)
  c.w.2 = rnorm(D,-1,0.5) 
  c.w.3 = rnorm(D,1,0.5)
  
  cx = rnorm(D,2,0.5)
  cr = rnorm(D,-1,1)
  cz = rnorm(D,-2,0.3)
  
  ################################################################################
  # Unmeasured Confounders 
  ################################################################################
  U1 = rnorm(N,-1,varval) # Z,Y 
  U2 = rnorm(N,-0.5,varval) # Z,W
  U3 = rnorm(N,0.5,varval) # Z,R 
  U4 = rnorm(N,1,varval) # W,X

  U1intv = rnorm(Nintv,-1,varval)
  U2intv = rnorm(Nintv,-0.5,varval)
  U3intv = rnorm(Nintv,0.5,varval)
  U4intv = rnorm(Nintv,1,varval)

  ################################################################################
  # Generate W
  ################################################################################
  fW = function(N,U1,U2){
    Uw = rnorm(N,0,0.5)
    W = matrix(0,ncol=D,nrow=N)
    for (idx in 1:D){
      # W[,idx] = rbinom(N,size=1,prob=inv.logit(c.w.1[idx]*U1+c.w.2[idx]*U2 + Uw))
      W[,idx] = c.w.1[idx]*U1+c.w.2[idx]*U2 + Uw
    }
    W = data.frame(W)
    colnames(W) = paste('W',1:D,sep="")
    return(W)
  }
  
  ################################################################################
  # Generate X
  ################################################################################
  fX = function(N,W,U1,U3){
    Ux = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    cxmat = as.matrix(cx)
    Wval = inv.logit(Wmat %*% cxmat)
    # Wval = myXOR(W)
    # X = rbinom(N,size=1,inv.logit(-1*Wval + -0.5*U1 - 0.2*U3 + Ux-2 ))
    X = rbinom(N,size=1,inv.logit(-5*Wval*U1 + -0.5*U1 - 0.2*U3*U1 + Ux-2 ))
    return(X)
  }
  
  ################################################################################
  # Generate R
  ################################################################################
  fR = function(N,W,U4){
    Ur = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    crmat = as.matrix(cr)
    Wval = inv.logit(Wmat %*% crmat)
    # Wval = myXOR(W)
    R = rbinom(N,size=1,inv.logit(-10*Wval - 1.2*U4*Wval + Ur - 2))
    return(R)
  }
  
  ################################################################################
  # Generate Z
  ################################################################################
  fZ = function(N,W,X,R,U4){
    Uz = rnorm(N,0,0.5)
    Wmat = as.matrix(2*W-1)
    czmat = as.matrix(cz)
    Wval = inv.logit(Wmat %*% czmat)
    # Wval = myXOR(W)
    Z = rbinom(N,size=1,inv.logit(0.5*Wval+U4 + 5*(2*X-1) - 9*(2*R-1) + Uz-1 - log(abs(Wval)+1) ))
    # Z = rbinom(N,size=1,inv.logit(0.5*Wval+U4 + 0.5*(2*X-1) - 0.9*(2*R-1) + Uz-1 ))
    return(Z)
  }

  ################################################################################
  # Generate Y
  ################################################################################
  fY = function(N,R,Z,U2,U3){
    Uy = rnorm(N,0,0.5)
    # Y = rbinom(N,size=1,inv.logit(-1*(2*R-1)*Z + 0.5*(2*Z-1)*log(abs(U2*U3)+1) - 3*R*U2- Uy +1))
    Y = 5*(2*R-1)*Z + 0.5*(2*Z-1)*log(abs(U2*U3)+1) - 10*R*U2- Uy +1
    # Y = rbinom(N,size=1,Y)
    # Y = (Y > 0.5)*1
    Y = (Y - min(Y))/(max(Y)-min(Y))
    # Y = rbinom(N,size=1,inv.logit(R*Z*U2*U3-1))
    return(Y)
  }

  
  
  ################################################################################
  # Data Generation 
  ################################################################################
  # OBS 
  W = fW(N,U1,U2)
  X = fX(N,W,U1,U3)
  R = fR(N,W,U4)
  Z = fZ(N,W,X,R,U4)
  Y = fY(N,R,Z,U2,U3)
  OBS = data.frame(W,X,R,Z,Y)
  
  # INTV 
  W.intv = fW(Nintv,U1intv,U2intv)
  X.intv = c(rep(0,Nintv/2),rep(1,Nintv/2))
  R.intv = c(rep(0,Nintv/4),rep(1,Nintv/4),rep(0,Nintv/4),rep(1,Nintv/4))
  Z.intv = fZ(Nintv,W.intv,X.intv,R.intv,U4intv)
  Y.intv = fY(Nintv,R.intv,Z.intv,U2intv,U3intv)
  INTV = data.frame(W.intv,X.intv,R.intv,Z.intv,Y.intv)
  return(list(OBS,INTV))
}

numStrata = function(D,numCate){
  return(2 * (numCate^D) * 2)
}

# 
# N = 100
# Nintv = 10000
# 
# D = 5
# numCate = 2
# C = numCate - 1
# 
# seednum = sample(1:1000000,1)
# mytmp = dataGen(seednum,N,Nintv,D,C)
# OBS = mytmp[[1]]
# INTV = mytmp[[2]]

