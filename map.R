load("Sweden.Rdata")
SwedeMat=as.matrix(SwedeMat)

rangval = c(0,80)
norder=4
bbasis = create.bspline.basis(rangval,norder=4,nbasis=23)

v=rep(0,11)

lambdaV = c()
GCV <- c()
for(i in 1:11){
  lambda={10^(i-10)}
  D2fdPar = fdPar(bbasis,Lfdobj=2,lambda=lambda)
  
  swede = smooth.basis(0:80,SwedeMat,D2fdPar)
  
  v[i]=mean(swede$gcv)
  lambdaV = c(lambdaV, rep(i, length(swede$gcv)))
  GCV = c(GCV, swede$gcv)
}
plot(v)

D2fdPar = fdPar(bbasis,Lfdobj=2,lambda=1e-7)
swedfd = smooth.basis(0:80,SwedeMat,D2fdPar)

plot(swedfd$fd)

Xlist = list( rep(1,144), as.numeric(1:144) )

betabasis = create.bspline.basis(rangval,norder=4,nbasis=23)

# First we cross-validate

lambdas = 10^(-5:5)

CVmat = matrix(0,11,144)

for(i in 1:11){
  print(i)
  betaPar = fdPar(betabasis,2,lambdas[i])
  bwtlist = list(betaPar,betaPar)
  
  for(j in 1:144){
    tXlist = list(Xlist[[1]][-i],Xlist[[2]][-i])
    regList = fRegress(swedfd$fd[-j],tXlist,bwtlist)
    
    errhat = swedfd$fd[j] - regList$betaestlist[[1]]$fd - j*regList$betaestlist[[2]]$fd
    
    CVmat[i,j] = inprod(errhat,errhat) 
  }
}

