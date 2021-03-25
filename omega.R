function(retornos, h){
ret <- retornos 
h <- h 
n <- ncol(ret)
n.act <- n.act
omegai <- rep(0,n)
for(j in 1:n){
  omegai[j]<- sum(pmax(ret[,j]-h,0))/sum(pmax(h-ret[,j],0))
}

names(omegai) <- colnames(retornos)
omegasort <- omegai[order(-omegai)]
names <- names(omegasort)
clasif.omega <- cbind(retornos[,names[1:n.act]])

ret.omega <- coredata(clasif.omega)

if (short == 1) {
  m <- model()
  m$variable(portfolio, lb = -1) # the portfolio choice vector; 
  m$maximize( omega(portfolio) )
  opt <- optimize(m, solver="glpk", 
                  data=list(returns = ret.omega)) 
  wpomega <- round(opt$solution[grep("portfolio", names(opt$solution))]/
                     opt$solution[grep("z", names(opt$solution))], 3)
  
  names(wpomega) <- names(clasif.omega)
  wpomega <- t(wpomega)
  
  muo <- colMeans(clasif.omega)
  covo <- cov(clasif.omega)
  rpomega <- muo%*%t(wpomega)
  sigmapomega <- wpomega%*%covo%*%t(wpomega)
  
  
  ret.po <- clasif.omega%*%t(wpomega)
  omegap <- sum(pmax(ret.po-h,0))/sum(pmax(h-ret.po,0))
  
  POM <- list()
  POM[[1]] <- wpomega
  POM[[2]] <- rpomega
  POM[[3]] <- sigmapomega
  return(POM)
}
else{
  m <- model()
  m$variable(portfolio, lb = 0) # the portfolio choice vector; 
  m$maximize( omega(portfolio) )
  opt <- optimize(m, solver="glpk", 
                  data=list(returns = ret.omega)) 
  wpomega <- round(opt$solution[grep("portfolio", names(opt$solution))]/
                     opt$solution[grep("z", names(opt$solution))], 3)
  
  names(wpomega) <- names(clasif.omega)
  wpomega <- t(wpomega)
  
  muo <- colMeans(clasif.omega)
  covo <- cov(clasif.omega)
  rpomega <- muo%*%t(wpomega)
  sigmapomega <- wpomega%*%covo%*%t(wpomega)
  
  
  ret.po <- clasif.omega%*%t(wpomega)
  omegap <- sum(pmax(ret.po-h,0))/sum(pmax(h-ret.po,0))
  
  POM <- list()
  POM[[1]] <- wpomega
  POM[[2]] <- rpomega
  POM[[3]] <- sigmapomega
  return(POM)
  
  }
}