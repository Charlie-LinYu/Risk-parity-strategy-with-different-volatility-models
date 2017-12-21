########  Risk Parity Weight Calculation ################
Basic_RiskParity<-function(covmatrix,weightmethod,ercmethod){
  
  #covmatrix<-covariance
  #covmatrix=across_covmatrix
  num<-length(covmatrix[1,])
  
  # method 1 : equally-weighted ##
  if(weightmethod=="ew"){
    weight<-rep(1/num,num)
  }
  
  ## other method
  eqn1<-function(x){
    z <- sum(x)
    return(z)
  }
  weight0<-rep(1/num,num)
  
  # method 2 : minimum variance ##
  # x is the weight that we want to get 
  if(weightmethod=="mv"){
    mv_method<-function(x,cov){
      return(t(x)%*%cov%*%t(t(x)))
    }
 
    optimal_mv<-function(x){return(mv_method(x,cov=covmatrix))}
    result<-solnp(weight0, fun=optimal_mv, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
    weight<-result$pars
  }
  
  # method 3 : most-diverisified portfolio ##
  if(weightmethod=="mdp"){
    mdp_method<-function(x,cov){
      return(-sum(x*diag(cov))/sqrt(t(x)%*%cov%*%t(t(x))))
    }
    optimal_mdp<-function(x){return(mdp_method(x,cov=covmatrix))}
    result<-solnp(weight0, fun=optimal_mdp, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
    weight<-result$pars
  }
  
  #method 4: equally-risk-contributed ##
  if(weightmethod=="erc" & ercmethod=="R"){
    weight<-Weights(PERC(covmatrix))/100
  }
  if(weightmethod=="erc" & ercmethod=="solnp"){
    erc_method<-function(x,cov){
      return(var(as.vector(x*(as.numeric(1/sqrt(t(x)%*%cov%*%t(t(x))))*t(cov%*%t(t(x))) ))))
    }
    optimal_erc<-function(x){return(erc_method(x,cov=covmatrix))}
    result<-solnp(weight0, fun=optimal_erc, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
    weight<-result$pars
  }
  if(weightmethod=="erc" & ercmethod=="gosolnp"){
    erc_method<-function(x,cov){
      return(var(as.vector(x*(as.numeric(1/sqrt(t(x)%*%cov%*%t(t(x))))*t(cov%*%t(t(x)))))))
    }
    optimal_erc<-function(x){return(erc_method(x,cov=covmatrix))}
    result<-gosolnp(weight0, fun=optimal_erc, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
    weight<-result$pars
  }
  return(weight)
}