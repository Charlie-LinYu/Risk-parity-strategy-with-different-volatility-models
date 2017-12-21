
############## 7 VaR and CVaR ################
########  Risk Parity Weight Calculation ################
Var_RiskParity<-function(var_criteria,mean_return,covmatrix,prob,ercmethod){
  num=length(covmatrix[1,])
  eqn1<-function(x){
    z = sum(x)
    return(z)
  }
  weight0=rep(1/num,num)
  
  if(var_criteria=='standard'){
    standard_method<-function(mu,x,cov){
      return(var(as.vector(x*(as.numeric(1/sqrt(t(x)%*%cov%*%t(t(x))))*t(cov%*%t(t(x))))-x*mu)))
      }
    optimal_standard<-function(x){return(standard_method(x,mu=mean_return,cov=covmatrix))}
    if(ercmethod=="solnp"){
      result<-solnp(weight0, fun=optimal_standard, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }else if(ercmethod=="gosolnp"){
        result<-gosolnp(weight0, fun=optimal_standard, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }
    }
    
  if(var_criteria=="var"){
    var_method<-function(mu,x,cov,p){
      return(var(as.vector(x*(as.numeric(1/sqrt(t(x)%*%cov%*%t(t(x))))*qnorm(p)*t(cov%*%t(t(x))))-x*mu)))
      }
    optimal_var<-function(x){return(var_method(x,mu=mean_return,cov=covmatrix,p=prob))}
    if(ercmethod=="solnp"){
      result<-solnp(weight0, fun=optimal_var, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }else if(ercmethod=="gosolnp"){
        result<-gosolnp(weight0, fun=optimal_var, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }
    }
  
  if(var_criteria=='cvar'){
    cvar_method<-function(mu,x,cov,p){
      return(var(as.vector(x*(as.numeric(1/sqrt(t(x)%*%cov%*%t(t(x))))*dnorm(qnorm(p))/(1-p)*t(cov%*%t(t(x))))-x*mu)))
      }
    optimal_cvar=function(x){return(cvar_method(x,mu=mean_return,cov=covmatrix,p=prob))}
    if(ercmethod=="solnp"){
      result<-solnp(weight0, fun=optimal_cvar, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }else if(ercmethod=="gosolnp"){
        result<-gosolnp(weight0, fun=optimal_cvar, eqfun = eqn1, eqB = 1, LB =rep(0,num),UB=rep(1,num))
      }
    }
  weight=result$pars
  }














