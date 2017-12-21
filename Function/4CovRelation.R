
####### 2 Covariance or Correlation Function ###################

Cov_Relation<-function(perioddata,covmatrix_criteria){
  
  correlation=cor(perioddata[,product_include],method = c("pearson"))
  # if(covmatrix_criteria=="submatrix"){
  #   productmatrix<-perioddata
  #   productmatrix[,product_include]<-sapply(product_include,function(x) productmatrix[,x]<-perioddata[,x]-mean(perioddata[,x]))
  #   for(i in product_include){
  #     productmatrix[,i]<-sapply(1:length(productmatrix[,"Date"]),function(x) productmatrix[x,i]<-max(-productmatrix[x,i],0))
  #   }
  #   covmatrix<-cov(productmatrix[,product_include])
  # }
  if(covmatrix_criteria=="RealizedVolatility"){
    productmatrix<-perioddata
    vol<-sqrt(colMeans(productmatrix[,product_include]*productmatrix[,product_include]))
    vol<-vol*sqrt(240)
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="LogRealizedVolatility"){
    productmatrix<-perioddata
    vol<-sqrt(colMeans(productmatrix[,product_include]*productmatrix[,product_include]))
    vol<-vol*sqrt(240)
    vol<-log(vol)


    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="ARCH"){
    productmatrix<-perioddata
    vol<-c(NA)
    for (p in product_include){
      model<-garch(productmatrix[,p],order=c(0,2))
      tmp<-mean(na.omit(model$fitted.values[,1]))
      vol<-c(vol,tmp)
    }
    vol<-na.omit(vol)
    vol<-vol*sqrt(240)
    
    #vol=log(vol)           ###!!!
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="GARCH"){
    productmatrix<-perioddata
    vol=c(NA)
    for (p in product_include){
      model<-garch(productmatrix[,p],order=c(1,1))
      tmp<-mean(na.omit(model$fitted.values[,1]))
      vol<-c(vol,tmp)
    }
    vol<-na.omit(vol)
    vol<-vol*sqrt(240)
    
    #vol=log(vol)           ###!!!
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="SV"){
    productmatrix<-perioddata
    vol<-c(NA)
    for (p in product_include){
      model<-svsample((productmatrix[,p]-mean(productmatrix[,p])), burnin = 10000,priormu = c(-8,1),priorphi = c(20, 1.5),priorsigma = 0.2)
      tmp<-model$summary$latent0[6]
      vol<-c(vol,tmp)
    }
    vol<-na.omit(vol)
    vol<-vol*sqrt(240)
    
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="VaR"){
    productmatrix<-perioddata
    vol<-c(NA)
    for (p in product_include){
      #tmp<-VaR(R=productmatrix[,p],p=prob,method = "historical")
      tmp<-quantile(productmatrix[,p],0.05)
      vol<-c(vol,tmp)
    }
    vol<-na.omit(vol)
    vol<-vol*sqrt(240)
    
    vol<-abs(vol)
    #vol<-log(vol)           ###!!!
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  if(covmatrix_criteria=="CVaR"){
    productmatrix<-perioddata
    vol<-c(NA)
    for (p in product_include){
      traditionalvar<-quantile(productmatrix[,p],0.05)
      tmp<-mean(productmatrix[productmatrix[,p]<=traditionalvar,p])
      #tmp<-ETL(R=productmatrix[,p],p=prob,method = "historical")
      vol<-c(vol,tmp)
    }
    vol<-na.omit(vol)
    vol<-vol*sqrt(240)
    
    vol<-abs(vol)
    #vol=log(vol)           ###!!!
    covmatrix<-(t(t(vol))%*%vol)*correlation
  }
  covmatrix<-data.frame(covmatrix)
  return(covmatrix)
}