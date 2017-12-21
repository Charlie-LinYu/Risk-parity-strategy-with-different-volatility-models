############### 5.3 Cluster Risk Parity #############
Cluster_RiskParity<-function(cluster_result,perioddata,time,weightmethod,ercmethod){
  #cluster_result=clusterresult 
  cluster_number<-unique(cluster_result) 
  clusterweight<-matrix(0,nrow=length(cluster_number),ncol=length(cluster_result)) 
  ## allcluster stores the product and their corresponding
  clusterelement<-matrix(NA,nrow=length(cluster_number),ncol=length(cluster_result)) 
  ## clusterelement is a matrix, each row stands for a big cluster, and the element in the row indicates whether the product belongs to the certain row.
  for(i in c(1:length(cluster_number))){
    for(j in c(1:length(cluster_result))){
      if(cluster_result[j]==cluster_number[i]){
        clusterelement[i,j]<-names(cluster_result[j])
      }
    }
  }
  for(i in 1:length(cluster_number)){
    #i=4
    ## elementnames choose the product in the i-th cluster 
    elementnames<-as.character(na.omit(clusterelement[i,]))
    if(length(elementnames)>1){
      clusterdata<-data.frame(Date=perioddata[,'Date'])
      clusterdata[,elementnames]<-perioddata[,elementnames]
      within_covmatrix<-Cov_Relation(clusterdata,"covariance")
      within_covmatrix<-as.matrix(within_covmatrix)
      within_weight<-Basic_RiskParity(within_covmatrix,"erc","solnp") #calculate the weight according to the risk parity
    }else{
      within_weight<-1
    }
    ## if elementnames have more than one product, we will have a cov-matrix; 
    ## if only one product, we will have the variance of that product 
    ## within_weight is the weight of each product in one cluster
    
    clusterweightvec<-rep(0,length(cluster_result))
    in_cluster<-(clusterelement[i,]>-Inf)
    in_cluster[is.na(in_cluster)]<-FALSE
    clusterweightvec[in_cluster]<-within_weight 
    
    clusterweight[i,]<-clusterweightvec
  }
  
  ## After we get the weight within each cluster,we will calculate a weighted-return for each cluster every day in the data.
  ## calculate the covariance of the weighted return and then using RP to weight among different clusters.
  clusterReturn<-data.frame(Date=perioddata$Date) 
  for(i in 1:length(cluster_number)){
    clusterReturn[,i+1]<-NA
    for(j in c(1:length(clusterReturn$Date))){
      clusterweight1<-clusterweight[i,]  ## clusterweight[i,] stores the i-th group 
      for(k in c(1:length(clusterweight1))){
        if(clusterweight1[k]==0){
          clusterweight1[k]<-NA ## since the product is not in cluster i
        }
      }
      clusterweight1<-as.numeric(na.omit(clusterweight1))
      clusterReturn[j,i+1]<-clusterweight1%*%as.numeric(perioddata[j,as.character(na.omit(clusterelement[i,]))])
    }
  }
  colnames(clusterReturn)[-1]<-sapply(c(1:length(cluster_number)), function(x) paste('cluster',x,sep=""))
  if(length(cluster_number)==1){
    across_weight<-1
    }
  else{
      across_covmatrix<-as.matrix(cov(clusterReturn[-1]))
    }
  weight0<-rep(1/length(cluster_number),length(cluster_number))
  across_weight<-Basic_RiskParity(across_covmatrix,weightmethod,ercmethod)
  
  weight<-as.numeric(t(clusterweight)%*%across_weight)
  return(weight)
}