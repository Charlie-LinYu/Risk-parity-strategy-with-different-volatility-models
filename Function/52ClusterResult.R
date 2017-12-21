######## 5.2 Cluster Risk Parity ################
############ part 1: cluster, decide how many group and product belongs to which one ######
Cluster_Result<-function(covmatrix,clustermethod,kcriteria){
  # covmatrix=correlation
  if(clustermethod=="static"){
    ## distance matrix
    distance<-as.dist(1 - covmatrix)
    #
    hclus12sampward <- hclust(distance,method="ward.D")
    ## display
    plot(hclus12sampward)
    ncluster=3
    # cut tree into 3 clusters
    allcluster <- cutree(hclus12sampward, k=ncluster)
  }## this method is not good, since it cannot choose the optimal k automatically 
  
  if(clustermethod=="dynamic"){
#     covmatrix<-correlation
    dissimilarity <- 1 - (covmatrix)
    distance <- as.dist(dissimilarity,diag = FALSE, upper = FALSE)
    
    xy <- cmdscale(distance) # get first 2 pricipal componenets
    n<-ncol(covmatrix) # Determine number of clusters
    n1 <- ceiling(n*2/3)
    p.exp <- rep(0,n1) # percentage of variance explained by clusters
    min.cor <- matrix(1,n1,n1)  # minimum correlation among all components in each cluster  
    for (i in 2:n1) {
      fit <- kmeans(xy, centers=i, iter.max=100, nstart=100)
      p.exp[i] = 1- fit$tot.withinss / fit$totss
      for (j in 1:i) {
        index <- fit$cluster == j
        min.cor[i,j] <- min(covmatrix[index,index])
      }
    }
    
    if(kcriteria=="cluster1"){
      # minimum number of clusters that explain at least 90% of variance
      # max(p.exp)
      num<-min(which(p.exp > 0.700))
      
      fit <- kmeans(xy, num, iter.max=100, nstart=100)
      allcluster<-fit$cluster
      clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
               main = paste('Major Market Clusters over, 3 Clusters'), sub='')
      # plotcluster(xy, fit$cluster)
    }else if(kcriteria=="cluster2"){
      # minimum number of clusters such that correlation among all components in each cluster is at least 40%
      # will not always work
      ## min.corr[-1,] is deleting the first row, thus we should add back 1 
      num<-min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1
      fit <- kmeans(xy, num, iter.max=100, nstart=100)
      allcluster<-fit$cluster
      #clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
      #main = paste('Major Market Clusters over, 3 Clusters,Cluster2'), sub='')
      
    }else if(kcriteria=="elbow"){
      dist.obj <- dist(xy)
      hclust.obj <- hclust(dist.obj)
      css.obj <- css.hclust(dist.obj,hclust.obj)
      ## css.obj contains the k, and the corresponding ev, which is part of total-between group SSE
      ## the larger of ev, the better
      ## but as the k becomes bigger, the marginal gain is smaller. thus we will have an optimal k
      ## thus the ev should be larger than ev.thres, but the increment should be not less than inc.thres
      elbow.obj <- elbow.batch(css.obj,ev.thres=0.90,inc.thres=0.01)
      ## then we will get a k that satisfies these requirement.
      k <- elbow.obj$k; 
      cutree.obj <- cutree(hclust.obj,k=k)
      mxy<-as.data.frame(xy)
      mxy$cluster <- cutree.obj
      clusplot(xy, cutree.obj , color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
               main = paste('Major Market Clusters over, 3 Clusters'), sub='')
      #plot(css.obj,elbow.obj,if.plot.new=FALSE)
      allcluster<-cutree.obj 
    }
  }
  return(allcluster)
}