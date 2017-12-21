#this function is used to draw the effecient frontier
get_EF_plot<-function(k,Numports,Weighttable,pvector,Ratedata,rp_index){
    ###k=15
    ###Numports=100
    ##used parameter  Weighttable,pvector,Ratedata,rp_index
    print(k)
    product_include<-pvector[Weighttable[k,pvector]==1]
    if(length(product_include)>0){
        perioddata<-Period_Data(Ratedata,product_include,historymonth,Weighttable[k,'startdate'])
        
        ExpCovariance<-Cov_Relation(perioddata,"covariance") # if we use cluster method, then cov_criteria should be "correlation" 
        #product_include<-colnames(covariance)
        ExpCovariance<-as.matrix(ExpCovariance)
        ExpCovariance<-ExpCovariance*120
        ExpReturn<-sapply(perioddata[,-1],Return.cumulative)
        ExpReturn<-as.vector(ExpReturn)
    }
    rp_index2<-rp_index
    rp_index2[,2]<-as.ts(rp_index2[,2])
    rp_index2<-dplyr::mutate(rp_index2,return=Return.calculate(rp_index2[,2])) %>% dplyr::filter(Date>=perioddata[1,1] &Date<=perioddata[length(perioddata[,1]),1])
    rp_index2<-data.frame(sqrt(120*var(rp_index2[,3])),Return.cumulative(rp_index2[,3]),"RP_Index")
    

    size <- length(ExpReturn)
    if(dim(ExpCovariance)[1] != size | dim(ExpCovariance)[2] != size    )
        stop("Size is wrong!")
    
    minReturn <- round(range(ExpReturn)[1], 4) + 1/10^4
    maxReturn <- round(range(ExpReturn)[2], 4)*0.999  ## get the interval of expected return
    
    
    seqReturn <- seq(minReturn, maxReturn, length.out = Numports)
    
    risks <- 0; risks <- risks[-1];
    for(i in 1:Numports){
        ###i=9
        ###  solve a point of (return,risk) by quadratic programming
        A <- cbind(rep(1, size), ExpReturn,diag(rep(1, size)),diag(rep(-1,size))) #constraints
        D <- ExpCovariance 
        x <- rep(0,length(ExpReturn)) # expected return
        b <- c(1, seqReturn[i], rep(0, size),rep(-1,size)) #value of constraints
        res <- solve.QP(D, x, A, b, meq = 2 )  
        
        risks <- c(risks,t(res$solution) %*% ExpCovariance %*% res$solution)
    }
    risks<-sqrt(risks)
    EF_data <- data.frame(PortRisk = risks, PortReturn = round(seqReturn,4))
    EF_data<-data.frame(EF_data,NA)
    names(EF_data)[3]<-"Product"
    product<-data.frame(sqrt(diag(ExpCovariance)),ExpReturn,product_include)
    names(product)<-names(EF_data)
    names(rp_index2)<-names(EF_data)
    product<-rbind(product,rp_index2)
    EF_data<-rbind(EF_data,product)
    EF_pic<-ggplot(EF_data,aes(x=PortRisk,y=PortReturn,color=Product))+geom_point()+
        geom_text(aes(label=Product),vjust = -1)+ggtitle(paste("Efficient Frontier on",Weighttable[k,1],seq=""))
    return(EF_pic)
}





