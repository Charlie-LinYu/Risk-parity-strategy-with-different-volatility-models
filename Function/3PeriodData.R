
######### 3 Period Data ###################
#### this function is used to keep the data of the product in the certain period and the history data of 
###### these products that used to estimate the covariance or correlation matrix. 
Period_Data<-function(Ratedata,product_include,historymonth,startdate){
#   startdate=Weighttable[i,'startdate']
  
  maturity <- as.Date(startdate)
  m <- as.POSIXlt(maturity)
  m$mon <- m$mon - historymonth
  daystart<-as.Date(m)
  # daystart<-as.Date(as.yearmon(startdate)-historymonth/12)
  
  dayend<-as.Date(startdate)-1
  
  logic<-(Ratedata[,"Date"]>=daystart & Ratedata[,"Date"]<=dayend)
  
  datamatrix<-as.data.frame(matrix(NA,ncol=length(product_include)+1,nrow=length(Ratedata[logic,"Date"])))
  
  colnames(datamatrix)<-c('Date',paste(product_include,sep='')) 
  
  datamatrix[,'Date']<-Ratedata[logic,"Date"]
  
  datamatrix[,product_include]<-sapply(product_include,function(x) datamatrix[,x]<-Ratedata[logic,x])
   
 
  ## if some product's return is 0 in the middle of data, that means that it doesn't have information for the price on that day.
  #clean up non-trade date
#   datamatrix<-datamatrix[,-which(datamatrix[which(datamatrix$Date==min(datamatrix[,'Date'])),]==0)]
  #update tradeable product
#   product_include<-colnames(datamatrix)
#   product_include<-product_include[-which(product_include=='Date')]
  # IfNoData=rep(TRUE,length(datamatrix[,'Date']))
  # for(i in 1:length(datamatrix[,'Date'])){
  #   for(j in product_include){
  #     if(datamatrix[i,j]==0){
  #       IfNoData[i]=FALSE
  #     }
  #   } 
  # }
  # datamatrix=datamatrix[IfNoData,]
  return(datamatrix)
}
# Notice that we only choose product that include in the period, since that some products don't have data
## at the beginning of this period, but from some dates, we should negelect them. 
## So we first choose the product_include and then find PeriodData of the included produts.