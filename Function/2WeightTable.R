
Weight_Table<-function(Ratedata,pvector,startdate,enddate,historymonth,rebalancemonth){
  # find begin of return time and begin of return data including historymonth
  #Ratedata=Ratedata
  
  # Ratedata[Ratedata==0]<-NA
 
  #rebalancemonth=6 # how much time to re-calculate the weight
  # yeardatebegin<-as.Date(seq(as.yearmon(startdate),as.yearmon(enddate),by=rebalancemonth/12))
  yeardatebegin<-as.Date(seq(startdate, enddate, by = paste(rebalancemonth,'month')))
  
  yeardatabegin<-unique(sort(yeardatebegin))
  yeardatebegin<-yeardatebegin[yeardatebegin<=enddate]
  # yeardatebegin<-yeardatebegin[startdate<=yeardatebegin]
  yeardatabegin<-unique(sort(yeardatebegin))
  
  # 3 define the start and end of time, form the time interval 
  yeardateend<-c(yeardatebegin[-1]-1,as.Date(enddate))
  
  weightmatrix<-as.data.frame(matrix(NA,ncol=length(pvector)+2,nrow=length(yeardatebegin)))
  colnames(weightmatrix)<-c('startdate','enddate',paste(pvector,sep='')) 

  Getmatrixdate <- function (stdate,endate) { 
    Calendar<-TradeCalendar(holiday_table_address,specialday_table_address,stdate,endate)
    matrixd<-c(as.character(min(Calendar$Date)),as.character(max(Calendar$Date)))
    return(matrixd)
  }
  colnames(weightmatrix)<-c('startdate','enddate',paste(pvector,sep='')) 
  for (i in 1:length(yeardateend)){
    #i=59
    # i=27
    weightmatrix[i,"startdate"]<-as.character(Getmatrixdate(stdate=yeardatebegin[i],endate=yeardateend[i])[1])
    weightmatrix[i,"enddate"]<-as.character(Getmatrixdate(yeardatebegin[i],yeardateend[i])[2])
  }
  ### for every interval, if the return_begin1<startdate, we will not include the products when calculating weights.
  
  
  # return_begin1<-rep(0,length(pvector))
  # for(i in 1:length(return_begin)){
  #   # i=1
  #   temp<-return_begin[i]
  #   month<-format(temp,"%m")
  #   year<-format(temp,"%Y")
  #   day<-format(temp,"%d")
  #   newmonth<-as.numeric(month)+historymonth
  #   if(as.numeric(month)+historymonth<=12){
  #     return_begin1[i]<-paste(year,as.character(newmonth),day,sep="-")
  #   }else{
  #     newyear<-as.character(as.numeric(year)+1)
  #     newmonth<-newmonth-12
  #     return_begin1[i]<-paste(newyear,as.character(newmonth),day,sep="-")
  #   }
  # }
  # return_begin1<-as.Date(return_begin1)
  return_begin<-as.Date(sapply(pvector,function(x) as.character(min(na.omit(Ratedata[,c(x,"Date")])$Date))))
  # head(Ratedata)

  
  # is.integer0 <- function(x)
  # {
  #   is.integer(x) && length(x) == 0L
  # }
  
  
  for (i in 1:length(yeardateend)){
    ## note that the return_begin1 start time including the history period 
    # weightmatrix[i,pvector]<-as.numeric(return_begin1<weightmatrix[i,"startdate"] & is.na(return_begin1)==FALSE)
    # i=1
    maturity <- as.Date(weightmatrix[i,"startdate"])
    m <- as.POSIXlt(maturity)
    m$mon <- m$mon - historymonth
    histdate<-as.Date(m)
    # weightmatrix[i,pvector]<-as.numeric(length(which(as.yearmon(Ratedata$Date)==as.yearmon(histdate)))>5 )
    weightmatrix[i,pvector]<-as.numeric(return_begin<=histdate & is.na(return_begin)==FALSE)
  }
  return(weightmatrix)
}










