Read_Data<-function(asset_class_address,pvector,startdate,enddate){
  
  # 1 read into data of the products that we choose 
  pvectorname<-pvector
  for(i in pvectorname){
    
    ## 1.1 read into data for every products
#     i<-pvectorname[1]
#     pvectorname<-i
    print(i)
    file<-paste(asset_class_address[grep(pattern=paste("\\b",i,".csv",sep=''),asset_class_address)],sep="")
    data<-read.csv(file,header=T)
    #dplyr and MASS package have same select function
    data<-dplyr::select(data,Date,Close)
    colnames(data)<-c("Date",i)
    data$Date<-as.Date(data$Date)
    # the second colume is the Close price,some products have 0 for no information, we turn all into NA for no-information
    data[,2][data[,2]==0]<-NA
    
    ## 1.2 merge all of them together 
    if(i==pvectorname[1]){
      inputdata<-data
    }else{
      inputdata<-merge(inputdata,data,by="Date",all=T)
    }
  }
  inputdata$Date<-as.Date(inputdata$Date)
  inputdata<-arrange(inputdata,Date)

  #find pre-startdate data
  maturity <- as.Date(startdate)
  m <- as.POSIXlt(maturity)
  # m$mon <- m$mon - historymonth 
  m$mon <- m$mon - historymonth - 1
  startdate<-as.Date(m)
  ## 1.3 use the commodity futures calendar
  #Calendar<-TradeCalendar(holiday_table_address,specialday_table_address,startdate,enddate)
  #inputdata<-inputdata[which(inputdata$Date%in%Calendar$Date),]
  
  ## 1.4 find start-date of each product when they have data,and replace NA with the price of ealier days, since they already have data
  price_begin<-as.Date(sapply(pvectorname,function(x) as.character(min(na.omit(inputdata[,c(x,"Date")])$Date))))
  
  #### Interpolation Missing Number
  inputdata<-cbind(inputdata%>%dplyr::select(Date),na.approx(inputdata%>%dplyr::select(-Date),na.rm = FALSE))

  # 2 calculate return 
  ratedata<-as.data.frame(matrix(NA,ncol=length(pvectorname)+1,nrow=length(inputdata[,"Date"])-1))
  colnames(ratedata)<-c('Date',paste(pvectorname,sep='')) 
  ratedata[,"Date"]<-inputdata[2:length(inputdata[,"Date"]),"Date"]
  ratedata[,"Date"]<-as.Date(as.character(ratedata[,"Date"]))
  ratedata[,pvectorname]<-sapply(pvectorname,function(x) diff(log(inputdata[,x])))
  #ratedata[ratedata==Inf]=0 
  return(ratedata)
}



