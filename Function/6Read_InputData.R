Read_InputData<-function(asset_class_address,pvector,startdate,enddate){
  
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
    data[,2][data[,2]==0]=NA
    
    ## 1.2 merge all of them together 
    if(i==pvectorname[1]){
      inputdata<-data
    }else{
      inputdata<-merge(inputdata,data,by="Date",all=T)
    }
  }
  inputdata$Date<-as.Date(inputdata$Date)
  inputdata<-arrange(inputdata,Date)
  
  ## 1.3 use the commodity futures calendar
  maturity <- as.Date(startdate)
  m <- as.POSIXlt(maturity)
  # m$mon <- m$mon - historymonth 
  m$mon <- m$mon - historymonth - 1
  startdate<-as.Date(m)
  
  Calendar<-TradeCalendar(holiday_table_address,specialday_table_address,startdate,enddate)
  inputdata<-inputdata[which(inputdata$Date%in%Calendar$Date),]
  
  ## 1.4 find start-date of each product when they have data,and replace NA with the price of ealier days, since they already have data
  price_begin<-as.Date(sapply(pvectorname,function(x) as.character(min(na.omit(inputdata[,c(x,"Date")])$Date))))
  
  #NA->previous price, means daily return = 0
  # for(i in 1:length(pvectorname)){
  #   #i=1
  #   begin<-price_begin[i]
  #   period<-inputdata[,c(pvectorname[i],"Date")]$Date
  #   for(j in 1:length(period)){
  #     #j=1
  #     price<-inputdata[,pvectorname[i]][inputdata$Date==period[j]]
  #     if(period[j]>begin & is.na(price)==TRUE){
  #       previous_price<-inputdata[,pvectorname[i]][inputdata$Date==period[j-1]]
  #       inputdata[,pvectorname[i]][inputdata$Date==period[j]]<-previous_price
  #     }
  #   }
  # }
  
  #### Interpolation Missing Number
  inputdata<-cbind(inputdata%>%dplyr::select(Date),na.approx(inputdata%>%dplyr::select(-Date),na.rm = FALSE))

  return(inputdata)
}