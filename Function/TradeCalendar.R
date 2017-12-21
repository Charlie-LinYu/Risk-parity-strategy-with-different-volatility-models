
TradeCalendar<-function(holiday_table_address,specialday_table_address,stdate,endate)
{
  
  holiday<-read.csv(holiday_table_address,header=TRUE)
  holiday$Date<-as.Date(holiday$Date)
  specialday<-read.csv(specialday_table_address,header=TRUE)
  specialday$exclude<-as.Date(specialday$exclude)
  specialday$include<-as.Date(specialday$include)
  adddata<-data.frame(cbind(Date=as.character(na.omit(specialday$include)),domiCon='a',nextCon='a',domiWeit=1,nextWeit=0,domiSet=0,nextSet=0))
  adddata$Date<-as.Date(adddata$Date)
  calendar<-data.frame(Date=as.Date(as.Date(stdate):as.Date(endate)))
  tradeCal<-data.frame(cbind(Date=calendar,domiCon='a',nextCon='a',domiWeit=1,nextWeit=0,domiSet=0,nextSet=0))
  #exclude weekend
  tradeCal[isWeekend(tradeCal$Date),]$Date<-NA
  
  #exclude holiday
  if (length(tradeCal[tradeCal$Date %in% holiday$Date,]$Date)>0)
  {tradeCal[tradeCal$Date %in% holiday$Date,]$Date<-NA}
  
  tradeCal<-na.omit(tradeCal)
  row.names(tradeCal)<-NULL
  
  if (sum(na.omit(specialday$exclude) %in% tradeCal$Date)!=0)
  {
    tradeCal<-tradeCal[-(which(tradeCal$Date %in% na.omit(specialday$exclude))),]
  }
  
  if (sum(adddata$Date>tradeCal[1,]$Date & adddata$Date<tradeCal[nrow(tradeCal),]$Date)!=0)
  {
    adddata<-adddata[adddata$Date>=tradeCal[1,]$Date]
    tradeCal<-rbind(tradeCal,adddata)
  }
  
  tradeCal<-tradeCal[order(tradeCal$Date),]
  
  return(tradeCal) 
}