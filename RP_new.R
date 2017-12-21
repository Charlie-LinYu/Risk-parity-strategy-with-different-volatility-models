library(zoo)
library(xts)
library(quantmod)
library(timeDate)
library(PerformanceAnalytics)
library(dplyr)
library(fGarch)
library(timeSeries)
library(tseries)
library(stochvol)
library(Rsolnp)
library(FRAPO) 
library(fPortfolio) 
library(lattice) 
library(fMultivar)
library(cluster)
library(ggplot2)
library(base)
library(fpc)
library(reshape2)
library(ggthemes)
library(quadprog)

workdir <- paste("E:\\academy\\fudan\\graduate thesis\\document\\exchange\\program\\RiskParityIndex\\RiskParityIndex",sep='')
setwd(paste(workdir,sep='/'))
#relative function read
Path <-'./Function' 
MyFun <- lapply(file.path(Path, List <- list.files(path=Path, pattern = "\\.[Rr]$")), source) 
holiday_table_address<-paste('E:\\academy\\fudan\\graduate thesis\\document\\exchange\\program\\RiskParityIndex\\RiskParityIndex\\holiday_table.csv',sep='')
specialday_table_address<-paste('E:\\academy\\fudan\\graduate thesis\\document\\exchange\\program\\RiskParityIndex\\RiskParityIndex\\specialday.csv',sep='')
##############################################################################################################
#define riskparity address
input_address<-paste("E:\\academy\\fudan\\graduate thesis\\document\\exchange\\program\\RiskParityIndex\\RiskParityIndex\\data",sep='')
#find all root .csv 
input_file_list<-list.files(path=input_address,recursive=T,pattern= "*.[csv]$",full.names=T)
#creat asset addresss, remove fundingcost from list
asset_address_list<-input_file_list[which(!input_file_list%in%input_file_list[grep(pattern='fundingcost',input_file_list)])]
#asset_class_list<-unlist(lapply(asset_class,function(x) asset_address_list[which(asset_address_list%in%asset_address_list[grep(pattern=x,asset_address_list)])]))
asset_class_list<-asset_address_list
asset_class_address<-asset_address_list
asset_name<-unlist(strsplit(unlist(strsplit(asset_class_list,split='/'))[grep(unlist(strsplit(asset_class_list,split='/')),pattern= ".[csv]$")],split='.csv'))
#asset_name<-c("ZZ100","ZZ200","ZZ500","ZZ1000")
#out put address
output_address<-paste("E:\\academy\\fudan\\graduate thesis\\document\\exchange\\program\\RiskParityIndex\\RiskParityIndex\\output",sep<-'')
#   dir.create(paste(output_address,sep=''))

set.seed(100)
pvector<-paste(asset_name,sep='')
startdate<-as.Date('2006-01-01')
enddate<-as.Date("2016-12-30")
historymonth<-6
rebalancemonth<-3
prob<-0.95  
weightmethod<-"erc"             # ew,mv,mdp,erc
ercmethod<-"solnp"             # R,solnp,gosolnp; R is not recommanded. gosolnp is more general but slowly. So we first try solnp,                                                       ## if solnp doesn't work well, we then try gosolnp. 
method<-c("RealizedVolatility","ARCH","GARCH","SV","VaR","CVaR") #"SV","LogRealizedVolatility",
#method<-c("LogRealizedVolatility","RealizedVolatility")

rp_final<-c()
rp_final_return<-c()
Ratedata<-Read_Data(asset_class_address,pvector,startdate,enddate)
product_include<-pvector
Weighttable<-Weight_Table(Ratedata,pvector,startdate,enddate,historymonth,rebalancemonth)

for (cov_criteria in method){
  Weightmatrix<-Weighttable
  volatilitytable<-Weightmatrix
  for(i in 1:length(Weightmatrix[,1])){
    print(i)
    # i=47
    #product_include<-pvector[Weighttable[i,pvector]==1] # find the product that used to weight in that period 
    perioddata<-Period_Data(Ratedata,product_include,historymonth,Weighttable[i,'startdate']) # find the product and corresponding data
    perioddata<-na.omit(perioddata)
    covariance<-Cov_Relation(perioddata,cov_criteria) # calculate covariance 
    covariance<-as.matrix(covariance)
    volatilitytable[i,product_include]<-diag(covariance)
    #product_include<-colnames(covariance)
    periodweight<-Basic_RiskParity(covariance,weightmethod,ercmethod)
    Weightmatrix[i,product_include]<-periodweight
  }
  weight_begin<-max(as.Date(sapply(pvector,function(x) as.character(min(na.omit(Weightmatrix[,c(x,"startdate")])$startdate)))))
  WeightMatrix<-Weightmatrix
  for(i in 1:length(pvector)){
    period<-WeightMatrix[,c(pvector[1],"startdate")]$startdate
    for(j in 1:length(period)){
      weight<-WeightMatrix[,pvector[i]][WeightMatrix$startdate==period[j]]
      if(period[j]>weight_begin & is.na(weight)==TRUE){
        previous_weight<-WeightMatrix[,pvector[i]][WeightMatrix$startdate==period[j-1]]
        WeightMatrix[,pvector[i]][WeightMatrix$startdate==period[j]]<-previous_weight
      }
    }
  }
  Ratedata<-Read_Data(asset_class_address,pvector,startdate,enddate)
  weightmatrix<-WeightMatrix
  #read price data
  inputdata<-Read_InputData(asset_class_address,pvector,startdate,enddate)
  #find non-zero startdate
  for(i in 1:length(weightmatrix[,1])){
    if(weightmatrix%>%dplyr::select(-startdate,-enddate)%>%.[i,1:length(pvector)]%>%sum(.)!=0){
      startdate<-weightmatrix[i,]$startdate
      break
    }
  }
  #find end date
  enddate<-weightmatrix[length(weightmatrix[,1]),]$enddate
  #
  inputdata<-inputdata%>%dplyr::filter(Date>=startdate & Date<=enddate)
  inputdata<-dplyr::filter(Ratedata,Date>=startdate & Date<=enddate)
  InitialIndexLevel<-1000
  rp_index<-c()
  weightmatrix$startdate<-as.Date(weightmatrix$startdate)
  weightmatrix$enddate<-as.Date(weightmatrix$enddate)
  weight_pic<-Weight_plot(weightmatrix,startdate,enddate,cov_criteria)
  show(weight_pic)
  for (i in 1:nrow(weightmatrix))
  {
    #  i<-3
    startday<-weightmatrix$startdate[i]
    endday<-weightmatrix$enddate[i]
    weightvector<-weightmatrix[i,pvector]
    begyear<-as.numeric(format(as.Date(startday),'%Y'))
    endyear<-as.numeric(format(as.Date(endday),'%Y'))
    year<-seq(begyear,endyear,by<-1)
    Variable <- c("domiSet","domiWeit","nextSet","indexset")
    logic<-(inputdata$Date>=startday & inputdata$Date<=endday)
    data_tmp<-inputdata[logic,]
    for(x in pvector){
      if(weightvector[x]==0){
        data_tmp[,x]<-rep(0,length(data_tmp[,"Date"]))
      }
    }
    rp_index<-rbind(rp_index,as.matrix(data_tmp[,pvector])%*%t(weightvector))
  }
  #####use return to calculate RP index, if price is used, this part should be omitted
  rp_index<-data.frame(rp_index,NA)
  rp_index[1,2]<-InitialIndexLevel
  for (i in 2:length(rp_index[,1])){
    rp_index[i,2]<-(1+rp_index[i,1])*rp_index[i-1,2]
  }
  rp_return<-rp_index[,1]
  rp_index<-rp_index[,2]
  ############################################################
  
  rp_index<-cbind(inputdata%>%dplyr::select(Date),rp_index)
  colnames(rp_index)<-c('Date',cov_criteria)
  rp_index<-na.omit(rp_index)
  rp_index<-rp_index%>%dplyr::filter(paste(cov_criteria,sep="")!=0)
  rp_return<-cbind(inputdata%>%dplyr::select(Date),rp_return)
  colnames(rp_return)<-c('Date',cov_criteria)
  rp_return<-na.omit(rp_return)
  rp_return<-rp_return%>%dplyr::filter(paste(cov_criteria,sep="")!=0)
  if (length(rp_final)==0){
    rp_final<-rp_index
    rp_final_return<-rp_return
  }
  else{
    rp_final<-data.frame(rp_final,rp_index[,2])
    rp_final_return<-data.frame(rp_final_return,rp_return[,2])
  }
  write.csv(volatilitytable,file<-paste(output_address,"\\",cov_criteria,"_Vol.csv",sep=""))
}
colnames(rp_final)<-c("Date",method)
rp_final$Date<-as.Date(rp_final$Date)
colnames(rp_final_return)<-c("Date",method)
rp_final_return$Date<-as.Date(rp_final_return$Date)
write.csv(rp_final,file<-paste(output_address,"\\","RP_Index.csv",sep=""))
Index_Plot<-Price_plot(indextable=rp_final, indexName=c("Date",method), pictureTitle="Index")
Index_Plot


###analysis
TypeofReturn<-"arithmetic"  #!!!!
safereturn<-0  

data_price<-rp_final
data_return<-rp_final_return
basisinfo<-matrix(NA,nrow = 10,ncol = length(method))
rownames(basisinfo)<-c("Yearly Return","Yearly Volatility","Sharp Ratio","Daily VaR","Daily CVaR","Max Drawdown","Daily Skewness","Monthly Skewness","Excess Daily Kurtosis","Excess Monthly Kurtosis")
colnames(basisinfo)<-method
basisinfo[1,]<-sapply(do.call(cbind,lapply(names(data_price)[-1], function(x) periodReturn(xts(data_price[x],order.by = data_price$Date),period="yearly",type = TypeofReturn))),mean)
basisinfo[2,]<-sqrt(diag(cov(data_return[method]))*240)
basisinfo[3,]<-(basisinfo[1,]-safereturn)/basisinfo[2,]
basisinfo[4,]<- -sapply(data_return[-1],function(x) PerformanceAnalytics::VaR(x,p=prob,method="historical"))
basisinfo[5,]<- -sapply(data_return[-1],function(x) PerformanceAnalytics::ETL(x,p=prob,method="historical"))
basisinfo[6,]<-sapply(drawdowns(as.timeSeries(data_return[-1])),min)
basisinfo[7,]<-sapply(data_return[-1], PerformanceAnalytics::skewness)
basisinfo[8,]<-sapply(do.call(cbind,lapply(names(data_price)[-1], function(x) periodReturn(xts(data_price[x],order.by = data_price$Date),period="monthly",type = TypeofReturn))),skewness)
basisinfo[9,]<-sapply(data_return[-1], function(x) PerformanceAnalytics::kurtosis(x,method="excess"))
basisinfo[10,]<-sapply(do.call(cbind,lapply(names(data_price)[-1], function(x) periodReturn(xts(data_price[x],order.by = data_price$Date),period="monthly",type = TypeofReturn))),kurtosis)
print(basisinfo)
write.csv(basisinfo,file=paste(output_address,"\\","basisinfo.csv",sep=""))


