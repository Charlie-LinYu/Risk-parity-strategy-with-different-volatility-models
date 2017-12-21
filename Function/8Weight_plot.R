#this function is used to plot stack histogram of weight table against each rebalance day
Weight_plot<-function(weighttable,date1,date2,criteria){
  weighttable<-dplyr::filter(weighttable,startdate >= date1 & enddate <= date2)
  weightforplot<-weighttable[,-2]
  weightforplot<-melt(weightforplot,id.vars = "startdate",variable.name = "Product",value.name = "Weight")
  num<-length(colnames(weighttable))-2
  plot_weight<-ggplot(weightforplot,aes(x=startdate,y=Weight,fill=Product)) + 
    geom_bar(stat="identity",position="stack",colour="white")+
    ###scale_y_continuous(limits = c(0,1))+
    ggtitle(paste("Weight distribution via ",criteria,sep=""))+
    theme_tufte()+
    scale_fill_manual(values = topo.colors(num))+
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
    theme(legend.key.size= unit(0.5, "cm"),legend.position="bottom",legend.title=element_blank())+  ###legend.position="bottom",
    guides(col=guide_legend(byrow = TRUE))
  return(plot_weight)
}