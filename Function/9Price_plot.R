Price_plot <- function(indextable, indexName, pictureTitle) {
  names(indextable) <- c(indexName)
  indextable<-tidyr::gather(indextable, key = 'colnames', value = 'index', -Date)
  indextable$colnames<-ordered(indextable$colnames,indexName[-1])
  indextable<-na.omit(indextable)
  return(
    ggplot(indextable) +
      geom_line(aes(x = Date, y = index, color = colnames), size = 0.5) +
      ggtitle(pictureTitle) + 
      xlab(NULL) + ylab(NULL) +
      scale_color_discrete(name = NULL) +
      # scale_colour_hue(l=30, c=230)+
      scale_x_date(date_breaks = '6 month', date_labels = '%Y-%m') +
      theme_bw()+
      theme(  plot.margin=unit(rep(0.5,4), 'lines'),
              panel.background=element_rect(fill='transparent', color='black'),
              panel.border=element_rect(fill='transparent', color='transparent'),
              panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(colour = "grey"))+
      theme(legend.position = c(0.13, 0.85),legend.key.size= unit(0.25, "cm"))+
      theme(axis.text.x = element_text(size = 10,  color = "black", vjust = 1, hjust = 1, angle = 45))
  )
}