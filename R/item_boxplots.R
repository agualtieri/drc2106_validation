item_boxplots <- function(data, group_var, items){
  data.m <- as.data.frame(data) %>% select(group_var, items) %>% melt()
  
  p <- ggplot(data.m, aes(x= group_var, y = value)) + geom_boxplot() + facet_wrap(~variable, ncol = 4)
  print(p)
}



item_boxplots_formatted <- function(data, group_var, items){
  
  # melt data
  data.m <- as.data.frame(data) %>% select(group_var, items) %>% melt()
  
  # helper function
  f <- function(x) {
    r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r}
  
  # calc mean, median, min, max
  medians <- plyr::ddply(data.m, .(group_var, variable), summarise, med = median(value,na.rm=TRUE))
  #medians2 <- plyr::ddply(medians, .(variable), summarise, med = median(med,na.rm=TRUE))
  mins <- plyr::ddply(data.m, .(variable), summarise, min = min(value,na.rm=TRUE))
  max <- plyr::ddply(data.m, .(variable), summarise, max = max(value,na.rm=TRUE))
  
  
  # produce plot
  p <- ggplot(data.m, aes(variable, value, width = 0.25)) +
    stat_summary(fun.data = f, geom = "boxplot", fill = "gainsboro") +
    theme_bw() +
    stat_summary(geom = "crossbar", width= 0.8, fatten=2, color="#EE5859",
                 fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))})+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color="grey85"),
          panel.grid.minor = element_blank()) +
    xlab("") +
    #ylab("Price (LYD)")+
    #ylim(0, 15) +
    # scale_y_continuous(breaks = seq(0, 15, 5)) +
    geom_text(data = mins, aes(x=variable, y = min, label = format(round(min,digits=0), nsmall=0)),size = 2.5,vjust = 1.5)+
    geom_text(data = medians, aes(x=variable, y = med, label = format(round(med,digits=), nsmall=0)),size =2.5, vjust =  0.6, hjust = -0.6) +
    geom_text(data = max, aes(x=variable, y = max, label = format(round(max,digits=0), nsmall=0)), size =2.5, vjust = -1.5) +
    theme(axis.text.x = element_text(angle = 0, size = 7, hjust = 0.5 ))+
    labs(y= " Price ($$$)")
  
  print(p)
  
}




