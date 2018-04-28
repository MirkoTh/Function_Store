library(ggplot2)
plotTheme <- function (plot){
  plot +
    theme(panel.background = element_rect(fill = "black")) +
    theme(panel.grid=element_blank(),panel.background=element_blank()) + theme_bw() +
    theme(plot.title = element_text(size = rel(1.75), hjust = 0)) +
    theme(legend.text = element_text(size = 11), legend.title = element_text(size = 11), 
          legend.key = element_blank(), legend.box = 'horizontal') +
    theme(legend.background = element_rect(colour = "grey50")) +
    theme(axis.title.x = element_text(size=16), axis.text.x  = element_text(size=14)) +
    theme(axis.title.y = element_text(size=16), axis.text.y  = element_text(size=14)) +
    theme(legend.position = "bottom")+ theme(strip.text.x = element_text(size = 14))+
    labs(title = "\n") + 
    theme(plot.background = element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}