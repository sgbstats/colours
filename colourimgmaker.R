library(tidyverse)
library(cowplot)

#makes the little squares for the rm
col=function(hex)
{
  data=data.frame("x"=c(0,0,1,1), "y"=c(0,1,1,0))
  
  out=data %>% ggplot2::ggplot(aes(x=x, y=y))+
    geom_polygon(fill=hex)+
    theme_nothing()+
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = NULL, y = NULL) 
  ggsave(paste("img/", substr(hex, 2,7), ".png", sep=""), plot=out, width = 15, height = 15, unit="px" )
  
}

# does the whole line
linemaker=function(hex, name)
{
  col(hex)
  
  rgb=col2rgb(hex)
  
  line=paste("|![",hex,"](img/", substr(hex,2,7),".png)","|",name,"|`",hex,"`|`rgb(",rgb[1],",",rgb[2],",", rgb[3], ")`|", sep="" )
  return(line)
}

# linemaker("#FAA61A", "Lib Dem Yellow")
# 
# cols=c("#E41A1C", "#FF7F00","#F6CB2F","#4DAF4A","#005EB8","#984EA3","#660099","#F77FBE","#FAA61A","#E4003B","#6AB023","#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#A6761D","#666666")
# 
# for(i in cols)
# {
#   col(i)
# }
# 
# paste("img/", substr(cols, 2,7), ".png", sep="")
