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

# partycol=c("#FAA61A","#151721","#072F5F","#f95f53","#EF3741","#007AC0","#67B437","#F26522","#FDDDD2","#C7E0CD","#F7A4A7","#FFEED2")
# parties=c("Lib Dem Yellow","Charcoal","Navy","Coral","Pseudo Labour","Pseudo Tory","Pseudo Green","ALDC","Peach","Green","Pink","Yellow")
# 
# for(i in 1:12)
# {
#   print(linemaker(partycol[i], parties[i]))
# }
