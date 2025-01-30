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
  
  line=paste("|![",hex,"](img/", substr(hex,2,7),".png)","| ",name,"|`",hex,"`|`rgb(",rgb[1],",",rgb[2],",", rgb[3], ")`|", sep="" )
  return(line)
}

# partycol=c("#FAA61A","#151721","#072F5F","#f95f53","#EF3741","#007AC0","#67B437","#F26522","#FDDDD2","#C7E0CD","#F7A4A7","#FFEED2")
# parties=c("Lib Dem Yellow","Charcoal","Navy","Coral","Pseudo Labour","Pseudo Tory","Pseudo Green","ALDC","Peach","Green","Pink","Yellow")
# 
# for(i in 1:12)
# {
#   print(linemaker(partycol[i], parties[i]))
# }

colour=tribble(~"col", ~"use",
               "#141414", "Background",
               "#595959", "Unsure",
               "#00698F", "Keyword",
               "#1EDAFB", "Constant",
               "#58C554", "Numeric",
               "#58C554", "String",
               "#997744", "Variable",
               "#4D4333", "Comment highlight",
               "#403F3E", "Console error",
               "#e6e1dc", "Text",
               "#00AEEF", "function",
               "#ff308f", "Comment- maybe",
               "#d44a88", "Better comment")

for(i in 1:nrow(colour))
{
  cat(paste0(linemaker(colour$col[i], colour$use[i])),"\n")
}


text_styles <- tribble(
  ~"use", ~"col",
    "Normal", "#e6e1dc",
    "Other", "#FF308F",
    "Attribute", "#e6e1dc",
    "SpecialString", "#008000",
    "Annotation", "#FF308F",
    "Function", "#FAA61A",
    "String", "#58C554",
    "ControlFlow", "#00AEEF",
    "Operator", "#FFFFFF",
    "Error", "#403F3E",
    "BaseN", "#008000",
    "Alert", "#7928a1",
    "Variable", "#FFFFFF",
    "BuiltIn", "null",
    "Extension", "null",
    "Preprocessor", "#7928a1",
    "Information", "#ff308f",
    "VerbatimString", "#008000",
    "Warning", "#696969",
    "Documentation", "#FF308F",
    "Import", "null",
    "Char", "#008000",
    "DataType", "#008000",
    "Float", "#FAA61A",
    "Comment", "#d44a88",
    "CommentVar", "#696969",
    "Constant", "#FF308F",
    "SpecialChar", "#00769e",
    "DecVal", "#58C554",
    "Keyword", "#FF308F") %>% 
  arrange(col)

for(i in 1:nrow(text_styles))
{
  cat(paste0(linemaker(text_styles$col[i], text_styles$use[i])),"\n")
}

css_styles <- tribble(
  ~CSS, ~Property, ~Value,
  "gutter", "background", "#141414",
  "gutter", "color", "#595959",
  "gutter-cellwarning", "background", "#FC0",
  "print-margin", "background", "#1D1D1D",
  "marker-layer selection", "background", "#494836",
  "marker-layer active-line", "background", "#333",
  "gutter-active-line", "background-color", "#222",
  "invisible", "color", "#404040",
  "keyword", "color", "#00698F",
  "keywordoperator", "color", "#FF308F",
  "constant", "color", "#1EDAFB",
  "constantlanguage", "color", "#FDC251",
  "constantlibrary", "color", "#8DFF0A",
  "constantnumeric", "color", "#58C554",
  "invalid", "color", "#FFFFFF",
  "invalid", "background-color", "#990000",
  "invaliddeprecated", "color", "#FFFFFF",
  "invaliddeprecated", "background-color", "#990000",
  "support", "color", "#999",
  "supportfunction", "color", "#00AEEF",
  "function", "color", "#00AEEF",
  "string", "color", "#58C554",
  "comment", "color", "#555",
  "variable", "color", "#997744",
  "metatag", "color", "#BE53E6",
  "entityotherattribute-name", "color", "#FFFF89",
  "fold", "background", "#222",
  "fold", "color", "#7AF",
  "marker-layer foreign_line", "background-color", "#2B2B2A",
  "node-selector", "background-color", "#fb6f1c",
  "comment-highlight", "color", "#4D4333",
  "comment-highlight", "background-color", "#D1B78A",
  "marker-layer active_debug_line", "background-color", "#8B7A27",
  "marker-layer find_line", "background-color", "#403F3E",
  "console_error", "background-color", "#403F3E"
  ) %>%
  arrange(Value)


for(i in 1:nrow(css_styles)){
  cat(paste0(linemaker(css_styles$CSS[i], css_styles$Property[i])),"\n")
}
