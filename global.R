Sys.setlocale(,"UK_ua")

library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)
library(png)
library(extrafont)

interp_points <- function (data) {
  
  df <- data.frame(line_id=c(),long=c(),lat=c())
  
  for (i in 1:nrow(data)) { 
    
    line <- data[i,]
    
    longseq <- seq(
      as.numeric(line["tb2"]),
      as.numeric(line["net2"]),
      as.numeric((line["net2"] - line["tb2"])/10)
    )
    latseq <- seq(
      as.numeric(line["tb"]),
      as.numeric(line["net"]),
      as.numeric(line["net"] - line["tb"])/10
    )
    
    for (j in 1:10) {
      df <- rbind(df,data.frame(line_id=i,long=longseq[j],lat=latseq[j],seg_num=j))
    }
  }
  df$lat <- zoo::na.locf(df$lat)
  for(i in unique(df$line_id)){
    df <- rbind(df,c(i,1,data$net[i],11))
  }
  df
}