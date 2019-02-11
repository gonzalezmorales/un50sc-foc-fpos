# Load necessary libraries
library(tidyr)
library(data.table)
library(plotly)
library(RColorBrewer)
library(stringr)
library(optiRum)
library(dplyr)


# set your working directory - change as needed
setwd("C:/Users/L.GonzalezMorales/Documents/GitHub/un50sc-foc-fpos/Analysis/")

# load functions
source('Code/f_read.tab2dataTable.r')
source('Code/f_writeTable2tab.r')
source('Code/f_h_barplot_stacked.r')
source('Code/f_h_barplot.r')
source('Code/f_v_barplot.r')
source('Code/f_doughnut.r')
source('Code/f_doughnut2.r')


#----------------------------------
# Read plots data
#----------------------------------

data <- read.tab2dataTable("Data/PlotsData.txt")
plotTypes   <- read.tab2dataTable("Data/PlotsTypes.txt")



for(i in 1:nrow(plotTypes)){
  
  type <- plotTypes[i,Plot.Type]
  qID <- plotTypes[i,QuestionID]
  qText <- data[QuestionID == qID,QuestionText][1]
  
  X <- data[QuestionID == qID,list(Group, Answer, x = TOTAL)]
  
  if(X[!is.na(Group),.N] == 0){
    
    x <- X[,x]
    x.labels <- X[,Answer]
    x.color  <- brewer.pal(length(x),"Blues")
    x.legend <- x.labels
    
  } else {
    
    
    x <- as.matrix(dcast(X, Answer ~ Group, value.var = "x")[,-1,with=FALSE])
    x[is.na(x)] <- 0
    
    x <- x[nrow(x):1,]
    
    x.labels  <- colnames(x) 
    x.legend <- dcast(X, Answer ~ Group, value.var = "x")[nrow(x):1,Answer]
    
    if(length(x)>2) {
      
      x.color  <- brewer.pal(nrow(x),"Blues")[nrow(x):1]
      
    } else if(length(x) == 1) {
      
      x.color  <- c("#2171B5","#BDD7E7")
      
    } else {
      
      x.color <- "#2171B5"
    }
    
    
    
  }
  
  # "Vertical bars - side by side"
  

  if(type == "Horizontal bars - stacked"){
 
      svg(filename=paste("Plot_", i, "_", qID, ".svg", sep = ""), width=8, height=4)
    
      h_barplot_stacked(x,x.labels,x.color, x.legend)
    
      dev.off()
  
    } else if (type == "Doughnut") {
    
      svg(filename=paste("Plot_", i, "_", qID, ".svg", sep = ""), width=8, height=4)
      
      doughnut2(x, x.labels, x.color, x.legend)
       
      dev.off()
  
    } else if (type == "Horizontal bars"){ 
      
      if (length(x)>3) {
        h <- 4 + round(length(x)/2) + .25
      } else {
        h <- 4
      }
    
      svg(filename=paste("Plot_", i, "_", qID, ".svg", sep = ""), width=8, height=h)
    
      h_barplot(x, x.labels, "#2171B5")
      
      dev.off()
    
    } else if (type == "Vertical bars") {
    
      svg(filename=paste("Plot_", i, "_", qID, ".svg", sep = ""), width=8, height=4)
    
      v_barplot(x, x.labels, x.color = "#2171B5")
      
      dev.off()
    
    }
  
    
  
  
  
  
}
