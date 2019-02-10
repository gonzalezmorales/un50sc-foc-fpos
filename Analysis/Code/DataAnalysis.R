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
source('Code/f_doughnut.r')
source('Code/f_read.tab2dataTable.r')
source('Code/f_writeTable2tab.r')

#----------------------------------
# Read raw data
#----------------------------------

responses <- read.tab2dataTable("Data/CleanResponses.txt")
columns   <- read.tab2dataTable("Data/Columns.txt")


#=====================================
# Organize questions into individual files
#=====================================

countryInfo <- names(responses)[1:14]

#-----------------------------
# T0.0 -- Respondents Details
#-----------------------------

# Select columns from main "responses" file
respondents <- responses[,c(countryInfo, "C01", paste("C0", 3:8, sep = "")), with = FALSE]

# Rename using tags from "columns" file
setnames(respondents, 
         c("C01", paste("C0", 3:8, sep = "")), 
         columns[Column%in%c(countryInfo, "C01", paste("C0", 3:8, sep = "")),QuestionText])

# Write as tab-delimited file:

writeTable2tab( respondents , "Output/Respondents.txt" )

#---------------------------------
# Questions
#---------------------------------

questions <- unique(columns[substr(QuestionID, 1, 1) != "X",list(QuestionID, QuestionText)])

response.keys <- dcast(columns[substr(QuestionID, 1, 1) != "X",], QuestionID + QuestionText ~ ResponseID, value.var = "MultipleChoiceText")

writeTable2tab( response.keys , "Output/Response_keys.txt" )

#------------------------------
  
for(i in 1:nrow(questions))
#for(i in 1:1)
{
  
  question.details <- columns[QuestionID == questions[i,QuestionID],]
  
  question.responses <- responses[,c(countryInfo,question.details[,Column]),with = FALSE]
  
  response.labels <- columns[QuestionID == questions[i,QuestionID],
                             ResponseID]
  
  setnames(question.responses, 
           question.details[,Column], 
           response.labels)
  
  question.responses <- cbind(question.details[1,list(QuestionID, QuestionText)],
                              question.responses)
  
  setcolorder(question.responses,
              c("QuestionID", "QuestionText",
                countryInfo,
                response.labels))
  
  writeTable2tab( question.responses,
                  paste("Output/Responses_", questions[i,QuestionID], ".txt", sep="") )
  
  
}



rm(list = c("questions",
            "question.details",
            "question.responses", 
            "response.labels",
            "countryInfo",
            "responses",
            "columns"))

#====================================
# ANALYSIS
#====================================

#-----------------------------
# T00 - Respondents by region
#-----------------------------

TX.RespondentsByRegion <- respondents[, .(.N), , by = .(RegionName)]
TX.RespondentsByRegion[, Percentage := round(N/nrow(respondents)*100,2)]
TX.RespondentsByRegion[, Table := "Respondents by region"]

TX.RespondentsByRegion<- TX.RespondentsByRegion[order(-rank(N)),
                                                list(Table, RegionName, N, Percentage)]

writeTable2tab( TX.RespondentsByRegion,"Output/TableTX.RespondentsByRegion.txt")

#- - - - - - - -
x        <- TX.RespondentsByRegion[,Percentage]
x.labels <- paste(TX.RespondentsByRegion[,RegionName],"\n",TX.RespondentsByRegion[,Percentage],"%",sep = "")
x.color  <- brewer.pal(nrow(TX.RespondentsByRegion),"Blues")
#- - - - - - - -

svg(filename="PlotTX.RespondentsByRegion.svg", width=6, height=4)

    par(mar=c(0,0,0,0))
    
    doughnut(x,
            labels = x.labels, 
            edges = 1000, 
            outer.radius = 0.70, 
            inner.radius=0.35, 
            col = x.color, 
            border = FALSE, 
            lty = NULL, 
            main = NULL,
            cex = 1)
    
dev.off()

#==================================
# SUMMARY TABLES - Major aggregates
#==================================

for(q in 1:nrow(response.keys)){
  
  QuestionID <- response.keys[q, QuestionID]
  QuestionText <- response.keys[q,QuestionText]
  
  responses <- read.tab2dataTable(paste("Output/Responses_",QuestionID,".txt",sep=""))
  
  response.columns <- names(responses[,-(1:16),with = FALSE])
  
  if(length(response.columns) == 1){
      
      response.labels <- QuestionText
      
      summary <- responses[, .(.N), by = R01]
      summary[, Percentage := round(N/nrow(responses)*100,2)]
  
      summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), "", summary)
      setnames(summary, "V3", "BlockID")
      setnames(summary, "V4", "BlockText")
      
      setnames(summary, eval(response.columns[1]), "Answer")
      
    } else {
      
      response.labels <- unlist(response.keys[q, response.columns, with = FALSE])
      
      summary <- responses[, .(.N), by = eval(response.columns[1])]
      summary[, Percentage := round(N/nrow(responses)*100,2)]
      
      summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), response.labels[1], summary)
  
      setnames(summary, "V3", "BlockID")
      setnames(summary, "V4", "BlockText")
      setnames(summary, eval(response.columns[1]), "Answer")
      
      for (i in 2:length(response.columns))
      {
        summary.b <- responses[, .(.N), by = eval(response.columns[i])]
        summary.b[, Percentage := round(N/nrow(responses)*100,2)]
       
        summary.b <- cbind(QuestionID, QuestionText, eval(response.columns[i]), response.labels[i], summary.b)
        
        setnames(summary.b, "V3", "BlockID")
        setnames(summary.b, "V4", "BlockText")
        setnames(summary.b, eval(response.columns[i]), "Answer")
  
        summary <- rbind(summary, summary.b)
        
      }
      
      
      rm(list = c("summary.b"))
      
    }
  
  table.name <- paste("Table_",QuestionID, sep="")
  
  assign(table.name, summary)
  
  writeTable2tab(get(table.name),paste("Output/",table.name,".txt", sep = ""))
  
  
}


#==================================
# SUMMARY TABLES - By region
#==================================

for(q in 1:nrow(response.keys)){
  
  QuestionID <- response.keys[q, QuestionID]
  QuestionText <- response.keys[q,QuestionText]
  
  responses <- read.tab2dataTable(paste("Output/Responses_",QuestionID,".txt",sep=""))

  response.columns <- names(responses[,-(1:16),with = FALSE])
  
  if(length(response.columns) == 1){
    
    response.labels <- QuestionText
    
    summary <- responses[, .(.N), by = list(R01, RegionName)]
    summary <- summary %>% group_by(RegionName) %>% mutate(Percentage = (N/sum(N) * 100))
    
    summaryT <- responses[, .(.N), by = list(R01)]
    summaryT <- summaryT %>% mutate(RegionName = "TOTAL") %>% mutate(Percentage = (N/sum(N) * 100))
    
    summary <- as.data.table(summary)
    summaryT <- as.data.table(summaryT)
   
    summary <- rbind(summary,summaryT)
    
    summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), "", summary)
    setnames(summary, "V3", "BlockID")
    setnames(summary, "V4", "BlockText")
    
    setnames(summary, eval(response.columns[1]), "Answer")
    
    
    
  } else {
    
    response.labels <- unlist(response.keys[q, response.columns, with = FALSE])
    
    
    summary <- responses[, .(.N), by = list(R01, RegionName)]
    summary <- summary %>% group_by(RegionName) %>% mutate(Percentage = (N/sum(N) * 100))
    
    summaryT <- responses[, .(.N), by = list(R01)]
    summaryT <- summaryT %>% mutate(RegionName = "TOTAL") %>% mutate(Percentage = (N/sum(N) * 100))
    
    
    summary <- as.data.table(summary)
    summaryT <- as.data.table(summaryT)
    
    summary <- rbind(summary,summaryT)
    
    summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), response.labels[1], summary)
    setnames(summary, "V3", "BlockID")
    setnames(summary, "V4", "BlockText")
    
    setnames(summary, eval(response.columns[1]), "Answer")
    
    
    for (i in 2:length(response.columns))
    {

    
      summary.b <- responses[, .(.N), by = eval(c(response.columns[i], "RegionName"))]
      summary.b <- summary.b %>% group_by(RegionName) %>% mutate(Percentage = (N/sum(N) * 100))
      
      summaryT.b <- responses[, .(.N), by = eval(c(response.columns[i], "RegionName"))]
      summaryT.b <- summaryT.b %>% mutate(RegionName = "TOTAL") %>% mutate(Percentage = (N/sum(N) * 100))
      
      
      summary.b <- as.data.table(summary.b)
      summaryT.b <- as.data.table(summaryT.b)
      
      summary.b <- rbind(summary.b,summaryT.b)
      
      summary.b <- cbind(QuestionID, QuestionText, eval(response.columns[i]), response.labels[i], summary.b)
      setnames(summary.b, "V3", "BlockID")
      setnames(summary.b, "V4", "BlockText")
      setnames(summary.b, eval(response.columns[i]), "Answer")
      
      
      summary <- rbind(summary, summary.b)
    }
    
    
    rm(list = c("summary.b"))
    
  }
  
  
  x <- dcast(summary, 
             QuestionID + QuestionText + BlockID + BlockText + Answer ~ RegionName, 
             value.var = "Percentage")
  
  table.name <- paste("TableByRegion_",QuestionID, sep="")
  
  assign(table.name, summary)
  
  writeTable2tab(get(table.name),paste("Output/",table.name,".txt", sep = ""))
  
  
}

#==================================
# SUMMARY TABLES - Pivot tables
#==================================

for(q in 1:nrow(response.keys)){
  
  
  QuestionID <- response.keys[q, QuestionID]
  
  table.name <- paste("Table_",QuestionID, sep="")
  table.name.ByRegion <- paste("TableByRegion_",QuestionID, sep="")
  
  x <- dcast(get(table.name), QuestionID + QuestionText + BlockID + BlockText ~ Answer, value.var = "Percentage")
  x.Region <- dcast(get(table.name.ByRegion), QuestionID + QuestionText + BlockID + BlockText + RegionName~ Answer, value.var = "Percentage")

  writeTable2tab(x , paste("Output/", table.name, "_pivot.txt" , sep = ""))
  writeTable2tab(x.Region , paste("Output/", table.name.ByRegion, "_pivot.txt", sep = ""))

}

