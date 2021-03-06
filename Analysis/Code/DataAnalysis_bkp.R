# Load necessary libraries
library(tidyr)
library(data.table)
library(plotly)
library(RColorBrewer)
library(stringr)
library(optiRum)


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

writeTable2tab( response.keys , "Output/Response-keys.txt" )

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
                  paste("Output/Responses-", questions[i,QuestionID], ".txt", sep="") )
  
  
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
TX.RespondentsByRegion[, Percentage := round(N/nrow(respondents)*100,0)]
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
  
  responses <- read.tab2dataTable(paste("Output/Responses-",QuestionID,".txt",sep=""))
  
  response.columns <- names(responses[,-(1:16),with = FALSE])
  
  if(length(response.columns) == 1){
      
      response.labels <- QuestionText
      
      summary <- responses[, .(.N), by = R01]
      summary[, Percentage := round(N/nrow(responses)*100,0)]
  
      summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), "", summary)
      setnames(summary, "V3", "BlockID")
      setnames(summary, "V4", "BlockText")
      
      setnames(summary, eval(response.columns[1]), "Answer")
      
    } else {
      
      response.labels <- unlist(response.keys[q, response.columns, with = FALSE])
      
      summary <- responses[, .(.N), by = eval(response.columns[1])]
      summary[, Percentage := round(N/nrow(responses)*100,0)]
      
      summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), response.labels[1], summary)
  
      setnames(summary, "V3", "BlockID")
      setnames(summary, "V4", "BlockText")
      setnames(summary, eval(response.columns[1]), "Answer")
      
      for (i in 2:length(response.columns))
      {
        summary.b <- responses[, .(.N), by = eval(response.columns[i])]
        summary.b[, Percentage := round(N/nrow(responses)*100,0)]
       
        summary.b <- cbind(QuestionID, QuestionText, eval(response.columns[i]), response.labels[i], summary.b)
        
        setnames(summary.b, "V3", "BlockID")
        setnames(summary.b, "V4", "BlockText")
        setnames(summary.b, eval(response.columns[i]), "Answer")
  
        summary <- rbind(summary, summary.b)
        
      }
      
      
      rm(list = c("summary.b"))
      
    }
  
  table.name <- paste("Table-",QuestionID, sep="")
  
  assign(table.name, summary)
  
  writeTable2tab(get(table.name),paste("Output/",table.name,".txt", sep = ""))
  
  
}


#==================================
# SUMMARY TABLES - By region
#==================================

for(q in 1:nrow(response.keys)){
  
  QuestionID <- response.keys[q, QuestionID]
  QuestionText <- response.keys[q,QuestionText]
  
  responses <- read.tab2dataTable(paste("Output/Responses-",QuestionID,".txt",sep=""))
  
  response.columns <- names(responses[,-(1:16),with = FALSE])
  
  if(length(response.columns) == 1){
    
    response.labels <- QuestionText
    
    summary <- responses[, .(.N), by = list(R01, RegionName)]
    summary[, Percentage := round(N/nrow(responses)*100,0)]
    
    summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), "", summary)
    setnames(summary, "V3", "BlockID")
    setnames(summary, "V4", "BlockText")
    
    setnames(summary, eval(response.columns[1]), "Answer")
    
  } else {
    
    response.labels <- unlist(response.keys[q, response.columns, with = FALSE])
    
    summary <- responses[, .(.N), by = eval(c(response.columns[1], "RegionName"))]
    summary[, Percentage := round(N/nrow(responses)*100,0)]
    
    summary <- cbind(QuestionID, QuestionText, eval(response.columns[1]), response.labels[1], summary)
    
    setnames(summary, "V3", "BlockID")
    setnames(summary, "V4", "BlockText")
    setnames(summary, eval(response.columns[1]), "Answer")
    
    for (i in 2:length(response.columns))
    {
      summary.b <- responses[, .(.N), by =  eval(c(response.columns[i], "RegionName"))]
      summary.b[, Percentage := round(N/nrow(responses)*100,0)]
      
      summary.b <- cbind(QuestionID, QuestionText, eval(response.columns[i]), response.labels[i], summary.b)
      
      setnames(summary.b, "V3", "BlockID")
      setnames(summary.b, "V4", "BlockText")
      setnames(summary.b, eval(response.columns[i]), "Answer")
      
      summary <- rbind(summary, summary.b)
      
    }
    
    
    rm(list = c("summary.b"))
    
  }
  
  table.name <- paste("TableByRegion-",QuestionID, sep="")
  
  assign(table.name, summary)
  
  writeTable2tab(get(table.name),paste("Output/",table.name,".txt", sep = ""))
  
  
}



#=============================

# T0.2 - Awareness of existence UNFPOS

T0.1.AwarenessUNFPOS.a <- responses[, .(.N), , by = .(C10)]
setnames(T0.1.AwarenessUNFPOS.a, "C10", "AwarenessUNFPOS")
T0.1.AwarenessUNFPOS.a[, Percentage := round(N/nrow(responses)*100,0)]
setnames(T0.1.AwarenessUNFPOS.a, "N", "N.ChiefStatistician_NSO")
setnames(T0.1.AwarenessUNFPOS.a, "Percentage", "Percentage.ChiefStatistician_NSO")


T0.1.AwarenessUNFPOS.b <- responses[, .(.N), , by = .(C11)]
setnames(T0.1.AwarenessUNFPOS.b, "C11", "AwarenessUNFPOS")
T0.1.AwarenessUNFPOS.b[, Percentage := round(N/nrow(responses)*100,0)]
setnames(T0.1.AwarenessUNFPOS.b, "N", "N.HeadsOrSeniorManagersNSS")
setnames(T0.1.AwarenessUNFPOS.b, "Percentage", "Percentage.HeadsOrSeniorManagersNSS")


T0.1.AwarenessUNFPOS.c <- responses[, .(.N), , by = .(C12)]
setnames(T0.1.AwarenessUNFPOS.c, "C12", "AwarenessUNFPOS")
T0.1.AwarenessUNFPOS.c[, Percentage := round(N/nrow(responses)*100,0)]
setnames(T0.1.AwarenessUNFPOS.c, "N", "N.MinistryOrDepartmentAboveNSO")
setnames(T0.1.AwarenessUNFPOS.c, "Percentage", "Percentage.MinistryOrDepartmentAboveNSO")



T0.1.AwarenessUNFPOS <-  merge(merge(T0.1.AwarenessUNFPOS.a,
              T0.1.AwarenessUNFPOS.b, by = "AwarenessUNFPOS", all = TRUE),
        T0.1.AwarenessUNFPOS.c, by = "AwarenessUNFPOS", all = TRUE)

rm(list = c("T0.1.AwarenessUNFPOS.a","T0.1.AwarenessUNFPOS.b", "T0.1.AwarenessUNFPOS.c"))

x1 <- unlist(T0.1.AwarenessUNFPOS[AwarenessUNFPOS=="Yes",
                            list(Percentage.ChiefStatistician_NSO,
                                 Percentage.HeadsOrSeniorManagersNSS,
                                 Percentage.MinistryOrDepartmentAboveNSO)])
x2 <- unlist(T0.1.AwarenessUNFPOS[AwarenessUNFPOS=="N/A",
                                  list(Percentage.ChiefStatistician_NSO,
                                       Percentage.HeadsOrSeniorManagersNSS,
                                       Percentage.MinistryOrDepartmentAboveNSO)])

x3 <- unlist(T0.1.AwarenessUNFPOS[AwarenessUNFPOS=="No",
                                  list(Percentage.ChiefStatistician_NSO,
                                       Percentage.HeadsOrSeniorManagersNSS,
                                       Percentage.MinistryOrDepartmentAboveNSO)])

x <- t(cbind(x1,x2,x3))
row.names(x) <- c("Aware", "N/A", "Not aware")

svg(filename="PlotT0.1.AwarenessUNFPOS.svg", 
    width=6.5, 
    height=3)

  par(mar=c(2,5,1,1), mgp=c(3, 1,0))
  barplot(x,
          xlim = c(0,ncol(x) + 1.82),
          names.arg = c("Chief Statistician\n/ NSO", 
                        "Heads or Senior \nManagers of NSS",
                        "Ministry or Deptartment\nto which NSO reports"),
          ylab = "Percent",
          col =  brewer.pal(3,"Blues")[3:1],
          border = NA,
          cex.names = 0.7,
          legend = rownames(x),
          args.legend = list(bty = "n", cex = 0.8)
          )
  
dev.off()
  

#=============================

# T0.2 - Awareness of existence UNFPOS


T0.2.UserFeedback.a <- responses[, .(.N/nrow(responses)*100),  by = .(C20)]
setnames(T0.2.UserFeedback.a, "C20", "Response")
setnames(T0.2.UserFeedback.a, "V1", "UserCouncilOrGroup")

T0.2.UserFeedback.b <- responses[, .(.N/nrow(responses)*100),  by = .(C21)]
setnames(T0.2.UserFeedback.b, "C21", "Response")
setnames(T0.2.UserFeedback.b, "V1", "UserSatisfactionSurvey")

T0.2.UserFeedback.c <- responses[, .(.N/nrow(responses)*100),  by = .(C22)]
setnames(T0.2.UserFeedback.c, "C22", "Response")
setnames(T0.2.UserFeedback.c, "V1", "IndependentReview")

T0.2.UserFeedback.d <- responses[, .(.N/nrow(responses)*100),  by = .(C23)]
setnames(T0.2.UserFeedback.d, "C23", "Response")
setnames(T0.2.UserFeedback.d, "V1", "UserWorkshops")

T0.2.UserFeedback.e <- responses[, .(.N/nrow(responses)*100),  by = .(C24)]
setnames(T0.2.UserFeedback.e, "C24", "Response")
setnames(T0.2.UserFeedback.e, "V1", "WebsiteTrafficAnalysis")

T0.2.UserFeedback.f <- responses[, .(.N/nrow(responses)*100),  by = .(C25)]
setnames(T0.2.UserFeedback.f, "C25", "Response")
setnames(T0.2.UserFeedback.f, "V1", "None")

T0.2.UserFeedback.g <- responses[, .(.N/nrow(responses)*100),  by = .(C26)]
setnames(T0.2.UserFeedback.g, "C26", "Response")
setnames(T0.2.UserFeedback.g, "V1", "Unknown")

T0.2.UserFeedback.h <- responses[!is.na(C27), .(.N/nrow(responses)*100)]
T0.2.UserFeedback.h[,Response:=1]
setnames(T0.2.UserFeedback.h, "V1", "Other")



T0.2.UserFeedback <-  merge(merge(merge(merge(merge(merge(merge(
                                  T0.2.UserFeedback.a,
                                  T0.2.UserFeedback.b, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.c, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.d, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.e, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.f, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.g, by = "Response", all = TRUE),
                                  T0.2.UserFeedback.h, by = "Response", all = TRUE)

x <- sort(unlist(T0.2.UserFeedback[Response==1, list(UserCouncilOrGroup,
                                    UserSatisfactionSurvey,
                                    IndependentReview,
                                    UserWorkshops,
                                    WebsiteTrafficAnalysis,
                                    Other)]), decreasing = TRUE)



svg(filename="PlotT0.2.UserFeedback.svg", 
    width=6.5, 
    height=4)

par(mar=c(3,6,1,1))
barplot(x,
        names.arg = names(x),
        xlab = "Percent",
        col =  brewer.pal(3,"Blues")[3],
        border = NA,
        horiz = TRUE,
        cex.names = 0.7
        
)
dev.off()
