# Load necessary libraries
library(tidyr)
library(data.table)
library(plotly)
library(RColorBrewer)

# set your working directory - change as needed
setwd("C:/Users/L.GonzalezMorales/Documents/GitHub/un50sc-foc-fpos/Analysis/")

# load functions
source('f_doughnut.r')
source('f_read.tab2dataTable.r')

#set margins for plots


#----------------------------------
# Read raw data
#----------------------------------

responses <- read.tab2dataTable("CleanResponses.txt")
columns   <- read.tab2dataTable("Columns.txt")

#=====================================
# Organize questions into individual files
#=====================================


countryInfo <- names(responses)[1:14]

# See https://stackoverflow.com/questions/32184252/how-to-select-columns-in-data-table-using-a-character-vector-of-certain-column-n
T0.0.respondents <- responses[,c(countryInfo, "C01", paste("C0", 3:8, sep = "")), with = FALSE]

setnames(T0.0.respondents, 
         c("C01", paste("C0", 3:8, sep = "")), 
         columns[Column%in%c(countryInfo, "C01", paste("C0", 3:8, sep = "")),QuestionText])







# T00 - Respondents by region

TX.RespondentsByRegion <- responses[, .(.N), , by = .(RegionName)]
TX.RespondentsByRegion[, Percentage := round(N/nrow(responses)*100,0)]

TX.RespondentsByRegion<- TX.RespondentsByRegion[order(-rank(N)),list(RegionName,N,Percentage)]

x <- as.data.frame(TX.RespondentsByRegion)

svg(filename="PlotT00.RespondentsByRegion.svg", 
    width=6, 
    height=4)

    par(mar=c(0,0,0,0))
    
    TX.RespondentsByRegion.Plot <- 
      doughnut(x$Percentage, 
               labels = paste(x$RegionName,
                              "\n",
                              x$Percentage,
                              "%", 
                              sep = ""), 
               edges = 1000, 
               outer.radius = 0.8, 
               inner.radius=0.4, 
               col = brewer.pal(length(x$Percentage),"Blues"), 
               border = FALSE, 
               lty = NULL, 
               main = NULL,
               cex = 1)
    
dev.off()




#=============================

# T0.1 - Respondents answering on behalf or NSO

T0.1.RespondentsOnBehalfOf <- responses[, .(.N), , by = .(C09)]
T0.1.RespondentsOnBehalfOf[, Percentage := round(N/nrow(responses)*100,0)]

T0.1.RespondentsOnBehalfOf<- T0.1.RespondentsOnBehalfOf[order(-rank(N)),list(C09,N,Percentage)]

setnames(T0.1.RespondentsOnBehalfOf, "C09", "Answering on behalf of")

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
