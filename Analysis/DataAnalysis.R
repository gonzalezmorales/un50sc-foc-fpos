# Load necessary libraries
library(tidyr)
library(data.table)
library(plotly)
library(RColorBrewer)
# set your working directory - change as needed
setwd("C:/Users/L.GonzalezMorales/Documents/GitHub/un50sc-foc-fpos/Analysis/")

#set margins for plots

#==================================
# FUNCTIONS
#==================================

# Doughnut plot (https://gist.github.com/chelsyx/ebc8ff5d7125a79d4297)

#' 
#' @param x a vector of non-negative numerical quantities.
#'          The values in \code{x} are displayed as the areas of 
#'          doughnut slices.
#' @param labels one or more expressions or character strings giving
#'          names for the slices.  Other objects are coerced by
#'          \code{\link{as.graphicsAnnot}}.  For empty or \code{NA}
#'          (after coercion to character) labels, no label nor 
#'          pointing line is drawn.
#' @param edges the circular outline of the doughnut is approximated 
#'          by a polygon with this many edges.
#' @param outer.radius the doughnut is drawn centered in a square box 
#'          whose sides range from \eqn{-1} to \eqn{1}.  If the 
#'          character strings labeling the slices are long it may be 
#'          necessary to use a smaller radius.
#' @param inner.radius Size of the inner radius to be whited 
#' @param clockwise logical indicating if slices are drawn clockwise 
#'          or counter clockwise (i.e., mathematically positive 
#'          direction), the latter is default. 
#' @param init.angle number specifying the \emph{starting angle} (in
#'          degrees) for the slices. Defaults to 0 (i.e., 
#'          \sQuote{3 o'clock}) unless \code{clockwise} is true where 
#'          \code{init.angle} defaults to 90 (degrees), (i.e., 
#'          \sQuote{12 o'clock}).
#' @param density the density of shading lines, in lines per inch.
#'          The default value of \code{NULL} means that no shading 
#'          lines are drawn. Non-positive values of \code{density} 
#'           also inhibit the drawing of shading lines.
#' @param angle the slope of shading lines, given as an angle in  
#'          degrees (counter-clockwise).
#' @param col a vector of colors to be used in filling or shading
#'          the slices. If missing a set of 6 pastel colours is
#'          used, unless \code{density} is specified when 
#'          \code{par("fg")} is used. 
#' @param border, lty (possibly vectors) arguments passed to 
#'          \code{\link{polygon}} which draws each slice.
#' @param main an overall title for the plot.
#' @param \dots graphical parameters can be given as arguments to
#'          \code{pie}.  They will affect the main title and 
#'          labels only.
#'          
#' @author Original pie plot by R Core, amended by Markus Gesmann 
#'         for a doughnut plot
#' @keywords hplot
#' @seealso \code{\link{pie}} 
#' @export
#' @examples
#'  x <- c(2,4,3,2,4)
#'  doughnut(x)
#'  ## Add lables
#'  doughnut(x, labels=LETTERS[1:5])

doughnut <-
  function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL, 
            angle = 45, col = NULL, border = FALSE, lty = NULL, 
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        palette()
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p), 
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
              angle = angle[i], border = border[i], 
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
             ...)
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i], 
              angle = angle[i], border = border[i], 
              col = "white", lty = lty[i])
    }
    
    title(main = main, ...)
    invisible(NULL)
  }



#----------------------------------
# Read raw data
#----------------------------------

responses <- as.data.table(read.table("CleanResponses.txt", 
                                      header = TRUE, 
                                      sep = "\t",
                                      quote = "",
                                      na.strings = "", 
                                      stringsAsFactors = FALSE,
                                      encoding = "UTF-8",
                                      comment.char = ""))


columns <- as.data.table(read.table("Columns.txt", 
                                    header = TRUE, 
                                    sep = "\t",
                                    quote = "",
                                    na.strings = "", 
                                    stringsAsFactors = FALSE,
                                    encoding = "UTF-8"))

#=====================================
# Analisis
#=====================================

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
