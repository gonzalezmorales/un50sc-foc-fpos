
h_barplot <-
  function (x, x.labels, x.color)
  {

    length.labels <- round(max(nchar(x.labels))/2,0) + 5
    
    par(mar=c(5,length.labels/2.2,3,1), xpd = TRUE)
    
    barplot(height = x[length(x):1],
            names.arg = sapply(x.labels[length(x):1], 
                               function(s) paste(strwrap(s, length.labels), 
                                                 collapse = "\n"), 
                               USE.NAMES = FALSE),
            las = 1,
            legend.text = FALSE,
            horiz = TRUE,
            col = x.color, 
            xlab = "percent",
            width = 0.75,
            space = 0.5,
            border = NA,
            axes = FALSE)
    
    # Add horizontal axis:
    axis(side = 1, lwd = 0)
    
}