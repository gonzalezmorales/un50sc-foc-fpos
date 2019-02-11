
v_barplot <-
  function (x, x.labels, x.color)
  {

    label.width <- 120 / length(x)
    label.lines <- round(max(nchar(x.labels))/label.width) + 4
    
    x.labels <-  sapply(x.labels, 
                             function(s) paste(strwrap(s, 15), 
                                               collapse = "\n"), 
                             USE.NAMES = FALSE)
    
    par(mar=c(label.lines,5,3,1), xpd = TRUE)
    
    mp <- barplot(height = x,
            names.arg = "",
            las = 1,
            legend.text = FALSE,
            horiz = FALSE,
            col = x.color, 
            ylab = "percent",
            width = 0.75,
            space = 0.25,
            border = NA,
            axes = FALSE)
    
    mtext(text = x.labels, side = 1, at = mp, line = label.lines-2)
    
    # Add horizontal axis:
    axis(side = 2, lwd = 0)
    
}