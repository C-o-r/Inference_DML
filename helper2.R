
## Timing in a loop and analyzing the results later using tic.log().
tic.clearlog()
for (x in 1:10)
{
  tic(x)
  Sys.sleep(1)
  toc(log = TRUE, quiet = TRUE)
}
log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
tic.clearlog()
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
mean(timings)
# [1] 1.001
writeLines(unlist(log.txt))


x <- c(4:64); 
y <- x # create some data
par(pch=22, col="red") # plotting symbol and color
par(mfrow=c(2,4)) # all plots on one page
opts = c("Computation times")

  heading = paste("type=",opts)
  plot(x, y, type="n", main=heading)
  lines(x, y, type=opts)
  
  
  
  
  # Create the base plotting window
  # type = "n" does not plot the points
  # Set the background color to "yellow"
  par(bg = "yellow")
  plot(1:10, type = "n")
  
  # Now set the plot region to grey
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "grey")
  
  # Now plot the points on the existing window
  points(1:10)
  
  
  
  In some of the plotting functions to do the plot region color, like 
  boxplot(), you would have to plot the full graphic and then replot it 
  after setting the plot region color. In this case, you would use the 
  'add = TRUE' argument in boxplot() for the second call. You could 
  feasibly get the information from boxplot.stats() and draw the boxes and 
  lines, etc., but just calling boxplot() twice is easier I think.
  
  A quick example for boxplot():
    
    x <- rnorm(100)
  par(bg = "thistle")
  boxplot(x)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "grey")
  boxplot(x, add = TRUE, col = "blue")
  
  
  3
  4
  5
  6
  7
  8
  9
  10
  11
  12
  13
  14
  15
  16
  
  
  # library
  library(plotrix)
  
  #create color palette
  library(RColorBrewer)
  my_colors = brewer.pal(8, "Set2") 
  
  # Create data
  x<-seq(1,100)
  y<-sin(x/5)+x/20
  
  # Plot x and y
  par(mar=c(4,4,2,2))
  clplot(x,y ,
         ylab="time",xlab	="amount of observations", main="", lwd=5, levels=c(1,2,3,4,5), col=my_colors, showcuts=T , bty="n")
  
  
  
  # 
  
  #dobule size of data.frame
  
  
  
  d <- data.frame(a = c(1,2,3),b = c(1,2,3))
  n <- 3
  do.call("rbind", replicate(n, d, simplify = FALSE))
  
  
  
  