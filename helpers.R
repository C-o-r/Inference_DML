ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)

lm.D9 <- lm(ctl ~ trt)

form            <- as.formula(paste(form_y, "~", form_x));    
fit.p           <- lm(weight ~ group,  x = TRUE, y = TRUE); 

a=list(x = fit.p$x, y=fit.p$y)



lassoF <- function(x, x, form_x, form_y, logit=FALSE, alp=alp, arg=arg, s=s)
  
  x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)

par(mar=c(5, 4, 4, 8) + 0.1)

plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE)

lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")