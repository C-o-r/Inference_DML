#Graph1_ complexity analyssi_ computation times 

setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")


f<- read.csv(file="c1_Forest_43_200.csv", header=TRUE, sep=",")

n<- read.csv(file="c1_Nnet_43_200.csv", header=TRUE, sep=",")

# actually 60_200
b<- read.csv(file="c1_TreeswhichBoost_43_200.csv", header=TRUE, sep=",")

l<- read.csv(file="c1_Rlasso_43_200.csv", header=TRUE, sep=",")

t<- read.csv(file="c1_TreesO_43_200.csv", header=TRUE, sep=",")


df1 = data.frame(f, n, l,t) 
df2= df1[-(1:17), , drop = FALSE]
df3 =cbind(df2, b)

x=60:200


#x <- c(43:200); 
#y <- as.numeric(timings) # create some data

colnames(df3) <- c("f","n","l","t","b") 

x <- c(60:200)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)


par(bg = "white")
plot(x, df3$f, type="b",
     pch=21, col="black",
     yaxt="n", lty=3,ylim=c(0, 1.5), ann=FALSE)

####change y lim to 1.5 end 30 for two grpahs!!!!


lines(x,df3$n , type="b", pch=22, col="black", lty=2)

lines(x,df3$l , type="b", pch=23, col="black", lty=2)

lines(x,df3$t , type="b", pch=24, col="black", lty=2)

lines(x,df3$b , type="b", pch=25, col="black", lty=2)

axis(2,  col.axis="black", las=2)

legend("topleft", inset=.05, title="Algorithm type", c("Forest","Nnet","Lasso","Tree","Boosting"),
       lty=c(1, 5), pch=c(21,22,23,24,25), col=c("black","black","black","black","black"))

title("Algorithm duration based on observations (1)",
      xlab="amount of observation in the dataset",
      ylab="time")



x<- x <- c(60:200)


