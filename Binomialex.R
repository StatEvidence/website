############################################################
## Author: 	Jeffrey D. Blume
## Date:	October 19, 2017
## File:	Binomialex.R
############################################################

x=10
n=205

theta=seq(0,0.25,0.001)

## Binomial Likelihood, standardized
lf=exp(x*log(theta) + (n-x) * log(1 - theta))
lf=lf/max(lf)

## Support Intervals
p1=theta[lf >= 1/8]
i1=rep(1/8, length(p1))
p2=theta[lf >= 1/32]
i2=rep(1/32, length(p2))

## Plot 
plot(theta, lf, type = "n", xlab = "Probability of CV Event", ylab = "Standardized Likelihood")
lines(theta,lf,col="black",lwd=2)

lines(p1,i1,col="dodgerblue",lwd=2)
points(min(p1),1/8,col="dodgerblue",pch=16,cex=1)
points(max(p1),1/8,col="dodgerblue",pch=16,cex=1)

lines(p2,i2,col="firebrick",lwd=2)
points(min(p2),1/32,col="firebrick",pch=16,cex=1)
points(max(p2),1/32,col="firebrick",pch=16,cex=1)


## Legend (by hand)
text(0.175, 0.86, paste("Max at", 
	signif(c(theta[lf == max(lf)]), digits = 2)),cex=1.2,col="black")
text(0.175, 0.80, paste("1/8  SI (", round(min(p1), 
	digits = 3), ",", round(max(p1), digits = 3), ")"),cex=1.2,col="dodgerblue")
text(0.175, 0.74, paste("1/32 SI (", round(min(p2), digits = 3), ",", round(max(p2), digits = 3), 
	")"),cex=1.2,col="firebrick")


y1=dbinom(10,205,0.06)/dbinom(10,205,10/205)
points(0.06,y1,col=1,pch=16,cex=0.8)
arrows(0.072,y1,0.063,y1,col=1,length=0.1)
text(0.075+0.01,y1,"L(0.06)\n= 0.784",cex=1.1)

y2=dbinom(10,205,0.03)/dbinom(10,205,10/205)
points(0.03,y2,col=1,pch=16,cex=0.8)
arrows(0.017,y2,0.027,y2,col=1,length=0.1)
text(0.018-0.012,y2,"L(0.03)\n= 0.350",cex=1.1)


text(0.18,0.68,"L(0.06)/L(0.03)=2.24 (Arrows)",cex=1.2)

############################################################
############################################################
x1=0
n1=30
x2=2
n2=70
x3=7
n3=105

lf.1=exp(x1*log(theta) + (n1-x1) * log(1 - theta))
lf.1=lf.1/max(lf.1,na.rm=T)

lf.2=exp(x2*log(theta) + (n2-x2) * log(1 - theta))
lf.2=lf.2/max(lf.2)

lf.3=exp(x3*log(theta) + (n3-x3) * log(1 - theta))
lf.3=lf.3/max(lf.3)

lines(theta,lf.1,col="darkgray",lty=2,lwd=1)
lines(theta,lf.2,col="darkgray",lty=2,lwd=1)
lines(theta,lf.3,col="darkgray",lty=2,lwd=1)

###
##
#