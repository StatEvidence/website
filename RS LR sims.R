##############################################
### Author:	J. Blume
### Date:	June 18, 2018
### File:	'RS LR sims.r'
###
### Purpose: Repeated Sampling Simulations
###   		 Likelihood Ratio w/ fixed alternative
###   		 Likelihood Ratio w/ restricted MLE
##############################################

#### Settings
sims 	<- 100000
n.max 	<- 1000
k 		<- 8
mu.1 	<- 0.5		## Try: 1, 0.5 (w/ n.max=1000) and 0.05 (n.max=10000)

ct.1 	<- 0
ct.a 	<- 0
n.stop.1 <- rep(-99,n.max)
n.stop.a <- rep(-99,n.max)

#### Simulation Loops
for(i in 1:sims) {

y <- rnorm(n.max, mean=0, sd=1)
mle <- cumsum(y)/(1:n.max)

mu.a <- pmax(mle,mu.1)

like.0 <- dnorm(y, mean=0, sd=1)
like.1 <- dnorm(y, mean=mu.1, sd=1)
like.a <- dnorm(y, mean=mu.a, sd=1)

lr.1 <- cumprod(like.1/like.0)
lr.a <- cumprod(like.a/like.0)

if (any(lr.1>=k)) {
	ct.1=ct.1+1 ;
	n.stop.1[i]=min(which(lr.1>=k))
	}
	else {n.stop.1[i] <- NA}

if (any(lr.a>=k)) {
	ct.a=ct.a+1 ;
	n.stop.a[i]=min(which(lr.a>=k))
	}
	else {n.stop.a[i] <- NA}

if (i%%10000==0) {print(i)}	
			}

#### Print results
cbind(fixed=c(p.err=round(ct.1/sims,3),round(summary(n.stop.1),3)),
	rMLE=c(p.err=round(ct.a/sims,3),round(summary(n.stop.a),3)))

#### Check of simulation accuracy
if (max(n.stop.1,na.rm=TRUE)==n.max) {print("Warning: Increase n.max")}
if (max(n.stop.a,na.rm=TRUE)==n.max) {print("Warning: Increase n.max")}
## Note: increase n.max so this condition is not met.

#### 99th percentile of stopping time (excluding NAs)
quantile(n.stop.1,probs=c(0.95,0.99),na.rm=TRUE)
quantile(n.stop.a,probs=c(0.95,0.99),na.rm=TRUE)

###
##
#