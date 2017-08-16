# Clear current workspace
rm(list = ls()) # Remove all variables
graphics.off() # Remove all plots

getwd()

#getwd {base}	
#getwd returns an absolute filepath representing the current working directory of the R process; setwd(dir)
#is used to set the working directory to dir.
#***************** Vectors *****************#
#   
a=vector(mode="numeric",length=3)

a=c(1,2,5.3,6,-2,4)  #numeric vector

a2=c("one","two","three")   #charecter vector

a3=c(TRUE,TRUE, FALSE, FALSE, TRUE)#logical vector

a[3]

a[1:4]

a[c(2,4)]

#***************** Exercise 1 *****************#
#  
 Nsim=100      #number of random numbers
    X=runif(Nsim)

    X1 = X[-Nsim] #vectors 
    X2 = X[-1]    #pairs that are adjacent

    plot(X1,X2)

## OR 

    par(mfrow=c(1,3))#create a graph with 1 row and 3 columns
                     #par can be used to set or query graphical parameters. 
                     #Parameters can be set by specifying them as arguments to par in tag = value form, 
                     #or by passing them as a list of tagged values.
    hist(X)
    hist(X1)
    hist(X2)
#***************** Matrices *****************#
#
K=matrix(nrow=4,ncol=3)

a=1:20
K=matrix(a,nrow=5,ncol=4)

K=matrix(a,nrow=5,ncol=4,byrow="TRUE")

K[,4]
K[3,]

K[2:4,1:3]
#***************** Exercise 2 *****************#
#
a1=vector(mode="numeric",length=10)
y=matrix(a1,nrow=2,ncol=5)

write.table(y,"/Users/ZerWinner/Desktop/R/Computational Finance/mydata.csv",sep=",")
mydata2=read.table("/Users/ZerWinner/Desktop/R/Computational Finance/mydata.csv",sep=",")
#***************** Exercise 3 *****************#
#
x=runif(10000)
mean(1/(1+x)) #Note the number of decimal places that are correct.
#***************** Exercise 5 *****************#
#a)
 Nsim = 10^5   
 y  = runif(Nsim)
 x  = exp(-y^(1.5))
 mu = mean(x)
 se  = sqrt(mean(x^2)-mu^2)/sqrt(Nsim)
 LCI = mu-1.96*se
 UCI = mu+1.96*se
#b)
u<-runif(100000)
y<-(1/u^2)*exp(-(1/u-1)^(1.5))
 mu=mean(y)
 se = sqrt(mean(y^2)-mu^2)/sqrt(Nsim)
 LCI = mu-1.96*se
 UCI = mu+1.96*se
#
y = 1/(u*(sqrt(-2*log(u))))*exp(-((-2*log(u))^(1.5)))
 mu = mean(y)
 se = sqrt(mean(y^2)-mu^2)/sqrt(Nsim)
 LCI = mu-1.96*se
 UCI = mu+1.96*se
#***************** Exercise 6 *****************#
#
n = 100000
f = function(x){
  u = runif(n, 0, x);
  return(x*mean((1/(1+u^2))))
}
arctan = f(sqrt(3))

#Compare with:
atan(sqrt(3))
pi/3

plot(sapply((seq(1, 2, 0.01)),f))#Generate regular sequences. seq is a standard generic with a default method.
#***************** Exercise 7 *****************#
#
#a
u=runif(10^4,0,1)
y=(8*u)^(1/3)
plot(density(y))
# Histograms can be a poor method in describing the shape of the distribution as the shape is affected by the number of bins. 
# A way to address this is Kernel density plots.
hist(y)
###b
u=runif(10^4,0,1)
y=(u^(-1/2)-1)^(1/2)
plot(density(y))
hist(y)
###c
u=runif(10^4,0,1)
y=(sqrt(1+8*u)-1)/2
plot(density(y))
hist(y)
############################################
###d
u=runif(10^4,0,1)
c=1
y=log((1-exp(-c))*u)/-c
plot(density(y))
hist(y)
#***************** Exercise 8 *****************#
#
#a
#1
U=runif(10^5,0,1)
#2
X=floor(1/(1-U))
##b
approx=mean((X+1)/X)
Eulers=(pi^2)/6

approx
Eulers
#***************** Exercise 9*****************#
#
#   Acceptance-Rejection Algorithm for discrete random variables
#1. Generate a rv Y distributed as q.
#2. Generate U (independent from Y).
#3. If U =p(Y)/cq(Y), then set X = Y; otherwise go back to 1.
#From a
U=runif(10^4,0,1)
#2
X=floor(1/(1-U))
#3
#
# Note that what needs to be checked each time is whether U<1/i=>1-u*i>0
#
##################################
X=X*(runif(10^4)<(X+1)/(2*X))
mean(X)
X=X*sign(1-U*X)
mean(X)

# sign returns a vector with the signs of the corresponding elements of x 
#(the sign of a real number is 1, 0, or -1 if the number is positive, zero, or negative, respectively).
#

X=X[!X %in% c(0)]
length(X)
X
#! indicates logical negation (NOT)
# Remove the zeroes from x
# The probability of acceptance is 1/c. Hence, we would like c to be as small as
# possible and ideally close to 2, ie c=12/pi^2
# 12/(pi^2)=1.215966=>1/(12/(pi^2))= 0.8223916, thus we will be accepting 82.23916% of the values.
#***************** Exercise 10*****************#
#
x=runif(10^4)
u=runif(10^4)
#c=4/3
x=x*(u<(3/4)*(1-x)*(1+3*x))
x=x[!x %in% c(0)]
length(x)

x=1-sqrt(runif(10^4))
u=runif(10^4)
#c=2
x=x*(u<(1+3*x)/4)
x=x[!x %in% c(0)]
length(x)










