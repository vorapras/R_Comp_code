# Press Control+L to clear console
# Remove all variables and data set
rm(list=ls())

# Show your directory
getwd()

# Generate uniform random number
Nsim=100
x=runif(Nsim)
x1=x[-Nsim] # Delete the last element
x2=x[-1] # Delete the first element
plot(x1,x2)

# Allocate number of spaces for plotting graph
par(mfrow=c(1,3))
hist(x)
hist(x1)
hist(x2)

# Create a vector and export to 
# your working directory in a csv file
# Then import back to P in a differnt object
y=matrix(c(1:10),2,5)

# Calculate integral of 1/1+x from [0,1]=log(2)
x=runif(10000000)
mean(1/(1+x)) # use law of large number 

# Calculate integral of function g(x) 
Nsim=10^5
y=runif(Nsim)
x=exp(-y^(1.5))
mu=mean(x)
se=sqrt(mean(x^2)-mu^2)
lcI=mu-1.96*se
ucI=mu+1.96*se

# Calculate integral of function g(x) 
u<-runif(100000)
y=(1/u)*exp(log(u)^(1.5))
mu=mean(y)
se=sqrt(mean(x^2)-mu^2)
lcI=mu-1.96*se
ucI=mu+1.96*se

# Calculate integral of function g(x) 
y=1/(u*sqrt(-2*log(u)))
mu=mean(y)
se=sqrt(mean(y^2)-mu^2)
lcI=mu-1.96*se
ucI=mu+1.96*se

# The inverse tangent function arctan
n = 100000
f=function(x)
{
  u=runif(n,0,x)
  return (x*mean((1/(1+u^2))))
}
arctan=f(sqrt(3))
# Compare with
atan(sqrt(3))
pi/3
plot(sapply((seq(1, 2, 0.01)),f)) # Generate regular sequence





