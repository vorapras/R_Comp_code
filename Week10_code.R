S = 100
n = 500 # number of x values at which u is compute 
xmin = -4
xmax = 4
x = seq(from=xmin, to=xmax, by=(xmax-xmin)/(n+1)) # grid of x
inds = seq(from=n+2, to = 1, by=-1) 
K = S/exp(-x[inds])
T = 1 #Time to maturity, assume t=0 so T-t=T
m = 1000 # number of time discretisations
r = 0.05; 
d = 0.02;
rd = r-d;
sigma = 0.4
s2 = sigma^2
tau = 0.5*T*s2
t =  seq(from = 0, to=tau, by=tau/(m+1)) # time grid
a = -0.5*(-1+(2*rd/s2) )
b = -0.25*((-1+(2*rd/s2) )^2) -(2*r/s2)
dt = t[2]-t[1] #delta t
dx = x[2]-x[1] #delta x
lambda = dt/(dx*dx)
lambda
u = matrix(nrow=n+2,ncol=m+2)
u[,1] = exp(0.5*(-1+(2*rd/s2))*x)*pmax(exp(x)-1,0)
u[1,1:m+2] = 0
u[n+2,1:m+2] = 0

###################
# EXPLICIT SCHEME #
###################

A = (1-2*lambda)*diag(n)
for (i in 2:n-1){
    A[i,i-1]=lambda;
    A[i,i+1]=lambda;
}
A[1,2] = lambda
A[n,n-1] = lambda
for (j  in 1:(m+1)){
	u[2:(n+1),j+1]=A %*% u[2:(n+1),j];
}

plot(x,u[,m+2],type="l")


###################
# IMPLICIT SCHEME #
###################

A = (1+2*lambda)*diag(n)
for (i in 2:n-1){
    A[i,i-1]=-lambda;
    A[i,i+1]=-lambda;
}
A[1,2] = -lambda
A[n,n-1] = -lambda
Ainv = solve(A)
for (j  in 1:(m+1)){
	u[2:(n+1),j+1]=Ainv %*% u[2:(n+1),j];
}
plot(x,u[,m+2],type="l")

#############
# CN SCHEME #
#############

A = (1+lambda)*diag(n)
for (i in 2:n-1){
    A[i,i-1]=-lambda/2;
    A[i,i+1]=-lambda/2;
}
A[1,2] = -lambda/2
A[n,n-1] = -lambda/2
Ainv = solve(A)
B = (1-lambda)*diag(n)
for (i in 2:n-1){
    B[i,i-1]=lambda/2;
    B[i,i+1]=lambda/2;
}
B[1,2] = lambda/2
B[n,n-1] = lambda/2
C = Ainv %*% B
for (j  in 1:(m+1)){
	u[2:(n+1),j+1]=C %*% u[2:(n+1),j];
}

plot(x,u[,m+2],type="l")

###############################
# Check against exact formula #
###############################

#Calculate BS formula
sst = sigma*sqrt(tau)
d1 = (log(S/K) + rd + tau )/sst
d2 = d1 - sst
AnPrice = ( S*exp(-d*T)*pnorm(d1) ) - (K*exp(-r*T)*pnorm(d2)  )

#transform to numerical solution of BS - PDS 
V = exp(a*x+b*tau) * u[,m+2] * K

#plot
istart=round(0.5*n + 1)
iend = 2*istart
inds=seq(from=istart, to=iend, by=1) 
plot(x[inds],V[inds],type="l")
lines(x[inds],AnPrice[inds],col="red")


