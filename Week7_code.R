#data 
Y0=log((20.53/100)^2)
Y1=log((19.38/100)^2)

#Monte Carlo parametes
N=100
m=10
Delta=1
delta=Delta/m
time=seq(from=0,by=delta,to=Delta)

#GBM parameters
mu=0.5
sigma=0.5
tau=0.1
meanGBM = Y0 + Delta*(mu-0.5*sigma^2)
varGBM = Delta*sigma^2

#########################################
#Activity 1 - Euler and Milstein schemes#
#########################################
Ve=matrix(nrow=N,ncol=m+1)
Vm=matrix(nrow=N,ncol=m+1)
for (i in 1:N){
Ve[i,1]=exp(Y0)
Vm[i,1]=exp(Y0)
Z=rnorm(m,0,sqrt(delta)) 
for (j in 1:m){
Ve[i,j+1]=Ve[i,j] + delta*mu*Ve[i,j] + sigma*Ve[i,j]*Z[j]
Vm[i,j+1]=Vm[i,j] + delta*mu*Vm[i,j] + sigma*Vm[i,j]*Z[j]
Vm[i,j+1]= Vm[i,j+1] + 0.5*delta*(sigma^2)*Vm[i,j]*(Z[j]^2 - 1)
}
}

#plot all paths of Milstein Scheme. Change to Ve for Euler Scheme
plot(time,Vm[1,],type="l",ylim=c(min(Vm),max(Vm)))
for (i in 2:N){
lines(time,Vm[i,])
}
points(time(m+1),exp(Y1),col="red") #Include observation Y1
points(time[m+1],exp(Y1+1.96*sqrt(tau)),col="blue") #Include lower endpoint of 95% CI for Y1
points(time[m+1],exp(Y1-1.96*sqrt(tau)),col="blue") #Include upper endpoint of 95% CI for Y1

#Plot empirical pdf of endpoints (from Euler and Milstein schemes) against the true pdf
logv1e= log(Ve[,m+1])
logv1m= log(Vm[,m+1])
plot(density(logv1e))
lines(density(logv1m),col="green")
xgrid=seq(from=-4.5,to=-1,by=0.001)
lines(xgrid,dnorm(xgrid,meanGBM, sqrt(varGBM)),col="red")



##################################################################
#Properties of Monte Carlo Estimators based on Euler and Milstein#
##################################################################
Niter = 500
MCe = matrix(nrow=Niter,ncol=1)
MCm = matrix(nrow=Niter,ncol=1)
for (k in 1:Niter) { 
for (i in 1:N){
Ve[i,1]=exp(Y0)
Vm[i,1]=exp(Y0)
Z=rnorm(m,0,sqrt(delta)) 
for (j in 1:m){
Ve[i,j+1]=Ve[i,j] + delta*mu*Ve[i,j] + sigma*Ve[i,j]*Z[j]
Vm[i,j+1]=Vm[i,j] + delta*mu*Vm[i,j] + sigma*Vm[i,j]*Z[j]
Vm[i,j+1]= Vm[i,j+1] + 0.5*delta*(sigma^2)*Vm[i,j]*(Z[j]^2 - 1)
}
}
logv1e= log(Ve[,m+1])
logv1m= log(Vm[,m+1])
dy2 = -(0.5/tau^2)*(Y1-logv1m)^2
MCm[k]=mean( 1/sqrt(2*pi*tau^2)*exp(dy2)  ) #Calculate and store the Milstein Monte Carlo estimate
dy2 = -(0.5/tau^2)*(Y1-logv1e)^2
MCe[k]=mean( 1/sqrt(2*pi*tau^2)*exp(dy2)  ) #Calculate and store the Eueler Monte Carlo estimate
}

#Plot the empirical densities of the Monte Carlo estimators against the true value of the likelihood
plot(density(MCe))
lines(density(MCm),col="green")
points(dnorm(Y1,meanGBM,sqrt(varGBM)),0,col="red")























