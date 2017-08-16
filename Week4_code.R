#############################################################
#########       Excercise 1                    ##############
#############################################################
T=1          # terminal date of the BS model
Nt = 5       # time steps + 1
dt = T/(Nt-1)# disc. parameter of bin approx. to BS
sigma = 0.1  # volatility of corresponding BS model

up = exp(sigma*sqrt(dt))
dn = exp(-sigma*sqrt(dt))

ir = 0.05                    # interest rate
pr = (exp(ir*dt)-dn)/(up-dn) # up proby for tree
K  = 1                       # strike price of the put         
S  = matrix(nrow=Nt,ncol=Nt) # S(i,j) is the stock price at t=i-1

#after j-1 up moves
P = matrix(nrow=Nt, ncol=Nt) # P(i,j) is the put price at t=i-1

#after j-1 up moves
S[1, 1] = 1.1                # initial stock price

#compute stock values
for (i in 2:Nt){
   S[i,1] = dn*S[i-1,1]
        for (j in 2:i){
   S[i,j] = up*S[i-1,j-1]
}
}

#Terminal option payoff
#P(Nt,:)=max(0, K-S(Nt,:));
#P[Nt, ]=(K-S[Nt, ])

for (i in 1 : (Nt)){
   P[Nt,i ] <- if ((S[Nt, i]-K) > 0) (1) else if ((S[Nt, i]-K) <= 0) 0 
}

#construct option price matrix
for (i in 1 : (Nt-1)){
   for (j in 1:(Nt-i)) {
      P[Nt-i,j]=exp(-ir*dt)*(pr*P[Nt-i+1,j+1]+(1-pr)*P[Nt-i+1,j]);}
}

plot(S,P)
#############################################################
#########       Excercise 2                    ##############
#############################################################
T=1          # terminal date of the BS model
Nt = 30      # time steps + 1
dt = T/(Nt-1)# disc. parameter of bin approx. to BS
sigma = 0.1  # volatility of corresponding BS model

up = exp(sigma*sqrt(dt))
dn = exp(-sigma*sqrt(dt))

ir = 0.05                    # interest rate
pr = (exp(ir*dt)-dn)/(up-dn) # up proby for tree
K  = 1                       # strike price of the put         
S  = matrix(nrow=Nt,ncol=Nt) # S(i,j) is the stock price at t=i-1

#after j-1 up moves
P = matrix(nrow=Nt, ncol=Nt) # P(i,j) is the put price at t=i-1

#after j-1 up moves
S[1, 1] = 1.1                # initial stock price

#compute stock values
for (i in 2:Nt){
   S[i,1] = dn*S[i-1,1]
        for (j in 2:i){
   S[i,j] = up*S[i-1,j-1]
}
}

#Terminal option payoff
#P(Nt,:)=max(0, K-S(Nt,:));
#P[Nt, ]=(K-S[Nt, ])

for (i in 1 : (Nt)){
   P[Nt,i ] <- if ((K-S[Nt, i]) > 0) (K-S[Nt, i]) else if ((K-S[Nt, i]) <= 0) 0 
}

#construct option price matrix
for (i in 1 : (Nt-1)){
   for (j in 1:(Nt-i)) {
      P[Nt-i,j]=exp(-ir*dt)*(pr*P[Nt-i+1,j+1]+(1-pr)*P[Nt-i+1,j]);}
}

plot(S,P)
################################################
d1=(log(S[1,1]/K)+(ir+sigma^2/2)*T)/(sigma*sqrt(T))
d2=d1-sigma*sqrt(T)
p=K*exp(-ir*T)*pnorm(-d2)-S[1,1]*pnorm(-d1)
#############################################################
#########       Excercise 3                    ##############
#############################################################
#To compute the price of an American put option on a stock with current value of 50, exercise price 50, time to #maturity ##months, annualized rate of interest r is 10%, annualized volatility σ of the stock is of 40%, and the #number of time ##steps n = 5. Use methods CRRBinomialTreeOption, BinomialTreeOption, BinomialTreePlot in #fOptions package from Rmetrics
library("fOptions")
CRRBinomialTreeOption(TypeFlag = "pa", S = 50, X = 50,
     Time = 5/12, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
## output : Option Price: 4.488459
 
##to plot an option tree (for put american)
CRRTree = BinomialTreeOption(TypeFlag = "pa", S = 50, X = 50,
      Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
    xlab = "n", ylab = "Option Value")
title(main = "Option Tree")
 
##re-run with TypeFlag = "ca" to get price for corresponding American call





## OR ##
price.american=function(n,opt="put"){
u.n=exp(sigma*sqrt(T/n)); d.n=1/u.n
p.n=(exp(r*T/n)-d.n)/(u.n-d.n)
SJ=matrix(0,n+1,n+1)
SJ[1,1]=S
for(i in(2:(n+1)))
{for(j in(1:i)) {SJ[i,j]=S*u.n^(i-j)*d.n^(j-1)}}
OPTe=matrix(0,n+1,n+1)
OPTa=matrix(0,n+1,n+1)
if(opt=="call"){
OPTa[n+1,]=(SJ[n+1,]-K)*(SJ[n+1,]>K)
OPTe[n+1,]=(SJ[n+1,]-K)*(SJ[n+1,]>K)
}
if(opt=="put"){
OPTa[n+1,]=(K-SJ[n+1,])*(SJ[n+1,]<K)
OPTe[n+1,]=(K-SJ[n+1,])*(SJ[n+1,]<K)
}
for(i in(n:1))
{
for(j in(1:i))
{if(opt=="call"){
OPTa[i,j]=max((SJ[i,j]-K)*(SJ[i,j]>K),
exp(-r*T/n)*(OPTa[i+1,j]*p.n+
(1-p.n)*OPTa[i+1,j+1]))}
if(opt=="put"){
OPTa[i,j]=max((K-SJ[i,j])*(K>SJ[i,j]),
exp(-r*T/n)*(OPTa[i+1,j]*p.n+
(1-p.n)*OPTa[i+1,j+1]))}
 
OPTe[i,j]=exp(-r*T/n)*(OPTe[i+1,j]*p.n+
(1-p.n)*OPTe[i+1,j+1])}}
priceop=c(OPTe[1,1],OPTa[1,1])
names(priceop)=c("E","A")
return(priceop)}

## Run it for  
##T=1                     # terminal date of the BS model
## n = 5                  # time steps 
## sigma = 0.1            # volatility of corresponding BS model
## r = 0.05               # interest rate
## K  = 1                 # strike price of the put         
## S = 1.1                # initial stock price


#############################################################
#########       Excercise 4                    ##############
#############################################################
#Put
T=1          # terminal date of the BS model
Nt = 5       # time steps + 1
dt = T/(Nt-1)# disc. parameter of bin approx. to BS
sigma = 0.1  # volatility of corresponding BS model

up = exp(sigma*sqrt(dt))
dn = exp(-sigma*sqrt(dt))

ir = 0.05                    # interest rate
pr = (exp(ir*dt)-dn)/(up-dn) # up proby for tree
K  = 1                       # strike price of the put         
S  = matrix(nrow=Nt,ncol=Nt) # S(i,j) is the stock price at t=i-1

#after j-1 up moves
P = matrix(nrow=Nt, ncol=Nt) # P(i,j) is the put price at t=i-1

#after j-1 up moves
S[1, 1] = 1.1                # initial stock price

#compute stock values
for (i in 2:Nt){
   S[i,1] = dn*S[i-1,1]
        for (j in 2:i){
   S[i,j] = up*S[i-1,j-1]
}
}

#Terminal option payoff
#P(Nt,:)=max(0, K-S(Nt,:));
#P[Nt, ]=(K-S[Nt, ])

for (i in 1 : (Nt)){
   P[Nt,i ] <- if ((K-S[Nt, i]) > 0) (K-S[Nt, i]) else if ((K-S[Nt, i]) <= 0) 0 
}

#construct option price matrix
for (i in 1 : (Nt-1)){
   for (j in 1:(Nt-i)) {
      P[Nt-i,j]=exp(-ir*dt)*(pr*P[Nt-i+1,j+1]+(1-pr)*P[Nt-i+1,j]);}
}

plot(S,P)

#Call
T=1          # terminal date of the BS model
Nt = 5       # time steps + 1
dt = T/(Nt-1)# disc. parameter of bin approx. to BS
sigma = 0.1  # volatility of corresponding BS model

up = exp(sigma*sqrt(dt))
dn = exp(-sigma*sqrt(dt))

ir = 0.05                    # interest rate
pr = (exp(ir*dt)-dn)/(up-dn) # up proby for tree
K  = 1                       # strike price of the put         
S  = matrix(nrow=Nt,ncol=Nt) # S(i,j) is the stock price at t=i-1

#after j-1 up moves
C = matrix(nrow=Nt, ncol=Nt) # P(i,j) is the put price at t=i-1

#after j-1 up moves
S[1, 1] = 1.1                # initial stock price

#compute stock values
for (i in 2:Nt){
   S[i,1] = dn*S[i-1,1]
        for (j in 2:i){
   S[i,j] = up*S[i-1,j-1]
}
}

#Terminal option payoff
for (i in 1 : (Nt)){
   C[Nt,i ] <- if ((S[Nt, i]-K) > 0) (S[Nt, i]-K) else if ((S[Nt, i]-K) <= 0) 0 
}

#construct option price matrix
for (i in 1 : (Nt-1)){
   for (j in 1:(Nt-i)) {
      C[Nt-i,j]=exp(-ir*dt)*(pr*C[Nt-i+1,j+1]+(1-pr)*C[Nt-i+1,j]);}
}

plot(S,C)
#############################################################
#########       Excercise 5                    ##############
#############################################################
T  = 1        # terminal date of the BS model
Nt = 5        # time steps + 1
dt = T/(Nt-1) # disc. parameter of bin approx. to BS
sigma = 0.1   # volatility of corresponding BS model
nsim = 1000   # M realisations

S  = matrix(nrow=nsim ,ncol=1)  # S(i,j) is the stock price at t=i-1
ir = 0.05                       # interest rate
K  = 1                          # strike price of the put         
S[1, 1] = 0.1                   # initial stock price

#compute stock values
for (i in 2:nsim){
   S[i,1] = S[i-1,1]*exp(ir-0.5*sigma^2)*dt+sigma*rnorm(1)*sqrt(dt)
}

for (i in 1 : nsim){
   V[i ,1 ] <- if ((K-S[i, 1]) > 0) (K-S[i, 1]) else if ((K-S[i, 1]) <= 0) 0 
}

P=exp(-ir*dt)*mean(V)































