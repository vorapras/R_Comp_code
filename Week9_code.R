# Initialize VIX data
Y0 = log((20.53/100)^2)
Y1 = log((19.39/100)^2)
# Number of sample path / Size of Monte Carlo
N = 1000
# Time increment
delta = 1/252
# Set up parameters
mu = 0
sigma = 1.5
tau = 0.02
# Generate Geometric Brownian Motion Random Variables
meanGBM = Y0+delta*(mu-0.5*sigma^2)
varGBM = delta*sigma^2
# Using Y(VIX) and Bayes theorem, so we can change the distribution
denom = tau^2+varGBM
meanV1 = (meanGBM*tau^2+Y1*varGBM)/denom
varV1 = (varGBM*tau^2)/denom

#Activity 1a)
# Simulate normal random varaible by using monte carlo
z = rnorm(N)
V1 = sqrt(varV1)*z + meanV1
# Compare Monte Carlo estimation of V1 vs true value
MCestV1 = mean(exp(V1))
trueV1  = exp(mean(V1)+0.5*varV1)
obsV1 = exp(Y1)
# Very close to each other
c(MCestV1,trueV1,obsV1)

#Activity 1c)
V = mean(exp(V1)^2)-mean(exp(V1))^2
c(MCestV1-1.96*sqrt(V),MCestV1+1.96*sqrt(V))
hist(exp(V1))
plot(density(exp(V1)))
quantile(exp(V1),probs=c(0.025,0.975))


# Activity 1b) Ornstein Uhlenbeck Case
k = 5
meanOU = mu+(Y0-mu)*exp(-k*delta)
varOU  = sigam^2(1-exp(-2*k*delta))
# Using Y(VIX) and Bayes theorem, so we can change the distribution
denom = tau^2+varOU 
meanV1 = (meanOU*tau^2+Y1*varOU)/denom
varV1 = (varOU*tau^2)/denom
# Simulate normal random varaible by using monte carlo
z = rnorm(N)
V1 = sqrt(varV1)*z + meanV1
# Compare Monte Carlo estimation of V1 vs true value
MCestV1 = mean(exp(V1))
trueV1  = exp(mean(V1)+0.5*varV1)
obsV1 = exp(Y1)
# Very close to each other
c(MCestV1,trueV1,obsV1)


# Anthithetic Variates
za = z
zb = -z
V1a = sqrt(varV1)*za + meanV1
V1b = sqrt(varV1)*zb + meanV1
# Compare Anthithetic Variates Monte Carlo estimation 
AnMCestV1 = 0.5*mean(exp(V1a))+0.5*mean(exp(V1b))
# Very close to each other
c(AnMCestV1,trueV1,obsV1)

# Control Variates
b = cov(exp(V1),z)
CVestV1 = mean(exp(V1)-b*z)
# Very close to each other
c(CVestV1,trueV1,obsV1)

# Activity 2) Forecasting Volatility
# Observe mu and sigma from historical data but it depends on the number of observation(recent data works best)
# Measure accuracy from how much mean deviate from the true value and variance 
N = 10000
z1 = rnorm(N)
V1 = sqrt(varV1)*z1 + meanV1
meanGBM2 = V1+delta*(mu-0.5*sigma^2)
z2 = rnorm(N)
V2 = sqrt(varGBM)*z2 + meanGBM2
mean(exp(V2))
plot(density(exp(V2)))
# Prob < 0.5 (Downward Trend)
sum(exp(V2) > exp(V1))/N


