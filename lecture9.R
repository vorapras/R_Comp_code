##############################################################################
## Code for FM457A and FM457B - Lecture 9 

#always start your code by clearing the memory
rm(list=ls())

#define a variable x, with value 1
x = 3

#define a variable y, with value 5
y = 5

#define a 1x5 vector V1
V1 = c(1.1,.99,1.07,.87,.95)

#define a 1x5 vector V2
V2 = c(.9,1.03,1.03,1.05,.99)

#define a 5x1 column vector V3
V3 = c(1,2,3,4,1)

#see which variables exist, use:
ls()

x=5

#delete variables x and y 
rm(x,y)

#define a 3x5 matrix M1
M1=matrix(c(1.01,1.01,1.03,.99,.93,.99,.98,.97,.96,.95,
    1.21,.77,.89,1.13,1.05), nrow=3)
M1

M1=matrix(c(1.01,1.01,1.03,.99,.93,.99,.98,.97,.96,.95,
    1.21,.77,.89,1.13,1.05), nrow=3, byrow=TRUE)
M1

#find out size of vectors or matrices
dim(M1)

#Define a 3x5 matrix M2, which uses V1 and V2 as pieces.
M2=matrix(c(V1,V2,V1), nrow=3, byrow=TRUE)
M2
#byrow=TRUE tells R that the matrix is filled by rows.
#Correspondingly, byrow=FALSE means that the matrix is 
#filled by columns (which is the default)

M21=matrix(c(V1,V2,V1), nrow=3) 
M21

#Define a 3x5 matrix M3 with all of its entries being zero
M3=matrix(0,nrow=3,ncol=5)

#Define a 3x5 matrix M4 with all of its entries being ones
M4=matrix(1,nrow=3,ncol=5)

#Define a variable z, whose value is the 3rd row, 2nd column of M1
z=M1[3,2]


##############################################################################
## Elementary Algebra

### Note: Adding two matrices which do not agree in dimensions is possible -
### this is dangerous since it might lead to unwanted results if you think 
### in terms of matrices! Example:
M5 = M1 + V1;
M5 #the row vector V1 is simply added to each row of the matrix

#matrix addition
M5 = M1 + M2

#element by element multiplication
M5 = M1*M2

#element by element division
M5 = M1/M2

#transpose a matrix
M5 = t(M1)

#matrix multiplication
M6 = M5%*%M1 #post-multiplying M5
M6

M7 = M1%*%M5 #pre-multiplying M5
M7

#delete matrix M5
rm(M5)

#scalar mutliplication
x=2
M5 = M1*x


##############################################################################
## Range of numbers

#number ranges
V3 = c(1:5)
V3

#number ranges with different step size
V3 = c(seq(1,13,3))
V3

M5 = c(1:2,1:5)
M5

V3 = M1[,2]
V3

##############################################################################
## Descriptive Statistics - Useful Functions

bp <- read.csv(file="data_bp.csv",head=TRUE,sep=",")
vlcm <- read.csv(file="data_vlcm.csv",head=TRUE,sep=",")

#look at part of the data
bp[1:5,]

#maximum of realized BP returns
max(bp[,4])

#minimum of realized Volcom returns and loacation of minimum
a = min(vlcm[,4])       #minimum value
b = which.min(vlcm[,4]) #location of minimum value

#mean of daily returns in percent 
100*c(mean(bp[,4]),mean(vlcm[,4]))

#std of daily returns in percent 
100*c(sd(bp[,4]),sd(vlcm[,4]))

#std of daily returns in percent 
cor(bp[,4],vlcm[,4])


##############################################################################
## Plot Time Series

#capture the number of rows and columns of a vector
c = dim(vlcm)[1]
d = dim(vlcm)[2]

#create a time index
time = seq(1,c,1)

#plot Volcom's return time series
plot(time,vlcm[,4],type='l',col=1)

#plot BP's return time series in the same graph - Note: you 
#must not close the existing graph before executing this line
lines(time,bp[,4],type='l',col=2)

#multiple plots
par(mfrow=c(2,1))
plot(time,vlcm[,3],type='l',col=1,main="Volcom Price",xlab="Time",ylab="Volcom Returns")
plot(vlcm[,4],bp[,4],col=2,main="Scatter Plot of Returns",xlab="Volcom",ylab="British Petroleum")


##############################################################################
## Saving the Work

#saves the variables x and z in a file called xy.RData
save(x, z, file = "xy.RData")

#save all variables in a file called workspace9.RData
save(list = ls(all.names = TRUE), file = "workspace9.RData")

#clear workspace
rm(list=ls())

#loads the variables in workspace9.RData
load("workspace9.RData")


##############################################################################
## Using Own Functions

### Discuss Function blackscholes

#you cannot simply call on the function. This will give you an error message
blackscholes(100,110,.05,1,.2)

#R needs to first "learn/read" the source code of the function
#This can be done by sourcing the respective .r file
source("blackscholes.r")

#now we are ready to use the BS pricing function
#the function returns the BS call and put price
blackscholes(100,110,.02,1,.35)


#you can also install packages directly using lines of code

#Install the package and load it
install.packages('fOptions')
library(fOptions)

#Calculate the value of the option and plot
optionVals<-BinomialTreeOption(TypeFlag="ce",S=100,X=100,Time=1,r=0.02,b=0,sigma=0.3,n=5)
BinomialTreePlot(optionVals, dx = 0, dy = 0.1, cex = 0.8, digits = 2)

blackscholes(100,100,.02,1,.3)

#Install the package and load it
install.packages('plot3D')
library(plot3D)
example(image2D)
example(surf3D)
