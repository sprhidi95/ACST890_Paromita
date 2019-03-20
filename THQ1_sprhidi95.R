#Q1
bondprice <- function (coupon,facevalue,y,n)
# y is the interest for every 6 months
# n is the last coupon payment
{
  j <- c(1:n) # interest till the nth payment
  P = (sum(coupon*exp(-y*j))) + (facevalue*exp(-y[n]*n))
  return (P)
}


#Q3
# (a) Read the dataset into R and name it dataset
setwd("C:/Users/user/Desktop/ACST 890/THQ 1")
singapore.economy = read.csv ("singapore.economy.csv", header = TRUE)
dataset <- singapore.economy
dim (dataset)

# (b) Exclude all records that contain NA
dataset <- na.omit (dataset)
dim (dataset)

# (c) Plot Singapore GDP against time.
plot(dataset$time, dataset$gdp, xlab = "Time", ylab = "GDP (%)", main = "Singapore GDP growth")

# (d) Calculate the mean and standard deviation of for each of the three periods
m<-tapply(dataset$gdp,dataset$period,mean) 
sd<-tapply(dataset$gdp,dataset$period,sd)
install.packages("data.table")
library(data.table)
stat.table<-data.table(m,sd)
stat.table

# (e) Using function pairs()
pairs (~gdp + exp + epg + hpr + gdpus + oil + crd + bci,data=dataset)

# (f)  
#lm() function to perform a simple linear regression 
simplelr = lm(gdp~exp, data=dataset)
# summary() function to print the results
simplelr
summary (simplelr)

# (g)
# Fit a multiple regression model to predict gdp
multilr = lm(gdp~exp+epg+hpr+oil+gdpus+crd, data=dataset)
# summary() function to print the results
multilr
summary (multilr)

# (h)
# Calculate the 5% quantile of gdp
qu<-quantile (dataset$gdp, 0.05 )
qu
# crisis state if gdp is lower than quantile
state = rep("normal", nrow(dataset))
state[dataset$gdp < qu] = "crisis"
# create a factor vector for the economy state
state = as.factor (state)
dataset = data.frame(dataset,state)
# fit the logistic regression model
traingdata = subset (dataset,period < 3)
logisticrm = glm (state ~ bci,data = traingdata,family = binomial)
summary (logisticrm)
# Compute the confusion matrix
table(state,state)
