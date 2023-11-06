set.seed(85) #RNG seed
#Generate a set containing 200 samples, with a mean of 10, SD=2.
x = rnorm(n=200, mean=10, sd=2) 

#Slope = 0.4. X is the vector created above.
#rnorm(200,0,1) is a vector containign different intercept-values.
y = 0.4*x+rnorm(200, 0, 1)

plot(x,y,las=1,
     xlab="Leaf length (mm)",
     ylab="leaf width (mm)")

#Generates a linear regreission, where y is a a function of x.
m = lm(y~x) 

#cf holds the coefficients of m.
cf = m$coef

#The first element contains the intercept-value.
#The second elemenet contains the slope.
predvals = cf[1] + cf[2]*x

#Change the plot parameters to host two plots.
par(mfrow=c(1,1))
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
#Adds regression line 'm' to the plot.
#Can also be done using the lines() function (see below)
abline(m) 

#Draws vertical lines between (x,y) and (x, predicted y-value)
segments(x, y, x, predvals) 

#Generates a histogram of the linear regression.
hist(residuals(m), xlab="", las=1)

#Generates different plots for the data. "Generic".
plot(x)

#Generates 200 new x-values, I think they should be the same.
newx = seq(min(x), max(x), length.out=200)

#cf[1] contains the intercept-value.
#cf[2] contains the slope value.
predy = cf[1] + cf[2]*newx

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)", main = "Newx")
#Adds predicted y values as a function of x.
lines(newx, predy)

#Draws vertical lines.
segments(x, y, x, predvals) 

#(PASTE IN COMMAND LINE)
summary(m) #Displays information about the linear regression.
#Residual median should be close to 0
#The quartiles should be symmetrical
#Min/max values should be symmetrical.

df = data.frame(x,y)
#(PASTE IN COMMAND LINE)
head(df)

#Slope of regression line is defined as the covariance of (y, x) divided by the variance in (x)
#cov(x,y)/var(x)

#(PASTE IN COMMAND LINE!!)
#Calculates the predicted y-value one standard-deviations away from the mean.
(cf[2]*(mean(x) + sd(x))) - (cf[2]*mean(x))
  
#(PASTE IN COMMAND LINE)
#Calculation of R^2. cor(x,y) is the pearson correlation coefficient.
cor(x,y)**2 #=0.43. This means that 43% of the variance in y is explained by x. The line is 43% fitted to the data.

y_hat = cf[1] + cf[2]*x
var(y_hat)

#Create dataframe containing x and y
dataFrame = data.frame(x,y)

#Sample random pairs of x and y
#There will be 100 bootstraps.
num_bootstraps = 20
sampled_model <- NULL #non-appendable
slope_estimates <- NULL #Appendable

#20 bootstraps
for (i in 1:num_bootstraps) {
  #Row
  sampled_rows <- dataFrame[sample(nrow(dataFrame), 10, replace=TRUE),]
  #Linear regression
  sampled_model <- lm(y~x, data = sampled_rows)
  #Estimates the slope of the linear regression and appends it to "slope estimates".
  slope_estimates[i] <- coef(sampled_model)[2]
}

standard_error <- sd(slope_estimates)
print(standard_error)
#Store samples in two vectors 

#Plot vectors against each other

#Make linear regression





