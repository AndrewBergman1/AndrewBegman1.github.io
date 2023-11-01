op <- par(mfrow=c(1,2))

#Creates vector x containing 100 values defined mean and sd
x = rnorm(n=100, mean=5, sd=1) #I guess mean and sd are distributed around the given values.
print(mean(x))
print(sd(x))

#Generates a histogram
#las = 1 --> x-axis labels are written horisontally.
hist(x, las=1, main="Histogram Generation Example")

set.seed(1)
x = rnorm(50, 10, 2) #n=50, mean=10, sd=2
se_x = sqrt(var(x)/length(x)) #Theoretical standard-error of vector x.


#BOOTSTRAPPING
out = NULL #Create vector 'out' with no elements.
for(i in 1:1000) { #1000 samples are picked.
  sample = sample(x, replace=TRUE) #a random sample (n=45) is generated from vector 'X'. Replace=TRUE means that the datapoint is placed in the datapool before the next pick.
  print(sample)
  out[i] = mean(sample) #The mean of the sample is placed in vector 'out'.
}

hist(out, las=1, main="Mean Of X")

print(quantile(out, c(0.025, 0.975))) #95% confidence interval (of x(?))
print(mean(x) - 1.96*se_x) #Theoretical 2.5% 
print(mean(x) + 1.96*se_x) #Theoretical 97.5%