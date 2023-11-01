#Optional exercise

#Generate data using rnorm
x = rnorm(n=100, mean=10, sd=2) #100 values, average=10, standard-deviation=2

#Create the output-vectors that will be filled when boostrapping.
sd_out = NULL #Create output vector for standard deviations
vc_out = NULL #Create output vector for variance coefficient.

#Bootstrap the data 10000 times, place average in sd-vector and CV-vector
for(i in 1:10000) {
  sample = sample(x, replace=TRUE)
  sd_out[i] = sd(log(sample)) #The standard deviation of log-sample
  vc_out[i] = sd(sample)/mean(sample) #The VC of untreated sample.
}

#Plot SD(log(x)) against CV(x)
plot(vc_out, sd_out, las=1, lty=1, pch=3, xlab = "log(std(x))", ylab = "vc_x", main="scatter plot. Red line = Linear regression.")
linear_regression_model = lm(sd_out~vc_out)
abline(linear_regression_model, col = "red")
