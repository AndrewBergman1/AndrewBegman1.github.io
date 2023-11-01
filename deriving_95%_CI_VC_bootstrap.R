plot_par <- par(mfrow=c(1,1))

#Randomize 100 datapoints with mean=5, sd =1
set.seed(1)
x = rnorm(n=100, mean=5, sd=1)
se_x = sqrt(var(x)/length(x))
#calculate the coefficient of variation 
#CV = sd/mean.

#BOOTSTRAPPING
vc_bootstrap = NULL #creates vector out with no elements.
for(i in 1:10000) {
  sample = sample(x, replace=TRUE) #Sample the vector x (n=45), put back each pick.
  vc_bootstrap[i] = sd(sample)/mean(sample) #Vector out contains the variance coefficients of each sample (there's 10000 iterations).
}

hist(vc_bootstrap, las=1, main="VC of X")


#The theoretical VC mean and the boostrap-calculated VC mean differ from each other.

#95% CI calculated using bootstrap method.
print(quantile(vc_bootstrap, c(0.025, 0.975)))

#95% CI calculated theoretically.
print(mean(vc_bootstrap) - 1.96*se_x)
print(mean(vc_bootstrap) + 1.96*se_x)

