#Reads a comma-separated excel-file containing the bird data into a dataframe.
birds = read.csv("C:/Users/Andrew/Downloads/bird_allometry.csv")

#Find the x-values (brain_mass) (birds[3])
#Find the y-values (body_mass) (birds[4])
#Generate a scatter plot of x and y
bird_brain = log(birds[,3]) #The ',' is required to get the 3rd column
bird_mass = log(birds[,4]) #The ',' is required to get the 4th column

plot(bird_mass, bird_brain,
     xlab = "log body mass",
     ylab = "log brain mass")

#Create a data frame from bird mass and brain size.
data_frame = data.frame(bird_brain, bird_mass)

#summary(birds) is used to find the biggest mass value.
summary(birds)
#We find the genus-name by looking in the column called "body_mass".
value_of_interest <- birds[birds$body_mass == 99000, "Genus_Species"]
print(value_of_interest) #Struthio_camelus

#Generate a linear regression on the scatter plot.
linear_regression = lm(bird_brain~bird_mass)

#Attribute the linear regression's coefficients to a vector
lrc = linear_regression$coef

#Creates a regression line that fits the data points and don't overextend.
fitted_regression = lrc[1] + lrc[2]*bird_mass

lines(bird_mass, fitted_regression, col = "red")

lines(bird_mass, expected_regression, col = "green")

r2 = cov(bird_brain,bird_mass)/var(bird_mass) #0.56

