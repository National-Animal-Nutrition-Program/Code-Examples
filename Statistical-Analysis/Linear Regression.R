#Conducting a basic linear regression in R

#A code segment brought to you by the National Aninmal Nutrition Program

#A National Research Support Project



#Code updated: 8/1/2014



#This code can be used to import data from a text file and fit a basic

#linear regression



#Step 1: Identify the active directory on your computer

get(wd)



#Step 2: Place a text file containing your data to regress into that file folder

#The text file should be named 'data.txt' and should have three columns, the first

#column should have no heading and number the data rows sequentially (the top left

#cell should be empty). The column containing your response data should be labeled

# "Y" and the column with your explanatory data should be labeled "X". Name the data

# file (d) and Tell R to read and import the data from this file. 



d <- read.table("data.txt")



#Step 3: Run a basic linear regression and retain the results of that regression

#as an object entitled m1.

X <- d$X

Y <- d$Y



m1 <- lm(Y~X)



#Step 4: Print a summary of m1. THis summary contains the values for coefficients

# fit in the linear regression, their standard errors, T values, and p values. The

# summary report also generates the residual standard error on X degrees of freedom

# as well as calculating the multiple R-squared, adjusted R-squared, F-statistic and 

# overall model p value. 



summary(m1)



#Step 5: Extract the coefficients from the model into a matrix called "c" and view them



c <- coefficients(m1)

c



#Step 6: Calculate the predicted y values by referencing the X and coefficient values



yhat <- c[1]+c[2]*X



#Step 7: Create a matrix binding X, Y and Yhat

out <- cbind(yhat, X, Y)





#Step 8: Plot the predicted verses measured y values. Title the plot "Predicted versus

# Observed" and label the axes appropriately. 



plot(yhat, Y, main="Predicted versus Observed", xlab="Observed Values", ylab="Predicted Values")



#Step 9: Add a best fit line to the graph and output a summary of that line



abline(lm(yhat~Y), col="red")

summary(lm(yhat~Y))





#Step 10: Create a scatter plot comparing Yhat and Y over the X values and add a legend



plot(Y, X, xlab="Explanatory Variable", ylab="Response Value", main="Comparison of Y and Yhat", col="blue")

points(yhat,X,col="red", pch=16)

legend(5,10, legend=c("Y","yhat"), col = c("blue", "red"), pch=16)
