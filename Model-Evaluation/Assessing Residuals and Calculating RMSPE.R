#Calculate and Plot the RMSPE 

#11/6/2014



#Create 100 random datapoints where X is uniformly distributed from 0 to 10 and Y is equivalent to 2*X plus

#some residual variation. The residual variation is normally distributed with mean 0 and sd 2

X <- runif(n=100, min=0,max=10)

Res <- rnorm(n=100,mean=0,sd=2)

Y=2*X+Res



#Fit a linear model to this data

data <- data.frame(Y,X)

m1 <- lm(Y~X, data=data)



#Summarize this model

summary(m1)



#Develop a function to calculate the room mean squared error of prediction and the percentages of mean

#squared error that partition out into mean bias, slope bias and residual bias.

RMSE <- function(o,p) {

res=o-p;

res2=res^2;

rm=sqrt(mean(res2));

uss=sum(res2);

n=length(o);

meanO=mean(o);

mb=sum(res)/n;

sse <- anova(lm(res~p))[2,2];

msb <- mb^2;

mspe <- rm^2;

msre <- sse/n;

msslope <- mspe-msre-msb;

mean <- msb/mspe;

slope <- msslope/mspe;

residual <- msre/mspe;

output <- c(rm, mean, slope, residual);

return(output)

}



#Calculate the root mean squared prediction error

RMSE(data$Y, fitted(m1))



#Calculate RMSPE as a fraction of the mean observed value

rmse <- RMSE(data$Y, fitted(m1))

rmse[1]/mean(data$Y)



#Plot the residuals against the predicted values and the predicted versus observed values. This requires

#the ggplot2 package, please ensure this is installed.

library(ggplot2)

res <- data$Y-fitted(m1)

Data <- c(res,data$Y)

Type <- rep(c("Residuals", "Observed"), each=length(res))

Predicted <- rep(fitted(m1), 2)

plotdata <- data.frame(Type, Data, Predicted)

ylab <- expression(paste("Observed Values or Observed - Predicted Values"))

p1 <- ggplot(plotdata, aes(y=Data, x=Predicted, color=Type, shape=Type))+scale_color_manual(values=c("black", "darkgrey"))+geom_point()+geom_smooth(method=lm, se=FALSE)+theme(axis.title=element_text(family= "serif", colour="black", size=10),axis.text=element_text(family= "serif", colour="black", size=10),legend.title=element_text(family="serif"),legend.text=element_text(family="serif"))+ylab(ylab)

p1
