#This code demonstrates how to create a plot with unique slopes for each study in a meta analysis

#11/6/2014



#Step 1 - generate some data

Citation <- c(rep("Jones et al., 2012", 4), rep("Johnson et al., 2010",6), rep("Smith, 1999",8), rep("McDonald et al., 2010",4), rep("Thompson et al., 2008",4))

X <- rep(c(1,6,1,3,6,2,4,6,8,2,8,1,10), each=2)

var <- c(rnorm(4,mean=0,sd=3),rnorm(6,mean=1,sd=2), rnorm(8, mean=-1, sd=2.5), rnorm(4, mean=-2, sd=5), rnorm(4, mean=2, sd=0.5))

Yeq <- 2*X+var

slopes <- runif(5, -1, 3)

slope <- c(rep(slopes[1],4), rep(slopes[2],6), rep(slopes[3],8), rep(slopes[4],4), rep(slopes[5],4))

Yueq <- slope*X+var

EVdat <- data.frame(Yeq, X, Citation)

UEVdat <- data.frame(Yueq, X, Citation)





#Step 2 - graph the data. This requires the ggplot2 package, please ensure you have this package installed

library(ggplot2)

greyscales <- c("#999999", "#CCCCCC", "#333333", "#000000", "#333000")

Ylab <-  expression(paste("Observed Outputted Values"))

Xlab <- expression(paste("Input Level"))

p1 <- ggplot(EVdat, aes(x=X, y=Yeq, color=Citation, shape=Citation))+scale_color_manual(values=greyscales)+geom_point()+geom_smooth(method=lm, se=FALSE)+xlab(Xlab)+ylab(Ylab)+theme(axis.title=element_text(family= "serif", colour="black", size=10),axis.text=element_text(family= "serif", colour="black", size=10),legend.title=element_text(family="serif"),legend.text=element_text(family="serif"))

p1



#Step 3 - make the same graph but with the unequal slope dataset

p2 <- ggplot(UEVdat, aes(x=X, y=Yueq, color=Citation, shape=Citation))+scale_color_manual(values=greyscales)+geom_point()+geom_smooth(method=lm, se=FALSE)+xlab(Xlab)+ylab(Ylab)+theme(axis.title=element_text(family= "serif", colour="black", size=10),axis.text=element_text(family= "serif", colour="black", size=10),legend.title=element_text(family="serif"),legend.text=element_text(family="serif"))

p2



#Step 4 - compare the graphs with equal between-study slope and unequal slopes. This requires the gridExtra package

library(gridExtra)

grid.arrange(p1,p2)
