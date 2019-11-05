#Building a General Form Simulation Interface for R

#A Coding Example provided by the National Animal Nutrition Program

#A National Research Support Project



#Code updated 8/1/2014



#This code iterates through several runs of a dynamic, mechanistic model to based on

#a series of input values provided in a text file.



#	Step 1: Initalize time values. Tstart is the initial timepoint for the model

#	Tstop indicates the end of the simulation and Tstep reflects the iteration step.



Tstart <- 1

Tstop <- 60

Tstep <- 1



# 	Step 2: Set up the variables that will be updated at each time point.



A <- 0.3

B <- 0.2

C <- 0.2

t <- Tstart



#	Step 3: Read the values of the input parameters from the text file



parms <- read.table("parms.txt")



#	Step 4: Set up the variabales that will govern the rate functions in the system



VAB <- parms$VmaxAB

KAB <- parms$KmAB

VBC <- parms$VmaxBC

KBC <- parms$KmBC

VC <- parms$VmaxC

KC <- parms$KmC

VCB <- parms$VmaxCB

KCB <- parms$KmCB

inA <- parms$iA

inB <- parms$iB

inC <- parms$iC

RA <- parms$ViA



#	Step 5: Tell the model how many times to simulate based on the number of input rows



nruns <- length(VAB)

n <- 1



# 	Step 5: Write an array that will hold the data during the simulation. Because

# 	this array will hold our data, we will call it "Output". Before the simulation

# 	this array will use a placeholder value of 1 for every cell. If tstep is not equal

#	to one, change the array length to equal the number of iteration steps.



val <- array(1, dim=c(4,Tstop))

out <- array(1, dim=c(nruns,Tstop))



# 	Step 6: Use a nested conditional while loop to run the simulation. The outer loop

#	reads the input values from the vectors defined in Step 4. The inner loop then

#	runs the model for each time point, initalizing at t=tstart, running the specified

#	dynamics from tstart+1 to tstop, outputting the pool sizes to the output array (in 

#	this case, pool C was chosen as the output to compare), and updating time. When the

#	inner loop concludes, the outer loop re-sets time to tstart and moves to the next

#	row of the input array. 



while(n<=nruns) {

iA <- inA[n]

iB <- inB[n]

iC <- inC[n]

VmaxAB <- VAB[n]

VmaxBC <- VBC[n]

VmaxCB <- VCB[n]

VmaxC <- VC[n]

KmAB <- KAB[n]

KmBC <- KBC[n]

KmCB <- KCB[n]

KmC <- KC[n]

kA <- RA[n]



while(t<=Tstop) {

if(t<=1) A <- iA else A <- val[1,t-1] 

if(t<=1) B <- iB else B <- val[2,t-1] 

if(t<=1) C <- iC else C <- val[3,t-1] 

kAB <- A*VmaxAB/(KmAB+A)

kBC <- B*VmaxBC/(KmBC+B)

kCB <- C*VmaxCB/(KmCB+C)

kC <- C*VmaxC/(KmC+C)

A <- A+kA-kAB

B <- B+kAB-kBC+kCB

C <- C+kBC-kC-kCB

val[1,t]<-A 

val[2,t]<-B 

val[3,t]<-C 

out[n,t] <- C

t <- t+1

}

t=Tstart

n <- n+1

}



#	Step 7: Print the Output array



print(out)



#	Step 8: Graph the outputs



plot(out[1,], pch=16, col="red", ylim=c(0,2), ylab="Pool Sizes", xlab="Time")

points(out[2,], pch=15, col="green")

points(out[3,], pch=17, col="blue")

points(out[4,], pch=18, col="gray")

lines(out[1,], col="red")

lines(out[2,], col="green")

lines(out[3,], col="blue")

lines(out[4,], col="gray")

legend(0.5,2, legend=c("Run 1", "Run 2", "Run 3", "Run 4"), col=c("green","blue","red", "gray"), pch=c(15,16,17,18))
