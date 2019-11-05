#Building a General Form Simulation Interface for R

#A coding example provided by the National Animal Nutrition Program

#A National Research Support Project



#Code updated 8/1/2014



#This code builds a simple simulation interface for a dynamic, mechanistic model

#The example scenario used in this case simulates three pools (A, B and C) with

#unidirectional flows between pools governed by mass-action kinetics as dependent

#on flow rates (k) into and out of pools. 



#	Step 1: Initalize time values. Tstart is the initial timepoint for the model

#	Tstop indicates the end of the simulation and Tstep reflects the iteration step.



Tstart <- 1

Tstop <- 20

Tstep <- 1



#	Step 2: Set the initial pool values. 



iA <- 10

iB <- 15 

iC <- 10



#	Step 3: Set up the rates for synthesis, transformation and degredation of each pool



kAsyn <- 1

kAdB <- 0.5

kAout <- 0.01

kBdC <- 0.1

kBout <- 0.05

kCout <- 0.05



# 	Step 4: Set up the variables that will be updated at each time point. These 

#	variables should be declared prior to their use within the loop. 



A <- 0

B <- 0

C <- 0

t <- Tstart



# 	Step 5: Write an array that will hold the data during the simulation. Because

# 	this array will hold our data, we will call it "Output". Before the simulation

# 	this array will use a placeholder value of 1 for every cell. The dimenstions 

#	of the array are defined to hold 4 rows and columns equal to the model runtime.

#	Note: if tstep is not 1, the user should consider adjusting the dimensions to

#	the number of calculation points (Tstop-Tstart/Tstep), rather than just using Tstop.



out <- array(1, dim=c(4,Tstop))



# 	Step 6: Use a conditional while loop to run the simulation. The loop syntax 

#	is states that while t is less than Tstop, compute the equations within the 

#	brackets. When Time is equat to Tstart the model initalizes, thereafter, the 

#	model runs based on the defined dynamics. The pool sizes for A, B and C are 

#	calculated based on their previous value and the flow rate constants defined 

#	in step 3. The output array is updated to hold the current value of each pool

#	and the total of all pools. Finally, time is updated by the timestep value. 





while(t<=Tstop) {

if(t<=Tstart) A <- iA else A <- out[1,t-1] + kAsyn - out[1,t-1]*kAout - out[1,t-1]*kAdB

if(t<=Tstart) B <- iB else B <- out[2,t-1] + out[1,t-1]*kAdB - out[2,t-1]*kBout - out[2,t-1]*kBdC

if(t<=Tstart) C <- iC else C <- out[3,t-1] + out[2,t-1]*kBdC - out[3,t-1]*kCout

out[1,t] <- A

out[2,t] <- B

out[3,t] <- C

out[4,t] <- A+B+C

t <- t+Tstep

}



#	Step 7: Print the Output array



print(out)



#	Step 8: Graph the output of pools A, B and C over time. This plot is established

#	by plotting the timeseries data from row 3 of the output table. The lines for

#	rows 1 and 2 are then added to the plot using different symbols and colors. 

#	Lines are added for each pool and a legend is defined and placed to label the lines.



plot(out[3,], pch=16, col="red", ylim=c(0,20), ylab="Pool Sizes", xlab="Time")

points(out[1,], pch=15, col="green")

points(out[2,], pch=17, col="blue")

lines(out[3,], col="red")

lines(out[1,], col="green")

lines(out[2,], col="blue")

legend(5,20, legend=c("A","B","C"), col=c("green","blue","red"), pch=c(15,16,17))
