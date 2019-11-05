#Building a Generalizable Population Growth Model for R

#A Coding Sample Built by the National Animal Nutrition Program, 

#A National Research Support Project



#Code updated 6/4/2014



#	Step 1: Initalize time values. This example starts at year 3 of a system and

#	run until the system reaches year 20. The previous model version required that

#	the model start running at year 3 due to the time required to build replacement

#	heifers. This model does not require this inconvenience.



Tstart <- 1

Tstop <- 20

Tstep <- 1



#	Step 2: Set the initial population parameters. This example simulates 300 hd. 



Pstart <- 300



#	Step 3: Set up the rules for culling and heifer retention. Here we assume half 

# 	of the offspring every year are female and 20% of those females are kept as 

# 	replacements. Each year 10% of mature cows are culled.



kCull <- 0.11

kFemale <- 0.5

kKeep <- 0.25



# 	Step 4: Set up the variables that will be updated at each time point.



Cows <- 0

t <- Tstart



# 	Step 5: Write an array that will hold the data during the simulation. Because

# 	this array will hold our data, we will call it "Output". Before the simulation

# 	this array will use a placeholder value of 1 for every cell. 



Output <- array(1, dim=c(1,20))





# 	Step 6: Use a conditional while loop to run the simulation. The loop syntax 

#	is as follows while t is less than Tstop, compute the equations within the 

#	brackets. If t>2, compute the number of cows assuming heifers from two years ago

#	are now calving and cows from last year are culled. Then record the new number

#	of cows in the Output array. Update the time and record it in the Output array.





while(t<=Tstop) {

if(t<=2) Cows <- Pstart else Cows <- Output[1,t-1] + Output[1,t-2]*kFemale*kKeep - Output[1,t-1]*kCull

Output[1,t] <- Cows

t <- t+1

}



#	Step 7: Print the Output array



print(Output)



#	Step 8: Graph the output over time

plot(Output[1,], pch=16, col="black", ylab="Population", xlab="Time")
