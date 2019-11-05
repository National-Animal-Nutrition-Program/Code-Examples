/*This Code Runs an Analysis of variance on data called "filerdata" which has two variables, "filter" and "flowrate"*/



/*Read the data into SAS*/

data filterdata;

  input filter $ flowrate;

  cards;

A 8.41

A 8.19

A 8.59

A 7.97

B 7.43

B 6.97

B 7.49

B 7.03

C 7.69

C 7.22

C 7.41

C 7.84

D 7.85

D 8.31

D 8.49

D 7.75

E 8.77

E 8.82

E 8.41

E 8.36

F 6.87

F 7.25

F 7.41

F 6.87

;

run;



/*Calculate the mean for each filter type*/

proc means;

  class filter;

  var flowrate;

run;



/*Run an analysis of variance on the data using the general linear model flowrate=filter*/

proc glm;

  class filter;

  model flowrate=filter;

  /*Conduct LSD Analysis (All pairwise tests and confidence intervals with no adjustment)*/

  lsmeans filter / alpha=0.05 pdiff cl;              

  /*All pairwise tests and confidence intervals using Tukey's method*/

  lsmeans filter / alpha=0.05 adjust=tukey pdiff cl; 

 /*All pairwise tests and confidence intervals using Tukey's method and test for equal varaince*/

   means filter / hovtest=bf alpha=0.05 tukey cldiff; 

  /*Compare all treatments to the control treatment labeled F*/

  means filter / alpha=0.05 dunnett('F');

  output out=new p=predict r=resid;

run;



/*plot your residual vs predicted*/

proc gplot data=new;

plot resid*predict;

run;



/*getting histograms and normal probability plot*/

proc capability data=new;

var resid;

histogram;

qqplot;

run;
