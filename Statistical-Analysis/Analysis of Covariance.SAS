/* This code compares three ways to run an analysis of covariance in SAS */





title3 "With Covariate the Age of the Individual";

data ancova;

input age oxygen treatment $;

cards;

31  17.05 aerobic

23   4.96 aerobic

27  10.40 aerobic

28  11.05 aerobic

22   0.26 aerobic

24   2.51 aerobic

23  -0.87 running

22 -10.74 running

22  -3.27 running

25  -1.97 running

27   7.50 running

20  -7.25 running

;

proc print data = ancova;

run;

title4 "Model With an Interation";

proc glm data = ancova;

class treatment;

model oxygen = treatment age treatment*age / solution;

output out=new p=pred r=resid;

means treatment/hovtest=bf;

*beware don't interpret it here if when we have an interaction just look at Brown Forsythe test;

run;

*do residual analysis as usual with resid and pred;

proc capability data=new;

var resid;

qqplot;

histogram;

run;

proc gplot data=new;

plot resid*pred;

run;



title4 "Model Without an Interation";

proc glm data = ancova;

class treatment;

model oxygen = treatment age / solution;

lsmeans treatment / pdiff stderr ;

output out=new1 p=pred r=resid;

means treatment/hovtest=bf;

*beware don't interpret it here if when we have an interaction just look at Brown Forsythe test;

run;

*do residual analysis as usual with resid and pred;

proc capability data=new1;

var resid;

qqplot;

histogram;

run;

proc gplot data=new1;

plot resid*pred;

run;

run;



*Using regression with dummy variables use SS III ;

title4 "Using Regression with dummy variables";

data dummy;

set ancova;

if treatment="aerobic" then trt_dummy=1;

else trt_dummy=0;

int=age*trt_dummy;

run;

proc reg data=dummy;

model oxygen= age trt_dummy int;

output out=new2 p=pred r=resid;

run;

*do residual analysis as usual with resid and pred;

proc capability data=new2;

var resid;

qqplot;

histogram;

run;

proc gplot data=new2;

plot resid*pred;

run;

run;

proc glm data=dummy;

model oxygen= age trt_dummy trt_dummy*age;

output out=new3 p=pred r=resid;

means treatment/hovtest=bf;

*beware don't interpret it here if when we have an interaction just look at Brown Forsythe test;

run;

*do residual analysis as usual with resid and pred;

proc capability data=new3;

var resid;

qqplot;

histogram;

run;

proc gplot data=new3;

plot resid*pred;

run;
