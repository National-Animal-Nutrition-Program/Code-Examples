/* A series of SAS programs using Proc Mixed */



/*program to do two way ANOVA with interaction, both random and do diagnostics and interaction plots*/

data lab_assay;

input a 	b	unit	y;

cards;

1	1	1	10.9

1	1	2	10.9

1	2	1	10.5

1	2	2	9.8

1	3	1	9.7

1	3	2	10.0

2	1	1	11.3

2	1	2	11.7

2	2	1	9.4

2	2	2	10.2

2	3	1	8.8

2	3	2	9.2

3	1	1	12.8

3	1	2	12.2

3	2	1	12.7

3	2	2	12.6

3	3	1	12.7

3	3	2	12.3

;

/*using mixed easier uses MLE*/

proc mixed data=lab_assay method=type3;

class a b;

model y= /outp=alc2 residuals ;

random a b a*b;

run;

/* for interaction plots we need to work a bit more*/

proc means data=lab_assay;

by a b;

var y;

output out=outm mean=ym;

run;

symbol1 color="red" v="star" interpol=join;

symbol2 color="blue" v="star" interpol=join;

symbol3 color="green" v="star" interpol=join;

proc gplot data=outm;			/* Interaction Plot */

   plot ym*a=b;			/* cell mean v. a by b */

   plot ym*b=a;			/* cell mean v. b by a */

   run;

goptions reset;

symbol1 color="red" v="star" interpol=none;

symbol2 color="black" v="circle" interpol=join;



proc gplot data=alc2;			/* Diagnostic Plots */

   plot y*pred pred*pred/ overlay;	/* observed v. predicted */	

   plot resid*pred;				/* residual v. predicted */ 

run;

proc capability data=alc2;

var resid;

qqplot;

run;





/*program to do two way ANOVA with interaction,one fixed one random and do diagnostics and interaction plots using PROC MIXED*/

data airtraffic;

input a 	b	unit	y;

cards;

1	1	1	18

1	1	2	16

1	2	1	31

1	2	2	35

1	3	1	22

1	3	2	27

1	4	1	39

1	4	2	36

1	5	1	15

1	5	2	12

2	1	1	13

2	1	2	15

2	2	1	33

2	2	2	30

2	3	1	24

2	3	2	21

2	4	1	35

2	4	2	38

2	5	1	10

2	5	2	16

3	1	1	24

3	1	2	28

3	2	1	42

3	2	2	46

3	3	1	40

3	3	2	37

3	4	1	52

3	4	2	57

3	5	1	28

3	5	2	24

;

/*using mixed easier uses MLE*/

proc mixed data=airtraffic method=type3;

class a b;

model y= a /outp=alc2 residuals ;

random  b a*b;

lsmeans a /adjust=tukey;

run;

/* for interaction plots we need to work a bit more*/

proc means data=airtraffic;

by a b;

var y;

output out=outm mean=ym;

run;

symbol1 color="red" v="star" interpol=join;

symbol2 color="blue" v="star" interpol=join;

symbol3 color="green" v="star" interpol=join;

symbol4 color="black" v="star" interpol=join;

symbol5 color="cyan" v="star" interpol=join;



proc gplot data=outm;			/* Interaction Plot */

   plot ym*a=b;			/* cell mean v. a by b */

   plot ym*b=a;			/* cell mean v. b by a */

   run;

   goptions reset;

symbol1 color="red" v="star" interpol=none;

symbol2 color="black" v="circle" interpol=join;

proc gplot data=alc2;			/* Diagnostic Plots */

   plot y*pred pred*pred / overlay;	/* observed v. predicted */	

   plot resid*pred;				/* residual v. predicted */ 

run;

proc capability data=alc2;

var resid;

qqplot;

run;
