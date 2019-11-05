/* A series of SAS Programs using GLM */









/*program to do two way ANOVA with interaction, both fixed and do diagnostics and interaction plots*/



data child_recall;

input a $	b	unit	y;

cards;

none	20	1	20

none	20	2	23

none	20	3	19

none	20	4	18

none	20	5	28

none	20	6	25

none	40	1	30

none	40	2	36

none	40	3	25

none	40	4	28

none	40	5	27

none	40	6	24

none	60	1	6

none	60	2	10

none	60	3	11

none	60	4	14

none	60	5	17

none	60	6	19

verbal	20	1	15

verbal	20	2	16

verbal	20	3	24

verbal	20	4	22

verbal	20	5	25

verbal	20	6	21

verbal	40	1	24

verbal	40	2	26

verbal	40	3	29

verbal	40	4	27

verbal	40	5	23

verbal	40	6	21

verbal	60	1	31

verbal	60	2	38

verbal	60	3	29

verbal	60	4	34

verbal	60	5	35

verbal	60	6	30

;

/* running the general linear model to look for main effects and interaction and save residuals*/

proc glm data=child_recall;

class a b;

model y=a b a*b;

lsmeans a*b /adjust=tukey out=out_ls;

output out=out p=py r=ry;

run;

symbol1 color="red" v="star" interpol=join;

symbol2 color="blue" v="star" interpol=join;

symbol3 color="green" v="star" interpol=join;

proc gplot data=out_ls;			/* Interaction Plot */

   plot lsmean*a=b;			/* cell mean v. a by b */

   plot lsmean*b=a;			/* cell mean v. b by a */

   run;

goptions reset;

symbol1 color="red" v="star" interpol=none;

symbol2 color="black" v="circle" interpol=join;



proc gplot data=out;			/* Diagnostic Plots */

   plot y*py py*py / overlay;	/* observed v. predicted */	

   plot ry*py;				/* residual v. predicted */ 

run;

proc capability data=out;

var ry;

qqplot;

run; 









/*Program to do a two way analysis of variance with interation, both random and do diagonostics and interaction plots */



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

proc glm data=lab_assay;

class a b;

model y=a b a*b;

random a b a*b;

lsmeans a*b / out=out_ls;

output out=out p=py r=ry;

run;

symbol1 color="red" v="star" interpol=join;

symbol2 color="blue" v="star" interpol=join;

symbol3 color="green" v="star" interpol=join;

proc gplot data=out_ls;			/* Interaction Plot */

   plot lsmean*a=b;			/* cell mean v. a by b */

   plot lsmean*b=a;			/* cell mean v. b by a */

   run;

goptions reset;

symbol1 color="red" v="star" interpol=none;

symbol2 color="black" v="circle" interpol=join;

proc plot data=out;			/* Diagnostic Plots */

   plot y*py py*py / overlay;	/* observed v. predicted */	

   plot ry*py;				/* residual v. predicted */ 

run;

proc capability data=out;

var ry;

qqplot;

run;











/*program to do two way ANOVA with interaction, one fixed one random and do diagnostics and interaction plots*/



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

/*using glm which is OLS based*/

proc glm data=airtraffic;

class a b;

model y=a b a*b;

random  b a*b/test;

lsmeans a / adjust=tukey e=a*b;

lsmeans a*b / out=out_ls;

output out=out p=py r=ry;

run;

symbol1 color="red" v="star" interpol=join;

symbol2 color="blue" v="star" interpol=join;

symbol3 color="green" v="star" interpol=join;

symbol4 color="black" v="star" interpol=join;

symbol5 color="cyan" v="star" interpol=join;

proc gplot data=out_ls;			/* Interaction Plot */

   plot lsmean*a=b;			/* cell mean v. a by b */

   plot lsmean*b=a;			/* cell mean v. b by a */

   run;

goptions reset;

symbol1 color="red" v="star" interpol=none;

symbol2 color="black" v="circle" interpol=join;

proc gplot data=out;			/* Diagnostic Plots */

   plot y*py py*py / overlay;	/* observed v. predicted */	

   plot ry*py;				/* residual v. predicted */ 

run;

proc capability data=out;

var ry;

qqplot;

run;









/*program to do a crossed and nested factor analysis */



title1 "crossed and nested factor analysis";

data tablet;

input y site	batch;

cards;

5.03	1	1

5.10	1	1

5.25	1	1

4.98	1	1

5.05	1	1

4.64	1	2

4.73	1	2

4.95	1	2

4.82	1	2

5.06	1	2

5.10	1	3

5.15	1	3

5.20	1	3

5.08	1	3

5.14	1	3

5.05	2	1

4.96	2	1

5.12	2	1

5.12	2	1

5.05	2	1

5.46	2	2

5.15	2	2

5.18	2	2

5.18	2	2

5.11	2	2

4.90	2	3

4.95	2	3

4.86	2	3

4.86	2	3

5.07	2	3

;

proc glm data=tablet;

class site batch;

model y=site batch(site);

random batch(site)/test;

run;
