/* ~~Code for Uploading Animal Performance Data Downloaded from Excel into SAS~~ */



proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s1 dbms="xlsx" replace;

	sheet="Studies";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s2 dbms="xlsx" replace;

	sheet="Treatments";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s3 dbms="xlsx" replace;

	sheet="IngrComp";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s4 dbms="xlsx" replace;

	sheet="Intake";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s5 dbms="xlsx" replace;

	sheet="Digesta";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s6 dbms="xlsx" replace;

	sheet="Milk";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s7 dbms="xlsx" replace;

	sheet="MilkComp";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s8 dbms="xlsx" replace;

	sheet="BW";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s9 dbms="xlsx" replace;

	sheet="BodyComp";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s10 dbms="xlsx" replace;

	sheet="Gain";

run;

proc import datafile="\\Client\C$\Users\rrwhite\Desktop\NRC 2001 Dairy Data - for upload.xlsx" out=s11 dbms="xlsx" replace;

	sheet="Diet";

run;

proc sort data=s11 out=s11sort;

	by PubID TrtID;

run;

proc transpose data=s11sort out=s11new ;

	by PubID TrtID;

run;

data full;

	merge s2 s3 s4 s5 s6 s7 s8 s9 s10;

	by TrtID;
