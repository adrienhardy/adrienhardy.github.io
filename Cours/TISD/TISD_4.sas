libname malib 'C:\Users\hardy\Desktop\TISD\SAS';run;

proc gchart data=malib.travf;
hbar act6;
run;

proc gchart data=malib.travf;
pie cite97;
run;

proc freq data=malib.travf;
tables act6 cite97;
run;

proc freq data=malib.travf;
tables act6*cite97/chisq;
run;

proc freq data=malib.travf;
tables cohab*act6/chisq;
run;
