/* INTRODUCTION A SAS

1 - Acquisition de données - Libraries

1.1 - Saisie de données */


data tp;
input numero taille poids sexe $ sexecode;
cards;
11 174 65 m 1
2 169 56 f 2
3 166 48 f 2
4 181 80 m 1
5 168 53 f 2
6 176 76 m 1
7 190 77 m 1
8 159 70 f 2
9 162 60 f 2
10 164 51 f 2
11 160 73 f 2
run;
 

proc print data=tp label;
label taille="taille de l'élève" poids="poids de l'élève";
run;  

proc format;
value sexecodage 1='masculin' 2='feminin';
run;

proc print data=tp;
format sexecode sexecodage. ;
run;

proc format;
value $ bsexecodage m='masculin' f='féminin';
run;

proc print data=tp;
format sexe $bsexecodage.;
run;

libname malib 'C:\Users\hardy\Desktop\TISD\SAS'; run ;

data malib.tp;
input numero taille poids sexe $ sexecode;
cards;
11 174 65 m 1
2 169 56 f 2
3 166 48 f 2
4 181 80 m 1
5 168 53 f 2
6 176 76 m 1
7 190 77 m 1
8 159 70 f 2
9 162 60 f 2
10 164 51 f 2
11 160 73 f 2
run;



/* 1.2 - Importation de données p*/

proc import out= ozone
datafile= "C:\Users\hardy\Desktop\TISD\SAS\ozone.xls"
DBMS=EXCEL REPLACE;
SHEET="tp1$";
GETNAMES=YES; 
MIXED=YES;
SCANTEXT=YES; 
USEDATE=YES;
SCANTIME=YES;
run;

proc print data=malib.ozone; run;


/* 2 - Manipulation des données */

data malib.nombres;
input x y;
cards;
5 5
2 -3
4.5 10
3.2 1
2 0
run;
 

data malib.calcul;
set malib.nombres;
a=x+y; b=x-y; c=x*y; d=x**y; e=min(x,y); f=max(x,y);
g=x/y; h=abs(y); i=exp(x); j=int(x); k=log(y); 
l=log10(x); m=sign(y); n=sqrt(x);
run;


data malib.compt;
do i=1 to 100 by 1;
x=rand('binomial',0.4,20);
y=1+x;
z=x;
x=x-1;
output malib.compt;
end;
run;



data malib.marchealeat;
retain x y (0 0);
do i=1 to 10 by 1;
y=y+x;
x=rand('uniform');
output malib.marchealeat;
end;
run;


proc sort data=malib.tp;
by taille;
run;


proc sort data=malib.tp;
by sexe; run;

proc print data=malib.tp;
by sexe;
run;

data malib.tp2;
set malib.tp;
keep numero taille poids sexecode;
run;


data malib.tp3;
set malib.tp;
drop taille poids;
run;


proc sort data=malib.tp2;
by numero;
run;
proc sort data=malib.tp3;
by numero;
run;

data malib.tp4;
merge malib.tp2 malib.tp3;
by numero;
run;

data malib.tab1;
input numero sexecode;
cards;
12 1
13 1
14 2
run;

data malib.tp5;
set malib.tp malib.tab1;
run;

data malib.tp6;
set malib.tp;
where sexecode=2;
run;

data malib.tp6;
set malib.tp;
if sexe='m' then delete;
run;

data malib.garcons malib.filles;
set malib.tp;
if sexecode=1 then output malib.garcons;
if sexecode=2 then output malib.filles;
run;

data malib.garcons2 malib.filles2;
set malib.tp;
select(sexecode);
when(1) output malib.garcons2;
otherwise output malib.filles2;
end;
run;



/* 3 - Etude d'une variable quantitative */


data malib.ozone;
set malib.ozone;
t=_n_;
run;

proc gplot data=malib.ozone;
plot maxO3*t;
symbol i=join;
run;

proc gchart data=malib.ozone;
vbar maxO3;
run;


proc means data=malib.ozone;
var maxO3;
run;

proc univariate data=malib.ozone plot;
var maxO3;
histogram maxO3 / lognormal (color=red)
weibull   (color=blue)
gamma     (color=yellow);
qqplot maxO3 / gamma(alpha=est sigma=est theta=est);
symbol v=circle;
run;

proc univariate data=malib.ozone plot;
var T12;
histogram T12 / lognormal (color=red)
weibull   (color=blue)
gamma     (color=yellow);
qqplot maxO3 / gamma(alpha=est sigma=est theta=est);
symbol v=circle;
run;

proc gplot data=malib.ozone;
plot maxO3*T12;
symbol i=RL v=circle;
run;

proc corr data=malib.ozone;
var maxO3 T12;
run;
