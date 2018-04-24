
/************************************************************************/

/* Macro : Retraiter les valeurs extrêmes de variables continues */

/************************************************************************/

/* pinf : percentile en-dessous duquel on retraite

psup : percentile au-dessus duquel on retraite 

style : T pour troncature

W pour winsorisation

*/

%macro corrige(table_in,table_out,liste_var,style,pinf=1,psup=99);

%if &style ne W and &style ne T %then %do;

%put ERREUR : VOUS DEVEZ DEFINIR STYLE=T POUR TRONCATURE ;

%put OU STYLE=W POUR WINSORISATION ;

%end;

%else %do ;

%local nbvar;

%let nbvar=%nbmots(&liste_var); 

%do i=1 %to &nbvar; %local var&i; 

%let var&i=%scan(&liste_var,&i); 

%end;

proc univariate data=&table_in noprint;
var &liste_var ;
output out=inter_macro_corrige 
pctlpre= %do i=1 %to &nbvar; pi_&i.a %end;
pctlpts=&pinf. &psup ;
run;

%if &pinf=2.5 and &psup=97.5 %then %do ; %let pinf=2_5;%let psup=97_5;%end;

data inter_macro_corrige;
set inter_macro_corrige (rename =(%do i=1 %to &nbvar.; pi_&i.a&pinf.=pi_&i. pi_&i.a&psup.=ps_&i.%end;));
run;
/*
proc means data = &table_in noprint;

var &liste_var;

output out=inter_macro_corrige 

%do i=1 %to &nbvar;

p&pinf(&&var&i)=pi_&i p&psup(&&var&i)=ps_&i

%end;

;

run;
*/
data _null_;

set inter_macro_corrige;

%do i=1 %to &nbvar;

call symput("pi_&i",pi_&i);

call symput("ps_&i",ps_&i);

%end;

run;

data &table_out;

set &table_in;

%do i=1 %to &nbvar;

%if &style=T %then %do;

if &&var&i <&&pi_&i then &&var&i = . ; 

if &&var&i >&&ps_&i then &&var&i = . ;

%end;

%if &style=W %then %do;

if &&var&i <&&pi_&i then &&var&i = &&pi_&i ; 

if &&var&i >&&ps_&i then &&var&i = &&ps_&i ;

%end;

%end;

run;

/*proc delete data=inter_macro_corrige; run;*/

%end;

%mend;

 
/****************************************************************/

/* Fonction : nombre de mots d'une chaine de caractères */

/****************************************************************/

/* fonction qui retourne le nombre de mots */

%macro nbmots(chaine); 

%local nb_mot chaine la chainec lb i; 

%let chaine = %left(&chaine); 

%let la = %length(&chaine); 

%let chainec = %cmpres(&chaine); 

%let lb = %length(&chainec);

%let i = %index(&chainec,%str( )); 

%let nb_mot = 1; 

%do %while(&i^=0); 

%let nb_mot = %eval(&nb_mot+1); 

%let chainec = %substr(&chainec,%eval(&i+1)); 

%let i = %index(&chainec,%str( )); 

%end; 

%if &lb=0 %then %let nb_mot=0;

&nb_mot

%mend nbmots;
