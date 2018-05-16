%let chemin_base=V:\DIRAG\outils_statistiques\projet_ussr;
%inc "&chemin_base.\code\macro\regionalisation.sas";
libname clap "&chemin_base.\donnees\clap" access=readonly;
libname cible "&chemin_base.\donnees";

options nonotes;
%regio_clap(
	libsrc=clap,
	tabent=entrep,
	tabetab=etab,
	libcible=cible,
	tabcible=regionalisation,
	fichierxls=&chemin_base.\donnees\clap\clap.xls
);
options notes;
