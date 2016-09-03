version 8
#delimit;
set more off;
set mem 10m;
log using bms2006ajps-simulate.log, replace;

/*	************************************************************************	*/
/*     	File Name:	bms2006ajps-simulate.do						*/
/*     	Date:   	December 06, 2005						*/
/*      Author: 	Frederick J. Boehmke						*/
/*      Purpose:	Generate Figure 1 in the paper. 				*/
/*      Input File:	bms2006ajps-mkdata.dta					 	*/
/*      Output File:	bms2006ajps-simulate.dta, bms2006ajps-simulate.gph  		*/
/*	Requires:	DURSEL program for Stata - see Boehmke's website for 		*/
/*			installation information.					*/
/*	Version:	Stata 8 or above - see VERSION NOTE below if you have 		*/
/*			Version 7 or below (a couple of variable names change).		*/
/*	************************************************************************	*/


use bms2006ajps-mkdata.dta;

	/********************************************************/
	/* Run the duration model with selection. Note that 	*/
	/* durselgr requires use of the "time" option. 		*/
	/********************************************************/

dursel wsurv tenl democ tendem rdpopl rwin, select(enter=democ autoc tennewL tennewAL total 
	majpow lntropen) dist(weibull) time rtcensor(rtcensor) cluster(ccode);

	/********************************************************/
	/* Preserve saves the data so that we can return to it 	*/
	/* after we collapse and calculate marginal effects.	*/
	/* This allows us to run the models on the same data. 	*/
	/********************************************************/

preserve;

	/********************************************************/
	/* Now we collapse the data to get a representative 	*/
	/* observation that we can manipulate for first 	*/
	/* differences. Note the use of the selected sample, 	*/
	/* which makes the means the same as those for the naive*/
	/* model. We then create two more observations that 	*/
	/* manipulate the values of democracy and autocracy and	*/
	/* their interactions so we can get a first difference.	*/
	/* durselgr uses max and min of dependent variable in	*/
	/* the data to set its scale, so save those as well. 	*/
	/* _n=2 is the autocracy and _n=3 is the democracy. 	*/
	/********************************************************/

collapse (mean) _all (min) wsurvmin=wsurv (max) wsurvmax=wsurv if enter==1;

  expand 3;

  replace wsurv = wsurvmin if _n==2;
  replace wsurv = wsurvmax if _n==3;

  replace democ	   = 0 if _n==2;  
  replace tendem   = 0 if _n==2;
  replace autoc    = 1 if _n==2;
  replace tennewAL = tennewL if _n==2;

  replace democ	   = 1 if _n==3;  
  replace tendem   = tenl*democ if _n==3;
  replace autoc    = 0 if _n==3;
  replace tennewAL = 0 if _n==3;
 
  summarize;

	/********************************************************/
	/* Run durselgr to create the data with the predicted 	*/
	/* survival function and save them to combine later. 	*/
	/********************************************************/

  durselgr wsurv if _n==1, survival saving(bms2006ajps-simulate-dursel-mean,  replace) nograph;
  durselgr wsurv if _n==2, survival saving(bms2006ajps-simulate-dursel-autoc, replace) nograph;
  durselgr wsurv if _n==3, survival saving(bms2006ajps-simulate-dursel-democ, replace) nograph;


	/********************************************************/
	/* Now do the same calculations for the naive model.  	*/
	/* The first stcurve command calculates the values at 	*/
	/* the means, the second does the same for democracies 	*/
	/* and autocracies. Two data sets are saved for merging.*/
	/********************************************************/

restore;

  stset wsurv if e(sample), failure(dio);

  streg tenl democ tendem rdpopl rwin, dist(weibull) cluster(ccode) time;

  summarize if e(sample);

  stcurve, survival range(0 25) nodraw
	outfile(bms2006ajps-simulate-naive-mean, replace);
  stcurve, survival range(0 25) nodraw at2(democ=0 tendem=0) at3(democ=1)
	outfile(bms2006ajps-simulate-naive, replace);


	/********************************************************/
	/* These commands combine the five data sets just 	*/
	/* created. durselgr uses the same values of _t for	*/
	/* its calculations, but stcurve uses different values. */
	/* This is why the stcurve data sets are appended. 	*/
	/********************************************************/

use bms2006ajps-simulate-dursel-mean, clear;

  rename survival surv_mean;

  joinby _t using bms2006ajps-simulate-dursel-autoc;
  
	rename survival surv_autoc;

  joinby _t using bms2006ajps-simulate-dursel-democ;
  
	rename survival surv_democ;

	/********************************************************/
	/* VERSION NOTE: If you are running Stata version 7 or  */
	/* below, you need to replace surv3 with surv2 and 	*/
	/* surv3 with surv4. The outputs are named differently.	*/
	/********************************************************/

  append using bms2006ajps-simulate-naive;

	rename surv3 surv_naive_autoc;
	rename surv4 surv_naive_democ;

  append using bms2006ajps-simulate-naive-mean;

	rename surv1 surv_naive_mean;

	/********************************************************/
	/* These commands generate the first differences and  	*/
	/* label them and the predicted survival funcations.	*/
	/********************************************************/

  generat fd_dursel = surv_democ - surv_autoc;
  generat fd_naive  = surv_naive_democ - surv_naive_autoc;

  label variable surv_mean  		"FIML Survival at Mean";
  label variable surv_autoc 		"FIML Survival for Autocracy";
  label variable surv_democ 		"FIML Survival for Democracy";

  label variable surv_naive_mean  	"Weibull Survival at Mean";
  label variable surv_naive_autoc 	"Weibull Survival for Autocracy";
  label variable surv_naive_democ 	"Weibull Survival for Democracy";

  label variable fd_dursel 		"FIML First Difference";
  label variable fd_naive  		"Weibull First Difference";

  save bms2006ajps-simulate, replace;


	/********************************************************/
	/* Now generate the graph of the predicted survival 	*/
	/* functions and the first differences.			*/
	/********************************************************/

twoway line surv_autoc surv_naive_autoc fd_dursel fd_naive _t, scheme(s1mono) 
	clpattern(solid longdash shortdash dash_dot) 
	yline(0) ylabel(-0.5(0.25)1)
	saving(bms2006ajps-simulate, replace) ytitle("");


	/********************************************************/
	/* This cleans up the individual data files merged 	*/
	/* together to obtain the different calculations.  	*/
	/********************************************************/

  erase bms2006ajps-simulate-dursel-democ.dta;
  erase bms2006ajps-simulate-dursel-mean.dta;
  erase bms2006ajps-simulate-dursel-autoc.dta;
  erase bms2006ajps-simulate-naive.dta;
  erase bms2006ajps-simulate-naive-mean.dta;

log close;
clear;
exit;
