#delimit;
version 8;
log using bms2006ajps-montecarlo-combine.log, text replace;
set mem 10m;

/*	************************************************************	*/
/*     	File Name:	bms2006ajps-montecarlo-combine.do		*/
/*     	Date:   	February 17, 2005				*/
/*      Author: 	Frederick J. Boehmke				*/
/*      Purpose:	Combine two error generating processes.		*/
/*	Input File:							*/
/*      Output File:	bms2006ajps-montecarlo-combine.log		*/
/*      		bms2006ajps-montecarlo.gph			*/
/*	************************************************************	*/


	/********************************************************/
	/* Combine the two data sets and set rhotrue equal to 	*/
	/* missing in the bivariate exponential data set to	*/
	/* summarize the two separately.			*/
	/********************************************************/


use "bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn-collapsed.dta", clear;

  append using "bms2006ajps-montecarlo-bve/bms2006ajps-montecarlo-bve-collapsed.dta";

  	replace rhotrue = . if bvecorr != .;


	/********************************************************/
	/* Generate the tables for the bivariate normal 	*/
	/* results, using all value of rhotrue.			*/
	/********************************************************/


table rhotrue, c(mean alpha1 mean beta1 mean alpha2 mean beta2 mean Z_alpha);
table rhotrue, c(mean alpha2 mean rhohat mean alphaexp mean alphawbl);
table rhotrue, c(mean beta2 mean betaexp mean betawbl mean betacox);
table rhotrue, c(mean sdalpha1 mean sealpha1);
table rhotrue, c(mean sdbeta1 mean sebeta1);
table rhotrue, c(mean sdalpha2 mean sealpha2);
table rhotrue, c(mean sdbeta2 mean sebeta2);
table rhotrue, c(mean sdZ_alph mean seZ_alph mean sdrhohat);

table rhotrue, c(mean sdalpha2 mean sdalpexp mean sdalpwbl);
table rhotrue, c(mean sdbeta2 mean sdbetexp mean sdbetwbl mean sdbetcox);

table rhotrue, c(mean alp2mse mean alpexmse mean alpwbmse);
table rhotrue, c(mean beta2mse mean betexmse mean betwbmse mean betcxmse);


	/********************************************************/
	/* Generate the tables for the bivariate exponential 	*/
	/* results, using all value of bvecorr (i.e., 		*/
	/* rho=alpha/4).					*/
	/********************************************************/


table bvecorr, c(mean alpha1 mean beta1 mean alpha2 mean beta2 mean Z_alpha);
table bvecorr, c(mean alpha2 mean rhohat mean alphaexp mean alphawbl);
table bvecorr, c(mean beta2 mean betaexp mean betawbl mean betacox);
table bvecorr, c(mean sdalpha1 mean sealpha1);
table bvecorr, c(mean sdbeta1 mean sebeta1);
table bvecorr, c(mean sdalpha2 mean sealpha2);
table bvecorr, c(mean sdbeta2 mean sebeta2);
table bvecorr, c(mean sdZ_alph mean seZ_alph mean sdrhohat); 

table bvecorr, c(mean sdalpha2 mean sdalpexp mean sdalpwbl);
table bvecorr, c(mean sdbeta2 mean sdbetexp mean sdbetwbl mean sdbetcox);

table bvecorr, c(mean alp2mse mean alpexmse mean alpwbmse);
table bvecorr, c(mean beta2mse mean betexmse mean betwbmse mean betcxmse);


	/********************************************************/
	/* Only include some observations to make the graph  	*/
	/* cleaner.						*/
	/********************************************************/


  generat temp  = 40*bvecorr;
  generat temp2 = round(40*bvecorr,2);

	generat graph = 1 if temp == temp2 & !missing(bvecorr);

	replace graph = 2 if rhotrue <= -0.25 & rhotrue >= -0.5 & !missing(rhotrue);
	replace graph = 3 if rhotrue >=  0.25 & rhotrue <=  0.5 & !missing(rhotrue);


twoway connected betawbl betacox beta2 alphawbl alpha2 rhotrue if graph==2, 
	ms(sh t O s d) msize(1.7 1.7 1.7 1.7 1.7) clpattern(longdash shortdash solid longdash solid) 
	mcolor(gs8 gs8 gs0 gs8 gs0) clcolor(gs8 gs8 gs0 gs8 gs0)

  ||    connected betawbl betacox beta2 alphawbl alpha2 rhotrue if graph==3,
	ms(sh t O s d) msize(1.7 1.7 1.7 1.7 1.7) clpattern(longdash shortdash solid longdash solid) 
	mcolor(gs8 gs8 gs0 gs8 gs0) clcolor(gs8 gs8 gs0 gs8 gs0)

  ||    connected betawbl betacox beta2 alphawbl alpha2 bvecorr if graph == 1, scheme(s1mono)
	ms(sh t O s d) msize(1.7 1.7 1.7 1.7 1.7) clpattern(longdash shortdash solid longdash solid) 
	mcolor(gs8 gs8 gs0 gs8 gs0) clcolor(gs8 gs8 gs0 gs8 gs0) 
	legend(order(4 5 1 2 3)) yline(0 0.75) xline(-0.25 0.25) 
	ytitle("Average Estimate") xtitle("Error Correlation") xlabel(-0.5(0.25)0.5) ylabel(-0.5(0.25)1)
	text(-0.25 -0.45 "TBVN", place(e)) text(-0.25 0.35 "TBVN", place(e)) text(-0.25 -0.05 "BVE", place(e))
	text(0.07 0.10 "alpha=0", place(e)) text(0.67 0.10 "beta=0.75", place(e))
	saving("bms2006ajps-montecarlo", replace);

log close;
clear;
exit;


