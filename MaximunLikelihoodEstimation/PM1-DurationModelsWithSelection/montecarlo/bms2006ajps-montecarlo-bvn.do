#delimit;
version 7.0;
set memory 30m;
set more off;
log using bms2006ajps-montecarlo-bvn, replace text;

/*	************************************************************************	*/
/*     	File Name:	bms2006ajps-montecarlo-bvn.do					*/
/*     	Date:   	March 26, 2007							*/
/*      Author: 	Frederick J. Boehmke						*/
/*      Purpose:	Monte Carlo analysis of FIML duration with selection model      */
/*			compared to various naive duration models. This program runs 	*/
/*			the analysis using bivariate normal errors transformed to	*/
/*			to exponential errors. 						*/
/*      Input File:	bms2006ajps-montecarlo-indvars.dta			 	*/
/*      Output File:	bms2006ajps-montecarlo-bvn.log 					*/
/*			bms2006ajps-montecarlo-bvn/XXX.dta				*/
/*	Requires:	DURSEL program for Stata - see Boehmke's website for 		*/
/*			installation information (these run using v2.0).		*/
/*	Version:	Stata 7 or above.				 		*/
/*	************************************************************************	*/


	/********************************************************/
	/* MKCORR generates correlated bivariate normal 	*/
	/* variates. Used for the independent variables and the	*/
	/* error terms.						*/
	/********************************************************/

program define mkcorr;
  version 4.0;

  local k = rowsof(P);
  matrix A = cholesky(P);

  local i 1;

  quietly {;
	while `i'<=`k' {;
	  gen c`i' = invnorm(uniform());
	  local i=`i'+1
	};
	local i 1;
	while `i'<=`k' {;
	  matrix row = A[`i',.];
	  matrix score v`i' = row;
	  local i=`i'+1;
	};
	local i 1 ;
	while `i' <= `k' {;
	  drop c`i';
	  local i=`i'+1;
	};
  };
end;


	/********************************************************/
	/* SELDUR is the program that estimates the different	*/
	/* models for each draw of the error terms. The 	*/
	/* term is passed as an argument and is stored in the	*/
	/* local variable `2', which is then named `rho'.	*/
	/********************************************************/


program define seldur;
	if "`1'"=="?" {;
		global S_1 "alpha1 beta1 alpha2 beta2 Z_alpha 
			sealpha1 sebeta1 sealpha2 sebeta2 seZ_alph rhotrue
			alphaexp betaexp sealpexp sebetexp
			alphawbl betawbl lnpwbl sealpwbl sebetwbl selnpwbl
			betacox sebetcox retcode";
		exit;
	};

		/********************************************************/
		/* Drop everything but the independent variables. 	*/
		/********************************************************/

	keep x1 x2;

		
		/********************************************************/
		/* Generate the correlated bivariate normal error 	*/
		/* terms. First set up the correlation matrix, then  	*/
		/* transform the bivariate normal draws into 		*/
		/* exponentials.					*/
		/********************************************************/

	local rho = `2';

	matrix P = (1, 1 \ 1, 1);

	matrix P[1,2] = `rho';
	matrix P[2,1] = `rho';

	mkcorr;

	gen epsilon1 = -log(1-norm(v1));
	gen epsilon2 = -log(1-norm(v2));


		/********************************************************/
		/* Now generate the duration variable, y2, and the 	*/
		/* selection variable, c, which is indicator for 	*/
		/* whether the latent y1 is greater than zero; 		*/
		/* stset the data for observations that select in. 	*/
		/********************************************************/


	generate y1	= exp( (-0.5 + 1*x1))*epsilon1;
	generate y2	= exp(-( 0 + 0.75*x2))*epsilon2;

	generat c 	= 1 if y1 >  1;
	replace c 	= 0 if y1 <= 1;

	stset y2 if c==1;


		/********************************************************/
		/* Estimate the three naive duration models. 		*/
		/* Save the coefficients and SEs into locals to post at */
		/* the end of the SELDUR program. 			*/
		/********************************************************/

		/********************************************************/
		/* Note that when estimating the parametric models, it 	*/
		/* is necessary to use the -time- option and multiply  	*/
		/* the estimate by -1. Stata's -nohr- option reports  	*/
		/* b*p, but we don't want the p in there, so use the 	*/
		/* -time- option to get estimates of -b, then multiple 	*/
		/* by -1.						*/
		/********************************************************/


	streg x2, dist(exp) time robust;

	  local alphaexp	= - _b[_cons];
	  local betaexp		= - _b[x2];
	  local sealpexp	=   _se[_cons];
	  local sebetexp	=   _se[x2];

	streg x2, dist(weibull) time robust;

	  local alphawbl	= - [_t]_b[_cons];
	  local sealpwbl	=   [_t]_se[_cons];
	  local betawbl		= - [_t]_b[x2];
	  local sebetwbl	=   [_t]_se[x2];
	  local lnpwbl		=   [ln_p]_b[_cons];
	  local selnpwbl	=   [ln_p]_se[_cons];


	stcox x2, nohr robust;

	  local betacox		= _b[x2];
	  local sebetcox	= _se[x2];


		/********************************************************/
		/* Estimate the FIMAL duration with selection model. 	*/
		/* Use the -capture- and -if- commands so that the 	*/
		/* SELDUR program doesn't terminate if -dursel- fails 	*/
		/* to converge. If it does, post missing values.	*/
		/********************************************************/


	capture dursel y2 x2, sel(c=x1) dist(exp) time iterate(30);

	local retcode=_rc;

	if _rc==0
	  {
	    ml display;

	    local alpha1	=   [c]_b[_cons];
	    local beta1		=   [c]_b[x1]; 
	    local alpha2	= - [y2]_b[_cons];
	    local beta2		= - [y2]_b[x2]; 
	    local Z_alpha	=   [Z_alpha]_b[_cons];

	    local sealpha1	= [c]_se[_cons];	
	    local sebeta1	= [c]_se[x1]; 
	    local sealpha2 	= [y2]_se[_cons];
	    local sebeta2 	= [y2]_se[x2];
	    local seZ_alph	= [Z_alpha]_se[_cons];
	  };
	else
	  {
	    local alpha1	= .;
	    local beta1		= .; 
	    local alpha2	= .;
	    local beta2		= .; 
	    local Z_alpha	= .;

	    local sealpha1	= .;	
	    local sebeta1	= .; 
	    local sealpha2 	= .;
	    local sebeta2 	= .;
	    local seZ_alph	= .;
	};

	post `1' (`alpha1') (`beta1') (`alpha2') (`beta2') (`Z_alpha')
		(`sealpha1') (`sebeta1')  (`sealpha2') (`sebeta2') (`seZ_alph') (`rho')
		(`alphaexp') (`betaexp') (`sealpexp') (`sebetexp')
		(`alphawbl') (`betawbl') (`lnpwbl') (`sealpwbl') (`sebetwbl') (`selnpwbl')
		(`betacox') (`sebetcox') (`retcode');

end;


	/********************************************************/
	/* Use MKCORR to generate correlated independent 	*/
	/* variables that will be held fixed throughout the  	*/
	/* analysis. Note that since I set the seed after this 	*/
	/* step, these values will be different every time. I  	*/
	/* have therefore commented this out and provided the  	*/
	/* draw of the independent variables that I used.	*/
	/********************************************************/

*** BEGIN COMMENTED OUT COMMANDS;
*
* matrix P = (1, 0.5 \ 0.5, 1);
* set obs 5000;
* mkcorr;
* rename v1 x1;
* rename v2 x2;
*
* save bms2006ajps-montecarlo-indvars,replace;
*
*** END COMMENTED OUT COMMMANDS;


	/********************************************************/
	/* Now set the seed and run the SELDUR program. 	*/
	/* I use the -for- loop to set the values of the 	*/
	/* correlation parameter. This generates one data set	*/
	/* for each value of this parameter.			*/
	/********************************************************/

	
set seed 54;

for num -0.9(0.1)0.9:

  quietly use bms2006ajps-montecarlo-indvars \

  simul seldur, reps(100) args( X) dots 
	saving("bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn-(rho=X).dta") replace;


	/********************************************************/
	/* Combine the data sets for the different values of 	*/
	/* the correlation parameter.				*/
	/********************************************************/


use "bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn-(rho=-.9).dta", clear;

for num -0.8(0.1)0.9: 

  append using "bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn-(rho=X).dta";


	/********************************************************/
	/* Create a few variables. rhohat is converted from  	*/
	/* (-inf, inf) to (-0.25 to 0.25); betapwbl and 	*/
	/* alphpwbl are generated for a comparison to the Cox 	*/
	/* estimates, which unavoidably include p. 		*/
	/********************************************************/


  generate rhohat  = (exp(2*Z_alpha)-1)/(exp(2*Z_alpha)+1)/4;
  generate pwbl	   = exp(lnpwbl);
  generate betapwbl= betawbl*pwbl;
  generate alphpwbl= alphawbl*pwbl;

  compress;

  save "bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn.dta", replace;


	/********************************************************/
	/* Tabulate the return codes to see if any draws  	*/
	/* resulted in a lack of convergence for the -dursel-	*/
	/* model. 						*/
	/********************************************************/


  table rhotrue retcode;


	/********************************************************/
	/* Collapse the data to get mean parameter estimates 	*/
	/* and their SDs. Use these to generate root mean 	*/
	/* square error values.					*/
	/********************************************************/


collapse (mean) alpha1 beta1 alpha2 beta2 Z_alpha rhohat alphaexp betaexp
		sealpha1 sebeta1 sealpha2 sebeta2 seZ_alph sealpexp sebetexp
		alphawbl betawbl lnpwbl pwbl sealpwbl sebetwbl selnpwbl
		betacox sebetcox betapwbl alphpwbl 
	 (sd)	sdalpha1=alpha1 sdbeta1=beta1 sdrhohat=rhohat sdalpha2=alpha2 
		sdbeta2=beta2 sdZ_alph=Z_alpha sdalpexp=alphaexp sdbetexp=betaexp
		sdalpwbl=alphawbl sdbetwbl=betawbl sdlnpwbl=lnpwbl sdpwbl=pwbl
		sdbetcox=betacox, by(rhotrue);


  generate alp2mse	= sqrt((alpha2-0)*(alpha2-0) 		+ sdalpha2*sdalpha2);
  generate beta2mse	= sqrt((beta2-0.75)*(beta2-0.75)	+ sdbeta2*sdbeta2);
  generate alpexmse	= sqrt((alphaexp-0)*(alphaexp-0)	+ sdalpexp*sdalpexp);
  generate betexmse	= sqrt((betaexp-0.75)*(betaexp-0.75)	+ sdbetexp*sdbetexp);
  generate alpwbmse	= sqrt((alphawbl-0)*(alphawbl-0)	+ sdalpwbl*sdalpwbl);
  generate betwbmse	= sqrt((betawbl-0.75)*(betawbl-0.75)	+ sdbetwbl*sdbetwbl);
  generate lnpwbmse	= sqrt((lnpwbl-0)*(lnpwbl-0)		+ sdlnpwbl*sdlnpwbl);
  generate betcxmse	= sqrt((betacox-0.75)*(betacox-0.75)	+ sdbetcox*sdbetcox);

  generate aexmsert	= alpexmse/alp2mse;
  generate awbmsert	= alpwbmse/alp2mse;

  generate bexmsert	= betexmse/beta2mse;
  generate bwbmsert	= betwbmse/beta2mse;
  generate bcxmsert	= betcxmse/beta2mse;

  generate a2exprat	= alpexmse/alpha2;
  generate a2wblrat	= alpwbmse/alpha2;

  generate b2exprat	= abs(betaexp-0.75)/abs(beta2-0.75);
  generate b2wblrat	= abs(betawbl-0.75)/abs(beta2-0.75);
  generate b2coxrat	= abs(betacox-0.75)/abs(beta2-0.75);


	/********************************************************/
	/* Label the variables and save the collapsed data.	*/
	/********************************************************/


  label data "Bivariate normal Monte Carlo analysis of FIML duration model.";

	label variable alp2mse 		"FIML Intercept";
	label variable alpexmse 	"Exponential Intercept";
	label variable beta2mse 	"FIML Slope";
	label variable betexmse 	"Exponential Slope";
	label variable alpwbmse 	"Weibull Intercept";
	label variable betwbmse 	"Weibull Slope";
	label variable lnpwbmse 	"Weibull Shape";
	label variable betcxmse 	"Cox Slope";
	label variable alpha2 		"FIML Intercept";
	label variable alphaexp 	"Exponential Intercept";
	label variable beta2 		"FIML Slope";
	label variable betaexp 		"Exponential Slope";
	label variable Z_alpha 		"Correlation Parameter";
	label variable rhohat 		"Correlation";
	label variable alphawbl 	"Weibull Intercept";
	label variable betawbl	 	"Weibull Slope";
	label variable lnpwbl	 	"Weibull Shape";
	label variable betacox 		"Cox Slope";
	label variable sealpha2 	"SE FIML Intercept";
	label variable sealpexp 	"SE Exponential Intercept";
	label variable sealpwbl 	"SE Weibull Intercept";
	label variable sebeta2 		"SE FIML Slope";
	label variable sebetexp 	"SE Exponential Slope";
	label variable sebetwbl 	"SE Weibull Slope";
	label variable sebetcox 	"SE Cox Slope";
	label variable sdbetexp 	"SD Exponential Slope";
	label variable sdbetcox 	"SD Cox Slope";
	label variable sdalpha2 	"SD FIML Intercept";
	label variable sdalpexp 	"SD Exponential Intercept";
	label variable sdalpwbl 	"SD Weibull Intercept";
	label variable sdbeta2 		"SD FIML Slope";
	label variable sdbetexp 	"SD Exponential Slope";
	label variable sdbetwbl 	"SD Weibull Slope";
	label variable alpha1 		"Selection Intercept";
	label variable beta1 		"Selection Slope";
	label variable sealpha1 	"SE Selection Intercept";
	label variable sebeta1 		"SE Selection Slope";
	label variable sdalpha1 	"SD Selection Intercept";
	label variable sdbeta1 		"SD Selection Slope";
	label variable seZ_alph 	"SE Correlation Parameter";
	label variable sdZ_alph 	"SD Correlation Parameter";
	label variable sdrhohat 	"SD Correlation";
	label variable pwbl 		"Weibull Duration Dependence";
	label variable alphpwbl 	"Weibull Intercept (b*p)";
	label variable betapwbl	 	"Weibull Slope (b*p)";
	label variable bexmsert 	"Exponential";
	label variable bwbmsert		"Weibull";
	label variable bcxmsert		"Cox";
	label variable rhotrue		"Bivariate Normal Error Correlation Parameter (alpha)";

  	compress;

  save "bms2006ajps-montecarlo-bvn/bms2006ajps-montecarlo-bvn-collapsed.dta", replace;

log close;
clear;
exit;
