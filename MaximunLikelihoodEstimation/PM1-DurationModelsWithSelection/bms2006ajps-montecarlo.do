#delimit;

/*	************************************************************************	*/
/*     	File Name:	bms2006ajps-montecarlo.do					*/
/*     	Date:   	March 26, 2007							*/
/*      Author: 	Frederick J. Boehmke						*/
/*      Purpose:	Runs Monte Carlo analysis in the paper. This program just	*/
/*			streamlines the process by running all three parts of the 	*/
/*			Monte Carlo analysis: the bivariate exponential simulations,	*/
/*			the buivariate normal simulations, and the program that 	*/
/*			combines them and graphs the results.				*/
/*	Calls:		montecarlo/bms2006ajps-montecarlo-bve.do			*/
/*			montecarlo/bms2006ajps-montecarlo-bvn.do			*/
/*			montecarlo/bms2006ajps-montecarlo-combine.do			*/
/*	Version:	Stata 7 or above.						*/
/*	************************************************************************	*/


cd montecarlo;

do bms2006ajps-montecarlo-bve.do;
  program drop _all;
  clear;

do bms2006ajps-montecarlo-bvn.do;
  program drop _all;
  clear;

do bms2006ajps-montecarlo-combine.do;
  program drop _all;
  clear;

exit;