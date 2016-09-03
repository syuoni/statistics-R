version 7
#delimit;

cd "D:\Documents\R\Statistics and Econometrics\DurationModelsWithSampleSelection";

set more off;
set mem 10m;
log using bms2006ajps-analyze.log, text replace;

/*	************************************************************************	*/
/*     	File Name:	bms2006ajps-analyze.do						*/
/*     	Date:   	December 06, 2005						*/
/*      Author: 	Frederick J. Boehmke						*/
/*      Purpose:	Replicate models in bms2006ajps.pdf studying the effect of      */
/*			non-random sample selection on leader's tenure in office 	*/
/*			after war initiation.						*/
/*      Input File:	bms2006ajps.dta						 	*/
/*      Output File:	bms2006ajps-analyze.log 					*/
/*	Requires:	DURSEL program for Stata - see Boehmke's website for 		*/
/*			installation information.					*/
/*	Version:	Stata 7 or above.				 		*/
/*	Note: 		The paper used DURSEL v1.9. The current version, DURSEL  	*/
/*			v2.0, estimates the negative of the coefficients in the		*/
/*			paper. This makes them comparable to streg coefficients.   	*/
/*			Note the two codings of tenure and tendem used in order to 	*/
/*			rectify the mistakes in the original model in the paper.  	*/
/*	************************************************************************	*/

use bms2006ajps-mkdata.dta;

	/********************************************************/
	/* Run the duration model with selection.               */
	/* Note that the paper used the more recent Jaggers and	*/
	/* Gurr values of democracy in the duration equation	*/
	/* but did not adjust the values of the interaction   	*/
	/* term appropriately. The second model rectifies this.	*/
	/********************************************************/

dursel wsurv tenl democ tendem     rdpopl rwin, select(enter=democ autoc tennewL tennewAL total 
	majpow lntropen) dist(weibull) time rtcensor(rtcensor) cluster(ccode);

dursel wsurv tenl democ tendem_fix rdpopl rwin, select(enter=democ autoc tennewL tennewAL total 
	majpow lntropen) dist(weibull) time rtcensor(rtcensor) cluster(ccode);

	/********************************************************/
	/* Run the probit model of leaders' selection into war. */
	/* Add "if e(sample)" to estimate on same observations. */
	/* Note that the paper incorrectly does not use the log	*/
	/* of tenure in the probit equation (it does in the 	*/
	/* DURSEL selection equation). The correct model and    */
	/* the replication model are presented below.		*/
	/********************************************************/

probit enter democ autoc tennew  tennewA  total majpow lntropen if e(sample), cluster(ccode);
probit enter democ autoc tennewL tennewAL total majpow lntropen if e(sample), cluster(ccode);

	/********************************************************/
	/* Run the duration model of leaders' tenure after war. */
	/* Add "if e(sample)" to estimate on same observations. */
	/* Note that dio (died in office) is coded 0 if leader  */
	/* dies in office since Stata's stset command wants     */
	/* this variable equal to 1 if an observation fails. We */
	/* stuck with BDMS coding, despite its odd semantics.   */
	/********************************************************/

stset wsurv if e(sample), failure(dio);

  streg tenl democ tendem     rdpopl rwin, dist(weibull) cluster(ccode) time;
  streg tenl democ tendem_fix rdpopl rwin, dist(weibull) cluster(ccode) time;

log close;
clear;
exit;

