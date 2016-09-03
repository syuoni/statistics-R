version 7
#delimit;

cd "D:\Documents\R\Statistics and Econometrics\DurationModelsWithSampleSelection";

set more off;
set mem 10m;
log using bms2006ajps-mkdata.log, text replace;

/*	************************************************************************	*/
/*     	File Name:	bms2006ajps-mkdata.do						*/
/*     	Date:   	December 06, 2005						*/
/*      Author: 	Frederick J. Boehmke						*/
/*      Purpose:	This file modifies tenure.dta in order to generate the data 	*/
/*			used in bms2006ajps.pdf. tenure.dta is the file with the 	*/
/*			merged data from Chiozza and Goemans's (2003) leader duration 	*/
/*			dataset (version 9) and Bueno de Mesquita and Siverson's 	*/
/*			(1995) original post-war leader survival data set. 		*/
/*      Input File:	bms2006ajps-rawdata.dta						*/
/*      Output File:	bms2006ajps-mkdata.dta, bms2006alps-mkdata.log 			*/
/*	Version:	Stata 7 or above.				 		*/
/*	************************************************************************	*/

use bms2006ajps-rawdata.dta;

summarize;

	/********************************************************/
	/* Generate the selection into war variable. 		*/
	/********************************************************/

generat enter = 0;
replace enter = 1 if wsurv > 0 & wsurv != .;

	/********************************************************/
	/* Note that dio (died in office) is coded 0 if leader  */
	/* dies in office since Stata's stset command wants     */
	/* this variable equal to 1 if an observation fails. We */
	/* stuck with BDMS coding, despite its odd semantics.   */
	/* Generate rtcensor for the DURSEL model.		*/
	/********************************************************/

generate rtcensor = 1 - dio;

	/********************************************************/
	/* Now generate the time-in-office variables for the    */
	/* Chiozza and Goemens data set in order to add them    */
	/* to the selection equation and test BDMS's sixth      */
	/* hypothesis.   					*/
	/********************************************************/

generat tennew  = year - year(eindate) + (1/365) * (365 - doy(eindate));
generat tennewA = tennew*autoc;

generat tennewL  = log(tennew);
generat tennewAL = tennewL*autoc;

	/********************************************************/
	/* This creates the updated democracy*log tenure	*/
	/* interaction for the leadure tenure equation. See	*/
	/* The readme file for more info on this variable. 	*/
	/********************************************************/

generat tendem_fix = democ*tenl;

summarize;

keep wsurv tenl democ tendem rdpopl rwin enter democ autoc tennewL tennewAL tennew tennewA
	total majpow lntropen rtcensor ccode dio year tendem_fix;

label data "Boehmke, Morey and Shannon (2006) - leader duration in office with selection.";

  label variable tenl		"Log tenure in office - post crisis leaders";
  label variable tendem		"Log tenure in office - post crisis democratic leaders";
  label variable tendem_fix	"Adjusted tendem - uses Jaggers and Gurr democracies";
  label variable majpow		"Major power (Small and Singer)";

  label variable total 		"Total bordering countries (COW)";

  label variable enter 		"Leader begins a war in the current year";
  label variable rtcensor 	"Leader dies before post-crisis tenure ends";
  label variable tennew 	"Tenure in office - all leaders";
  label variable tennewA 	"Tenure in office - all autocratic leaders";
  label variable tennewL 	"Log tenure in office - all leaders";
  label variable tennewAL 	"Log tenure in office - all autocratic leaders";

  compress;

  summarize;

save bms2006ajps-mkdata, replace;

log close;
clear;
exit;
