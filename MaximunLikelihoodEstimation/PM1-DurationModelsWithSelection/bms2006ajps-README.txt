These files include data and estimation code for:

Boehmke, Frederick J., Daniel Morey and Megan Shannon. 2006. “Selection Bias and 
Continuous-Time Duration Models: Consequences and a Proposed Solution.” American 
Journal of Political Science 50 (1): 192-207.

Compiled on December 07, 2005 by Frederick J. Boehmke (frederick-boehmke@uiowa.edu).
Updated on April 10, 2007 to include Monte Carlo code.


INCLUDED:

  2 Stata data sets:

   bms2006ajps-rawdata.dta 		The data set before new variables are created.
   bms2006ajps-mkdata.dta		The final data set.
   bms2006ajps-montecarlo-indvars.dta	The independent variables used in the Monte Carlo.

  7 Stata do files:

   bms2006ajps-mkdata.do 		Creates the data set (bms2006ajps-mkdata.dta) 
					used for estimation.   
   bms2006ajps-analyze.do 		Runs the models in the paper.
   bms2006ajps-simulate.do 		Generates the graph used in the paper.

   bms2006ajps-montecarlo.do 		Runs the next three files for the Monte Carlo 
					analyses in the paper.

   bms2006ajps-montecarlo-bve.do 	The bivariate exponential Monte Carlo.
   bms2006ajps-montecarlo-bnv.do 	The bivariate normal Monte Carlo.
   bms2006ajps-montecarlo-combine.do 	Combines and summarizes the Monte Carlo
					analysis.

  1 preprint copy of the paper:

	bms2006ajps.pdf


NOTES:

1. To run the model in the paper, you will need to install the DURSEL estimator
   in Stata. See Boehmke's web page for he latest version (DURSEL version 2.0 
   currently available at http://myweb.uiowa.edu/fboehmke/methods.html). 
   Further, note that the paper was estimated with version 1.9, which affects 
   the sign of the coefficients in the duration equation.

2. To create the leader selection and duration dataset we combined two separate 
   data sets of leader tenure. For data on all world leaders from 1919 to 1975 
   we employ Chiozza and Goemans's (2003) leader duration dataset (version 9).  
   This provided the data for the selection portion of our analysis. To these 
   data we added Bueno de Mesquita and Siverson's (1995) original post-war 
   leader survival data set. This provided the data for the duration portion of 
   our analysis. Because the two data sets do not have a common code for 
   observations, we merged them observation by observation. Thus we do not 
   currently have code to merge them in Stata. These are the data contained in    
   bms2006ajps-rawdata.dta.

3. Variable manipulations are performed in bms2006ajps-mkdata.do, which creates
   the data set bms2006ajps-mkdata.dta used for the analysis.

4. The paper used DURSEL v1.9. The current version, DURSEL v2.0, estimates the 
   negative of the coefficients reported in the paper. This makes them 
   comparable to streg coefficients. Thus coefficients estimated using the 
   versions of DURSEL after 1.9 will be the negative of those contained in the 
   paper.

5. The paper incorrectly reports results for a probit model that includes values    
   of pre-crisis tenure (including the interaction with autocracy) that are not 
   logged. The reported values in the DURSEL model do use the logarithmic 
   transformation. The substantive results are not affected by this error. The 
   included analysis file estimates both the model reported in the paper as well 
   as the correct model using the transformed values.

6. The model in the paper has an error in the coding of the tenure*democracy 
   interaction. We recoded the democracy variable to take into account updates 
   in the calculation of it in the Chiozza and Goemans data set that uses the 
   Jaggers and Gurr version, but did not udpate the interaction. Using the 
   updated variable results in four cases in our sample that change from 
   democracy to autocracy and one that goes from autocracy to democracy. Using 
   the corrected interaction term does not affect our results, but we include 
   the correct interaction term and run both versions of the model in the
   included analysis file.

7. The programs are written for Stata version 8 or above. If you have versions
   prior to this, you will have to change the version command in all three files.
   In addition, you must change two lines in bms2006ajps-simulate.do since the 
   output of one of Stata's commands (stcurve) changes.

8. You do not need to use the bms2006ajps-montecarlo.do file - just call the
   three programs individually. This program just streamlines the process. Just
   run the bms2006ajps-montecarlo-combine.do rpogram last to generate the
   tables and the graph (Figure 1).


VARIABLES:

These are the variables in the final Stata data set. See bms2006ajps-mkdata.dta 
for data labels (also below). See the paper for additional information on 
variable sources and construction. 

Here are the variables used in the analysis:


  wsurv tenl democ tendem rdpopl rwin enter democ autoc tennewL tennewAL tennew 
  tennewA total majpow lntropen rtcensor ccode dio year tendem_fix

ccode 		Singer and Small country code
autoc 		Autocracy. See Jaggers and Gurr 1995
democ 		Democracy. See Jaggers and Gurr 1995
lntropen 	Natural log of trade openness. Trade openness == trade 
		standardized by energy co
rwin 		Winners
dio 		died in office or still in
rdpopl 		log of battle deaths/10K pop
wsurv 		surv + .001 for weibull
tenl		Log tenure in office - post crisis leaders
tendem 		Log tenure in office - post crisis democratic leaders
majpow 		Major power (Small and Singer)
total 		Total bordering countries (COW)
enter 		Leader begins a war in the current year
rtcensor 	Leader dies before post-crisis tenure ends
tennew 		Tenure in office - all leaders
tennewA 	Tenure in office - all autocratic leaders
tennewL 	Log tenure in office - all leaders
tennewAL	Log tenure in office - all autocratic leaders
year		Year
tendem_fix	Updated tenure*democracy variable for the duration equation. See 
		note (6) above.
