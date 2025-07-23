*codes for health event data cleaning
version 15.1
*Woking directory 
 cd "C:\Users\user\Desktop\Chapter 5\Codes and shapefiles\CODES"
*Dataset
use "Sea lice data.dta"

*filter data
keep if inlist(month,4,5,6)/*keep months from April to June)*/
keep if inlist(fish_spec,2,6) /*keep only pink and chum*/
keep if inlist(sampling_unit,4,6,9)/*select only one migration route*/
drop if source==4/*Marine Environmental Research Program was not considererd in the analysis*/
*generate a grouping variable to inuclde as a random effect
egen unit_yearmonth= group(sampling_unit year month)


***********************************************************************
***********************MOTILES (any species)***************************
***********************************************************************


*Mixed-effect logistic model
*2003-2009

preserve

*filter year
drop if year>2009
drop if year==2005 /*Martin Krkosek's team did not collect data in this year*/

*Univariable mixed-effect logistic regression
melogit sealicemot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicemot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicemot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicemot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicemot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicemot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit


*multivariable mixed-effect logistic regression
melogit sealicemot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc

*pairwise comparision
pwcompare i.source , pveffects groups mcompare(bonferroni)

*Pairwise log-odds with 95% CI
lincom 5.source - 2.source, level(95)       // Krkosek vs DFO
lincom 6.source - 2.source, level(95)       // Raincoast vs DFO
lincom 6.source - 5.source, level(95)       // Raincoast vs Krkosek

*Exponentiated contrasts (Odds Ratios) with 95% CI
lincom 5.source - 2.source, eform level(95) // Krkosek vs DFO
lincom 6.source - 2.source, eform level(95) // Raincoast vs DFO
lincom 6.source - 5.source, eform level(95) // Raincoast vs Krkosek


restore

***********************motiles (any species)***************************
*Mixed-effect logistic model
*2010-2012
preserve

*filter year
drop if year<2010
drop if year>2012

*Univariable mixed-effect logistic model
melogit sealicemot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicemot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicemot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicemot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicemot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicemot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect logistic model
melogit sealicemot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc
restore

***********************motiles (any species)***************************
*Mixed-effect logistic model
*2016-2023
preserve

*filter year
drop if year<2016


*Univariable mixed-effect logistic model
melogit sealicemot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicemot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicemot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicemot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicemot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicemot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect logistic model
melogit sealicemot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or
testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc
restore




***********************motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2003-2009
preserve

*filter year
drop if year>2009
drop if year==2005 /*Martin Krkosek's team did not collect data in this year*/

*Univariable mixed-effect negative binomial model
menbreg sealice_mot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_mot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_mot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_mot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_mot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_mot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_mot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*pairwise comparision
pwcompare i.source , pveffects groups mcompare(bonferroni)

* Step 2: Pairwise log-odds comparisons with 95% CI
lincom 5.source - 2.source, level(95)       // Krkosek vs DFO
lincom 6.source - 2.source, level(95)       // Raincoast vs DFO
lincom 6.source - 5.source, level(95)       // Raincoast vs Krkosek

* Step 3: Exponentiated contrasts (Odds Ratios) with 95% CI
lincom 5.source - 2.source, eform level(95) // Krkosek vs DFO
lincom 6.source - 2.source, eform level(95) // Raincoast vs DFO
lincom 6.source - 5.source, eform level(95) // Raincoast vs Krkosek


*Get observed vs predicted proportion of zeros
count if sealice_mot==0
scalar obs_zeros = r(N) //save number of zero counts

count sealice_mot
scalar total_obs = r(N) //save number total obseravations counts
scalar prop_obs_zeros = obs_zeros / total_obs
display "Observed proportion of zeros" = prop_obs_zeros


* Mean predicted probability of zero
summarize p0
scalar prop_pred_zeros = r(mean)
display "Mean predicted probability of zero (p0) = " prop_pred_zeros


restore

***********************motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2010-2012
preserve

*filter year
drop if year<2010
drop if year>2012 

*Univariable mixed-effec negative binomial model
menbreg sealice_mot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_mot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_mot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_mot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_mot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_mot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_mot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

restore

***********************motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2016-2023
preserve

*filter year
drop if year<2016


*Univariable mixed-effec negative binomial model
menbreg sealice_mot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_mot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_mot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_mot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_mot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_mot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_mot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

restore


***********************************************************************
***********************NON-MOTILES (any species)***************************
***********************************************************************


*Mixed-effect logistic model
*2003-2009

preserve

*filter year
drop if year>2009
drop if year==2005 /*Martin Krkosek's team did not collect data in this year*/

*Univariable mixed-effect logistic regression
melogit sealicenmot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicenmot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicenmot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicenmot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicenmot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicenmot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit


*multivariable mixed-effect logistic regression
melogit sealicenmot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc

*pairwise comparision
pwcompare i.source , pveffects groups mcompare(bonferroni)

*Pairwise log-odds with 95% CI
lincom 5.source - 2.source, level(95)       // Krkosek vs DFO
lincom 6.source - 2.source, level(95)       // Raincoast vs DFO
lincom 6.source - 5.source, level(95)       // Raincoast vs Krkosek

*Exponentiated contrasts (Odds Ratios) with 95% CI
lincom 5.source - 2.source, eform level(95) // Krkosek vs DFO
lincom 6.source - 2.source, eform level(95) // Raincoast vs DFO
lincom 6.source - 5.source, eform level(95) // Raincoast vs Krkosek

restore

***********************Non-motiles (any species)***************************
*Mixed-effect logistic model
*2010-2012

preserve

*filter year
drop if year<2010
drop if year>2012 

*Univariable mixed-effect logistic regression
melogit sealicenmot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicenmot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicenmot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicenmot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicenmot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicenmot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit


*multivariable mixed-effect logistic regression
melogit sealicenmot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc
restore

***********************Non-motiles (any species)***************************
*Mixed-effect logistic model
*2016-2023

preserve

*filter year
drop if year<2016 

*Univariable mixed-effect logistic regression
melogit sealicenmot_bin i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealicenmot_bin i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealicenmot_bin i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealicenmot_bin i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealicenmot_bin i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealicenmot_bin i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit


*multivariable mixed-effect logistic regression
melogit sealicenmot_bin i.source i.length_cat i.fish_spec  i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:,or

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*ICC
estat icc
restore

***********************Non-motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2003-2009
preserve

*filter year
drop if year>2009
drop if year==2005 /*Martin Krkosek's team did not collect data in this year*/

*Univariable mixed-effect negative binomial model
menbreg sealice_nmot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_nmot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_nmot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_nmot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_nmot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_nmot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_nmot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

*pairwise comparision
pwcompare i.source , pveffects groups mcompare(bonferroni)

* Step 2: Pairwise log-odds comparisons with 95% CI
lincom 5.source - 2.source, level(95)       // Krkosek vs DFO
lincom 6.source - 2.source, level(95)       // Raincoast vs DFO
lincom 6.source - 5.source, level(95)       // Raincoast vs Krkosek

* Step 3: Exponentiated contrasts (Odds Ratios) with 95% CI
lincom 5.source - 2.source, eform level(95) // Krkosek vs DFO
lincom 6.source - 2.source, eform level(95) // Raincoast vs DFO
lincom 6.source - 5.source, eform level(95) // Raincoast vs Krkosek

restore

***********************Non-motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2010-2012
preserve

*filter year
drop if year<2010
drop if year>2012 

*Univariable mixed-effec negative binomial model
menbreg sealice_nmot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_nmot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_nmot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_nmot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_nmot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_nmot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_nmot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

restore

***********************Non-motiles (any species)***************************
*Mixed-effect negative binomial model logistic model
*2016-2023
preserve

*filter year
drop if year<2016


*Univariable mixed-effec negative binomial model
menbreg sealice_nmot i.source||unit_yearmonth:|| event_id:
testparm i.source
melogit sealice_nmot i.length_cat||unit_yearmonth:|| event_id:
testparm i.length_cat
melogit sealice_nmot i.fish_spec||unit_yearmonth:|| event_id:
testparm i.fish_spec
melogit sealice_nmot i.year||unit_yearmonth:|| event_id:
testparm i.year
melogit sealice_nmot i.month||unit_yearmonth:|| event_id:
testparm i.month
melogit sealice_nmot i.sampling_unit||unit_yearmonth:|| event_id:
testparm i.sampling_unit

*Multivariable mixed-effect negative binomial model
menbreg sealice_nmot i.source i.length_cat i.fish_spec i.year i.month i.sampling_unit ||unit_yearmonth:|| event_id:, irr

testparm i.source
testparm i.length_cat
testparm i.fish_spec
testparm i.year
testparm i.month
testparm i.sampling_unit

restore
