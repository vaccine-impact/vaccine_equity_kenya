// Childhood vaccination coverage in Kenya by socioeconomic, geographic,
// maternal, child, and birth setting characteristics: equity impact analysis

// Survey analysis of 2014 Kenya DHS

// Using DHS datasets for analysis
// https://www.dhsprogram.com/data/Using-DataSets-for-Analysis.cfm

// log file
log using analysis.log, replace

// set working directory where DHS dataset is located
cd "C:\Users\kajam\OneDrive\Documents\GitHub\vaccine_equity_kenya\Stata"

// 2014 Kenya DHS dataset
// download from DHS upon registration
// https://www.dhsprogram.com/data/dataset/Kenya_Standard-DHS_2014.cfm
use KEKR72FL.dta, clear

// Sampling weights
// https://blog.dhsprogram.com/sampling-weighting-at-dhs/
// https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm

// 2-stage cluster sampling procedure
// survey weighting
// v005 - women's individual sample weight
// v021 - primary sampling unit
// v023 - stratification used in sample design
generate wgt = v005 / 1000000
svyset [pw = wgt], psu (v021) strata (v023)

// Install Stata programs for data analysis
// collin -- Collinearity diagnostics
// https://stats.idre.ucla.edu/stata/ado/analysis/
* findit collin
* net describe collin, from (https://stats.idre.ucla.edu/stat/stata/ado/analysis)
* net install collin
net install collin, from (https://stats.idre.ucla.edu/stat/stata/ado/analysis) replace

// -----------------------------------------------------------------------------
// Restrict data to living children aged 12-23 months and drop all other observations
tab b5
gen childalive = 0
replace childalive = 1 if inlist(b5, 1)  // child is alive (yes is coded as "1")
keep if childalive == 1
*Reduces n from 20,964 to 20,093 (deleted 871)

gen child=0
replace child = 1 if inlist(hw1, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)  // 12-23 months
keep if child==1
*Reduces n from 20,093 to 3,965 (deleted 16,128)
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  proportions of male and female children
svy: tab b4
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Generate new variables

// Generate new variable for maternal age (in months)
gen matage = v008 - v011

// Generate new variable for maternal age at birth
gen matagebirth = b3 - v011

// Generate new variables for each vaccine to categorize as vaccinated, not vaccinated, unknown
* BCG
gen bcg=h2
recode bcg 0=0 1/3=1 8=8
label var bcg "BCG"
label define labbcg 0 "No" 1 "Yes" 8 "Unknown"
label value bcg labbcg
tab bcg h2 // to verify coding works

* DPT1
gen dpt1=h3
recode dpt1 0=0 1/3=1 8=8
label var dpt1 "DPT1"
label define labdpt1 0 "No" 1 "Yes" 8 "Unknown"
label value dpt1 labdpt1
tab dpt1 h3 // to verify coding works

* DPT2
gen dpt2=h5
recode dpt2 0=0 1/3=1 8=8
label var dpt2 "DPT2"
label define labdpt2 0 "No" 1 "Yes" 8 "Unknown"
label value dpt2 labdpt2
tab dpt2 h5 // to verify coding works

* DPT3
gen dpt3=h7
recode dpt3 0=0 1/3=1 8=8
label var dpt3 "DPT3"
label define labdpt3 0 "No" 1 "Yes" 8 "Unknown"
label value dpt3 labdpt3
tab dpt3 h7 // to verify coding works

* Polio 0
gen polio0=h0
recode polio0 0=0 1/3=1 8=8
label var polio0 "Polio 0"
label define labpolio0 0 "No" 1 "Yes" 8 "Unknown"
label value polio0 labpolio0
tab polio0 h0 // to verify coding works

* Polio 1
gen polio1=h4
recode polio1 0=0 1/3=1 8=8
label var polio1 "Polio 1"
label define labpolio1 0 "No" 1 "Yes" 8 "Unknown"
label value polio1 labpolio1
tab polio1 h4 // to verify coding works

* Polio 2
gen polio2=h6
recode polio2 0=0 1/3=1 8=8
label var polio2 "Polio 2"
label define labpolio2 0 "No" 1 "Yes" 8 "Unknown"
label value polio2 labpolio2
tab polio2 h6 // to verify coding works

* Polio 3
gen polio3=h8
recode polio3 0=0 1/3=1 8=8
label var polio3 "Polio 3"
label define labpolio3 0 "No" 1 "Yes" 8 "Unknown"
label value polio3 labpolio3
tab polio3 h8 // to verify coding works

* Measles
gen measles=h9
recode measles 0=0 1/3=1 8=8
label var measles "Measles"
label define labmeasles 0 "No" 1 "Yes" 8 "Unknown"
label value measles labmeasles
tab measles h9 // to verify coding works

* PCV1
gen pcv1=spn1
recode pcv1 0=0 1/3=1 8=8
label var pcv1 "PCV1"
label define labpcv1 0 "No" 1 "Yes" 8 "Unknown"
label value pcv1 labpcv1
tab pcv1 spn1 // to verify coding works

* PCV2
gen pcv2=spn2
recode pcv2 0=0 1/3=1 8=8
label var pcv2 "PCV2"
label define labpcv2 0 "No" 1 "Yes" 8 "Unknown"
label value pcv2 labpcv2
tab pcv2 spn2 // to verify coding works

* PCV3
gen pcv3=spn3
recode pcv3 0=0 1/3=1 8=8
label var pcv3 "PCV3"
label define labpcv3 0 "No" 1 "Yes" 8 "Unknown"
label value pcv3 labpcv3
tab pcv3 spn3 // to verify coding works
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Drop unknown and missing values
keep if bcg     == 0 | bcg     == 1
keep if dpt1    == 0 | dpt1    == 1
keep if dpt2    == 0 | dpt2    == 1
keep if dpt3    == 0 | dpt3    == 1
keep if polio1  == 0 | polio1  == 1
keep if polio2  == 0 | polio2  == 1
keep if polio3  == 0 | polio3  == 1
keep if measles == 0 | measles == 1
keep if pcv1    == 0 | pcv1    == 1
keep if pcv2    == 0 | pcv2    == 1
keep if pcv3    == 0 | pcv3    == 1
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Generate new variable for full vaccination vs. not full vaccination 
// (excluding polio dose given at birth)
gen fullvac=0
replace fullvac = 1 if inlist(bcg, 1) & inlist(dpt1, 1) & inlist(dpt2, 1) & inlist(dpt3, 1) & inlist(polio1, 1) & inlist(polio2, 1) & inlist(polio3, 1) & inlist(measles, 1) & inlist(pcv1, 1) & inlist(pcv2, 1) & inlist(pcv3, 1)
tab fullvac

// Generate new variables for basic vaccination
gen basicvac=0
replace basicvac = 1 if inlist(bcg, 1) & inlist(dpt1, 1) & inlist(dpt2, 1) & inlist(dpt3, 1) & inlist(polio1, 1) & inlist(polio2, 1) & inlist(polio3, 1) & inlist(measles, 1)
tab basicvac

// Generate new variable for all three DPT vaccinations
gen dptfull=0
replace dptfull = 1 if inlist(dpt1, 1) & inlist(dpt2, 1) & inlist(dpt3, 1)

// Generate new variable for all three polio vaccinations (excluding polio dose given at birth)
gen poliofull=0
replace poliofull = 1 if inlist(polio1, 1) & inlist(polio2, 1) & inlist(polio3, 1)

// Generate new variable for all three PCV vaccinations
gen pcvfull=0
replace pcvfull = 1 if inlist(pcv1, 1) & inlist(pcv2, 1) & inlist(pcv3, 1)
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Recode and relabel variables of interest 

// Recode birth order into four groups 
gen birthord=bord
recode birthord 1=1 2/3=2 4/5=3 6/14=4
label var birthord "Birth order - Few"
label define labbirthord 1 "1" 2 "2-3" 3 "4-5" 4 "6+"
label value birthord labbirthord
tab birthord bord

// Recode maternal age at birth into three groups
gen matagebirth3=matagebirth
recode matagebirth3 min/239=1 240/359=2 360/max=3
label var matagebirth3 "Maternal age at birth"
label define labmatagebirth3 1 "10-19" 2 "20-29" 3 "30+"
label value matagebirth3 labmatagebirth3
// tab matagebirth3 matagebirth     // too many values

// Recode household head into a binary variable
gen motherhhd=v150
recode motherhhd 1=1 2/12=2
label var motherhhd "Head of household - Mother"
label define labmotherhhd 1 "Mother" 2 "Other"
label value motherhhd labmotherhhd
tab motherhhd v150

// Recode place of delivery into home (own home or other's homes), clinical setting (public or private), and other 
gen delivery=m15
recode delivery 11/12=1 21/36=2 96=3
label var delivery "Delivery setting"
label define labdelivery 1 "Home" 2 "Clinical setting" 3 "Other"
label value delivery labdelivery
tab delivery m15

// Recode religion to group Christian faiths
gen religion=v130
recode religion 3=1 4=2 1/2=3 96=4
label var religion "Religion"
label define labreligion 1 "Muslim" 2 "No religion" 3 "Christian" 4 "Other"
label value religion labreligion
tab religion v130

// Recode ethnicity into fewer groups
gen ethnic2=v131
recode ethnic2 11=1 8=2 21=3 7=4 2=5 6=6 3=7 4=8 1=9 5=9 9/10=9 12/20=9 22=9 96=9
label var ethnic2 "Ethicity"
label define labethnic2 1 "Somali" 2 "Maasai" 3 "Mbere" 4 "Luo" 5 "Kalenjin" 6 "Luhya" 7 "Kamba" 8 "Kikuya" 9 "Other"
label value ethnic2 labethnic2

// similar to region, recode so they are ordered from lowest to highest coverage
gen ethnic=v131
recode ethnic 1=7 2=3 3=5 4=6 5=7 6=4 7=2 8/10=7 11=1 12/96=7
label var ethnic "Ethicity"
label define labethnic 1 "Somali" 2 "Luo" 3 "Kalenjin" 4 "Luhya" 5 "Kamba" 6 "Kikuya" 7 "Other"
label value ethnic labethnic

// Recode and rename remaining variables with user-friendly labels

// Recode sex of child
gen sex=b4
recode sex 1=1 2=2
label var sex "Child sex"
label define labsex 1 "Male" 2 "Female"
label value sex labsex
tab sex b4

// Recode maternal education status
gen educ=v106
recode educ 0=1 1=2 2=3 3=4
label var educ "Maternal education"
label define labeduc 1 "No education" 2 "Primary" 3 "Secondary" 4 "Higher"
label value educ labeduc
tab educ v106

// Recode partnership status
gen union=v502
recode union 0=3 1=2 2=1
label var union "Partnership status"
label define labunion 1 "Formerly" 2 "Currently" 3 "Never"
label value union labunion
tab union v502

// Recode wealth status
gen wealth=v190
label var wealth "Wealth index"
label define labwealth 1 "Poorest" 2 "Poorer" 3 "Middle" 4 "Richer" 5 "Richest"
label value wealth labwealth
tab wealth v190

// Recode rural/urban area of residence
gen rural=v025
recode rural 1=2 2=1
label var rural "Rural-Urban"
label define labrural 1 "Rural" 2 "Urban"
label value rural labrural
tab rural v025

// Recode region
gen region=v024
recode region 1=5 2=1 3=8 4=7 5=3 7=6 8=4 9=2
label var region "Region"
label define labregion 1 "North eastern" 2 "Nairobi" 3 "Rift Valley" 4 "Nyanza" 5 "Coast" 6 "Western" 7 "Central" 8 "Eastern"
label value region labregion
tab region v024
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Descriptive characteristics

// *****************************************************************************
// Table: Vaccination coverage in Kenya at the national level
// Mean coverage and 95% CI, by vaccine
svy: mean bcg
svy: mean dpt1
svy: mean dpt2
svy: mean dpt3
svy: mean dptfull
svy: mean polio1
svy: mean polio2
svy: mean polio3
svy: mean poliofull
svy: mean measles
svy: mean pcv1
svy: mean pcv2
svy: mean pcv3
svy: mean pcvfull
svy: mean fullvac
// *****************************************************************************

// *****************************************************************************
// Table: Inequities in full immunisation coverage in Kenya associated with
// socioeconomic, geographic, maternal, child, and place of birth characteristics.
 
// *****************************************************************************
// table column: Children in each subgroup (%)
svy: tab fullvac
svy: tab sex
svy: tab birthord
svy: tab matagebirth3
svy: tab delivery
svy: tab educ
svy: tab union
svy: tab motherhhd
svy: tab wealth
svy: tab religion
svy: tab ethnic2
svy: tab rural
svy: tab region
// *****************************************************************************

// *****************************************************************************
// Full vaccination coverage in each subgroup
// table column: Full vaccination coverage (%) + 95% confidence intervals
svy: proportion fullvac, over (sex)
svy: proportion fullvac, over (birthord)
svy: proportion fullvac, over (matagebirth3)
svy: proportion fullvac, over (delivery)
svy: proportion fullvac, over (educ)
svy: proportion fullvac, over (union)
svy: proportion fullvac, over (motherhhd)
svy: proportion fullvac, over (wealth)
svy: proportion fullvac, over (religion)
svy: proportion fullvac, over (ethnic2)
svy: proportion fullvac, over (rural)
svy: proportion fullvac, over (region)
// *****************************************************************************
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Inferential characteristics

// *****************************************************************************
// Crude OR and 95% CI
// table column: Crude odds ratio (OR and 95% CI) + p-value
svy: logistic fullvac i.sex, base
svy: logistic fullvac i.birthord, base
svy: logistic fullvac i.matagebirth3, base
svy: logistic fullvac i.delivery, base
svy: logistic fullvac i.educ, base
svy: logistic fullvac i.union, base
svy: logistic fullvac i.motherhhd, base
svy: logistic fullvac i.wealth, base
svy: logistic fullvac i.religion, base
svy: logistic fullvac i.ethnic2, base
svy: logistic fullvac i.rural, base
svy: logistic fullvac i.region, base
// *****************************************************************************

/* 
Regression diagnostics:
https://stats.idre.ucla.edu/stata/webbooks/reg/chapter2/stata-webbooksregressionwith-statachapter-2-regression-diagnostics/

Collinearity:
http://www.philender.com/courses/categorical/notes2/collin.html
*/


// -----------------------------------------------------------------------------
// Multivariable regression of full model with 12 explanatory variables
svy: logistic fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.religion i.ethnic2 i.rural i.region, base

// -----------------------------------------------------------------------------
// Check for collinearity
// Multicollinearity occurs when there are high correlations among predictor variables, 
// leading to unreliable and unstable estimates of regression coefficients. 

regress fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.religion i.ethnic2 i.rural i.region, base
estat vif

// Check impact of removing region on collinearity 
// check for collinearity between region and ethnicity
regress fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.religion i.ethnic2 i.rural, base
estat vif

// Check impact of removing ethnicity on collinearity 
// check for collinearity between ethnicity and region
regress fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.religion i.rural i.region, base
estat vif
// remove ethnicity

// Check impact of removing ethnicity and religion on collinearity 
// check for collinearity between religion and region
regress fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.rural i.region, base
estat vif
// remove ethnicity and religion

// Check impact of removing maternal age at birth on collinearity
// check for collinearity between maternal age and birth order
regress fullvac i.sex i.birthord i.delivery i.educ i.union i.motherhhd i.wealth i.rural i.region, base
estat vif
// keep maternal age at birth

// Full test for collinearity
collin sex birthord matagebirth3 delivery educ union motherhhd wealth religion ethnic2 rural region
// -----------------------------------------------------------------------------

// *****************************************************************************
// Final regression model, with all variables (except ethnicity, religion)
// table column: Adjusted odds ratio (AOR and 95% CI) + p-value
svy: logistic fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth i.rural i.region, base
// *****************************************************************************

// -----------------------------------------------------------------------------
// Tests for effect modification
** Check sample sizes in each category
tab matagebirth3 educ, col
tab wealth matagebirth3, col
tab wealth educ, col
tab wealth rural, col
tab rural delivery, col

** Recode education to be binary variable
gen educbin=educ
recode educbin 1=1 2/4=2
label var educbin "Education - Binary"
label define labeducbin 1 "None" 2 "Some"
label value educbin labeducbin
tab educbin educ

* Test for interaction -- Maternal age at birth and maternal education
svy: logistic fullvac i.sex i.birthord i.matagebirth3##i.educbin i.delivery i.union i.motherhhd i.wealth i.rural i.region, base
contrast matagebirth3##educbin
margins matagebirth3##educbin
marginsplot
margins r.educbin@matagebirth3
marginsplot, yline(0)

* Test for interaction -- Household wealth and maternal age at birth
svy: logistic fullvac i.sex i.birthord i.delivery i.educ i.union i.motherhhd i.wealth##i.matagebirth3 i.rural i.region, base
contrast wealth##matagebirth3
margins wealth##matagebirth3
marginsplot
margins r.matagebirth3@wealth
marginsplot, yline(0)

* Test for interaction -- Household wealth and maternal education
svy: logistic fullvac i.sex i.birthord i.matagebirth3 i.delivery i.union i.motherhhd i.wealth##i.educbin i.rural i.region, base
contrast wealth##educbin
margins wealth##educbin
marginsplot
margins r.educbin@wealth
marginsplot, yline(0)

* Test for interaction -- Household wealth and place of residence
svy: logistic fullvac i.sex i.birthord i.matagebirth3 i.delivery i.educ i.union i.motherhhd i.wealth##i.rural i.region, base
contrast wealth##rural
margins wealth##rural
marginsplot
margins r.rural@wealth
marginsplot, yline(0)
// -----------------------------------------------------------------------------

log close
