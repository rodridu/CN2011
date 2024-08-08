**********************************************************************************************
****   Purpose:      Replication of Cao and Naranmoothy 2011 paper                           *
****   Output:       The tables showed up in the paper                                       *
****   Notes:        -                                                                       *
****   Author:       Hongzhen Du                                                             *
****   Last updated: 25 Aug, 2024.                                                           * 
**********************************************************************************************

gen time = yq(year, quarter)
format time %tq

xtset company time
sort company time

*******************************************************
*****************Table 2 Replication*******************
*******************************************************

putdocx begin

putdocx paragraph, style(Heading1)
putdocx text ("Table 2 Replication")

* Create the lagged variables
forvalues k = 1/4 {
    gen dsue_t`k' = dsue[_n-`k']
}

********************* Panel A: SUE Persistence Regression *********************
putdocx paragraph, style(Heading2)
putdocx text ("Panel A: SUE Persistence Regression")

* Step 1: Time Series Regression using xtreg
forvalues k = 1/4 {
    capture preserve
    sort company time

    * Fixed Effect Regression for each lagged variable
    xtreg dsue dsue_t`k', fe

    * Store the coefficients and standard errors
    predict b_dsue_t`k', xb
    predict se_dsue_t`k', stdp

    * Optionally, store these results in a new dataset or variables for further analysis
    gen beta_dsue_t`k' = b_dsue_t`k'
    gen stderr_dsue_t`k' = se_dsue_t`k'

    restore
}

* Step 2: Fama-Macbeth Regression using xtfmb
tsset company time

global regvars "dsue_t1 dsue_t2 dsue_t3 dsue_t4"
xtfmb dsue $regvars, verbose lag(4)
putdocx paragraph
putdocx text ("Fama-Macbeth Regression Results")
putdocx table table2_panelA = etable
est store m1

******************** Panel B: 3-Day Abnormal Returns Regression********************
putdocx paragraph, style(Heading2)
putdocx text ("Panel B: 3-Day Abnormal Returns Regression")

* Step 1: Time Series Regression using xtreg
forvalues k = 1/4 {
    capture preserve
    sort company time

    xtreg ff_w1 dsue_t`k', fe

    * Store the coefficients and standard errors
    predict b_ff_w1_t`k', xb
    predict se_ff_w1_t`k', stdp

    * Optionally, store these results in a new dataset or variables for further analysis
    gen beta_ff_w1_t`k' = b_ff_w1_t`k'
    gen stderr_ff_w1_t`k' = se_ff_w1_t`k'

    restore
}

* Step 2: Fama-Macbeth Regression using xtfmb
tsset company time

xtfmb ff_w1 $regvars, verbose lag(4)
putdocx paragraph
putdocx text ("Fama-Macbeth Regression Results")
putdocx table table2_panelB = etable
est store m2

********************* Panel C: Quarter-Long Abnormal Returns Regression **************
putdocx paragraph, style(Heading2)
putdocx text ("Panel C: Quarter-Long Abnormal Returns Regression")

* Step 1: Time Series Regression using xtreg
forvalues k = 1/4 {
    capture preserve
    sort company time

    xtreg ff_d60 dsue_t`k', fe

    * Store the coefficients and standard errors
    predict b_ff_d60_t`k', xb
    predict se_ff_d60_t`k', stdp

    * Optionally, store these results in a new dataset or variables for further analysis
    gen beta_ff_d60_t`k' = b_ff_d60_t`k'
    gen stderr_ff_d60_t`k' = se_ff_d60_t`k'

    restore
}

* Step 2: Fama-Macbeth Regression using xtfmb
tsset company time

xtfmb ff_d60 $regvars, verbose lag(4)
putdocx paragraph
putdocx text ("Fama-Macbeth Regression Results")
putdocx table table2_panelC = etable
est store m3

*******************************************************
*****************Table 3 Replication*******************
*******************************************************

putdocx paragraph, style(Heading1)
putdocx text ("Table 3 Replication")

* Ensure the data is sorted by company and time
sort company time

* Calculate the average market cap for the most recent eight quarters
by company: gen market_cap_avg8 = ( market_cap[_n-1] + market_cap[_n-2] + market_cap[_n-3] + market_cap[_n-4] + market_cap[_n-5] + market_cap[_n-6] + market_cap[_n-7] + market_cap[_n-8]) / 8

* Use rangestat to calculate the rolling standard deviation for each company
rangestat (sd) earnings_var= epsfxq, int(time -8 -1) by(company)

gen earnings_volatility = earnings_var/market_cap_avg8

* Deciles of quarterly earnings volatility
xtile evol_decile = earnings_volatility, nq(10)

* Calculate EVOL
gen evol = (evol_decile - 0.5) / 9

* Quintiles of quarterly earnings volatility
xtile evol_quintile = earnings_volatility, nq(5)

**********Panel A: Foster Model********************
putdocx paragraph, style(Heading2)
putdocx text ("Panel A: Foster Model")

bysort company: gen earnings_diff = epsfxq - epsfxq[_n-3]
bysort company: gen dsue_diff = dsue - dsue[_n-1]

foreach q in 1 2 3 4 5 {
    reg earnings_diff dsue_diff if evol_quintile == `q'
    putdocx table table3_panelA_model`q' = etable
}

* Calculate difference between Q1 and Q5
reg earnings_diff dsue_diff if evol_quintile == 1
est store Q1
reg earnings_diff dsue_diff if evol_quintile == 5
est store Q5
suest Q1 Q5
test [Q1_mean=Q5_mean]
putdocx table table3_panelA_diff = etable

***********Panel B: AR(1) Model*********************
putdocx paragraph, style(Heading2)
putdocx text ("Panel B: AR(1) Model")

bysort company: gen epsfxq_lag = epsfxq[_n-1]

foreach q in 1 2 3 4 5 {
    reg epsfxq epsfxq_lag if evol_quintile == `q'
    putdocx table table3_panelB_model`q' = etable
}

* Calculate difference between Q1 and Q5
reg epsfxq epsfxq_lag if evol_quintile == 1
est store Q1
reg epsfxq epsfxq_lag if evol_quintile == 5
est store Q5
suest Q1 Q5
test [Q1_mean=Q5_mean]
putdocx table table3_panelB_diff = etable

*******************************************************
*****************Table 4 Replication*******************
*******************************************************

putdocx paragraph, style(Heading1)
putdocx text ("Table 4 Replication")

* Create the interaction lagged terms
gen evol_dsue = evol * dsue
gen size_dsue = size * dsue
gen loss = 0
replace loss = 1 if epsfxq < 0
gen loss_dsue = loss * dsue
gen I = 0
replace I = 1 if quarter == (quarter[_n+1] - 1) | quarter == (quarter[_n-1] + 1)
gen I_dsue = I * dsue

******************* Model 1 *****************
tsset company time
global regvars1 "dsue_t1 evol_dsue evol"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsue $regvars1, fe
predict b_dsue_model1, xb
predict se_dsue_model1, stdp
gen beta_dsue_model1 = b_dsue_model1
gen stderr_dsue_model1 = se_dsue_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsue $regvars1, verbose lag(4)
putdocx table table4_model1 = etable
est store m1

***************** Model 2 *******************
tsset company time
global regvars2 "dsue_t1 size_dsue size"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsue $regvars2, fe
predict b_dsue_model2, xb
predict se_dsue_model2, stdp
gen beta_dsue_model2 = b_dsue_model2
gen stderr_dsue_model2 = se_dsue_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsue $regvars2, verbose lag(4)
putdocx table table4_model2 = etable
est store m2

***************** Model 3 *********************
tsset company time
global regvars3 "dsue_t1 evol_dsue evol size_dsue size"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsue $regvars3, fe
predict b_dsue_model3, xb
predict se_dsue_model3, stdp
gen beta_dsue_model3 = b_dsue_model3
gen stderr_dsue_model3 = se_dsue_model3
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsue $regvars3, verbose lag(4)
putdocx table table4_model3 = etable
est store m3

***************** Model 4 ***************************
tsset company time
global regvars4 "dsue_t1 evol_dsue evol loss_dsue loss"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsue $regvars4, fe
predict b_dsue_model4, xb
predict se_dsue_model4, stdp
gen beta_dsue_model4 = b_dsue_model4
gen stderr_dsue_model4 = se_dsue_model4
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsue $regvars4, verbose lag(4)
putdocx table table4_model4 = etable
est store m4

***************** Model 5 ****************************
tsset company time
global regvars5 "dsue_t1 evol_dsue evol I_dsue I"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsue $regvars5, fe
predict b_dsue_model5, xb
predict se_dsue_model5, stdp
gen beta_dsue_model5 = b_dsue_model5
gen stderr_dsue_model5 = se_dsue_model5
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsue $regvars5, verbose lag(4)
putdocx table table4_model5 = etable
est store m5

*******************************************************
*****************Table 2 Replication*******************
*******************************************************

putdocx paragraph, style(Heading1)
putdocx text ("Table 5 Replication")

**********Panel A: 3-Day Returns**********************

*********************** Model 1 **********************
tsset company time

global regvars1 "dsue evol_dsue evol size size_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_w1 $regvars1, fe
predict b_ff_w1_model1, xb
predict se_ff_w1_model1, stdp
gen beta_ff_w1_model1 = b_ff_w1_model1
gen stderr_ff_w1_model1 = se_ff_w1_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_w1 $regvars1, verbose lag(4)
putdocx table table5_panelA_model1 = etable
est store m1

*********************** Model 2 **********************
tsset company time

global regvars2 "dsue evol_dsue evol size size_dsue loss loss_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_w1 $regvars2, fe
predict b_ff_w1_model2, xb
predict se_ff_w1_model2, stdp
gen beta_ff_w1_model2 = b_ff_w1_model2
gen stderr_ff_w1_model2 = se_ff_w1_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_w1 $regvars2, verbose lag(4)
putdocx table table5_panelA_model2 = etable
est store m2

*********************** Model 3 **********************
tsset company time

global regvars3 "dsue evol_dsue evol size size_dsue I I_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_w1 $regvars3, fe
predict b_ff_w1_model3, xb
predict se_ff_w1_model3, stdp
gen beta_ff_w1_model3 = b_ff_w1_model3
gen stderr_ff_w1_model3 = se_ff_w1_model3
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_w1 $regvars3, verbose lag(4)
putdocx table table5_panelA_model3 = etable
est store m3

***************Panel B: Quarterly Returns**********************

*********************** Model 1 **********************
tsset company time
global regvars1 "dsue evol_dsue evol size size_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_d60 $regvars1, fe
predict b_ff_d60_model1, xb
predict se_ff_d60_model1, stdp
gen beta_ff_d60_model1 = b_ff_d60_model1
gen stderr_ff_d60_model1 = se_ff_d60_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_d60 $regvars1, verbose lag(4)
putdocx table table5_panelB_model1 = etable
est store m1

*********************** Model 2 **********************
tsset company time
global regvars2 "dsue evol_dsue evol size size_dsue loss loss_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_d60 $regvars2, fe
predict b_ff_d60_model2, xb
predict se_ff_d60_model2, stdp
gen beta_ff_d60_model2 = b_ff_d60_model2
gen stderr_ff_d60_model2 = se_ff_d60_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_d60 $regvars2, verbose lag(4)
putdocx table table5_panelB_model2 = etable
est store m2

*********************** Model 3 **********************
tsset company time
global regvars3 "dsue evol_dsue evol size size_dsue I I_dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_d60 $regvars3, fe
predict b_ff_d60_model3, xb
predict se_ff_d60_model3, stdp
gen beta_ff_d60_model3 = b_ff_d60_model3
gen stderr_ff_d60_model3 = se_ff_d60_model3
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_d60 $regvars3, verbose lag(4)
putdocx table table5_panelB_model3 = etable
est store m3

*******************************************************
*****************Table 6 Replication*******************
*******************************************************

putdocx paragraph, style(Heading1)
putdocx text ("Table 6 Replication")

bysort company: gen size_t1 = size[_n+1]
gen size_t1_dsue = size_t1 * dsue_diff

* Define Mishkin Model
preserve

drop if missing(dsue) | missing(dsue_t1) | missing(earnings_volatility) | missing(evol_dsue) | missing(size_t1) | missing(size_t1_dsue)

* First Equation
nl (dsue_t1 = {a} + {b} * dsue + {c} * earnings_volatility + {d} * evol_dsue)
putdocx table table6_model1 = etable

* Second Equation (3-Day)
nl (ff_w1 = {a_star} - {beta} * dsue_t1 - {beta_b_star} * dsue - {beta_c_star} * evol - {beta_d_star} * evol_dsue + {g} * size_t1 + {h} * size_t1_dsue)
putdocx table table6_model2 = etable

* Second Equation (Quarter-Long)
nl (ff_d60 = {a_star} - {beta} * dsue_t1 - {beta_b_star} * dsue - {beta_c_star} * evol - {beta_d_star} * evol_dsue + {g} * size_t1 + {h} * size_t1_dsue)
putdocx table table6_model3 = etable

restore

*******************************************************
*****************Table 9 Replication*******************
*******************************************************

putdocx paragraph, style(Heading1)
putdocx text ("Table 9 Replication")

* Calculate dsuea: I/B/E/S actual minus I/B/E/S median forecast during the 90-day period before the earnings announcement date, scaled by price per share at quarter end
gen dsuea = (dsue - dsue_t3) / market_cap
gen evol_dsuea = evol * dsuea
gen size_dsuea = size * dsuea
gen dsuea_t1 = dsuea[_n+1]
****************Panel A: Dependent Var DSUEA_t_t+1*******************
********************* Model 1 **********************
tsset company time
global regvars1 "dsuea"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsuea_t1 $regvars1, fe
predict b_dsuea_t1_model1, xb
predict se_dsuea_t1_model1, stdp
gen beta_dsuea_t1_model1 = b_dsuea_t1_model1
gen stderr_dsuea_t1_model1 = se_dsuea_t1_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsuea_t1 $regvars1, verbose lag(4)
putdocx table table9_panelA_model1 = etable
est store m1

********************* Model 2 **********************
tsset company time
global regvars2 "dsuea evol_dsuea evol"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg dsuea_t1 $regvars2, fe
predict b_dsuea_t1_model2, xb
predict se_dsuea_t1_model2, stdp
gen beta_dsuea_t1_model2 = b_dsuea_t1_model2
gen stderr_dsuea_t1_model2 = se_dsuea_t1_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb dsuea_t1 $regvars2, verbose lag(4)
putdocx table table9_panelA_model2 = etable
est store m2

****************Panel B: Dependent Var AR_t+1*********
* 3-Day Returns Regression
*********************** Model 1 **********************
tsset company time
global regvars3 "dsuea evol_dsuea evol size size_dsuea"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_w1 $regvars3, fe
predict b_ff_w1_model1, xb
predict se_ff_w1_model1, stdp
gen beta_ff_w1_model1 = b_ff_w1_model1
gen stderr_ff_w1_model1 = se_ff_w1_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_w1 $regvars3, verbose lag(4)
putdocx table table9_panelB_3day_model1 = etable
est store m3

*********************** Model 2 **********************
tsset company time
global regvars4 "dsuea evol_dsuea evol size size_dsuea dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_w1 $regvars4, fe
predict b_ff_w1_model2, xb
predict se_ff_w1_model2, stdp
gen beta_ff_w1_model2 = b_ff_w1_model2
gen stderr_ff_w1_model2 = se_ff_w1_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_w1 $regvars4, verbose lag(4)
putdocx table table9_panelB_3day_model2 = etable
est store m4

* Quarterly Returns Regression
*********************** Model 1 **********************
tsset company time
global regvars5 "dsuea evol_dsuea evol size size_dsuea"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_d60 $regvars5, fe
predict b_ff_d60_model1, xb
predict se_ff_d60_model1, stdp
gen beta_ff_d60_model1 = b_ff_d60_model1
gen stderr_ff_d60_model1 = se_ff_d60_model1
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_d60 $regvars5, verbose lag(4)
putdocx table table9_panelB_quarterly_model1 = etable
est store m5

*********************** Model 2 **********************
tsset company time
global regvars6 "dsuea evol_dsuea evol size size_dsuea dsue"

* Step 1: Time Series Regression using xtreg
capture preserve
sort company time
xtreg ff_d60 $regvars6, fe
predict b_ff_d60_model2, xb
predict se_ff_d60_model2, stdp
gen beta_ff_d60_model2 = b_ff_d60_model2
gen stderr_ff_d60_model2 = se_ff_d60_model2
restore

* Step 2: Fama-Macbeth Regression using xtfmb
xtfmb ff_d60 $regvars6, verbose lag(4)
putdocx table table9_panelB_quarterly_model2 = etable
est store m6

putdocx save "Cao_Naranmoothy_Replication.docx", replace
