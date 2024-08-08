* Function to generate lagged variables
program define generate_lags
    syntax varlist, prefix(string)
    foreach var of varlist `varlist' {
        forvalues k = 1/4 {
            gen `prefix'`var'_t`k' = `var'[_n-`k']
        }
    }
end

* Function to perform time series regression and store results
program define time_series_regression
    syntax varlist, depvar(string) prefix(string)
    foreach var of varlist `varlist' {
        capture preserve
        sort company time
        xtreg `depvar' `var', fe
        predict b_`prefix'`var', xb
        predict se_`prefix'`var', stdp
        gen beta_`prefix'`var' = b_`prefix'`var'
        gen stderr_`prefix'`var' = se_`prefix'`var'
        restore
    }
end

* Function to perform Fama-Macbeth regression
program define fama_macbeth_regression
    syntax varlist, depvar(string) store(string)
    xtfmb `depvar' `varlist', verbose lag(4)
    est store `store'
end


gen time = yq(year, quarter)
format time %tq

xtset company time
sort company time

*******************************************************
*****************Table 2 Replication*******************
*******************************************************

* Create the lagged variables
forvalues k = 1/4 {
    gen dsue_t`k' = dsue[_n-`k']
}

********************* Panel A: SUE Persistence Regression *********************
time_series_regression dsue_t1 dsue_t2 dsue_t3 dsue_t4, depvar(dsue) prefix(dsue_)

global regvars "dsue_t1 dsue_t2 dsue_t3 dsue_t4"
fama_macbeth_regression $regvars, depvar(dsue) store(m1)

******************** Panel B: 3-Day Abnormal Returns Regression********************
time_series_regression dsue_t1 dsue_t2 dsue_t3 dsue_t4, depvar(ff_w1) prefix(ff_w1_)

fama_macbeth_regression $regvars, depvar(ff_w1) store(m2)

********************* Panel C: Quarter-Long Abnormal Returns Regression **************
time_series_regression dsue_t1 dsue_t2 dsue_t3 dsue_t4, depvar(ff_d60) prefix(ff_d60_)

fama_macbeth_regression $regvars, depvar(ff_d60) store(m3)

*******************************************************
*****************Table 3 Replication*******************
*******************************************************

sort company time

* Calculate the average market cap for the most recent eight quarters
by company: gen market_cap_avg8 = (market_cap[_n-1] + market_cap[_n-2] + market_cap[_n-3] + market_cap[_n-4] + market_cap[_n-5] + market_cap[_n-6] + market_cap[_n-7] + market_cap[_n-8]) / 8

* Use rangestat to calculate the rolling standard deviation for each company
rangestat (sd) earnings_var = epsfxq, int(time -8 -1) by(company)
gen earnings_volatility = earnings_var / market_cap_avg8

* Deciles and quintiles of quarterly earnings volatility
xtile evol_decile = earnings_volatility, nq(10)
gen evol = (evol_decile - 0.5) / 9
xtile evol_quintile = earnings_volatility, nq(5)

**********Panel A: Foster Model********************
bysort company: gen earnings_diff = epsfxq - epsfxq[_n-3]
bysort company: gen dsue_diff = dsue - dsue[_n-1]

foreach q in 1 2 3 4 5 {
    reg earnings_diff dsue_diff if evol_quintile == `q'
}

* Calculate difference between Q1 and Q5
reg earnings_diff dsue_diff if evol_quintile == 1
est store Q1
reg earnings_diff dsue_diff if evol_quintile == 5
est store Q5
suest Q1 Q5
test [Q1_mean=Q5_mean]

***********Panel B: AR(1) Model*********************
bysort company: gen epsfxq_lag = epsfxq[_n-1]

foreach q in 1 2 3 4 5 {
    reg epsfxq epsfxq_lag if evol_quintile == `q'
}

* Calculate difference between Q1 and Q5
reg epsfxq epsfxq_lag if evol_quintile == 1
est store Q1
reg epsfxq epsfxq_lag if evol_quintile == 5
est store Q5
suest Q1 Q5
test [Q1_mean=Q5_mean]

*******************************************************
*****************Table 4 Replication*******************
*******************************************************

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
global regvars1 "dsue_t1 evol_dsue evol"
time_series_regression dsue_t1 evol_dsue evol, depvar(dsue) prefix(dsue_model1_)
fama_macbeth_regression $regvars1, depvar(dsue) store(m1)

***************** Model 2 *******************
global regvars2 "dsue_t1 evol_dsue evol size_dsue size"
time_series_regression dsue_t1 evol_dsue evol size_dsue size, depvar(dsue) prefix(dsue_model2_)
fama_macbeth_regression $regvars2, depvar(dsue) store(m2)

***************** Model 3 *********************
global regvars3 "dsue_t1 evol_dsue evol loss_dsue loss"
time_series_regression dsue_t1 evol_dsue evol loss_dsue loss, depvar(dsue) prefix(dsue_model3_)
fama_macbeth_regression $regvars3, depvar(dsue) store(m3)

***************** Model 4 ***************************
global regvars4 "dsue_t1 evol_dsue evol size_dsue size loss_dsue loss"
time_series_regression dsue_t1 evol_dsue evol size_dsue size loss_dsue loss, depvar(dsue) prefix(dsue_model4_)
fama_macbeth_regression $regvars4, depvar(dsue) store(m4)

***************** Model 5 ****************************
global regvars5 "dsue_t1 evol_dsue evol size_dsue size loss_dsue loss I_dsue I"
time_series_regression dsue_t1 evol_dsue evol size_dsue size loss_dsue loss I_dsue I, depvar(dsue) prefix(dsue_model5_)
fama_macbeth_regression $regvars5, depvar(dsue) store(m5)

*******************************************************
*****************Table 5 Replication*******************
*******************************************************

**********Panel A: 3-Day Returns**********************
*********************** Model 1 **********************
global regvars1 "dsue evol_dsue evol size size_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue, depvar(ff_w1) prefix(ff_w1_model1_)
fama_macbeth_regression $regvars1, depvar(ff_w1) store(m1)

*********************** Model 2 **********************
global regvars2 "dsue evol_dsue evol size size_dsue loss loss_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue loss loss_dsue, depvar(ff_w1) prefix(ff_w1_model2_)
fama_macbeth_regression $regvars2, depvar(ff_w1) store(m2)

*********************** Model 3 **********************
global regvars3 "dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue loss loss_dsue I I_dsue, depvar(ff_w1) prefix(ff_w1_model3_)
fama_macbeth_regression $regvars3, depvar(ff_w1) store(m3)

***************Panel B: Quarterly Returns**********************
*********************** Model 1 **********************
global regvars1 "dsue evol_dsue evol size size_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue, depvar(ff_d60) prefix(ff_d60_model1_)
fama_macbeth_regression $regvars1, depvar(ff_d60) store(m1)

*********************** Model 2 **********************
global regvars2 "dsue evol_dsue evol size size_dsue loss loss_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue loss loss_dsue, depvar(ff_d60) prefix(ff_d60_model2_)
fama_macbeth_regression $regvars2, depvar(ff_d60) store(m2)

*********************** Model 3 **********************
global regvars3 "dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue loss loss_dsue I I_dsue, depvar(ff_d60) prefix(ff_d60_model3_)
fama_macbeth_regression $regvars3, depvar(ff_d60) store(m3)

*******************************************************
*****************Table 6 Replication*******************
*******************************************************
bysort company: gen size_t1 = size[_n+1]
gen size_t1_dsue = size_t1 * dsue_diff

* Define Mishkin Model
preserve
drop if missing(dsue) | missing(dsue_t1) | missing(earnings_volatility) | missing(evol_dsue) | missing(size_t1) | missing(size_t1_dsue)

* First Equation
nl (dsue_t1 = {a} + {b} * dsue + {c} * earnings_volatility + {d} * evol_dsue)

* Second Equation (3-Day)
nl (ff_w1 = {a_star} - {beta} * dsue_t1 - {beta_b_star} * dsue - {beta_c_star} * evol - {beta_d_star} * evol_dsue + {g} * size_t1 + {h} * size_t1_dsue)

* Second Equation (Quarter-Long)
nl (ff_d60 = {a_star} - {beta} * dsue_t1 - {beta_b_star} * dsue - {beta_c_star} * evol - {beta_d_star} * evol_dsue + {g} * size_t1 + {h} * size_t1_dsue)
restore

*******************************************************
*****************Table 9 Replication*******************
*******************************************************

****************Panel A: Dependent Var DSUE_t+1*******************
********************* Model 1 **********************
global regvars1 "dsue evol_dsue evol size"
time_series_regression dsue_t1 evol_dsue evol size, depvar(dsue) prefix(dsue_t1_model1_)
fama_macbeth_regression $regvars1, depvar(dsue_t1) store(m1)

********************* Model 2 **********************
global regvars2 "dsue evol_dsue evol size size_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue, depvar(dsue) prefix(dsue_t1_model2_)
fama_macbeth_regression $regvars2, depvar(dsue_t1) store(m2)

****************Panel B: Dependent Var AR_t+1*********
* 3-Day Returns Regression
*********************** Model 1 **********************
global regvars3 "dsue evol_dsue evol size"
time_series_regression dsue_t1 evol_dsue evol size, depvar(ff_w1) prefix(ff_w1_model1_)
fama_macbeth_regression $regvars3, depvar(ff_w1) store(m3)

*********************** Model 2 **********************
global regvars4 "dsue evol_dsue evol size size_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue, depvar(ff_w1) prefix(ff_w1_model2_)
fama_macbeth_regression $regvars4, depvar(ff_w1) store(m4)

* Quarterly Returns Regression
*********************** Model 1 **********************
global regvars5 "dsue evol_dsue evol size"
time_series_regression dsue_t1 evol_dsue evol size, depvar(ff_d60) prefix(ff_d60_model1_)
fama_macbeth_regression $regvars5, depvar(ff_d60) store(m5)

*********************** Model 2 **********************
global regvars6 "dsue evol_dsue evol size size_dsue"
time_series_regression dsue_t1 evol_dsue evol size size_dsue, depvar(ff_d60) prefix(ff_d60_model2_)
fama_macbeth_regression $regvars6, depvar(ff_d60) store(m6)
