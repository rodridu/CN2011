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
xtdescribe
sort company time

// * Calculate abnormal_return
// gen abnormal_return = ff_d60 - ffm_d60
// sum abnormal_return

// * Define Long-Term Window：Start from 2 days after current announcement day, until the day before next announcement day 
// sort company time
// by company time: gen start_date = rdq + 2
// by company time: gen end_date = rdq[_n+1] - 1 if company == company[_n+1]
//
// * Calculate Abnormal Return for the Long-Term Window
// bys company time: egen long_window_return = total(abnormal_return) if datadate >= start_date & datadate <= end_date
//
// * Define Short-Term Window：One day before announcement day to one day after announcement day
// by company year quarter: gen day_before = rdq - 1
// by company year quarter: gen day_after = rdq + 1
//
// * Calculate Abnormal Return for the Short-Term-Term Window
// by company year quarter: gen short_window_return = abnormal_return[_n-1] + abnormal_return + abnormal_return[_n+1] if datadate >= day_before & datadate <= day_after


*******************************************************
*****************Table 2 Replication*******************
*******************************************************

* Create the lagged variables
forvalues k = 1/4 {
    gen dsue_t`k' = dsue[_n-`k']
}

* Panel A: SUE Persistence Regression
forvalues k = 1/4 {
    capture preserve
    sort company time

    * 使用 xtreg 进行固定效应回归
    xtreg dsue dsue_t`k', fe

    * 使用 Newey-West 标准误估计
    newey dsue dsue_t`k', lag(`=floor(4)') force

    restore
}

* Panel B: 3-Day Abnormal Returns Regression
forvalues k = 1/4 {
    capture preserve
    sort company time

    * 使用 xtreg 进行固定效应回归
    xtreg ff_w1 dsue_t`k', fe

    * 使用 Newey-West 标准误估计
    newey ff_w1 dsue_t`k', lag(`=floor(4)') force

    restore
}

* Panel C: Quarter-Long Abnormal Returns Regression
forvalues k = 1/4 {
    capture preserve
    sort company time

    * 使用 xtreg 进行固定效应回归
    xtreg ff_d60 dsue_t`k', fe

    * 使用 Newey-West 标准误估计
    newey ff_d60 dsue_t`k', lag(`=floor(4)') force

    restore
}

*******************************************************
*****************Table 3 Replication*******************
*******************************************************

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

***********Panel B:AR(1) Model*********************
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

* Model 1
xtreg dsue dsue_t1 evol_dsue evol, fe
newey dsue dsue_t1 evol_dsue evol, lag(`=floor(4)') force

* Model 2
xtreg dsue dsue_t1 evol_dsue evol size_dsue size, fe
newey dsue dsue_t1 evol_dsue evol size_dsue size, lag(`=floor(4)') force

* Model 3
xtreg dsue dsue_t1 evol_dsue evol loss_dsue loss, fe
newey dsue dsue_t1 evol_dsue evol loss_dsue loss, lag(`=floor(4)') force

* Model 4
xtreg dsue dsue_t1 evol_dsue evol size_dsue size loss_dsue loss, fe
newey dsue dsue_t1 evol_dsue evol size_dsue size loss_dsue loss, lag(`=floor(4)') force

* Model 5
xtreg dsue dsue_t1 evol_dsue evol size_dsue size loss_dsue loss I_dsue I, fe
newey dsue dsue_t1 evol_dsue evol size_dsue size loss_dsue loss I_dsue I, lag(`=floor(4)') force

*******************************************************
*****************Table 5 Replication*******************
*******************************************************

**********Panel A: 3-Day Returns**********************
* Model 1
sort company time
xtreg ff_w1 dsue evol_dsue evol, fe
newey ff_w1 dsue evol_dsue evol, lag(`=floor(4)') force

* Model 2
sort company time
xtreg ff_w1 dsue evol_dsue evol size size_dsue, fe
newey ff_w1 dsue evol_dsue evol size size_dsue, lag(`=floor(4)') force

* Model 3
sort company time
xtreg ff_w1 dsue evol_dsue evol size size_dsue loss loss_dsue, fe
newey ff_w1 dsue evol_dsue evol size size_dsue loss loss_dsue, lag(`=floor(4)') force

* Model 4
sort company time
xtreg ff_w1 dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue, fe
newey ff_w1 dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue, lag(`=floor(4)') force

***************Panel B: Quarterly Returns**********************
* Model 1
sort company time
xtreg ff_d60 dsue evol_dsue evol, fe
newey ff_d60 dsue evol_dsue evol, lag(`=floor(4)') force

* Model 2
sort company time
xtreg ff_d60 dsue evol_dsue evol size size_dsue, fe
newey ff_d60 dsue evol_dsue evol size size_dsue, lag(`=floor(4)') force

* Model 3
sort company time
xtreg ff_d60 dsue evol_dsue evol size size_dsue loss loss_dsue, fe
newey ff_d60 dsue evol_dsue evol size size_dsue loss loss_dsue, lag(`=floor(4)') force

* Model 4
sort company time
xtreg ff_d60 dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue, fe
newey ff_d60 dsue evol_dsue evol size size_dsue loss loss_dsue I I_dsue, lag(`=floor(4)') force

*******************************************************
*****************Table 6 Replication*******************
*******************************************************
bysort company:gen size_t1 = size[_n+1]
gen size_t1_dsue = size_t1*dsue_diff

* Define Mishkin Model
preserve

drop if missing(dsue) | missing(dsue_t1) | missing(earnings_volatility) | missing(evol_dsue) | missing(size_t1) | missing(size_t1_dsue)

* Define the nonlinear equation with initial values and estimate the parameters
nl (dsue_t1 = {a} + {b}*dsue + {c}*earnings_volatility + {d}*evol_dsue)
// nl (dsue_t1 = {a=1} + {b=0.5}*dsue_t + {c=0.5}*earnings_volatility + {d=0.5}*dsue_evol)

* Define the nonlinear equation and estimate the parameters
nl (ff_w1 = {a_star} - {beta}*dsue_t1 - {beta_b_star}*dsue - {beta_c_star}*evol - {beta_d_star}*evol_dsue + {g}*size_t1 + {h}*size_t1_dsue)

// nl (AR_t1 = {a_star=1} - {beta=0.5}*DSUE_t1 - {beta_b_star=0.5}*DSUE_t - {beta_c_star=0.5}*EVOL_t - {beta_d_star=0.5}*DSUE_EVOL_t + {g=0.5}*Size_t1 + {h=0.5}*Size_DSUE_t1)

restore


*******************************************************
*****************Table 7 Replication*******************
*******************************************************
// * Create the extreme deciles for dsue
// gen extreme_good_news = 0
// gen extreme_bad_news = 0
// replace extreme_good_news = 1 if suedecile >= 9
// replace extreme_bad_news = 1 if suedecile <= 2
//
// local vars spread dollar_volume price market_value age analyst_coverage total_return_volatility excess_return_volatility
//
// * Define the variables
// sort evol_quintile
// foreach var in spread dollar_volume price market_value age analyst_coverage total_return_volatility excess_return_volatility {
//     by evol_quintile: egen `var'_mean = mean(`var') if extreme_good_news == 1
//     by evol_quintile: egen `var'_std = sd(`var') if extreme_good_news == 1
//     by evol_quintile: egen `var'_p25 = pctile(`var') if extreme_good_news == 1, p(25)
//     by evol_quintile: egen `var'_median = median(`var') if extreme_good_news == 1
//     by evol_quintile: egen `var'_p75 = pctile(`var') if extreme_good_news == 1, p(75)
// }
//
// sort evol_quintile
// foreach var in spread dollar_volume price market_value age analyst_coverage total_return_volatility excess_return_volatility {
//     by evol_quintile: egen `var'_mean_bad = mean(`var') if extreme_bad_news == 1
//     by evol_quintile: egen `var'_std_bad = sd(`var') if extreme_bad_news == 1
//     by evol_quintile: egen `var'_p25_bad = pctile(`var') if extreme_bad_news == 1, p(25)
//     by evol_quintile: egen `var'_median_bad = median(`var') if extreme_bad_news == 1
//     by evol_quintile: egen `var'_p75_bad = pctile(`var') if extreme_bad_news == 1, p(75)
// }
//
// * Calculate the p-value difference
// foreach var in spread dollar_volume price market_value age analyst_coverage total_return_volatility excess_return_volatility {
//     ttest `var' if extreme_good_news == 1, by(evol_quintile)
//     ttest `var' if extreme_bad_news == 1, by(evol_quintile)
// }
//


// * Generate Spread chart
// twoway (scatter spread_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter spread_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Spread") xtitle("Volatility Quintiles") ytitle("Spread") legend(off)
//
// * Generate Dollar Volume chart
// twoway (scatter dollar_volume_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter dollar_volume_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Dollar Volume") xtitle("Volatility Quintiles") ytitle("Dollar Volume") legend(off)
//
// * Generate Price chart
// twoway (scatter price_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter price_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Price") xtitle("Volatility Quintiles") ytitle("Price") legend(off)
//
// * Generate Market Value chart
// twoway (scatter market_value_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter market_value_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Market Value") xtitle("Volatility Quintiles") ytitle("Market Value") legend(off)
//
// * Generate Total Return Volatility chart
// twoway (scatter total_return_volatility_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter total_return_volatility_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Total Return Volatility") xtitle("Volatility Quintiles") ytitle("Total Return Volatility") legend(off)
//
// * Generate Excess Return Volatility chart
// twoway (scatter excess_return_volatility_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter excess_return_volatility_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Excess Return Volatility") xtitle("Volatility Quintiles") ytitle("Excess Return Volatility") legend(off)
//
// * Generate Age chart
// twoway (scatter age_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter age_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Age") xtitle("Volatility Quintiles") ytitle("Age") legend(off)
//
// * Generate Analyst Coverage chart
// twoway (scatter analyst_coverage_mean_good evol_quintile if extreme_good_news == 1, mcolor(black) msymbol(diamond) mlabel(evol_quintile)) ///
//        (scatter analyst_coverage_mean_bad evol_quintile if extreme_bad_news == 1, mcolor(gray) msymbol(diamond) mlabel(evol_quintile)), ///
//        title("Analyst Coverage") xtitle("Volatility Quintiles") ytitle("Analyst Coverage") legend(off)
	   

*******************************************************
*****************Table 8 Replication*******************
*******************************************************

// ****************Panel A: Full Sample*******************
// * 3-Day Returns Regression
// sort company time
// xtreg ff_w1 dsue evol_dsue evol spread spread_dsue, fe
// newey ff_w1 dsue evol_dsue evol spread spread_dsue, lag(`=floor(4)') force
//
// * Quarterly Returns Regression
// sort company time
// xtreg ff_d60 dsue evol_dsue evol spread spread_dsue, fe
// newey ff_d60 dsue evol_dsue evol spread spread_dsue, lag(`=floor(4)') force
//
// ****************Panel B: Extreme Deciles*******************
// * Create the extreme deciles of dsue
// gen extreme_deciles = 0
// replace extreme_deciles = 1 if suedecile >= 9 | suedecile <= 2
//
// * 3-Day Returns Regression
// sort company time
// xtreg ff_w1 evol spread if extreme_deciles == 1, fe
// newey ff_w1 evol spread if extreme_deciles == 1, lag(`=floor(4)') force
//
// xtreg ff_w1 evol spread spread_dsue if extreme_deciles == 1, fe
// newey ff_w1 evol spread spread_dsue if extreme_deciles == 1, lag(`=floor(4)') force
//
// * Quarterly Returns Regression
// sort company time
// xtreg ff_d60 evol spread if extreme_deciles == 1, fe
// newey ff_d60 evol spread if extreme_deciles == 1, lag(`=floor(4)') force
//
// xtreg ff_d60 evol spread spread_dsue if extreme_deciles == 1, fe
// newey ff_d60 evol spread spread_dsue if extreme_deciles == 1, lag(`=floor(4)') force


*******************************************************
*****************Table 9 Replication*******************
*******************************************************

****************Panel A: Dependent Var DSUE_t+1*******************
* Model 1
sort company time
xtreg dsue_t1 dsue evol_dsue evol size, fe
newey dsue_t1 dsue evol_dsue evol size, lag(`=floor(4)') force

* Model 2
sort company time
xtreg dsue_t1 dsue evol_dsue evol size size_dsue, fe
newey dsue_t1 dsue evol_dsue evol size size_dsue, lag(`=floor(4)') force

****************Panel B: Dependent Var AR_t+1*********
* 3-Day Returns Regression
* Model 1
sort company time
xtreg ff_w1 dsue evol_dsue evol size, fe
newey ff_w1 dsue evol_dsue evol size, lag(`=floor(4)') force

* Model 2
sort company time
xtreg ff_w1 dsue evol_dsue evol size size_dsue, fe
newey ff_w1 dsue evol_dsue evol size size_dsue, lag(`=floor(4)') force

* Quarterly Returns Regression
* Model 1
sort company time
xtreg ff_d60 dsue evol_dsue evol size, fe
newey ff_d60 dsue evol_dsue evol size, lag(`=floor(4)') force

* Model 2
sort company time
xtreg ff_d60 dsue evol_dsue evol size size_dsue, fe
newey ff_d60 dsue evol_dsue evol size size_dsue, lag(`=floor(4)') force


*******************************************************
*****************Table 10 Replication******************
*******************************************************

// ****************Panel A: 3-Day Returns*********
// * Model 1: AR_{t+1}
// sort company time
// xtreg ff_w1 dsue evol_dsue evol size size_dsue book_to_market, fe
// newey ff_w1 dsue evol_dsue evol size size_dsue book_to_market, lag(`=floor(4)') force
//
// * Model 2: EAR_{t+1} (Raw Return of CRSP Weighted Index)
// sort company time
// xtreg ff_w1 dsue evol_dsue evol size size_dsue book_to_market, fe
// newey ff_w1 dsue evol_dsue evol size size_dsue book_to_market, lag(`=floor(4)') force
//
// ****************Panel B: Quarterly Returns*********
// * Model 1: AR_{t+1}
// sort company time
// xtreg ff_d60 dsue evol_dsue evol size size_dsue book_to_market, fe
// newey ff_d60 dsue evol_dsue evol size size_dsue book_to_market, lag(`=floor(4)') force
//
// * Model 2: EAR_{t+1}
// sort company time
// xtreg ff_d60 dsue evol_dsue evol size size_dsue book_to_market, fe
// newey ff_d60 dsue evol_dsue evol size size_dsue book_to_market, lag(`=floor(4)') force
