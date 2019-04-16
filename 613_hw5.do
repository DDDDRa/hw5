/******************************************************************************
Project:	ECON 613 Stata Short Tutorial
Created by: Weijia Qiu
Created:	2019/04/15
******************************************************************************/


clear all
set more off, perm
set scrollbufsize 2000000
capture log close
log using  "/Users/huangguo/Desktop/613/session 2.smcl", replace

//HW1 
//Exercise 1
set obs 10000
set seed 100
generate x1=runiform(0,1)
generate x2=rgamma(3,2)
generate x3=rbinomial(10000,0.3)
generate eps=rnormal(2,1)
generate Y=0.5+1.2*x1-0.9*x2+0.1*x3+eps
egen mean_Y=mean(Y)
generate ydum=0 
replace ydum=1 if Y>mean_Y & Y!=0
//Exercise 2
//Question 1
correlate Y x1
// Conclusion: the correlation is 0.047, which is far different from 1.2
//Question 2 and Question 3 (Calculate standard errors by using OLS formula)
regress ydum x1 x2 x3
// Question 3 (Calculate standrad errors by using bootstrap)
bootstrap, reps(49) seed(100): regress ydum x1 x2 x3
bootstrap, reps(499) seed(100): regress ydum x1 x2 x3
//Exercise 3
mlexp ( ydum*lnnormal({b1}*x1 + {b2}*x2 + {b3}*x3+{b0}) + (1-ydum)*lnnormal(-({b1}*x1 + {b2}*x2 + {b3}*x3+ {b0})) )
///Exercise 4
regress ydum x1 x2 x3
//Interpretation: All the coefficients in the linear regression model are significant，
mlogit ydum x1 x2 x3
//Interpretation: All the coefficients in the linear regression model are significant，
probit ydum x1 x2 x3
//Interpretation: All the coefficients in the linear regression model are significant，
//Comparision: The coefficients in the probit model and logit model are very close except their intercepts. The signs of the coefficients are respectively the same in all these three models. 
//Exercise 5
probit ydum x1 x2 x3
margins, dydx(*) vce(delta)
mlogit ydum x1 x2 x3
margins, dydx(*) vce(delta)




/// HW3
//Exercise 1
//Q1
clear
insheet using/Users/huangguo/Desktop/613/hw3/product.csv
sort hhid
save data1,replace
clear
insheet using/Users/huangguo/Desktop/613/hw3/demos.csv
save data2,replace
sort hhid
merge 1:m hhid using data1.dta
gen n = 4470
gen v2 = _n
summarize phse_stk ppk_tub ppk_stk pgen_stk pfl_tub phse_stk ppk_tub ppk_stk pgen_stk pfl_tub
tab choice
tab choice income

//Q2
//Use conditional logit model
rename (ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub)(c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
reshape long c,i(v2) j(price)
gen dum = cond(price == choice,1,0)
asclogit dum c,case(v2) alternatives(price)
est sto c_logit
estat mfx
//Interpretation Q2.3: Because beta is negative, which indicates that the higher the price it is, the less utility that an individual will have by choosing the product, and the less likely it is that an individual is going to choose the product.
//alfa1, alfa3, alfa4, alfa5,alfa9 are all negative, which indicates that compared with the product 1 (PPk_Stk), product 2,4,5,6,10 ( PBB_Stk, PHse_Stk,PGen_Stk,PImp_Stk,PHse_Tub) are less preferred and thus are less likely to be chosen given the same price.
//alfa2, alfa6,alfa7, alfa8, are all positive, which indicates that compare with the product 1 (PPk_Stk), product 3,7,8,9 (PFl_Stk,PSS_Tub,PPk_Tub,PFl_Tub) are more preferred and thus they are more likely to be chosen given the same price.

//Q3&4
asclogit d, case(v2) alternatives(price) casevar(income)
est sto m_logit
estat mfx

//Q5
asmixlogit dum, random(c) casevar(income) alternatives(price) case(v2)
estimate store haha
drop if choice == 10
drop if price == 10
asmixlogit dum, random(c) casevar(income) alternative(price) case(v2)
estimate store hahapartial
hausman hahapartial haha, alleqs constant
//The result of hausman test shows that we can hardly reject IIA. The reason might be that the sample of the product 10 is too small to show any significant difference if we remove its observations


//HW4
clear
insheet using /Users/huangguo/Desktop/613/hw4/Koop-Tobias.csv,names
//convert data into panel data
xtset personid timetrnd
bysort personid: gen t = _n
//Represent the panel dimension of wages for 5 randomly selected individuals
tabulate timetrnd logwage if personid == 3
tabulate timetrnd logwage if personid == 13
tabulate timetrnd logwage if personid == 133
tabulate timetrnd logwage if personid == 1133
tabulate timetrnd logwage if personid == 1333

//Q 2
//Random effect model
xtreg logwage educ potexper, re

//Q 3
//Fixed effect model
//Between Estimator 
xtreg logwage educ potexper,be 
//Within Estimator 
xtreg logwage educ potexper,fe
//First time difference estimator 
xtset personid t
xtdes
gen logwage_D = D.logwage
gen educ_D = D.educ
gen potexper_D = D.potexper
xtreg logwage_D educ_D potexper_D, fe
//Q4
mlexp (ln(normalden(logwage, {b0} + {b1}*educ+ {b2}*potexper, {sigma})))
//Q4.3
//The standard errors in the previous model are wrong because of some auto-correlation issues in the model.
//In this case, the standard errors we calculated in the previous model by using ols method can hardly get the robust standard error of coefficients
//Alternative approach: We can use robust ols method to get the adjusted standard errors of coefficients.We can also use gls to get robust standard errors of coefficients.

log close

