// Question 1

use "ee2002ext.dta"

// Question 2

generate log_wages = log(salfr)
_pctile log_wages, p(.5,99.5)
return list

global p0050=r(r1)
global p9950=r(r2)

replace log_wages = . if log_wages < $p0050
replace log_wages = . if log_wages > $p9950
drop if ddipl1==7

//Question 3

gen age_sq = agd^2

// Question 4

regress log_wages s agd age_sq ddipl1


// Question 5

// We obtain the following output:

//      Source |       SS           df       MS      Number of obs   =    52,599
//-------------+----------------------------------   F(4, 52594)     =   6397.96
//       Model |   4901.1115         4  1225.27787   Prob > F        =    0.0000
//    Residual |  10072.3193    52,594  .191510806   R-squared       =    0.3273
//-------------+----------------------------------   Adj R-squared   =    0.3273
//       Total |  14973.4308    52,598   .28467681   Root MSE        =    .43762

//------------------------------------------------------------------------------
//   log_wages | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
//-------------+----------------------------------------------------------------
//           s |   -.352907   .0038325   -92.08   0.000    -.3604187   -.3453952
//         agd |   .0518277   .0013716    37.79   0.000     .0491393    .0545161
//      age_sq |  -.0004834   .0000169   -28.54   0.000    -.0005166   -.0004502
//      ddipl1 |   .1514887   .0012035   125.87   0.000     .1491298    .1538475
//       _cons |   7.755334   .0273793   283.26   0.000      7.70167    7.808997

// All coefficients are more than twice the size of their std. errors, so all
// appear to be statistically significant on first glance. 

// Since we are dealing with a log-variable, we interpret the coefficients as 
// percentage points changes.

// The coefficient for gender indicates that women's incomes are lower by approximately 35
// percentage ponts relative to men. The coefficient for age tells us that an increase
// in age by one year increases income by about 5%. An increase in the level of 
// schooling leads to a 15% income increase.

// There is a negative relationship between log wages and age squared.

// Schooling and gender seem to have a strong impact on log wages relative to
// the other variables.

// Question 6

ereturn list

global N = e(N)          
global K = e(df_m)          
global K_plus_1 = $K + 1 
global SSR = e(rss) 

display $N
display $K
display $K_plus_1
display $SSR

// Question 7

test ddipl1

// ( 1)  ddipl1 = 0

//       F(  1, 52594) =15843.80
//            Prob > F =    0.0000

// Question 8

regress log_wages s agd age_sq ibn.ddipl1

//      Source |       SS           df       MS      Number of obs   =    52,599
//-------------+----------------------------------   F(8, 52590)     =   3356.67
//       Model |  5061.30288         8   632.66286   Prob > F        =    0.0000
//    Residual |  9912.12796    52,590   .18847933   R-squared       =    0.3380
//-------------+----------------------------------   Adj R-squared   =    0.3379
//       Total |  14973.4308    52,598   .28467681   Root MSE        =    .43414
//
//------------------------------------------------------------------------------
//   log_wages | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
//-------------+----------------------------------------------------------------
//           s |   -.359064   .0038268   -93.83   0.000    -.3665647   -.3515633
//         agd |   .0521668   .0013642    38.24   0.000      .049493    .0548406
//      age_sq |  -.0004929   .0000168   -29.26   0.000    -.0005259   -.0004599
//             |
//      ddipl1 |
//          1  |   -.803534   .0069229  -116.07   0.000     -.817103    -.789965
//          2  |  -.5547167   .0089135   -62.23   0.000    -.5721873   -.5372461
//          3  |  -.5825761   .0065391   -89.09   0.000    -.5953928   -.5697593
//          4  |  -.4155193   .0074172   -56.02   0.000    -.4300572   -.4009815
//          5  |  -.2223124   .0075681   -29.37   0.000     -.237146   -.2074789
//          6  |          0  (omitted)
//             |
//       _cons |   8.747749   .0273879   319.40   0.000     8.694068    8.801429
//------------------------------------------------------------------------------

test i2.ddipl1 == i3.ddipl1

// We obtain

// ( 1)  2.ddipl1 - 3.ddipl1 = 0
//
//       F(  1, 52590) =   12.81
//            Prob > F =    0.0003

// With this result, we can reject the null hypothesis that the effect of
// level 2 and level 3 schooling is equal.

// Question 9

constraint define 1 i2.ddipl1 == i3.ddipl1
constraint dir

cnsreg log_wages s agd age_sq ibn.ddipl1, c(1)

//note: 6.ddipl1 omitted because of collinearity.

//Constrained linear regression                          Number of obs =  52,599
//                                                       F(7, 52591)   = 3833.50
//                                                       Prob > F      =  0.0000
//                                                       Root MSE      =  0.4342
//
// ( 1)  2.ddipl1 - 3.ddipl1 = 0
//------------------------------------------------------------------------------
//   log_wages | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
//-------------+----------------------------------------------------------------
//           s |  -.3582423   .0038204   -93.77   0.000    -.3657302   -.3507543
//         agd |   .0520977   .0013642    38.19   0.000     .0494238    .0547715
//      age_sq |  -.0004918   .0000168   -29.20   0.000    -.0005248   -.0004588
//             |
//      ddipl1 |
//          1  |  -.8035912   .0069237  -116.06   0.000    -.8171617   -.7900207
//          2  |  -.5770788   .0063568   -90.78   0.000    -.5895382   -.5646194
//          3  |  -.5770788   .0063568   -90.78   0.000    -.5895382   -.5646194
//          4  |  -.4155048   .0074181   -56.01   0.000    -.4300443   -.4009653
//          5  |  -.2223334   .0075689   -29.37   0.000    -.2371686   -.2074982
//          6  |          0  (omitted)
//             |
//       _cons |   8.747426   .0273908   319.36   0.000      8.69374    8.801112
//------------------------------------------------------------------------------


// Question 10

regress log_wages s agd age_sq

//      Source |       SS           df       MS      Number of obs   =    52,601
//-------------+----------------------------------   F(3, 52597)     =   2498.31
//       Model |  1868.00215         3  622.667385   Prob > F        =    0.0000
//    Residual |  13109.0279    52,597  .249235278   R-squared       =    0.1247
//-------------+----------------------------------   Adj R-squared   =    0.1247
//       Total |  14977.0301    52,600  .284734412   Root MSE        =    .49923
//
//------------------------------------------------------------------------------
//   log_wages | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
//-------------+----------------------------------------------------------------
//           s |  -.3181829   .0043607   -72.97   0.000      -.32673   -.3096359
//         agd |   .0512955   .0015644    32.79   0.000     .0482293    .0543617
//      age_sq |  -.0005346   .0000193   -27.68   0.000    -.0005725   -.0004968
//       _cons |   8.306033   .0308211   269.49   0.000     8.245623    8.366442
//------------------------------------------------------------------------------

// We note here that the number of observations increases - why? 

// Question 11

ereturn list
global SSR_model_2 = e(rss)
display $SSR_model_2

// ... yields 13109.028.

//Question 12

regress log_wages s agd age_sq ddipl1
test ddipl1

// ... yields 

// ( 1)  ddipl1 = 0
//
//      F(  1, 52594) =15843.80
//            Prob > F =    0.0000

gen F_stat_global = (($SSR_model_2 - $SSR) ) / ($SSR / ($N - $K_plus_1 )), 

. display F_stat_global

// We obtain: 15856.591 comapred to 15843.80 previously.

// Question 13

display invF(4, $N - 4, 0.05)

// The F-statistic is much larger than our p-value, we can thus reject the null
// that education has no effect.

// Question 14

// Note: 1 = male, 2 = female

test (i5.ddipl1 + i2.s = i3.ddipl1 + i1.s)

// First we run the F-test:

// ( 1)  - 1bn.s + 2o.s - 3.ddipl1 + 5.ddipl1 = 0
//
//       F(  1, 52590) =    0.03
//            Prob > F =    0.8625





































