// Setup 

cd "/Users/lionelchambon/Desktop/LIONEL/LIONEL_edu/MASTER/M1/Cours/Econometrics_II/Data"

// Preparing the data. Credits to Vivan for showing me how to do this.

import delimited using "401k.raw"
replace v1 = stritrim(v1)
split v1
drop v1

rename v11 prate
rename v12 mrate
rename v13 empl_active
rename v14 empl_total


// Question 1

egen mean_prate = mean(prate)
display mean_prate

// The result is: 87.362907. This tells us that roughly 87% of active workers 
// have an account.

egen mean_mrate = mean(mrate)
display mean_mrate

// The result is: 0.73151237. This tells us that on average, each $ contributed 
// by a worker is matched roughly to .73 cts by the employer.

// Question 2

regress prate mrate

// The regression yields the following table. The non-adjusted R2 is 0.0747.

//      Source |       SS           df       MS      Number of obs   =     1,534
//-------------+----------------------------------   F(1, 1532)      =    123.68
//       Model |  32001.7271         1  32001.7271   Prob > F        =    0.0000
//    Residual |  396383.812     1,532   258.73617   R-squared       =    0.0747
//-------------+----------------------------------   Adj R-squared   =    0.0741
//       Total |  428385.539     1,533  279.442622   Root MSE        =    16.085
//
//------------------------------------------------------------------------------
//       prate | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
//-------------+----------------------------------------------------------------
//       mrate |   5.861079   .5270107    11.12   0.000      4.82734    6.894818
//       _cons |   83.07546   .5632844   147.48   0.000     81.97057    84.18035
//------------------------------------------------------------------------------

// The result is confirmed when running

ereturn list

//scalars:
//                  e(N) =  1534
//               e(df_m) =  1
//               e(df_r) =  1532
//                  e(F) =  123.6847830580641
//                 e(r2) =  .0747030982731007
//               e(rmse) =  16.08527805050407
//                e(mss) =  32001.72705102782
//                e(rss) =  396383.8123818268
//               e(r2_a) =  .0740991185722346
//                 e(ll) =  -6436.956478968455
//               e(ll_0) =  -6496.506833075103
//               e(rank) =  2

//macros:
//            e(cmdline) : "regress prate mrate"
//              e(title) : "Linear regression"
//          e(marginsok) : "XB default"
//                e(vce) : "ols"
//             e(depvar) : "prate"
//                e(cmd) : "regress"
//         e(properties) : "b V"
//            e(predict) : "regres_p"
//              e(model) : "ols"
//          e(estat_cmd) : "regress_estat"
//
//matrices:
//                  e(b) :  1 x 2
//                  e(V) :  2 x 2
//               e(beta) :  1 x 1
//
//functions:
//             e(sample)

// which reveals N=1534.

// Question 3

regress prate mrate
predict prate_predict, xb 
predict prate_residuals, residuals

summarize prate_predict

//    Variable |        Obs        Mean    Std. dev.       Min        Max
//-------------+---------------------------------------------------------
//prate_pred~t |      1,534    87.36291    4.568942   83.13406   111.8534

// The mean predicted value of our regression is 87.36291.
// Question: Is it usual that the predicted mean is identical to the sample mean?

summarize prate_residuals

//    Variable |        Obs        Mean    Std. dev.       Min        Max
//-------------+---------------------------------------------------------
//prate_resi~s |      1,534    2.62e-09    16.08003  -82.30267   16.80732

// Since the mean of our residuals is very close to, I conclude that the sum 
// must be so, too.

// Question 4

// Interpreting coefficients: 

// The constant tells us that without treatment, that is,
// zero employer contributions in this context, the expected participation rate
// is 83.07546.

// The beta coefficient is 5.861079 and tells us that a one unit, that is,
// 1$ increase in employer contributions would lead to a roughly 5.9% increase
// in worker participation.

// Question 5

display 83.07546 + (5.861079)*3.5 

// yields 103.58924. This result is obviously implausible, as a participation
// rate > 100% is impossible. Perhaps because using mrate = 3.5 is
// so far off the mean, roughly 0.73, that the model simply can't predict it
// well.

// Question 6

// From our previous list: e(r2) =  .0747030982731007.
// This says that only about 7% of the variation in worker participation
// is explained by employer contributions. This does not seem very large,
// and could be explained by the fact that workers rely on their pension
// for their retirement, regardless of how much their employer contributes.

// Question 7

scatter mrate prate
scatter prate mrate

gen mrate_binned = round(mrate, 0.1)
preserve
collapse (mean) prate, by(mrate_binned) 
list
restore

// We obtain the following results:

//     +----------------------+
//     | mrate_~d       prate |
//     |----------------------|
//  1. |        0   81.817391 |
//  2. |       .1   81.311207 |
//  3. |       .2   78.645714 |
//  4. |       .3   83.672727 |
//  5. |       .4   84.550787 |
//     |----------------------|
//  6. |       .5   85.773856 |
//  7. |       .6   88.179592 |
//  8. |       .7   92.580851 |
//  9. |       .8   91.783824 |
// 10. |       .9      92.808 |
//     |----------------------|
// 11. |        1   92.597959 |
// 12. |      1.1   94.908571 |
// 13. |      1.2   95.536842 |
// 14. |      1.3   96.884615 |
// 15. |      1.4   95.705556 |
//     |----------------------|
// 16. |      1.5   99.407692 |
// 17. |      1.6        96.6 |
// 18. |      1.7   99.653333 |
// 19. |      1.8   95.981818 |
// 20. |      1.9   94.282353 |
//     |----------------------|
// 21. |        2   96.708333 |
// 22. |      2.1        97.9 |
// 23. |      2.2   94.071429 |
// 24. |      2.3   99.177778 |
// 25. |      2.4         100 |
//     |----------------------|
// 26. |      2.5      99.975 |
// 27. |      2.6         100 |
// 28. |      2.7         100 |
// 29. |      2.8         100 |
// 30. |      2.9       97.12 |
//     |----------------------|
// 31. |        3      79.875 |
// 32. |      3.1         100 |
// 33. |      3.2         100 |
// 34. |      3.3         100 |
// 35. |      3.4         100 |
//     |----------------------|
// 36. |      3.5       96.45 |
// 37. |      3.6         100 |
// 38. |      3.7       99.85 |
// 39. |      3.8        94.8 |
// 40. |      3.9         100 |
//     |----------------------|
// 41. |        4      95.625 |
// 42. |      4.1        94.2 |
// 43. |      4.2       77.25 |
// 44. |      4.3         100 |
// 45. |      4.4         100 |
//     |----------------------|
// 46. |      4.6   95.333333 |
// 47. |      4.7        80.5 |
// 48. |      4.8         100 |
// 49. |      4.9         100 |
//     +----------------------+


scatter prate mrate_binned
scatter mrate_binned prate

graph twoway (lfit prate mrate_binned) (scatter prate mrate_binned)
graph twoway (lfit mrate_binned prate) (scatter mrate_binned prate)

// The values appear much less clustered on the graph, which makes it more readable
// and makes the predicted values easier to identify.



















