cd "D:\Documents\R\Statistics and Econometrics"
use womenwk.dta, clear

heckman lw education age children, select(age married children education)
// init params are from R
// better than Stata's result, in lnlikelihood
heckman lw education age children, select(age married children education) from(0.027607759 0.001249352 -0.074724514 2.967625839 0.030639893 0.278960526 0.393687174 0.041411731 -1.945028363 -1.295039648 -1.211976218, copy)
