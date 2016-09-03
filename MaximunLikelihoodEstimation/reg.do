cd "D:\Documents\R\Statistics and Econometrics"
import delimited reg.csv, clear

gen y_dummy = (y>18)
probit y_dummy x1 x3 x4
logit y_dummy x1 x3 x4


