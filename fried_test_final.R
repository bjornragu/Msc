library(xtable)
library(PMCMR)
library(scmamp)
#all models average over data sets for all data performance measures
fri_data2 <- data.frame(
  score = c(2.000, 3.000, 1.667, 1.000,
            5.333, 4.333, 4.667, 3.000,
            1.000, 1.000, 1.333, 1.000,
            3.000, 2.667, 4.333, 2.333,
            2.667, 2.333, 4.667, 2.000,
            5.000, 4.000, 3.333, 2.333,
            6.333, 6.333, 7.333, 3.333,
            8.333, 7.667, 7.333, 4.333,
            7.000, 6.667, 7.000, 3.667),
  model = factor(c("LR", "LR", "LR","LR",
                   "DT", "DT", "DT","DT",
                   "RF", "RF", "RF","RF",
                   "MLP1", "MLP1", "MLP1","MLP1",
                   "MLP3", "MLP3", "MLP3","MLP3",
                   "MLP5", "MLP5", "MLP5","MLP5",
                   "DBN1", "DBN1", "DBN1","DBN1",
                   "DBN3", "DBN3", "DBN3","DBN3",
                   "DBN5", "DBN5", "DBN5", "DBN5")),
  measure = factor(rep(c("AUC", "BS", "PG", "EMPC"),9)))

fried.test_overall2 <- friedman.test(score~model|measure, fri_data2) #look at table to get adjusted value for test statistic http://www.statisticshowto.com/friedmans-test/
nemenyi2 = posthoc.friedman.nemenyi.test(score~model|measure,data=fri_data2)
xtable(nemenyi2$p.value, digits=c(3))
#compare classifiers with base classifier (RF)
lr = 1.917; dt = 4.333; rf = 1.083; mlp1 = 3.083; mlp3 = 2.917; mlp5= 3.667; dbn1 = 5.833; dbn3 = 6.917; dbn5 = 6.083
k=9
n=4
denominator = sqrt((k*(k+1))/(6*n))
#test
lr.test <- lr-rf/denominator
dt.test <- dt-rf/denominator
mlp1.test <- mlp1-rf/denominator
mlp3.test <- mlp3-rf/denominator
mlp5.test <- mlp5-rf/denominator
dbn1.test <- dbn1-rf/denominator
dbn3.test <- dbn3-rf/denominator
dbn5.test <- dbn5-rf/denominator
correction <- k-1
lr.pvalue=(2*pnorm(-abs(lr.test)))
dt.pvalue=2*pnorm(-abs(dt.test))
mlp1.pvalue=2*pnorm(-abs(mlp1.test))
mlp3.pvalue=2*pnorm(-abs(mlp3.test))
mlp5.pvalue=2*pnorm(-abs(mlp5.test))
dbn1.pvalue=2*pnorm(-abs(dbn1.test))
dbn3.pvalue=2*pnorm(-abs(dbn3.test))
dbn5.pvalue=2*pnorm(-abs(dbn5.test))
c(lr.test,dt.test,mlp1.test,mlp3.test,mlp5.test,dbn1.test,dbn3.test,dbn5.test)
pvalues = c(lr.pvalue,dt.pvalue,mlp1.pvalue,mlp3.pvalue,mlp5.pvalue,dbn1.pvalue,dbn3.pvalue,dbn5.pvalue)
#rom correction for pvalues (scmamp package)
adjpvalues = adjustRom(pvalues, alpha = 0.05)
adjpvalues  

