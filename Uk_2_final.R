library(psych)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
library(rJava)
library(corrplot)
library(Biocomb)
library(FSelector)
library(caret)
library(mlbench)
library(caTools)
library(pROC)
require(caret)
require(ROCR)
library(RWeka)
library(hmeasure)
library(ModelMetrics)
library(MLmetrics)
library(Information)
library(randomForest)
library(EMP)
##preprocessing----
##importing data
uk <- read.table("~/Documents/Thesis/Bjorn data/UK/input.txt")
colnames(uk) <- c("Censor","Censore", "Open", "Age", "Amount", "Curradd", "Curremp", "Custgend", 
                  "Depchild","Freqpaid", "Homephon", "Insprem", "Loantype", "Marstat", "Term", 
                  "Homwowns", "Purpose")
any(is.na(uk)) #no mis val 
##correlation based feature selection
cor.uk <-cor(uk)
highlyCorrelated <- findCorrelation(cor.uk, cutoff=0.5, names = TRUE)
highlyCorrelated #trerm and age
corrplot(cor.uk, method="number", type="upper")
vif = function(df){diag(solve(cor(df)))}
vif(uk) 
########################
########-WOE-###########
########################
uk_woe <- Information::create_infotables(data=uk, y ="Censor", parallel = FALSE)
uk_woedata <- uk
uk_woe$Tables$Custgend
uk_woedata[,"Custgend"] <- ifelse(uk[,"Custgend"] == 1, as.matrix(uk_woe$Tables$Custgend$WOE)[1,], as.matrix(uk_woe$Tables$Custgend$WOE)[2,])
table(uk$Custgend, uk_woedata$Custgend)
uk_woe$Tables$Freqpaid
uk_woedata[,"Freqpaid"] <- ifelse(uk[,"Freqpaid"] == 1, as.matrix(uk_woe$Tables$Freqpaid$WOE)[1,],  
                          ifelse(uk[,"Freqpaid"] == 2, as.matrix(uk_woe$Tables$Freqpaid$WOE)[1,],
                                 ifelse(uk[,"Freqpaid"] == 3, as.matrix(uk_woe$Tables$Freqpaid$WOE)[2,],
                                        ifelse(uk[,"Freqpaid"] == 4, as.matrix(uk_woe$Tables$Freqpaid$WOE)[2,],
                                               as.matrix(uk_woe$Tables$Freqpaid$WOE)[2,]))))
table(uk$Freqpaid, uk_woedata$Freqpaid)   
uk_woe$Tables$Homephon
uk_woedata[,"Homephon"] <- ifelse(uk[,"Homephon"] == 1, as.matrix(uk_woe$Tables$Homephon$WOE)[1,], as.matrix(uk_woe$Tables$Homephon$WOE)[2,])
table(uk$Homephon, uk_woedata$Homephon)

uk_woe$Tables$Loantype
uk_woedata[,"Loantype"] <- ifelse(uk[,"Loantype"] == 1, as.matrix(uk_woe$Tables$Loantype$WOE)[1,], as.matrix(uk_woe$Tables$Loantype$WOE)[2,])
table(uk$Loantype, uk_woedata$Loantype)

uk_woe$Tables$Marstat
uk_woedata[,"Marstat"] <- ifelse(uk[,"Marstat"] == 1, as.matrix(uk_woe$Tables$Marstat$WOE)[1,], as.matrix(uk_woe$Tables$Marstat$WOE)[2,])
table(uk$Marstat, uk_woedata$Marstat)

uk_woe$Tables$Homwowns
uk_woedata[,"Homwowns"] <- ifelse(uk[,"Homwowns"] == 1, as.matrix(uk_woe$Tables$Homwowns$WOE)[1,], as.matrix(uk_woe$Tables$Homwowns$WOE)[2,])
table(uk_woedata$Homwowns, uk$Homwowns)

uk_woe$Tables$Purpose
uk_woedata[,"Purpose"] <- ifelse(uk[,"Purpose"] == 1, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],  
                                 ifelse(uk[,"Purpose"] == 2, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],
                                        ifelse(uk[,"Purpose"] == 3, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],
                                               ifelse(uk[,"Purpose"] == 4, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],
                                                      ifelse(uk[,"Purpose"] == 5, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],
                                                             ifelse(uk[,"Purpose"] == 6, as.matrix(uk_woe$Tables$Purpose$WOE)[1,],
                                                                    ifelse(uk[,"Purpose"] == 7, as.matrix(uk_woe$Tables$Purpose$WOE)[2,],
                                                                           ifelse(uk[,"Purpose"] == 8, as.matrix(uk_woe$Tables$Purpose$WOE)[2,],
                                                                                  ifelse(uk[,"Purpose"] == 9, as.matrix(uk_woe$Tables$Purpose$WOE)[2,],
                                                                                         ifelse(uk[,"Purpose"] == 10, as.matrix(uk_woe$Tables$Purpose$WOE)[3,],
                                                                                                ifelse(uk[,"Purpose"] == 11, as.matrix(uk_woe$Tables$Purpose$WOE)[3,],
                                                                                                       ifelse(uk[,"Purpose"] == 12, as.matrix(uk_woe$Tables$Purpose$WOE)[4,],
                                                                                                              ifelse(uk[,"Purpose"] == 13, as.matrix(uk_woe$Tables$Purpose$WOE)[4,],
                                                                                                                     ifelse(uk[,"Purpose"] == 14, as.matrix(uk_woe$Tables$Purpose$WOE)[4,],
                                                                                                                            ifelse(uk[,"Purpose"] == 15, as.matrix(uk_woe$Tables$Purpose$WOE)[4,],
                                                                                                                                   ifelse(uk[,"Purpose"] == 16, as.matrix(uk_woe$Tables$Purpose$WOE)[4,],
                                                                                                                                          ifelse(uk[,"Purpose"] == 17, as.matrix(uk_woe$Tables$Purpose$WOE)[5,],
                                                                                                                                                 ifelse(uk[,"Purpose"] == 18, as.matrix(uk_woe$Tables$Purpose$WOE)[5,],
                                                                                                                                                        ifelse(uk[,"Purpose"] == 19, as.matrix(uk_woe$Tables$Purpose$WOE)[6,],
                                                                                                                                                               ifelse(uk[,"Purpose"] == 20, as.matrix(uk_woe$Tables$Purpose$WOE)[6,],
                                                                                                                                                                      ifelse(uk[,"Purpose"] == 21, as.matrix(uk_woe$Tables$Purpose$WOE)[7,],
                                                                                                                                                                             ifelse(uk[,"Purpose"] == 22, as.matrix(uk_woe$Tables$Purpose$WOE)[7,],
                                                                                                                                                                                    ifelse(uk[,"Purpose"] == 23, as.matrix(uk_woe$Tables$Purpose$WOE)[7,],
                                                                                                                                                                                           ifelse(uk[,"Purpose"] == 24, as.matrix(uk_woe$Tables$Purpose$WOE)[7,],
                                                                                                                                                                                                  ifelse(uk[,"Purpose"] == 25, as.matrix(uk_woe$Tables$Purpose$WOE)[7,],
                                                                           as.matrix(uk_woe$Tables$Purpose$WOE)[8,])))))))))))))))))))))))))


table(uk_woedata$Purpose,uk$Purpose)

####creating factors#####
uk_woedata$Censor <- as.factor(uk_woedata$Censor)
uk$Censor <- as.factor(uk$Censor)
uk$Censore <- as.factor(uk$Censore)
uk$Custgend <- as.factor(uk$Custgend)
uk$Freqpaid <- as.factor(uk$Freqpaid)
uk$Homephon <- as.factor(uk$Homephon)
uk$Loantype <- as.factor(uk$Loantype)
uk$Marstat <- as.factor(uk$Marstat)
uk$Homwowns <- as.factor(uk$Homwowns)
uk$Purpose <- as.factor(uk$Purpose) 

levels(uk_woedata$Censor) <- make.names(levels(factor(uk_woedata$Censor)))
levels(uk$Censor) <- make.names(levels(factor(uk$Censor)))
levels(uk$Censore) <- make.names(levels(factor(uk$Censore)))
levels(uk$Custgend) <- make.names(levels(factor(uk$Custgend)))
levels(uk$Freqpaid) <- make.names(levels(factor(uk$Freqpaid)))
levels(uk$Homephon) <- make.names(levels(factor(uk$Homephon)))
levels(uk$Loantype) <- make.names(levels(factor(uk$Loantype)))
levels(uk$Marstat) <- make.names(levels(factor(uk$Marstat)))
levels(uk$Homwowns) <- make.names(levels(factor(uk$Homwowns)))
levels(uk$Purpose) <- make.names(levels(factor(uk$Purpose)))

#################################################################
##DATA PARTITIONING.Use Nx2-foldcross-validation(Dietterich,1998)
##################################################################
##UK_data
k=5
a <- createDataPartition(uk$Censor, times = k, p = 0.5, list = FALSE) #https://dataanalyticsblog.com/2016/03/27/splitting-data-into-train-and-test-using-caret-package-in-r/
uk_train1 <- uk[a[,1],]
uk_test1 <- uk[-a[,1],]
uk_train2 <- uk[a[,2],]
uk_test2 <- uk[-a[,2],]
uk_train3 <- uk[a[,3],]
uk_test3 <- uk[-a[,3],]
uk_train4 <- uk[a[,4],]
uk_test4 <- uk[-a[,4],]
uk_train5 <- uk[a[,5],]
uk_test5 <- uk[-a[,5],]

b <- createDataPartition(uk_woedata$Censor, times = k, p = 0.5, list = FALSE) #https://dataanalyticsblog.com/2016/03/27/splitting-data-into-train-and-test-using-caret-package-in-r/
uk_woe_train1 <- uk_woedata[b[,1],]
uk_woe_test1 <- uk_woedata[-b[,1],]
uk_woe_train2 <- uk_woedata[b[,2],]
uk_woe_test2 <- uk_woedata[-b[,2],]
uk_woe_train3 <- uk_woedata[b[,3],]
uk_woe_test3 <- uk_woedata[-b[,3],]
uk_woe_train4 <- uk_woedata[b[,4],]
uk_woe_test4 <- uk_woedata[-b[,4],]
uk_woe_train5 <- uk_woedata[b[,5],]
uk_woe_test5 <- uk_woedata[-b[,5],]

formula <- Censor~Open+Age+Amount+Curradd+Curremp+Custgend+Depchild+Freqpaid+Homephon+Insprem+Loantype+Marstat+Term+Homwowns+Purpose

#######################################
##custom summary functions#############
#######################################
Brier <- function(a, p) {
  if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
  temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
  temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
  dif <- (temp.df$a-temp.df$p)^2
  dif.sum <- sum(dif)
  brier <- (1/length(a))*(dif.sum)
}

brierSummary <- function (data, lev = "X1", model = NULL) {
  levels(data$obs) <- c('0', '1')
  out <- Brier(as.numeric(as.character(levels(data$obs)))[data$obs], data[, lev[2]])  
  names(out) <- "BrierScore"
  out
}

normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) 
    accum.losses <- temp.df$actual / total.losses 
    gini.sum <- cumsum(accum.losses - null.losses) 
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

# 2x versions - create the normalized gini summary function to pass into caret 
nginiSummary <- function (data, lev = "X1", model = NULL) {
  levels(data$obs) <- c('0', '1')
  out <- normalizedGini(as.numeric(levels(data$obs))[data$obs], data[, lev[2]])  
  names(out) <- "NormalizedGini"
  out
}


giniSummary <- function (data, lev = "X1", model = NULL) {
  levels(data$obs) <- c('0', '1')
  out <- Gini(as.numeric(levels(data$obs))[data$obs], data[, lev[2]])  
  names(out) <- "Gini"
  out
}

empSummary <- function (data, lev = NULL, model = NULL) {
  require(EMP)
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  empObject <- try(EMP:::empCreditScoring(data[, lev[1]],data$obs))
  rocAUC <- if (class(empObject)[1] == "try-error") 
    NA
  else empObject$EMPC
  out <- c(empObject$EMPC)
  names(out) <- c("empc")
  out
}

train_control_roc <- trainControl(method="cv", number=5, savePredictions=TRUE,classProbs=TRUE, summaryFunction=twoClassSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_gini <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=giniSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_brier <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=brierSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_emp <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=empSummary, verboseIter = TRUE, allowParallel = TRUE)

###############################
#######logistic regression#####
###############################

###data 1, train-test
#ROC curve 
set.seed(123); model_log1a_roc <- train(formula, data=uk_woe_train1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log1a_roc <- predict(model_log1a_roc,uk_woe_test1,type="prob")
log1a.ROC <- roc(predictor=predb_log1a_roc$X0,
                 response=uk_woe_test1$Censor,
                 levels=rev(levels(uk_woe_test1$Censor)))
log1a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); model_log1a_gini <- train(formula, data=uk_woe_train1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log1a_gini$results
model_log1a_gini$resample
pred_log1a_gini<- predict(model_log1a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, pred_log1a_gini$X1)
Gini(uk_woe_test1$Censorb, pred_log1a_gini$X1)
#b <= 0.4
p <- pred_log1a_gini$X1[pred_log1a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[pred_log1a_gini$X1<=0.4]))
log1a.ngini <- normalizedGini(a, p)
log1a.ngini
log1a.gini <-Gini(a, p)
log1a.gini

#Brier score
set.seed(123); model_log1a_brier <- train(formula, data=uk_woe_train1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log1a_brier$results
model_log1a_brier$resample
pred_log1a_brier <- predict(model_log1a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
log1a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), pred_log1a_brier$X1)
log1a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); model_log1b_roc <- train(formula, data=uk_woe_test1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log1b_roc <- predict(model_log1b_roc,uk_woe_train1,type="prob")
log1b.ROC <- roc(predictor=predb_log1b_roc$X0,
                 response=uk_woe_train1$Censor,
                 levels=rev(levels(uk_woe_train1$Censor)))
log1b.ROC
#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); model_log1b_gini <- train(formula, data=uk_woe_test1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log1b_gini$results
model_log1b_gini$resample
pred_log1b_gini<- predict(model_log1b_gini, newdata = uk_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, pred_log1b_gini$X1)
Gini(uk_woe_train1$Censorb, pred_log1b_gini$X1)
#b <= 0.4
p <- pred_log1b_gini$X1[pred_log1b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[pred_log1b_gini$X1<=0.4]))
log1b.ngini <- normalizedGini(a, p)
log1b.ngini
log1b.gini <-Gini(a, p)
log1b.gini

#Brier score
set.seed(123); model_log1b_brier <- train(formula, data=uk_woe_test1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log1b_brier$results
model_log1b_brier$resample
pred_log1b_brier <- predict(model_log1b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
log1b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), pred_log1b_brier$X1)
log1b.bs

###data 2, train-test
#ROC curve 
set.seed(123); model_log2a_roc <- train(formula, data=uk_woe_train2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log2a_roc <- predict(model_log2a_roc,uk_woe_test2,type="prob")
log2a.ROC <- roc(predictor=predb_log2a_roc$X0,
                 response=uk_woe_test2$Censor,
                 levels=rev(levels(uk_woe_test2$Censor)))
log2a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); model_log2a_gini <- train(formula, data=uk_woe_train2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log2a_gini$results
model_log2a_gini$resample
pred_log2a_gini<- predict(model_log2a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, pred_log2a_gini$X1)
Gini(uk_woe_test2$Censorb, pred_log2a_gini$X1)
#b <= 0.4
p <- pred_log2a_gini$X1[pred_log2a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[pred_log2a_gini$X1<=0.4]))
log2a.ngini <- normalizedGini(a, p)
log2a.ngini
log2a.gini <-Gini(a, p)
log2a.gini

#Brier score
set.seed(123); model_log2a_brier <- train(formula, data=uk_woe_train2, method = "glm",family="binomial", trControl=train_control_brier, maximize=FALSE, metric='BrierScore', preProc=c("center","scale"))
model_log2a_brier$results
model_log2a_brier$resample
pred_log2a_brier <- predict(model_log2a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
log2a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), pred_log2a_brier$X1)
log2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_log2b_roc <- train(formula, data=uk_woe_test2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log2b_roc <- predict(model_log2b_roc,uk_woe_train2,type="prob")
log2b.ROC <- roc(predictor=predb_log2b_roc$X0,
                 response=uk_woe_train2$Censor,
                 levels=rev(levels(uk_woe_train2$Censor)))
log2b.ROC
#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); model_log2b_gini <- train(formula, data=uk_woe_test2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log2b_gini$results
model_log2b_gini$resample
pred_log2b_gini<- predict(model_log2b_gini, newdata = uk_woe_train2, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, pred_log2b_gini$X1)
Gini(uk_woe_train2$Censorb, pred_log2b_gini$X1)
#b <= 0.4
p <- pred_log2b_gini$X1[pred_log2b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[pred_log2b_gini$X1<=0.4]))
log2b.ngini <- normalizedGini(a, p)
log2b.ngini
log2b.gini <-Gini(a, p)
log2b.gini

#Brier score
set.seed(123); model_log2b_brier <- train(formula, data=uk_woe_test2, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_log2b_brier$results
model_log2b_brier$resample
pred_log2b_brier <- predict(model_log2b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
log2b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), pred_log2b_brier$X1)
log2b.bs

###data 3, train-test
#ROC curve 
set.seed(123); model_log3a_roc <- train(formula, data=uk_woe_train3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log3a_roc <- predict(model_log3a_roc,uk_woe_test3,type="prob")
log3a.ROC <- roc(predictor=predb_log3a_roc$X0,
                 response=uk_woe_test3$Censor,
                 levels=rev(levels(uk_woe_test3$Censor)))
log3a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); model_log3a_gini <- train(formula, data=uk_woe_train3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log3a_gini$results
model_log3a_gini$resample
pred_log3a_gini<- predict(model_log3a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, pred_log3a_gini$X1)
Gini(uk_woe_test3$Censorb, pred_log3a_gini$X1)
#b <= 0.4
p <- pred_log3a_gini$X1[pred_log3a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[pred_log3a_gini$X1<=0.4]))
log3a.ngini <- normalizedGini(a, p)
log3a.ngini
log3a.gini <-Gini(a, p)
log3a.gini

#Brier score
set.seed(123); model_log3a_brier <- train(formula, data=uk_woe_train3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log3a_brier$results
model_log3a_brier$resample
pred_log3a_brier <- predict(model_log3a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
log3a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), pred_log3a_brier$X1)
log3a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); model_log3b_roc <- train(formula, data=uk_woe_test3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log3b_roc <- predict(model_log3b_roc,uk_woe_train3,type="prob")
log3b.ROC <- roc(predictor=predb_log3b_roc$X0,
                 response=uk_woe_train3$Censor,
                 levels=rev(levels(uk_woe_train3$Censor)))
log3b.ROC
#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); model_log3b_gini <- train(formula, data=uk_woe_test3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log3b_gini$results
model_log3b_gini$resample
pred_log3b_gini<- predict(model_log3b_gini, newdata = uk_woe_train3, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, pred_log3b_gini$X1)
Gini(uk_woe_train3$Censorb, pred_log3b_gini$X1)
#b <= 0.4
p <- pred_log3b_gini$X1[pred_log3b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[pred_log3b_gini$X1<=0.4]))
log3b.ngini <- normalizedGini(a, p)
log3b.ngini
log3b.gini <-Gini(a, p)
log3b.gini

#Brier score
set.seed(123); model_log3b_brier <- train(formula, data=uk_woe_test3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log3b_brier$results
model_log3b_brier$resample
pred_log3b_brier <- predict(model_log3b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
log3b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), pred_log3b_brier$X1)
log3b.bs

###data 4, train-test
#ROC curve 
set.seed(123); model_log4a_roc <- train(formula, data=uk_woe_train4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log4a_roc <- predict(model_log4a_roc,uk_woe_test4,type="prob")
log4a.ROC <- roc(predictor=predb_log4a_roc$X0,
                 response=uk_woe_test4$Censor,
                 levels=rev(levels(uk_woe_test4$Censor)))
log4a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); model_log4a_gini <- train(formula, data=uk_woe_train4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log4a_gini$results
model_log4a_gini$resample
pred_log4a_gini<- predict(model_log4a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, pred_log4a_gini$X1)
Gini(uk_woe_test4$Censorb, pred_log4a_gini$X1)
#b <= 0.4
p <- pred_log4a_gini$X1[pred_log4a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[pred_log4a_gini$X1<=0.4]))
log4a.ngini <- normalizedGini(a, p)
log4a.ngini
log4a.gini <-Gini(a, p)
log4a.gini

#Brier score
set.seed(123); model_log4a_brier <- train(formula, data=uk_woe_train4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log4a_brier$results
model_log4a_brier$resample
pred_log4a_brier <- predict(model_log4a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
log4a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), pred_log4a_brier$X1)
log4a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); model_log4b_roc <- train(formula, data=uk_woe_test4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log4b_roc <- predict(model_log4b_roc,uk_woe_train4,type="prob")
log4b.ROC <- roc(predictor=predb_log4b_roc$X0,
                 response=uk_woe_train4$Censor,
                 levels=rev(levels(uk_woe_train4$Censor)))
log4b.ROC
#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); model_log4b_gini <- train(formula, data=uk_woe_test4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log4b_gini$results
model_log4b_gini$resample
pred_log4b_gini<- predict(model_log4b_gini, newdata = uk_woe_train4, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, pred_log4b_gini$X1)
Gini(uk_woe_train4$Censorb, pred_log4b_gini$X1)
#b <= 0.4
p <- pred_log4b_gini$X1[pred_log4b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[pred_log4b_gini$X1<=0.4]))
log4b.ngini <- normalizedGini(a, p)
log4b.ngini
log4b.gini <-Gini(a, p)
log4b.gini

#Brier score
set.seed(123); model_log4b_brier <- train(formula, data=uk_woe_test4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_log4b_brier$results
model_log4b_brier$resample
pred_log4b_brier <- predict(model_log4b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
log4b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), pred_log4b_brier$X1)
log4b.bs

###data 5, train-test
#ROC curve 
set.seed(123); model_log5a_roc <- train(formula, data=uk_woe_train5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log5a_roc <- predict(model_log5a_roc,uk_woe_test5,type="prob")
log5a.ROC <- roc(predictor=predb_log5a_roc$X0,
                 response=uk_woe_test5$Censor,
                 levels=rev(levels(uk_woe_test5$Censor)))
log5a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); model_log5a_gini <- train(formula, data=uk_woe_train5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log5a_gini$results
model_log5a_gini$resample
pred_log5a_gini<- predict(model_log5a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, pred_log5a_gini$X1)
Gini(uk_woe_test5$Censorb, pred_log5a_gini$X1)
#b <= 0.4
p <- pred_log5a_gini$X1[pred_log5a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[pred_log5a_gini$X1<=0.4]))
log5a.ngini <- normalizedGini(a, p)
log5a.ngini
log5a.gini <-Gini(a, p)
log5a.gini

#Brier score
set.seed(123); model_log5a_brier <- train(formula, data=uk_woe_train5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_log5a_brier$results
model_log5a_brier$resample
pred_log5a_brier <- predict(model_log5a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
log5a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), pred_log5a_brier$X1)
log5a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); model_log5b_roc <- train(formula, data=uk_woe_test5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_log5b_roc <- predict(model_log5b_roc,uk_woe_train5,type="prob")
log5b.ROC <- roc(predictor=predb_log5b_roc$X0,
                 response=uk_woe_train5$Censor,
                 levels=rev(levels(uk_woe_train5$Censor)))
log5b.ROC
#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); model_log5b_gini <- train(formula, data=uk_woe_test5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_log5b_gini$results
model_log5b_gini$resample
pred_log5b_gini<- predict(model_log5b_gini, newdata = uk_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, pred_log5b_gini$X1)
Gini(uk_woe_train5$Censorb, pred_log5b_gini$X1)
#b <= 0.4
p <- pred_log5b_gini$X1[pred_log5b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[pred_log5b_gini$X1<=0.4]))
log5b.ngini <- normalizedGini(a, p)
log5b.ngini
log5b.gini <-Gini(a, p)
log5b.gini

#Brier score
set.seed(123); model_log5b_brier <- train(formula, data=uk_woe_test5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_log5b_brier$results
model_log5b_brier$resample
pred_log5b_brier <- predict(model_log5b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
log5b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), pred_log5b_brier$X1)
log5b.bs

##Restults logistic regression!
results_log_AUC <- cbind(log1a.ROC$auc,log1b.ROC$auc,log2a.ROC$auc,log2b.ROC$auc,log3a.ROC$auc,log3b.ROC$auc,log4a.ROC$auc,log4b.ROC$auc,log5a.ROC$auc,log5b.ROC$auc)
results_log_bs <- cbind(log1a.bs,log1b.bs,log2a.bs,log2b.bs,log3a.bs,log3b.bs,log4a.bs,log4b.bs,log5a.bs,log5b.bs)
results_log_ngini <- cbind(log1a.ngini,log1b.ngini,log2a.ngini,log2b.ngini,log3a.ngini,log3b.ngini,log4a.ngini,log4b.ngini,log5a.ngini,log5b.ngini)
results_log_gini <- cbind(log1a.gini,log1b.gini,log2a.gini,log2b.gini,log3a.gini,log3b.gini,log4a.gini,log4b.gini,log5a.gini,log5b.gini)
mean(results_log_AUC)
mean(results_log_bs)
mean(results_log_ngini)
mean(results_log_gini)
#########################################
#######J4.8 Decission tree###############
#########################################

DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
###data 1, train-test
#ROC curve 
set.seed(123); model_DT1a_roc <- train(formula, data=uk_woe_train1, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT1a_roc <- predict(model_DT1a_roc,uk_woe_test1,type="prob")
DT1a.ROC <- roc(predictor=predb_DT1a_roc$X0,
                response=uk_woe_test1$Censor,
                levels=rev(levels(uk_woe_test1$Censor)))
DT1a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); model_DT1a_gini <- train(formula, data=uk_woe_train1, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT1a_gini$results
model_DT1a_gini$resample
pred_DT1a_gini<- predict(model_DT1a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, pred_DT1a_gini$X1)
Gini(uk_woe_test1$Censorb, pred_DT1a_gini$X1)
#b <= 0.4
p <- pred_DT1a_gini$X1[pred_DT1a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[pred_DT1a_gini$X1<=0.4]))
DT1a.ngini <- normalizedGini(a, p)
DT1a.ngini
DT1a.gini <-Gini(a, p)
DT1a.gini

#Brier score
set.seed(123); model_DT1a_brier <- train(formula, data=uk_woe_train1, method = "J48", trControl=train_control_brier, metric='BrierScore', tuneGrid=DTgrid,, maximize=FALSE, preProc=c("center","scale"))
model_DT1a_brier$results
model_DT1a_brier$resample
pred_DT1a_brier <- predict(model_DT1a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
DT1a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), pred_DT1a_brier$X1)
DT1a.bs


###data 1 - test-train
#ROC curve 
set.seed(123); model_DT1b_roc <- train(formula, data=uk_woe_test1, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT1b_roc <- predict(model_DT1b_roc,uk_woe_train1,type="prob")
DT1b.ROC <- roc(predictor=predb_DT1b_roc$X0,
                response=uk_woe_train1$Censor,
                levels=rev(levels(uk_woe_train1$Censor)))
DT1b.ROC
#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); model_DT1b_gini <- train(formula, data=uk_woe_test1, method = "J48", trControl=train_control_gini,tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT1b_gini$results
model_DT1b_gini$resample
pred_DT1b_gini<- predict(model_DT1b_gini, newdata = uk_woe_train1, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, pred_DT1b_gini$X1)
Gini(uk_woe_train1$Censorb, pred_DT1b_gini$X1)
#b <= 0.4
p <- pred_DT1b_gini$X1[pred_DT1b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[pred_DT1b_gini$X1<=0.4]))
DT1b.ngini <- normalizedGini(a, p)
DT1b.ngini
DT1b.gini <-Gini(a, p)
DT1b.gini

#Brier score
set.seed(123); model_DT1b_brier <- train(formula, data=uk_woe_test1, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_DT1b_brier$results
model_DT1b_brier$resample
pred_DT1b_brier <- predict(model_DT1b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
DT1b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), pred_DT1b_brier$X1)
DT1b.bs

###data 2, train-test
#ROC curve 
set.seed(123); model_DT2a_roc <- train(formula, data=uk_woe_train2, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT2a_roc <- predict(model_DT2a_roc,uk_woe_test2,type="prob")
DT2a.ROC <- roc(predictor=predb_DT2a_roc$X0,
                response=uk_woe_test2$Censor,
                levels=rev(levels(uk_woe_test2$Censor)))
DT2a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); model_DT2a_gini <- train(formula, data=uk_woe_train2, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT2a_gini$results
model_DT2a_gini$resample
pred_DT2a_gini<- predict(model_DT2a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, pred_DT2a_gini$X1)
Gini(uk_woe_test2$Censorb, pred_DT2a_gini$X1)
#b <= 0.4
p <- pred_DT2a_gini$X1[pred_DT2a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[pred_DT2a_gini$X1<=0.4]))
DT2a.ngini <- normalizedGini(a, p)
DT2a.ngini
DT2a.gini <-Gini(a, p)
DT2a.gini

#Brier score
set.seed(123); model_DT2a_brier <- train(formula, data=uk_woe_train2, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_DT2a_brier$results
model_DT2a_brier$resample
pred_DT2a_brier <- predict(model_DT2a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
DT2a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), pred_DT2a_brier$X1)
DT2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_DT2b_roc <- train(formula, data=uk_woe_test2, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT2b_roc <- predict(model_DT2b_roc,uk_woe_train2,type="prob")
DT2b.ROC <- roc(predictor=predb_DT2b_roc$X0,
                response=uk_woe_train2$Censor,
                levels=rev(levels(uk_woe_train2$Censor)))
DT2b.ROC
#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); model_DT2b_gini <- train(formula, data=uk_woe_test2, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT2b_gini$results
model_DT2b_gini$resample
pred_DT2b_gini<- predict(model_DT2b_gini, newdata = uk_woe_train2, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, pred_DT2b_gini$X1)
Gini(uk_woe_train2$Censorb, pred_DT2b_gini$X1)
#b <= 0.4
p <- pred_DT2b_gini$X1[pred_DT2b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[pred_DT2b_gini$X1<=0.4]))
DT2b.ngini <- normalizedGini(a, p)
DT2b.ngini
DT2b.gini <-Gini(a, p)
DT2b.gini

#Brier score
set.seed(123); model_DT2b_brier <- train(formula, data=uk_woe_test2, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_DT2b_brier$results
model_DT2b_brier$resample
pred_DT2b_brier <- predict(model_DT2b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
DT2b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), pred_DT2b_brier$X1)
DT2b.bs

###data 3, train-test
#ROC curve 
set.seed(123); model_DT3a_roc <- train(formula, data=uk_woe_train3, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT3a_roc <- predict(model_DT3a_roc,uk_woe_test3,type="prob")
DT3a.ROC <- roc(predictor=predb_DT3a_roc$X0,
                response=uk_woe_test3$Censor,
                levels=rev(levels(uk_woe_test3$Censor)))
DT3a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); model_DT3a_gini <- train(formula, data=uk_woe_train3, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT3a_gini$results
model_DT3a_gini$resample
pred_DT3a_gini<- predict(model_DT3a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, pred_DT3a_gini$X1)
Gini(uk_woe_test3$Censorb, pred_DT3a_gini$X1)
#b <= 0.4
p <- pred_DT3a_gini$X1[pred_DT3a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[pred_DT3a_gini$X1<=0.4]))
DT3a.ngini <- normalizedGini(a, p)
DT3a.ngini
DT3a.gini <-Gini(a, p)
DT3a.gini

#Brier score
set.seed(123); model_DT3a_brier <- train(formula, data=uk_woe_train3, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_DT3a_brier$results
model_DT3a_brier$resample
pred_DT3a_brier <- predict(model_DT3a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
DT3a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), pred_DT3a_brier$X1)
DT3a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); model_DT3b_roc <- train(formula, data=uk_woe_test3, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT3b_roc <- predict(model_DT3b_roc,uk_woe_train3,type="prob")
DT3b.ROC <- roc(predictor=predb_DT3b_roc$X0,
                response=uk_woe_train3$Censor,
                levels=rev(levels(uk_woe_train3$Censor)))
DT3b.ROC
#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); model_DT3b_gini <- train(formula, data=uk_woe_test3, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT3b_gini$results
model_DT3b_gini$resample
pred_DT3b_gini<- predict(model_DT3b_gini, newdata = uk_woe_train3, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, pred_DT3b_gini$X1)
Gini(uk_woe_train3$Censorb, pred_DT3b_gini$X1)
#b <= 0.4
p <- pred_DT3b_gini$X1[pred_DT3b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[pred_DT3b_gini$X1<=0.4]))
DT3b.ngini <- normalizedGini(a, p)
DT3b.ngini
DT3b.gini <-Gini(a, p)
DT3b.gini

#Brier score
set.seed(123); model_DT3b_brier <- train(formula, data=uk_woe_test3, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_DT3b_brier$results
model_DT3b_brier$resample
pred_DT3b_brier <- predict(model_DT3b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
DT3b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), pred_DT3b_brier$X1)
DT3b.bs

###data 4, train-test
#ROC curve 
set.seed(123); model_DT4a_roc <- train(formula, data=uk_woe_train4, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT4a_roc <- predict(model_DT4a_roc,uk_woe_test4,type="prob")
DT4a.ROC <- roc(predictor=predb_DT4a_roc$X0,
                response=uk_woe_test4$Censor,
                levels=rev(levels(uk_woe_test4$Censor)))
DT4a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); model_DT4a_gini <- train(formula, data=uk_woe_train4, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT4a_gini$results
model_DT4a_gini$resample
pred_DT4a_gini<- predict(model_DT4a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, pred_DT4a_gini$X1)
Gini(uk_woe_test4$Censorb, pred_DT4a_gini$X1)
#b <= 0.4
p <- pred_DT4a_gini$X1[pred_DT4a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[pred_DT4a_gini$X1<=0.4]))
DT4a.ngini <- normalizedGini(a, p)
DT4a.ngini
DT4a.gini <-Gini(a, p)
DT4a.gini

#Brier score
set.seed(123); model_DT4a_brier <- train(formula, data=uk_woe_train4, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_DT4a_brier$results
model_DT4a_brier$resample
pred_DT4a_brier <- predict(model_DT4a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
DT4a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), pred_DT4a_brier$X1)
DT4a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); model_DT4b_roc <- train(formula, data=uk_woe_test4, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT4b_roc <- predict(model_DT4b_roc,uk_woe_train4,type="prob")
DT4b.ROC <- roc(predictor=predb_DT4b_roc$X0,
                response=uk_woe_train4$Censor,
                levels=rev(levels(uk_woe_train4$Censor)))
DT4b.ROC
#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); model_DT4b_gini <- train(formula, data=uk_woe_test4, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT4b_gini$results
model_DT4b_gini$resample
pred_DT4b_gini<- predict(model_DT4b_gini, newdata = uk_woe_train4, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, pred_DT4b_gini$X1)
Gini(uk_woe_train4$Censorb, pred_DT4b_gini$X1)
#b <= 0.4
p <- pred_DT4b_gini$X1[pred_DT4b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[pred_DT4b_gini$X1<=0.4]))
DT4b.ngini <- normalizedGini(a, p)
DT4b.ngini
DT4b.gini <-Gini(a, p)
DT4b.gini

#Brier score
set.seed(123); model_DT4b_brier <- train(formula, data=uk_woe_test4, method = "J48", trControl=train_control_brier, metric='BrierScore', tuneGrid=DTgrid, maximize=FALSE, preProc=c("center","scale"))
model_DT4b_brier$results
model_DT4b_brier$resample
pred_DT4b_brier <- predict(model_DT4b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
DT4b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), pred_DT4b_brier$X1)
DT4b.bs

###data 5, train-test
#ROC curve 
set.seed(123); model_DT5a_roc <- train(formula, data=uk_woe_train5, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT5a_roc <- predict(model_DT5a_roc,uk_woe_test5,type="prob")
DT5a.ROC <- roc(predictor=predb_DT5a_roc$X0,
                response=uk_woe_test5$Censor,
                levels=rev(levels(uk_woe_test5$Censor)))
DT5a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); model_DT5a_gini <- train(formula, data=uk_woe_train5, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid, metric='Gini', preProc=c("center","scale"))
model_DT5a_gini$results
model_DT5a_gini$resample
pred_DT5a_gini<- predict(model_DT5a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, pred_DT5a_gini$X1)
Gini(uk_woe_test5$Censorb, pred_DT5a_gini$X1)
#b <= 0.4
p <- pred_DT5a_gini$X1[pred_DT5a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[pred_DT5a_gini$X1<=0.4]))
DT5a.ngini <- normalizedGini(a, p)
DT5a.ngini
DT5a.gini <-Gini(a, p)
DT5a.gini

#Brier score
set.seed(123); model_DT5a_brier <- train(formula, data=uk_woe_train5, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_DT5a_brier$results
model_DT5a_brier$resample
pred_DT5a_brier <- predict(model_DT5a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
DT5a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), pred_DT5a_brier$X1)
DT5a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); model_DT5b_roc <- train(formula, data=uk_woe_test5, method = "J48", trControl=train_control_roc, tuneGrid=DTgrid, metric='ROC', preProc=c("center","scale"))
predb_DT5b_roc <- predict(model_DT5b_roc,uk_woe_train5,type="prob")
DT5b.ROC <- roc(predictor=predb_DT5b_roc$X0,
                response=uk_woe_train5$Censor,
                levels=rev(levels(uk_woe_train5$Censor)))
DT5b.ROC
#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); model_DT5b_gini <- train(formula, data=uk_woe_test5, method = "J48", trControl=train_control_gini, tuneGrid=DTgrid,  metric='Gini', preProc=c("center","scale"))
model_DT5b_gini$results
model_DT5b_gini$resample
pred_DT5b_gini<- predict(model_DT5b_gini, newdata = uk_woe_train5, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, pred_DT5b_gini$X1)
Gini(uk_woe_train5$Censorb, pred_DT5b_gini$X1)
#b <= 0.4
p <- pred_DT5b_gini$X1[pred_DT5b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[pred_DT5b_gini$X1<=0.4]))
DT5b.ngini <- normalizedGini(a, p)
DT5b.ngini
DT5b.gini <-Gini(a, p)
DT5b.gini

#Brier score
set.seed(123); model_DT5b_brier <- train(formula, data=uk_woe_test5, method = "J48", trControl=train_control_brier, tuneGrid=DTgrid, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_DT5b_brier$results
model_DT5b_brier$resample
pred_DT5b_brier <- predict(model_DT5b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
DT5b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), pred_DT5b_brier$X1)
DT5b.bs

##Restults DT!
results_DT_AUC <- cbind(DT1a.ROC$auc,DT1b.ROC$auc,DT2a.ROC$auc,DT2b.ROC$auc,DT3a.ROC$auc,DT3b.ROC$auc,DT4a.ROC$auc,DT4b.ROC$auc,DT5a.ROC$auc,DT5b.ROC$auc)
mean(results_DT_AUC)
results_DT_bs <- cbind(DT1a.bs,DT1b.bs,DT2a.bs,DT2b.bs,DT3a.bs,DT3b.bs,DT4a.bs,DT4b.bs,DT5a.bs,DT5b.bs)
mean(results_DT_bs)
results_DT_ngini <- cbind(DT1a.ngini,DT1b.ngini,DT2a.ngini,DT2b.ngini,DT3a.ngini,DT3b.ngini,DT4a.ngini,DT4b.ngini,DT5a.ngini,DT5b.ngini)
mean(results_DT_ngini)
results_DT_gini <- cbind(DT1a.gini,DT1b.gini,DT2a.gini,DT2b.gini,DT3a.gini,DT3b.gini,DT4a.gini,DT4b.gini,DT5a.gini,DT5b.gini)
mean(results_DT_gini)
 
##############################
#######Random forest######### 
#############################
#Custom function for RF
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

m <- floor(log2(length(uk_train1$Censor)+1))
RFtunegrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))

m <- floor(log2(length(uk_woe_train1$Censor)+1))
RFtunegrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))

###data 1, train-test
#ROC curve 
set.seed(123); model_RF1a_roc <- train(formula, data=uk_woe_train1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF1a_roc <- predict(model_RF1a_roc,uk_woe_test1,type="prob")
RF1a.ROC <- roc(predictor=predb_RF1a_roc$X0,
                response=uk_woe_test1$Censor,
                levels=rev(levels(uk_woe_test1$Censor)))
RF1a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); model_RF1a_gini <- train(formula, data=uk_woe_train1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF1a_gini$results
model_RF1a_gini$resample
pred_RF1a_gini<- predict(model_RF1a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, pred_RF1a_gini$X1)
Gini(uk_woe_test1$Censorb, pred_RF1a_gini$X1)
#b <= 0.4
p <- pred_RF1a_gini$X1[pred_RF1a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[pred_RF1a_gini$X1<=0.4]))
RF1a.ngini <- normalizedGini(a, p)
RF1a.ngini
RF1a.gini <-Gini(a, p)
RF1a.gini

#Brier score
set.seed(123); model_RF1a_brier <- train(formula, data=uk_woe_train1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_RF1a_brier$results
model_RF1a_brier$resample
pred_RF1a_brier <- predict(model_RF1a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
RF1a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), pred_RF1a_brier$X1)
RF1a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); model_RF1b_roc <- train(formula, data=uk_woe_test1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF1b_roc <- predict(model_RF1b_roc,uk_woe_train1,type="prob")
RF1b.ROC <- roc(predictor=predb_RF1b_roc$X0,
                response=uk_woe_train1$Censor,
                levels=rev(levels(uk_woe_train1$Censor)))
RF1b.ROC
#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); model_RF1b_gini <- train(formula, data=uk_woe_test1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF1b_gini$results
model_RF1b_gini$resample
pred_RF1b_gini<- predict(model_RF1b_gini, newdata = uk_woe_train1, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, pred_RF1b_gini$X1)
Gini(uk_woe_train1$Censorb, pred_RF1b_gini$X1)
#b <= 0.4
p <- pred_RF1b_gini$X1[pred_RF1b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[pred_RF1b_gini$X1<=0.4]))
RF1b.ngini <- normalizedGini(a, p)
RF1b.ngini
RF1b.gini <-Gini(a, p)
RF1b.gini

#Brier score
set.seed(123); model_RF1b_brier <- train(formula, data=uk_woe_test1, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_RF1b_brier$results
model_RF1b_brier$resample
pred_RF1b_brier <- predict(model_RF1b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
RF1b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), pred_RF1b_brier$X1)
RF1b.bs

#Data2

#ROC curve 
set.seed(123); model_RF2a_roc <- train(formula, data=uk_woe_train2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF2a_roc <- predict(model_RF2a_roc,uk_woe_test2,type="prob")
RF2a.ROC <- roc(predictor=predb_RF2a_roc$X0,
                response=uk_woe_test2$Censor,
                levels=rev(levels(uk_woe_test2$Censor)))
RF2a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); model_RF2a_gini <- train(formula, data=uk_woe_train2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF2a_gini$results
model_RF2a_gini$resample
pred_RF2a_gini<- predict(model_RF2a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, pred_RF2a_gini$X1)
Gini(uk_woe_test2$Censorb, pred_RF2a_gini$X1)
#b <= 0.4
p <- pred_RF2a_gini$X1[pred_RF2a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[pred_RF2a_gini$X1<=0.4]))
RF2a.ngini <- normalizedGini(a, p)
RF2a.ngini
RF2a.gini <-Gini(a, p)
RF2a.gini

#Brier score
set.seed(123); model_RF2a_brier <- train(formula, data=uk_woe_train2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_RF2a_brier$results
model_RF2a_brier$resample
pred_RF2a_brier <- predict(model_RF2a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
RF2a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), pred_RF2a_brier$X1)
RF2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_RF2b_roc <- train(formula, data=uk_woe_test2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF2b_roc <- predict(model_RF2b_roc,uk_woe_train2,type="prob")
RF2b.ROC <- roc(predictor=predb_RF2b_roc$X0,
                response=uk_woe_train2$Censor,
                levels=rev(levels(uk_woe_train2$Censor)))
RF2b.ROC
#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); model_RF2b_gini <- train(formula, data=uk_woe_test2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF2b_gini$results
model_RF2b_gini$resample
pred_RF2b_gini<- predict(model_RF2b_gini, newdata = uk_woe_train2, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, pred_RF2b_gini$X1)
Gini(uk_woe_train2$Censorb, pred_RF2b_gini$X1)
#b <= 0.4
p <- pred_RF2b_gini$X1[pred_RF2b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[pred_RF2b_gini$X1<=0.4]))
RF2b.ngini <- normalizedGini(a, p)
RF2b.ngini
RF2b.gini <-Gini(a, p)
RF2b.gini

#Brier score
set.seed(123); model_RF2b_brier <- train(formula, data=uk_woe_test2, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_RF2b_brier$results
model_RF2b_brier$resample
pred_RF2b_brier <- predict(model_RF2b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
RF2b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), pred_RF2b_brier$X1)
RF2b.bs

#data 3
#ROC curve 
set.seed(123); model_RF3a_roc <- train(formula, data=uk_woe_train3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF3a_roc <- predict(model_RF3a_roc,uk_woe_test3,type="prob")
RF3a.ROC <- roc(predictor=predb_RF3a_roc$X0,
                response=uk_woe_test3$Censor,
                levels=rev(levels(uk_woe_test3$Censor)))
RF3a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); model_RF3a_gini <- train(formula, data=uk_woe_train3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF3a_gini$results
model_RF3a_gini$resample
pred_RF3a_gini<- predict(model_RF3a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, pred_RF3a_gini$X1)
Gini(uk_woe_test3$Censorb, pred_RF3a_gini$X1)
#b <= 0.4
p <- pred_RF3a_gini$X1[pred_RF3a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[pred_RF3a_gini$X1<=0.4]))
RF3a.ngini <- normalizedGini(a, p)
RF3a.ngini
RF3a.gini <-Gini(a, p)
RF3a.gini

#Brier score
set.seed(123); model_RF3a_brier <- train(formula, data=uk_woe_train3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_RF3a_brier$results
model_RF3a_brier$resample
pred_RF3a_brier <- predict(model_RF3a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
RF3a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), pred_RF3a_brier$X1)
RF3a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_RF3b_roc <- train(formula, data=uk_woe_test3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF3b_roc <- predict(model_RF3b_roc,uk_woe_train3,type="prob")
RF3b.ROC <- roc(predictor=predb_RF3b_roc$X0,
                response=uk_woe_train3$Censor,
                levels=rev(levels(uk_woe_train3$Censor)))
RF3b.ROC
#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); model_RF3b_gini <- train(formula, data=uk_woe_test3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF3b_gini$results
model_RF3b_gini$resample
pred_RF3b_gini<- predict(model_RF3b_gini, newdata = uk_woe_train3, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, pred_RF3b_gini$X1)
Gini(uk_woe_train3$Censorb, pred_RF3b_gini$X1)
#b <= 0.4
p <- pred_RF3b_gini$X1[pred_RF3b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[pred_RF3b_gini$X1<=0.4]))
RF3b.ngini <- normalizedGini(a, p)
RF3b.ngini
RF3b.gini <-Gini(a, p)
RF3b.gini

#Brier score
set.seed(123); model_RF3b_brier <- train(formula, data=uk_woe_test3, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_RF3b_brier$results
model_RF3b_brier$resample
pred_RF3b_brier <- predict(model_RF3b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
RF3b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), pred_RF3b_brier$X1)
RF3b.bs

#data 4
#ROC curve 
set.seed(123); model_RF4a_roc <- train(formula, data=uk_woe_train4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF4a_roc <- predict(model_RF4a_roc,uk_woe_test4,type="prob")
RF4a.ROC <- roc(predictor=predb_RF4a_roc$X0,
                response=uk_woe_test4$Censor,
                levels=rev(levels(uk_woe_test4$Censor)))
RF4a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); model_RF4a_gini <- train(formula, data=uk_woe_train4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF4a_gini$results
model_RF4a_gini$resample
pred_RF4a_gini<- predict(model_RF4a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, pred_RF4a_gini$X1)
Gini(uk_woe_test4$Censorb, pred_RF4a_gini$X1)
#b <= 0.4
p <- pred_RF4a_gini$X1[pred_RF4a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[pred_RF4a_gini$X1<=0.4]))
RF4a.ngini <- normalizedGini(a, p)
RF4a.ngini
RF4a.gini <-Gini(a, p)
RF4a.gini

#Brier score
set.seed(123); model_RF4a_brier <- train(formula, data=uk_woe_train4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_RF4a_brier$results
model_RF4a_brier$resample
pred_RF4a_brier <- predict(model_RF4a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
RF4a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), pred_RF4a_brier$X1)
RF4a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_RF4b_roc <- train(formula, data=uk_woe_test4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF4b_roc <- predict(model_RF4b_roc,uk_woe_train4,type="prob")
RF4b.ROC <- roc(predictor=predb_RF4b_roc$X0,
                response=uk_woe_train4$Censor,
                levels=rev(levels(uk_woe_train4$Censor)))
RF4b.ROC
#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); model_RF4b_gini <- train(formula, data=uk_woe_test4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF4b_gini$results
model_RF4b_gini$resample
pred_RF4b_gini<- predict(model_RF4b_gini, newdata = uk_woe_train4, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, pred_RF4b_gini$X1)
Gini(uk_woe_train4$Censorb, pred_RF4b_gini$X1)
#b <= 0.4
p <- pred_RF4b_gini$X1[pred_RF4b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[pred_RF4b_gini$X1<=0.4]))
RF4b.ngini <- normalizedGini(a, p)
RF4b.ngini
RF4b.gini <-Gini(a, p)
RF4b.gini

#Brier score
set.seed(123); model_RF4b_brier <- train(formula, data=uk_woe_test4, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_RF4b_brier$results
model_RF4b_brier$resample
pred_RF4b_brier <- predict(model_RF4b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
RF4b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), pred_RF4b_brier$X1)
RF4b.bs

#data 5
#ROC curve 
set.seed(123); model_RF5a_roc <- train(formula, data=uk_woe_train5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF5a_roc <- predict(model_RF5a_roc,uk_woe_test5,type="prob")
RF5a.ROC <- roc(predictor=predb_RF5a_roc$X0,
                response=uk_woe_test5$Censor,
                levels=rev(levels(uk_woe_test5$Censor)))
RF5a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); model_RF5a_gini <- train(formula, data=uk_woe_train5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF5a_gini$results
model_RF5a_gini$resample
pred_RF5a_gini<- predict(model_RF5a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, pred_RF5a_gini$X1)
Gini(uk_woe_test5$Censorb, pred_RF5a_gini$X1)
#b <= 0.4
p <- pred_RF5a_gini$X1[pred_RF5a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[pred_RF5a_gini$X1<=0.4]))
RF5a.ngini <- normalizedGini(a, p)
RF5a.ngini
RF5a.gini <-Gini(a, p)
RF5a.gini

#Brier score
set.seed(123); model_RF5a_brier <- train(formula, data=uk_woe_train5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
model_RF5a_brier$results
model_RF5a_brier$resample
pred_RF5a_brier <- predict(model_RF5a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
RF5a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), pred_RF5a_brier$X1)
RF5a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); model_RF5b_roc <- train(formula, data=uk_woe_test5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
predb_RF5b_roc <- predict(model_RF5b_roc,uk_woe_train5,type="prob")
RF5b.ROC <- roc(predictor=predb_RF5b_roc$X0,
                response=uk_woe_train5$Censor,
                levels=rev(levels(uk_woe_train5$Censor)))
RF5b.ROC
#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); model_RF5b_gini <- train(formula, data=uk_woe_test5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
model_RF5b_gini$results
model_RF5b_gini$resample
pred_RF5b_gini<- predict(model_RF5b_gini, newdata = uk_woe_train5, type='prob')

#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, pred_RF5b_gini$X1)
Gini(uk_woe_train5$Censorb, pred_RF5b_gini$X1)
#b <= 0.4
p <- pred_RF5b_gini$X1[pred_RF5b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[pred_RF5b_gini$X1<=0.4]))
RF5b.ngini <- normalizedGini(a, p)
RF5b.ngini
RF5b.gini <-Gini(a, p)
RF5b.gini

#Brier score
set.seed(123); model_RF5b_brier <- train(formula, data=uk_woe_test5, method = customRF, tuneGrid=RFtunegrid, trControl=train_control_brier, metric='BrierScore',maximize=FALSE, preProc=c("center","scale"))
model_RF5b_brier$results
model_RF5b_brier$resample
pred_RF5b_brier <- predict(model_RF5b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
RF5b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), pred_RF5b_brier$X1)
RF5b.bs

##Restults RF!
results_RF_AUC <- cbind(RF1a.ROC$auc,RF1b.ROC$auc,RF2a.ROC$auc,RF2b.ROC$auc,RF3a.ROC$auc,RF3b.ROC$auc,RF4a.ROC$auc,RF4b.ROC$auc,RF5a.ROC$auc,RF5b.ROC$auc)
mean(results_RF_AUC)
results_RF_bs <- cbind(RF1a.bs,RF1b.bs,RF2a.bs,RF2b.bs,RF3a.bs,RF3b.bs,RF4a.bs,RF4b.bs,RF5a.bs,RF5b.bs)
mean(results_RF_bs)
results_RF_ngini <- cbind(RF1a.ngini,RF1b.ngini,RF2a.ngini,RF2b.ngini,RF3a.ngini,RF3b.ngini,RF4a.ngini,RF4b.ngini,RF5a.ngini,RF5b.ngini)
mean(results_RF_ngini)
results_RF_gini <- cbind(RF1a.gini,RF1b.gini,RF2a.gini,RF2b.gini,RF3a.gini,RF3b.gini,RF4a.gini,RF4b.gini,RF5a.gini,RF5b.gini)
mean(results_RF_gini)


###########################
#########MLP###############
###########################
MLP1 <- list(label = "Multilayer Perceptron Network with Dropout",
             library = "keras",
             loop = NULL,
             type = c('Regression', "Classification"),
             parameters = data.frame(
               parameter = c('size', 'dropout', 
                             "batch_size",
                             "lr", "rho", "decay", 
                             "activation"),
               class = c(rep('numeric', 6), "character"),
               label = c('#Hidden Units', 'Dropout Rate', 
                         "Batch Size", "Learning Rate",
                         "Rho", "Learning Rate Decay",
                         "Activation Function")
             ),
             grid = function(x, y, len = NULL, search = "grid") {
               afuncs <- c("sigmoid", "relu", "tanh")
               if(search == "grid") {
                 out <- expand.grid(
                   size = ((1:len) * 2) - 1, 
                   dropout = seq(0, .7, length = len), 
                   batch_size = floor(nrow(x)/3),
                   lr = 2e-6,
                   rho = .9,
                   decay = 0,
                   activation = "relu"
                 )
               } else {
                 n <- nrow(x)
                 out <- data.frame(
                   size = sample(2:20, replace = TRUE, size = len),
                   dropout = runif(len, max = .7), 
                   batch_size = floor(n*runif(len, min = .1)),
                   lr = runif(len),
                   rho = runif(len),
                   decay = 10^runif(len, min = -5, 0),
                   activation = sample(
                     afuncs, 
                     size = len, 
                     replace = TRUE
                   )
                 )
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               require(dplyr)
               K <- keras::backend()
               K$clear_session()
               if(!is.matrix(x)) x <- as.matrix(x)
               model <- keras::keras_model_sequential()
               model %>% 
                 keras::layer_dense(
                   units = param$size, 
                   activation = as.character(param$activation), 
                   kernel_initializer = keras::initializer_glorot_uniform(),
                   input_shape = ncol(x)
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))
               if(is.factor(y)) {
                 y <- class2ind(y)
                 model %>% 
                   keras::layer_dense(
                     units = length(lev), 
                     activation = 'softmax'
                   ) %>%
                   keras::compile(
                     loss = "categorical_crossentropy",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "accuracy"
                   )
               } else {
                 model %>% 
                   keras::layer_dense(
                     units = 1, 
                     activation = 'linear'
                   ) %>%
                   keras::compile(
                     loss = "mean_squared_error",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "mean_squared_error"
                   )
               }
               model %>% keras::fit(
                 x = x, 
                 y = y,
                 batch_size = param$batch_size,
                 ...
               )
               if(last)
                 model <- keras::serialize_model(model)
               list(object = model)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               ## check for model type
               if(ncol(out) == 1) {
                 out <- out[, 1]
               } else {
                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
               }
               out
             },
             prob =  function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               colnames(out) <- modelFit$obsLevels
               as.data.frame(out)
             },
             varImp = NULL,
             tags = c("Neural Network"),
             sort = function(x) x[order(x$size, -x$dropout),],
             notes = paste("After `train` completes, the keras model object is serialized",
                           "so that it can be used between R session. When predicting, the", 
                           "code will temporarily unsearalize the object. To make the", 
                           "predictions more efficient, the user might want to use ", 
                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                           "R session so that that operation is only done once.",
                           "Also, this model cannot be run in parallel due to",
                           "the nature of how tensorflow does the computations.",
                           
                           "Unlike other packages used by `train`, the `dplyr`",
                           "package is fully loaded when this model is used."),
             check = function(pkg) {
               testmod <- try(keras::keras_model_sequential(),
                              silent = TRUE)
               if(inherits(testmod, "try-error"))
                 stop("Could not start a sequential model. ",
                      "`tensorflow` might not be installed. ",
                      "See `?install_tensorflow`.", 
                      call. = FALSE)
               TRUE
             })


MLP3 <- list(label = "Multilayer Perceptron Network with Dropout",
             library = "keras",
             loop = NULL,
             type = c('Regression', "Classification"),
             parameters = data.frame(
               parameter = c('size1', 'size2', 'size3', 
                             'dropout',  "batch_size",
                             "lr", "rho", "decay", 
                             "activation"),
               class = c(rep('numeric', 8), "character"),
               label = c('#Hidden Units1', '#Hidden Units2', '#Hidden Units3', 
                         'Dropout Rate', "Batch Size", "Learning Rate",
                         "Rho", "Learning Rate Decay",
                         "Activation Function")
             ),
             grid = function(x, y, len = NULL, search = "grid") {
               afuncs <- c("sigmoid", "relu", "tanh")
               if(search == "grid") {
                 out <- expand.grid(
                   size1 = ((1:len) * 2) - 1, 
                   size2 = ((1:len) * 2) - 1, 
                   size3 = ((1:len) * 2) - 1, 
                   dropout = seq(0, .7, length = len), 
                   batch_size = floor(nrow(x)/3),
                   lr = 2e-6,
                   rho = .9,
                   decay = 0,
                   activation = "relu"
                 )
               } else {
                 n <- nrow(x)
                 out <- data.frame(
                   size1 = sample(2:20, replace = TRUE, size = len),
                   size2 = sample(2:20, replace = TRUE, size = len),
                   size3 = sample(2:20, replace = TRUE, size = len),
                   dropout = runif(len, max = .7), 
                   batch_size = floor(n*runif(len, min = .1)),
                   lr = runif(len),
                   rho = runif(len),
                   decay = 10^runif(len, min = -5, 0),
                   activation = sample(
                     afuncs, 
                     size = len, 
                     replace = TRUE
                   )
                 )
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               require(dplyr)
               K <- keras::backend()
               K$clear_session()
               if(!is.matrix(x)) x <- as.matrix(x)
               model <- keras::keras_model_sequential()
               model %>% 
                 keras::layer_dense(
                   units = param$size1, 
                   activation = as.character(param$activation), 
                   kernel_initializer = keras::initializer_glorot_uniform(),
                   input_shape = ncol(x)
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1)) %>% 
                 keras::layer_dense(
                   units = param$size2, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))%>% 
                 keras::layer_dense(
                   units = param$size3, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))
               
               if(is.factor(y)) {
                 y <- class2ind(y)
                 model %>% 
                   keras::layer_dense(
                     units = length(lev), 
                     activation = 'softmax'
                   ) %>%
                   keras::compile(
                     loss = "categorical_crossentropy",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "accuracy"
                   )
               } else {
                 model %>% 
                   keras::layer_dense(
                     units = 1, 
                     activation = 'linear'
                   ) %>%
                   keras::compile(
                     loss = "mean_squared_error",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "mean_squared_error"
                   )
               }
               model %>% keras::fit(
                 x = x, 
                 y = y,
                 batch_size = param$batch_size,
                 ...
               )
               if(last)
                 model <- keras::serialize_model(model)
               list(object = model)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               ## check for model type
               if(ncol(out) == 1) {
                 out <- out[, 1]
               } else {
                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
               }
               out
             },
             prob =  function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               colnames(out) <- modelFit$obsLevels
               as.data.frame(out)
             },
             varImp = NULL,
             tags = c("Neural Network"),
             sort = function(x) x[order(x$size1, x$size2, x$size3, -x$dropout),],
             notes = paste("After `train` completes, the keras model object is serialized",
                           "so that it can be used between R session. When predicting, the", 
                           "code will temporarily unsearalize the object. To make the", 
                           "predictions more efficient, the user might want to use ", 
                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                           "R session so that that operation is only done once.",
                           "Also, this model cannot be run in parallel due to",
                           "the nature of how tensorflow does the computations.",
                           
                           "Unlike other packages used by `train`, the `dplyr`",
                           "package is fully loaded when this model is used."),
             check = function(pkg) {
               testmod <- try(keras::keras_model_sequential(),
                              silent = TRUE)
               if(inherits(testmod, "try-error"))
                 stop("Could not start a sequential model. ",
                      "`tensorflow` might not be installed. ",
                      "See `?install_tensorflow`.", 
                      call. = FALSE)
               TRUE
             })

MLP5 <- list(label = "Multilayer Perceptron Network with Dropout",
             library = "keras",
             loop = NULL,
             type = c('Regression', "Classification"),
             parameters = data.frame(
               parameter = c('size1', 'size2', 'size3', 'size4', 'size5', 
                             'dropout',  "batch_size",
                             "lr", "rho", "decay", 
                             "activation"),
               class = c(rep('numeric', 10), "character"),
               label = c('#Hidden Units1', '#Hidden Units2', '#Hidden Units3', '#Hidden Units4', '#Hidden Units5', 
                         'Dropout Rate', "Batch Size", "Learning Rate",
                         "Rho", "Learning Rate Decay",
                         "Activation Function")
             ),
             grid = function(x, y, len = NULL, search = "grid") {
               afuncs <- c("sigmoid", "relu", "tanh")
               if(search == "grid") {
                 out <- expand.grid(
                   size1 = ((1:len) * 2) - 1, 
                   size2 = ((1:len) * 2) - 1, 
                   size3 = ((1:len) * 2) - 1, 
                   size4 = ((1:len) * 2) - 1, 
                   size5 = ((1:len) * 2) - 1, 
                   dropout = seq(0, .7, length = len), 
                   batch_size = floor(nrow(x)/3),
                   lr = 2e-6,
                   rho = .9,
                   decay = 0,
                   activation = "relu"
                 )
               } else {
                 n <- nrow(x)
                 out <- data.frame(
                   size1 = sample(2:20, replace = TRUE, size = len),
                   size2 = sample(2:20, replace = TRUE, size = len),
                   size3 = sample(2:20, replace = TRUE, size = len),
                   size4 = sample(2:20, replace = TRUE, size = len),
                   size5 = sample(2:20, replace = TRUE, size = len),
                   dropout = runif(len, max = .7), 
                   batch_size = floor(n*runif(len, min = .1)),
                   lr = runif(len),
                   rho = runif(len),
                   decay = 10^runif(len, min = -5, 0),
                   activation = sample(
                     afuncs, 
                     size = len, 
                     replace = TRUE
                   )
                 )
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               require(dplyr)
               K <- keras::backend()
               K$clear_session()
               if(!is.matrix(x)) x <- as.matrix(x)
               model <- keras::keras_model_sequential()
               model %>% 
                 keras::layer_dense(
                   units = param$size1, 
                   activation = as.character(param$activation), 
                   kernel_initializer = keras::initializer_glorot_uniform(),
                   input_shape = ncol(x)
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1)) %>% 
                 keras::layer_dense(
                   units = param$size2, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))%>% 
                 keras::layer_dense(
                   units = param$size3, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1)) %>% 
                 keras::layer_dense(
                   units = param$size4, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))%>% 
                 keras::layer_dense(
                   units = param$size5, 
                   activation = as.character(param$activation), 
                 ) %>%
                 keras::layer_dropout(rate = param$dropout,
                                      seed = sample.int(1000, 1))
               
               if(is.factor(y)) {
                 y <- class2ind(y)
                 model %>% 
                   keras::layer_dense(
                     units = length(lev), 
                     activation = 'softmax'
                   ) %>%
                   keras::compile(
                     loss = "categorical_crossentropy",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "accuracy"
                   )
               } else {
                 model %>% 
                   keras::layer_dense(
                     units = 1, 
                     activation = 'linear'
                   ) %>%
                   keras::compile(
                     loss = "mean_squared_error",
                     optimizer = keras::optimizer_rmsprop(
                       lr = param$lr,
                       rho = param$rho,
                       decay = param$decay
                     ),
                     metrics = "mean_squared_error"
                   )
               }
               model %>% keras::fit(
                 x = x, 
                 y = y,
                 batch_size = param$batch_size,
                 ...
               )
               if(last)
                 model <- keras::serialize_model(model)
               list(object = model)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               ## check for model type
               if(ncol(out) == 1) {
                 out <- out[, 1]
               } else {
                 out <- modelFit$obsLevels[apply(out, 1, which.max)]
               }
               out
             },
             prob =  function(modelFit, newdata, submodels = NULL) {
               if(inherits(modelFit$object, "raw"))
                 modelFit$object <- keras::unserialize_model(modelFit$object)
               if(!is.matrix(newdata)) 
                 newdata <- as.matrix(newdata)
               out <- predict(modelFit$object, newdata)
               colnames(out) <- modelFit$obsLevels
               as.data.frame(out)
             },
             varImp = NULL,
             tags = c("Neural Network"),
             sort = function(x) x[order(x$size1, x$size2, x$size3, x$size4, x$size5, -x$dropout),],
             notes = paste("After `train` completes, the keras model object is serialized",
                           "so that it can be used between R session. When predicting, the", 
                           "code will temporarily unsearalize the object. To make the", 
                           "predictions more efficient, the user might want to use ", 
                           "`keras::unsearlize_model(object$finalModel$object)` in the current", 
                           "R session so that that operation is only done once.",
                           "Also, this model cannot be run in parallel due to",
                           "the nature of how tensorflow does the computations.",
                           
                           "Unlike other packages used by `train`, the `dplyr`",
                           "package is fully loaded when this model is used."),
             check = function(pkg) {
               testmod <- try(keras::keras_model_sequential(),
                              silent = TRUE)
               if(inherits(testmod, "try-error"))
                 stop("Could not start a sequential model. ",
                      "`tensorflow` might not be installed. ",
                      "See `?install_tensorflow`.", 
                      call. = FALSE)
               TRUE
             })

#training control
train_control_roc <- trainControl(method="cv", number=5, savePredictions=TRUE,classProbs=TRUE, summaryFunction=twoClassSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_gini <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=giniSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_brier <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=brierSummary, verboseIter = TRUE, allowParallel = TRUE)
#batch size
batch <- floor(nrow(uk_train1)/3)


##############################
###########MLP1###############
##############################
MLP1grid <- expand.grid(.size=c(5,10,15,20), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01,0.001), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP11a_roc <- train(formula, data=uk_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP1grid <- expand.grid(.size=c(15), .dropout=c(0.25), .batch_size=batch, .lr=c(0.05), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP11a_roc <- train(formula, data=uk_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
ukpredb_MLP11a_roc <- predict(uk_model_MLP11a_roc,uk_woe_test1,type="prob")
uk_MLP11a.ROC <- roc(predictor=ukpredb_MLP11a_roc$X0,
                     response=uk_woe_test1$Censor,
                     levels=rev(levels(uk_woe_test1$Censor)))
uk_MLP11a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); uk_model_MLP11a_gini <- train(formula, data=uk_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP11a_gini$results
uk_model_MLP11a_gini$resample
uk_pred_MLP11a_gini<- predict(uk_model_MLP11a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, uk_pred_MLP11a_gini$X1)
Gini(uk_woe_test1$Censorb, uk_pred_MLP11a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP11a_gini$X1[uk_pred_MLP11a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[uk_pred_MLP11a_gini$X1<=0.4]))
uk_MLP11a.ngini <- normalizedGini(a, p)
uk_MLP11a.ngini
uk_MLP11a.gini <-Gini(a, p)
uk_MLP11a.gini

#Brier score
set.seed(123); uk_model_MLP11a_brier <- train(formula, data=uk_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP11a_brier$results
uk_model_MLP11a_brier$resample
uk_pred_MLP11a_brier <- predict(uk_model_MLP11a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_MLP11a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), uk_pred_MLP11a_brier$X1)
uk_MLP11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_MLP11b_roc <- train(formula, data=uk_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP11b_roc <- predict(uk_model_MLP11b_roc, uk_woe_train1,type="prob")
uk_MLP11b.ROC <- roc(predictor=ukpredb_MLP11b_roc$X0,
                     response=uk_woe_train1$Censor,
                     levels=rev(levels(uk_woe_train1$Censor)))
uk_MLP11b.ROC

#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); uk_model_MLP11b_gini <- train(formula, data=uk_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP11b_gini$results
uk_model_MLP11b_gini$resample
uk_pred_MLP11b_gini<- predict(uk_model_MLP11b_gini, newdata = uk_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, uk_pred_MLP11b_gini$X1)
Gini(uk_woe_train1$Censorb, uk_pred_MLP11b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP11b_gini$X1[uk_pred_MLP11b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[uk_pred_MLP11b_gini$X1<=0.4]))
uk_MLP11b.ngini <- normalizedGini(a, p)
uk_MLP11b.ngini
uk_MLP11b.gini <-Gini(a, p)
uk_MLP11b.gini

#Brier score
set.seed(123); uk_model_MLP11b_brier <- train(formula, data=uk_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP11b_brier$results
uk_model_MLP11b_brier$resample
uk_pred_MLP11b_brier <- predict(uk_model_MLP11b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_MLP11b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), uk_pred_MLP11b_brier$X1)
uk_MLP11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP12a_roc <- train(formula, data=uk_woe_train2, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP12a_roc <- predict(uk_model_MLP12a_roc,uk_woe_test2,type="prob")
uk_MLP12a.ROC <- roc(predictor=ukpredb_MLP12a_roc$X0,
                     response=uk_woe_test2$Censor,
                     levels=rev(levels(uk_woe_test2$Censor)))
uk_MLP12a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); uk_model_MLP12a_gini <- train(formula, data=uk_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP12a_gini$results
uk_model_MLP12a_gini$resample
uk_pred_MLP12a_gini<- predict(uk_model_MLP12a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, uk_pred_MLP12a_gini$X1)
Gini(uk_woe_test2$Censorb, uk_pred_MLP12a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP12a_gini$X1[uk_pred_MLP12a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[uk_pred_MLP12a_gini$X1<=0.4]))
uk_MLP12a.ngini <- normalizedGini(a, p)
uk_MLP12a.ngini
uk_MLP12a.gini <-Gini(a, p)
uk_MLP12a.gini

#Brier score
set.seed(123); uk_model_MLP12a_brier <- train(formula, data=uk_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP12a_brier$results
uk_model_MLP12a_brier$resample
uk_pred_MLP12a_brier <- predict(uk_model_MLP12a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_MLP12a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), uk_pred_MLP12a_brier$X1)
uk_MLP12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_MLP12b_roc <- train(formula, data=uk_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP12b_roc <- predict(uk_model_MLP12b_roc, uk_woe_train2,type="prob")
uk_MLP12b.ROC <- roc(predictor=ukpredb_MLP12b_roc$X0,
                     response=uk_woe_train2$Censor,
                     levels=rev(levels(uk_woe_train2$Censor)))
uk_MLP12b.ROC

#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); uk_model_MLP12b_gini <- train(formula, data=uk_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP12b_gini$results
uk_model_MLP12b_gini$resample
uk_pred_MLP12b_gini<- predict(uk_model_MLP12b_gini, newdata = uk_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, uk_pred_MLP12b_gini$X1)
Gini(uk_woe_train2$Censorb, uk_pred_MLP12b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP12b_gini$X1[uk_pred_MLP12b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[uk_pred_MLP12b_gini$X1<=0.4]))
uk_MLP12b.ngini <- normalizedGini(a, p)
uk_MLP12b.ngini
uk_MLP12b.gini <-Gini(a, p)
uk_MLP12b.gini

#Brier score
set.seed(123); uk_model_MLP12b_brier <- train(formula, data=uk_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP12b_brier$results
uk_model_MLP12b_brier$resample
uk_pred_MLP12b_brier <- predict(uk_model_MLP12b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_MLP12b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), uk_pred_MLP12b_brier$X1)
uk_MLP12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP13a_roc <- train(formula, data=uk_woe_train3, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP13a_roc <- predict(uk_model_MLP13a_roc,uk_woe_test3,type="prob")
uk_MLP13a.ROC <- roc(predictor=ukpredb_MLP13a_roc$X0,
                     response=uk_woe_test3$Censor,
                     levels=rev(levels(uk_woe_test3$Censor)))
uk_MLP13a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); uk_model_MLP13a_gini <- train(formula, data=uk_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP13a_gini$results
uk_model_MLP13a_gini$resample
uk_pred_MLP13a_gini<- predict(uk_model_MLP13a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, uk_pred_MLP13a_gini$X1)
Gini(uk_woe_test3$Censorb, uk_pred_MLP13a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP13a_gini$X1[uk_pred_MLP13a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[uk_pred_MLP13a_gini$X1<=0.4]))
uk_MLP13a.ngini <- normalizedGini(a, p)
uk_MLP13a.ngini
uk_MLP13a.gini <-Gini(a, p)
uk_MLP13a.gini

#Brier score
set.seed(123); uk_model_MLP13a_brier <- train(formula, data=uk_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP13a_brier$results
uk_model_MLP13a_brier$resample
uk_pred_MLP13a_brier <- predict(uk_model_MLP13a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_MLP13a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), uk_pred_MLP13a_brier$X1)
uk_MLP13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_MLP13b_roc <- train(formula, data=uk_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP13b_roc <- predict(uk_model_MLP13b_roc, uk_woe_train3,type="prob")
uk_MLP13b.ROC <- roc(predictor=ukpredb_MLP13b_roc$X0,
                     response=uk_woe_train3$Censor,
                     levels=rev(levels(uk_woe_train3$Censor)))
uk_MLP13b.ROC

#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); uk_model_MLP13b_gini <- train(formula, data=uk_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP13b_gini$results
uk_model_MLP13b_gini$resample
uk_pred_MLP13b_gini<- predict(uk_model_MLP13b_gini, newdata = uk_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, uk_pred_MLP13b_gini$X1)
Gini(uk_woe_train3$Censorb, uk_pred_MLP13b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP13b_gini$X1[uk_pred_MLP13b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[uk_pred_MLP13b_gini$X1<=0.4]))
uk_MLP13b.ngini <- normalizedGini(a, p)
uk_MLP13b.ngini
uk_MLP13b.gini <-Gini(a, p)
uk_MLP13b.gini

#Brier score
set.seed(123); uk_model_MLP13b_brier <- train(formula, data=uk_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP13b_brier$results
uk_model_MLP13b_brier$resample
uk_pred_MLP13b_brier <- predict(uk_model_MLP13b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_MLP13b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), uk_pred_MLP13b_brier$X1)
uk_MLP13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP14a_roc <- train(formula, data=uk_woe_train4, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP14a_roc <- predict(uk_model_MLP14a_roc,uk_woe_test4,type="prob")
uk_MLP14a.ROC <- roc(predictor=ukpredb_MLP14a_roc$X0,
                     response=uk_woe_test4$Censor,
                     levels=rev(levels(uk_woe_test4$Censor)))
uk_MLP14a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); uk_model_MLP14a_gini <- train(formula, data=uk_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP14a_gini$results
uk_model_MLP14a_gini$resample
uk_pred_MLP14a_gini<- predict(uk_model_MLP14a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, uk_pred_MLP14a_gini$X1)
Gini(uk_woe_test4$Censorb, uk_pred_MLP14a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP14a_gini$X1[uk_pred_MLP14a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[uk_pred_MLP14a_gini$X1<=0.4]))
uk_MLP14a.ngini <- normalizedGini(a, p)
uk_MLP14a.ngini
uk_MLP14a.gini <-Gini(a, p)
uk_MLP14a.gini

#Brier score
set.seed(123); uk_model_MLP14a_brier <- train(formula, data=uk_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP14a_brier$results
uk_model_MLP14a_brier$resample
uk_pred_MLP14a_brier <- predict(uk_model_MLP14a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_MLP14a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), uk_pred_MLP14a_brier$X1)
uk_MLP14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_MLP14b_roc <- train(formula, data=uk_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP14b_roc <- predict(uk_model_MLP14b_roc, uk_woe_train4,type="prob")
uk_MLP14b.ROC <- roc(predictor=ukpredb_MLP14b_roc$X0,
                     response=uk_woe_train4$Censor,
                     levels=rev(levels(uk_woe_train4$Censor)))
uk_MLP14b.ROC

#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); uk_model_MLP14b_gini <- train(formula, data=uk_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP14b_gini$results
uk_model_MLP14b_gini$resample
uk_pred_MLP14b_gini<- predict(uk_model_MLP14b_gini, newdata = uk_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, uk_pred_MLP14b_gini$X1)
Gini(uk_woe_train4$Censorb, uk_pred_MLP14b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP14b_gini$X1[uk_pred_MLP14b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[uk_pred_MLP14b_gini$X1<=0.4]))
uk_MLP14b.ngini <- normalizedGini(a, p)
uk_MLP14b.ngini
uk_MLP14b.gini <-Gini(a, p)
uk_MLP14b.gini

#Brier score
set.seed(123); uk_model_MLP14b_brier <- train(formula, data=uk_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP14b_brier$results
uk_model_MLP14b_brier$resample
uk_pred_MLP14b_brier <- predict(uk_model_MLP14b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_MLP14b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), uk_pred_MLP14b_brier$X1)
uk_MLP14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP15a_roc <- train(formula, data=uk_woe_train5, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP15a_roc <- predict(uk_model_MLP15a_roc,uk_woe_test5,type="prob")
uk_MLP15a.ROC <- roc(predictor=ukpredb_MLP15a_roc$X0,
                     response=uk_woe_test5$Censor,
                     levels=rev(levels(uk_woe_test5$Censor)))
uk_MLP15a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); uk_model_MLP15a_gini <- train(formula, data=uk_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP15a_gini$results
uk_model_MLP15a_gini$resample
uk_pred_MLP15a_gini<- predict(uk_model_MLP15a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, uk_pred_MLP15a_gini$X1)
Gini(uk_woe_test5$Censorb, uk_pred_MLP15a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP15a_gini$X1[uk_pred_MLP15a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[uk_pred_MLP15a_gini$X1<=0.4]))
uk_MLP15a.ngini <- normalizedGini(a, p)
uk_MLP15a.ngini
uk_MLP15a.gini <-Gini(a, p)
uk_MLP15a.gini

#Brier score
set.seed(123); uk_model_MLP15a_brier <- train(formula, data=uk_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP15a_brier$results
uk_model_MLP15a_brier$resample
uk_pred_MLP15a_brier <- predict(uk_model_MLP15a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_MLP15a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), uk_pred_MLP15a_brier$X1)
uk_MLP15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_MLP15b_roc <- train(formula, data=uk_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP15b_roc <- predict(uk_model_MLP15b_roc, uk_woe_train5,type="prob")
uk_MLP15b.ROC <- roc(predictor=ukpredb_MLP15b_roc$X0,
                     response=uk_woe_train5$Censor,
                     levels=rev(levels(uk_woe_train5$Censor)))
uk_MLP15b.ROC

#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); uk_model_MLP15b_gini <- train(formula, data=uk_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP15b_gini$results
uk_model_MLP15b_gini$resample
uk_pred_MLP15b_gini<- predict(uk_model_MLP15b_gini, newdata = uk_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, uk_pred_MLP15b_gini$X1)
Gini(uk_woe_train5$Censorb, uk_pred_MLP15b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP15b_gini$X1[uk_pred_MLP15b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[uk_pred_MLP15b_gini$X1<=0.4]))
uk_MLP15b.ngini <- normalizedGini(a, p)
uk_MLP15b.ngini
uk_MLP15b.gini <-Gini(a, p)
uk_MLP15b.gini

#Brier score
set.seed(123); uk_model_MLP15b_brier <- train(formula, data=uk_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP15b_brier$results
uk_model_MLP15b_brier$resample
uk_pred_MLP15b_brier <- predict(uk_model_MLP15b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_MLP15b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), uk_pred_MLP15b_brier$X1)
uk_MLP15b.bs

##Restults MLP1!
uk_results_MLP1_AUC <- cbind(uk_MLP11a.ROC$auc,uk_MLP11b.ROC$auc,uk_MLP12a.ROC$auc,uk_MLP12b.ROC$auc,uk_MLP13a.ROC$auc,uk_MLP13b.ROC$auc,uk_MLP14a.ROC$auc,
                             uk_MLP14b.ROC$auc,uk_MLP15a.ROC$auc,uk_MLP15b.ROC$auc)
uk_results_MLP1_bs <- cbind(uk_MLP11a.bs,uk_MLP11b.bs,uk_MLP12a.bs,uk_MLP12b.bs,uk_MLP13a.bs,uk_MLP13b.bs,uk_MLP14a.bs,uk_MLP14b.bs,uk_MLP15a.bs,uk_MLP15b.bs)
uk_results_MLP1_ngini <- cbind(uk_MLP11a.ngini,uk_MLP11b.ngini,uk_MLP12a.ngini,uk_MLP12b.ngini,uk_MLP13a.ngini,uk_MLP13b.ngini,uk_MLP14a.ngini,uk_MLP14b.ngini,
                               uk_MLP15a.ngini,uk_MLP15b.ngini)
uk_results_MLP1_gini <- cbind(uk_MLP11a.gini,uk_MLP11b.gini,uk_MLP12a.gini,uk_MLP12b.gini,uk_MLP13a.gini,uk_MLP13b.gini,uk_MLP14a.gini,uk_MLP14b.gini,
                              uk_MLP15a.gini,uk_MLP15b.gini)
mean(uk_results_MLP1_AUC)
mean(uk_results_MLP1_bs)
mean(uk_results_MLP1_ngini)
mean(uk_results_MLP1_gini)


##############################
###########MLP3###############
##############################

MLP3grid <- expand.grid(.size1=c(5,10,15,20), .size2=c(5,10,15,20), .size3=c(5,10,15,20), .dropout=c(0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP31a_roc <- train(formula, data=uk_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune:
MLP3grid <- expand.grid(.size1=c(20), .size2=c(20), .size3=c(5), .dropout=c(0.25), .batch_size=batch, .lr=c(0.05), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
set.seed(123); uk_model_MLP31a_roc <- train(formula, data=uk_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP31a_roc <- predict(uk_model_MLP31a_roc,uk_woe_test1,type="prob")
uk_MLP31a.ROC <- roc(predictor=ukpredb_MLP31a_roc$X0,
                     response=uk_woe_test1$Censor,
                     levels=rev(levels(uk_woe_test1$Censor)))
uk_MLP31a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); uk_model_MLP31a_gini <- train(formula, data=uk_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP31a_gini$results
uk_model_MLP31a_gini$resample
uk_pred_MLP31a_gini<- predict(uk_model_MLP31a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, uk_pred_MLP31a_gini$X1)
Gini(uk_woe_test1$Censorb, uk_pred_MLP31a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP31a_gini$X1[uk_pred_MLP31a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[uk_pred_MLP31a_gini$X1<=0.4]))
uk_MLP31a.ngini <- normalizedGini(a, p)
uk_MLP31a.ngini
uk_MLP31a.gini <-Gini(a, p)
uk_MLP31a.gini

#Brier score
set.seed(123); uk_model_MLP31a_brier <- train(formula, data=uk_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP31a_brier$results
uk_model_MLP31a_brier$resample
uk_pred_MLP31a_brier <- predict(uk_model_MLP31a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_MLP31a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), uk_pred_MLP31a_brier$X1)
uk_MLP31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_MLP31b_roc <- train(formula, data=uk_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP31b_roc <- predict(uk_model_MLP31b_roc, uk_woe_train1,type="prob")
uk_MLP31b.ROC <- roc(predictor=ukpredb_MLP31b_roc$X0,
                     response=uk_woe_train1$Censor,
                     levels=rev(levels(uk_woe_train1$Censor)))
uk_MLP31b.ROC

#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); uk_model_MLP31b_gini <- train(formula, data=uk_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP31b_gini$results
uk_model_MLP31b_gini$resample
uk_pred_MLP31b_gini<- predict(uk_model_MLP31b_gini, newdata = uk_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, uk_pred_MLP31b_gini$X1)
Gini(uk_woe_train1$Censorb, uk_pred_MLP31b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP31b_gini$X1[uk_pred_MLP31b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[uk_pred_MLP31b_gini$X1<=0.4]))
uk_MLP31b.ngini <- normalizedGini(a, p)
uk_MLP31b.ngini
uk_MLP31b.gini <-Gini(a, p)
uk_MLP31b.gini

#Brier score
set.seed(123); uk_model_MLP31b_brier <- train(formula, data=uk_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP31b_brier$results
uk_model_MLP31b_brier$resample
uk_pred_MLP31b_brier <- predict(uk_model_MLP31b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_MLP31b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), uk_pred_MLP31b_brier$X1)
uk_MLP31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP32a_roc <- train(formula, data=uk_woe_train2, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP32a_roc <- predict(uk_model_MLP32a_roc,uk_woe_test2,type="prob")
uk_MLP32a.ROC <- roc(predictor=ukpredb_MLP32a_roc$X0,
                     response=uk_woe_test2$Censor,
                     levels=rev(levels(uk_woe_test2$Censor)))
uk_MLP32a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); uk_model_MLP32a_gini <- train(formula, data=uk_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP32a_gini$results
uk_model_MLP32a_gini$resample
uk_pred_MLP32a_gini<- predict(uk_model_MLP32a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, uk_pred_MLP32a_gini$X1)
Gini(uk_woe_test2$Censorb, uk_pred_MLP32a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP32a_gini$X1[uk_pred_MLP32a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[uk_pred_MLP32a_gini$X1<=0.4]))
uk_MLP32a.ngini <- normalizedGini(a, p)
uk_MLP32a.ngini
uk_MLP32a.gini <-Gini(a, p)
uk_MLP32a.gini

#Brier score
set.seed(123); uk_model_MLP32a_brier <- train(formula, data=uk_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP32a_brier$results
uk_model_MLP32a_brier$resample
uk_pred_MLP32a_brier <- predict(uk_model_MLP32a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_MLP32a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), uk_pred_MLP32a_brier$X1)
uk_MLP32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_MLP32b_roc <- train(formula, data=uk_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP32b_roc <- predict(uk_model_MLP32b_roc, uk_woe_train2,type="prob")
uk_MLP32b.ROC <- roc(predictor=ukpredb_MLP32b_roc$X0,
                     response=uk_woe_train2$Censor,
                     levels=rev(levels(uk_woe_train2$Censor)))
uk_MLP32b.ROC

#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); uk_model_MLP32b_gini <- train(formula, data=uk_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP32b_gini$results
uk_model_MLP32b_gini$resample
uk_pred_MLP32b_gini<- predict(uk_model_MLP32b_gini, newdata = uk_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, uk_pred_MLP32b_gini$X1)
Gini(uk_woe_train2$Censorb, uk_pred_MLP32b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP32b_gini$X1[uk_pred_MLP32b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[uk_pred_MLP32b_gini$X1<=0.4]))
uk_MLP32b.ngini <- normalizedGini(a, p)
uk_MLP32b.ngini
uk_MLP32b.gini <-Gini(a, p)
uk_MLP32b.gini

#Brier score
set.seed(123); uk_model_MLP32b_brier <- train(formula, data=uk_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP32b_brier$results
uk_model_MLP32b_brier$resample
uk_pred_MLP32b_brier <- predict(uk_model_MLP32b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_MLP32b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), uk_pred_MLP32b_brier$X1)
uk_MLP32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP33a_roc <- train(formula, data=uk_woe_train3, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP33a_roc <- predict(uk_model_MLP33a_roc,uk_woe_test3,type="prob")
uk_MLP33a.ROC <- roc(predictor=ukpredb_MLP33a_roc$X0,
                     response=uk_woe_test3$Censor,
                     levels=rev(levels(uk_woe_test3$Censor)))
uk_MLP33a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); uk_model_MLP33a_gini <- train(formula, data=uk_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP33a_gini$results
uk_model_MLP33a_gini$resample
uk_pred_MLP33a_gini<- predict(uk_model_MLP33a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, uk_pred_MLP33a_gini$X1)
Gini(uk_woe_test3$Censorb, uk_pred_MLP33a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP33a_gini$X1[uk_pred_MLP33a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[uk_pred_MLP33a_gini$X1<=0.4]))
uk_MLP33a.ngini <- normalizedGini(a, p)
uk_MLP33a.ngini
uk_MLP33a.gini <-Gini(a, p)
uk_MLP33a.gini

#Brier score
set.seed(123); uk_model_MLP33a_brier <- train(formula, data=uk_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP33a_brier$results
uk_model_MLP33a_brier$resample
uk_pred_MLP33a_brier <- predict(uk_model_MLP33a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_MLP33a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), uk_pred_MLP33a_brier$X1)
uk_MLP33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_MLP33b_roc <- train(formula, data=uk_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP33b_roc <- predict(uk_model_MLP33b_roc, uk_woe_train3,type="prob")
uk_MLP33b.ROC <- roc(predictor=ukpredb_MLP33b_roc$X0,
                     response=uk_woe_train3$Censor,
                     levels=rev(levels(uk_woe_train3$Censor)))
uk_MLP33b.ROC

#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); uk_model_MLP33b_gini <- train(formula, data=uk_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP33b_gini$results
uk_model_MLP33b_gini$resample
uk_pred_MLP33b_gini<- predict(uk_model_MLP33b_gini, newdata = uk_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, uk_pred_MLP33b_gini$X1)
Gini(uk_woe_train3$Censorb, uk_pred_MLP33b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP33b_gini$X1[uk_pred_MLP33b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[uk_pred_MLP33b_gini$X1<=0.4]))
uk_MLP33b.ngini <- normalizedGini(a, p)
uk_MLP33b.ngini
uk_MLP33b.gini <-Gini(a, p)
uk_MLP33b.gini

#Brier score
set.seed(123); uk_model_MLP33b_brier <- train(formula, data=uk_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP33b_brier$results
uk_model_MLP33b_brier$resample
uk_pred_MLP33b_brier <- predict(uk_model_MLP33b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_MLP33b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), uk_pred_MLP33b_brier$X1)
uk_MLP33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP34a_roc <- train(formula, data=uk_woe_train4, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP34a_roc <- predict(uk_model_MLP34a_roc,uk_woe_test4,type="prob")
uk_MLP34a.ROC <- roc(predictor=ukpredb_MLP34a_roc$X0,
                     response=uk_woe_test4$Censor,
                     levels=rev(levels(uk_woe_test4$Censor)))
uk_MLP34a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); uk_model_MLP34a_gini <- train(formula, data=uk_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP34a_gini$results
uk_model_MLP34a_gini$resample
uk_pred_MLP34a_gini<- predict(uk_model_MLP34a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, uk_pred_MLP34a_gini$X1)
Gini(uk_woe_test4$Censorb, uk_pred_MLP34a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP34a_gini$X1[uk_pred_MLP34a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[uk_pred_MLP34a_gini$X1<=0.4]))
uk_MLP34a.ngini <- normalizedGini(a, p)
uk_MLP34a.ngini
uk_MLP34a.gini <-Gini(a, p)
uk_MLP34a.gini

#Brier score
set.seed(123); uk_model_MLP34a_brier <- train(formula, data=uk_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP34a_brier$results
uk_model_MLP34a_brier$resample
uk_pred_MLP34a_brier <- predict(uk_model_MLP34a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_MLP34a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), uk_pred_MLP34a_brier$X1)
uk_MLP34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_MLP34b_roc <- train(formula, data=uk_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP34b_roc <- predict(uk_model_MLP34b_roc, uk_woe_train4,type="prob")
uk_MLP34b.ROC <- roc(predictor=ukpredb_MLP34b_roc$X0,
                     response=uk_woe_train4$Censor,
                     levels=rev(levels(uk_woe_train4$Censor)))
uk_MLP34b.ROC

#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); uk_model_MLP34b_gini <- train(formula, data=uk_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP34b_gini$results
uk_model_MLP34b_gini$resample
uk_pred_MLP34b_gini<- predict(uk_model_MLP34b_gini, newdata = uk_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, uk_pred_MLP34b_gini$X1)
Gini(uk_woe_train4$Censorb, uk_pred_MLP34b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP34b_gini$X1[uk_pred_MLP34b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[uk_pred_MLP34b_gini$X1<=0.4]))
uk_MLP34b.ngini <- normalizedGini(a, p)
uk_MLP34b.ngini
uk_MLP34b.gini <-Gini(a, p)
uk_MLP34b.gini

#Brier score
set.seed(123); uk_model_MLP34b_brier <- train(formula, data=uk_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP34b_brier$results
uk_model_MLP34b_brier$resample
uk_pred_MLP34b_brier <- predict(uk_model_MLP34b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_MLP34b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), uk_pred_MLP34b_brier$X1)
uk_MLP34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP35a_roc <- train(formula, data=uk_woe_train5, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP35a_roc <- predict(uk_model_MLP35a_roc,uk_woe_test5,type="prob")
uk_MLP35a.ROC <- roc(predictor=ukpredb_MLP35a_roc$X0,
                     response=uk_woe_test5$Censor,
                     levels=rev(levels(uk_woe_test5$Censor)))
uk_MLP35a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); uk_model_MLP35a_gini <- train(formula, data=uk_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP35a_gini$results
uk_model_MLP35a_gini$resample
uk_pred_MLP35a_gini<- predict(uk_model_MLP35a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, uk_pred_MLP35a_gini$X1)
Gini(uk_woe_test5$Censorb, uk_pred_MLP35a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP35a_gini$X1[uk_pred_MLP35a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[uk_pred_MLP35a_gini$X1<=0.4]))
uk_MLP35a.ngini <- normalizedGini(a, p)
uk_MLP35a.ngini
uk_MLP35a.gini <-Gini(a, p)
uk_MLP35a.gini

#Brier score
set.seed(123); uk_model_MLP35a_brier <- train(formula, data=uk_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP35a_brier$results
uk_model_MLP35a_brier$resample
uk_pred_MLP35a_brier <- predict(uk_model_MLP35a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_MLP35a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), uk_pred_MLP35a_brier$X1)
uk_MLP35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_MLP35b_roc <- train(formula, data=uk_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP35b_roc <- predict(uk_model_MLP35b_roc, uk_woe_train5,type="prob")
uk_MLP35b.ROC <- roc(predictor=ukpredb_MLP35b_roc$X0,
                     response=uk_woe_train5$Censor,
                     levels=rev(levels(uk_woe_train5$Censor)))
uk_MLP35b.ROC

#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); uk_model_MLP35b_gini <- train(formula, data=uk_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP35b_gini$results
uk_model_MLP35b_gini$resample
uk_pred_MLP35b_gini<- predict(uk_model_MLP35b_gini, newdata = uk_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, uk_pred_MLP35b_gini$X1)
Gini(uk_woe_train5$Censorb, uk_pred_MLP35b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP35b_gini$X1[uk_pred_MLP35b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[uk_pred_MLP35b_gini$X1<=0.4]))
uk_MLP35b.ngini <- normalizedGini(a, p)
uk_MLP35b.ngini
uk_MLP35b.gini <-Gini(a, p)
uk_MLP35b.gini

#Brier score
set.seed(123); uk_model_MLP35b_brier <- train(formula, data=uk_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP35b_brier$results
uk_model_MLP35b_brier$resample
uk_pred_MLP35b_brier <- predict(uk_model_MLP35b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_MLP35b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), uk_pred_MLP35b_brier$X1)
uk_MLP35b.bs


##Restults MLP3!
uk_results_MLP3_AUC <- cbind(uk_MLP31a.ROC$auc,uk_MLP31b.ROC$auc,uk_MLP32a.ROC$auc,uk_MLP32b.ROC$auc,uk_MLP33a.ROC$auc,uk_MLP33b.ROC$auc,uk_MLP34a.ROC$auc,
                             uk_MLP34b.ROC$auc,uk_MLP35a.ROC$auc,uk_MLP35b.ROC$auc)
uk_results_MLP3_bs <- cbind(uk_MLP31a.bs,uk_MLP31b.bs,uk_MLP32a.bs,uk_MLP32b.bs,uk_MLP33a.bs,uk_MLP33b.bs,uk_MLP34a.bs,uk_MLP34b.bs,uk_MLP35a.bs,uk_MLP35b.bs)
uk_results_MLP3_ngini <- cbind(uk_MLP31a.ngini,uk_MLP31b.ngini,uk_MLP32a.ngini,uk_MLP32b.ngini,uk_MLP33a.ngini,uk_MLP33b.ngini,uk_MLP34a.ngini,uk_MLP34b.ngini,
                               uk_MLP35a.ngini,uk_MLP35b.ngini)
uk_results_MLP3_gini <- cbind(uk_MLP31a.gini,uk_MLP31b.gini,uk_MLP32a.gini,uk_MLP32b.gini,uk_MLP33a.gini,uk_MLP33b.gini,uk_MLP34a.gini,uk_MLP34b.gini,
                              uk_MLP35a.gini,uk_MLP35b.gini)
mean(uk_results_MLP3_AUC)
mean(uk_results_MLP3_bs)
mean(uk_results_MLP3_ngini)
mean(uk_results_MLP3_gini)

##############################
###########MLP5###############
##############################

MLP5grid <- expand.grid(.size1=c(5,10,15), .size2=c(5,10,15), .size3=c(5,10,15), .size4=c(5,10,15), .size5=c(5,10,15), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP51a_roc <- train(formula, data=uk_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP5grid <- expand.grid(.size1=c(10), .size2=c(5), .size3=c(10), .size4=c(5), .size5=c(15), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
#ROC curve 
set.seed(123); uk_model_MLP51a_roc <- train(formula, data=uk_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP51a_roc <- predict(uk_model_MLP51a_roc,uk_woe_test1,type="prob")
uk_MLP51a.ROC <- roc(predictor=ukpredb_MLP51a_roc$X0,
                     response=uk_woe_test1$Censor,
                     levels=rev(levels(uk_woe_test1$Censor)))
uk_MLP51a.ROC

#normalizedGini
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_woe_test1$Censorb <- as.numeric(levels(uk_woe_test1$Censorb))[uk_woe_test1$Censorb]
set.seed(123); uk_model_MLP51a_gini <- train(formula, data=uk_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP51a_gini$results
uk_model_MLP51a_gini$resample
uk_pred_MLP51a_gini<- predict(uk_model_MLP51a_gini, newdata = uk_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test1$Censorb, uk_pred_MLP51a_gini$X1)
Gini(uk_woe_test1$Censorb, uk_pred_MLP51a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP51a_gini$X1[uk_pred_MLP51a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test1$Censorb[uk_pred_MLP51a_gini$X1<=0.4]))
uk_MLP51a.ngini <- normalizedGini(a, p)
uk_MLP51a.ngini
uk_MLP51a.gini <-Gini(a, p)
uk_MLP51a.gini

#Brier score
set.seed(123); uk_model_MLP51a_brier <- train(formula, data=uk_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP51a_brier$results
uk_model_MLP51a_brier$resample
uk_pred_MLP51a_brier <- predict(uk_model_MLP51a_brier, newdata = uk_woe_test1, type='prob')
uk_woe_test1$Censorb <- uk_woe_test1$Censor
levels(uk_woe_test1$Censorb) <- c('0','1')
uk_MLP51a.bs <- Brier(as.numeric(as.character(uk_woe_test1$Censorb)), uk_pred_MLP51a_brier$X1)
uk_MLP51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_MLP51b_roc <- train(formula, data=uk_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP51b_roc <- predict(uk_model_MLP51b_roc, uk_woe_train1,type="prob")
uk_MLP51b.ROC <- roc(predictor=ukpredb_MLP51b_roc$X0,
                     response=uk_woe_train1$Censor,
                     levels=rev(levels(uk_woe_train1$Censor)))
uk_MLP51b.ROC

#normalizedGini
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_woe_train1$Censorb <- as.numeric(levels(uk_woe_train1$Censorb))[uk_woe_train1$Censorb]
set.seed(123); uk_model_MLP51b_gini <- train(formula, data=uk_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP51b_gini$results
uk_model_MLP51b_gini$resample
uk_pred_MLP51b_gini<- predict(uk_model_MLP51b_gini, newdata = uk_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train1$Censorb, uk_pred_MLP51b_gini$X1)
Gini(uk_woe_train1$Censorb, uk_pred_MLP51b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP51b_gini$X1[uk_pred_MLP51b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train1$Censorb[uk_pred_MLP51b_gini$X1<=0.4]))
uk_MLP51b.ngini <- normalizedGini(a, p)
uk_MLP51b.ngini
uk_MLP51b.gini <-Gini(a, p)
uk_MLP51b.gini

#Brier score
set.seed(123); uk_model_MLP51b_brier <- train(formula, data=uk_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP51b_brier$results
uk_model_MLP51b_brier$resample
uk_pred_MLP51b_brier <- predict(uk_model_MLP51b_brier, newdata = uk_woe_train1, type='prob')
uk_woe_train1$Censorb <- uk_woe_train1$Censor
levels(uk_woe_train1$Censorb) <- c('0','1')
uk_MLP51b.bs <- Brier(as.numeric(as.character(uk_woe_train1$Censorb)), uk_pred_MLP51b_brier$X1)
uk_MLP51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP52a_roc <- train(formula, data=uk_woe_train2, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP52a_roc <- predict(uk_model_MLP52a_roc,uk_woe_test2,type="prob")
uk_MLP52a.ROC <- roc(predictor=ukpredb_MLP52a_roc$X0,
                     response=uk_woe_test2$Censor,
                     levels=rev(levels(uk_woe_test2$Censor)))
uk_MLP52a.ROC

#normalizedGini
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_woe_test2$Censorb <- as.numeric(levels(uk_woe_test2$Censorb))[uk_woe_test2$Censorb]
set.seed(123); uk_model_MLP52a_gini <- train(formula, data=uk_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP52a_gini$results
uk_model_MLP52a_gini$resample
uk_pred_MLP52a_gini<- predict(uk_model_MLP52a_gini, newdata = uk_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test2$Censorb, uk_pred_MLP52a_gini$X1)
Gini(uk_woe_test2$Censorb, uk_pred_MLP52a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP52a_gini$X1[uk_pred_MLP52a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test2$Censorb[uk_pred_MLP52a_gini$X1<=0.4]))
uk_MLP52a.ngini <- normalizedGini(a, p)
uk_MLP52a.ngini
uk_MLP52a.gini <-Gini(a, p)
uk_MLP52a.gini

#Brier score
set.seed(123); uk_model_MLP52a_brier <- train(formula, data=uk_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP52a_brier$results
uk_model_MLP52a_brier$resample
uk_pred_MLP52a_brier <- predict(uk_model_MLP52a_brier, newdata = uk_woe_test2, type='prob')
uk_woe_test2$Censorb <- uk_woe_test2$Censor
levels(uk_woe_test2$Censorb) <- c('0','1')
uk_MLP52a.bs <- Brier(as.numeric(as.character(uk_woe_test2$Censorb)), uk_pred_MLP52a_brier$X1)
uk_MLP52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_MLP52b_roc <- train(formula, data=uk_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP52b_roc <- predict(uk_model_MLP52b_roc, uk_woe_train2,type="prob")
uk_MLP52b.ROC <- roc(predictor=ukpredb_MLP52b_roc$X0,
                     response=uk_woe_train2$Censor,
                     levels=rev(levels(uk_woe_train2$Censor)))
uk_MLP52b.ROC

#normalizedGini
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_woe_train2$Censorb <- as.numeric(levels(uk_woe_train2$Censorb))[uk_woe_train2$Censorb]
set.seed(123); uk_model_MLP52b_gini <- train(formula, data=uk_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP52b_gini$results
uk_model_MLP52b_gini$resample
uk_pred_MLP52b_gini<- predict(uk_model_MLP52b_gini, newdata = uk_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train2$Censorb, uk_pred_MLP52b_gini$X1)
Gini(uk_woe_train2$Censorb, uk_pred_MLP52b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP52b_gini$X1[uk_pred_MLP52b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train2$Censorb[uk_pred_MLP52b_gini$X1<=0.4]))
uk_MLP52b.ngini <- normalizedGini(a, p)
uk_MLP52b.ngini
uk_MLP52b.gini <-Gini(a, p)
uk_MLP52b.gini

#Brier score
set.seed(123); uk_model_MLP52b_brier <- train(formula, data=uk_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP52b_brier$results
uk_model_MLP52b_brier$resample
uk_pred_MLP52b_brier <- predict(uk_model_MLP52b_brier, newdata = uk_woe_train2, type='prob')
uk_woe_train2$Censorb <- uk_woe_train2$Censor
levels(uk_woe_train2$Censorb) <- c('0','1')
uk_MLP52b.bs <- Brier(as.numeric(as.character(uk_woe_train2$Censorb)), uk_pred_MLP52b_brier$X1)
uk_MLP52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP53a_roc <- train(formula, data=uk_woe_train3, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP53a_roc <- predict(uk_model_MLP53a_roc,uk_woe_test3,type="prob")
uk_MLP53a.ROC <- roc(predictor=ukpredb_MLP53a_roc$X0,
                     response=uk_woe_test3$Censor,
                     levels=rev(levels(uk_woe_test3$Censor)))
uk_MLP53a.ROC

#normalizedGini
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_woe_test3$Censorb <- as.numeric(levels(uk_woe_test3$Censorb))[uk_woe_test3$Censorb]
set.seed(123); uk_model_MLP53a_gini <- train(formula, data=uk_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP53a_gini$results
uk_model_MLP53a_gini$resample
uk_pred_MLP53a_gini<- predict(uk_model_MLP53a_gini, newdata = uk_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test3$Censorb, uk_pred_MLP53a_gini$X1)
Gini(uk_woe_test3$Censorb, uk_pred_MLP53a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP53a_gini$X1[uk_pred_MLP53a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test3$Censorb[uk_pred_MLP53a_gini$X1<=0.4]))
uk_MLP53a.ngini <- normalizedGini(a, p)
uk_MLP53a.ngini
uk_MLP53a.gini <-Gini(a, p)
uk_MLP53a.gini

#Brier score
set.seed(123); uk_model_MLP53a_brier <- train(formula, data=uk_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP53a_brier$results
uk_model_MLP53a_brier$resample
uk_pred_MLP53a_brier <- predict(uk_model_MLP53a_brier, newdata = uk_woe_test3, type='prob')
uk_woe_test3$Censorb <- uk_woe_test3$Censor
levels(uk_woe_test3$Censorb) <- c('0','1')
uk_MLP53a.bs <- Brier(as.numeric(as.character(uk_woe_test3$Censorb)), uk_pred_MLP53a_brier$X1)
uk_MLP53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_MLP53b_roc <- train(formula, data=uk_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP53b_roc <- predict(uk_model_MLP53b_roc, uk_woe_train3,type="prob")
uk_MLP53b.ROC <- roc(predictor=ukpredb_MLP53b_roc$X0,
                     response=uk_woe_train3$Censor,
                     levels=rev(levels(uk_woe_train3$Censor)))
uk_MLP53b.ROC

#normalizedGini
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_woe_train3$Censorb <- as.numeric(levels(uk_woe_train3$Censorb))[uk_woe_train3$Censorb]
set.seed(123); uk_model_MLP53b_gini <- train(formula, data=uk_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP53b_gini$results
uk_model_MLP53b_gini$resample
uk_pred_MLP53b_gini<- predict(uk_model_MLP53b_gini, newdata = uk_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train3$Censorb, uk_pred_MLP53b_gini$X1)
Gini(uk_woe_train3$Censorb, uk_pred_MLP53b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP53b_gini$X1[uk_pred_MLP53b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train3$Censorb[uk_pred_MLP53b_gini$X1<=0.4]))
uk_MLP53b.ngini <- normalizedGini(a, p)
uk_MLP53b.ngini
uk_MLP53b.gini <-Gini(a, p)
uk_MLP53b.gini

#Brier score
set.seed(123); uk_model_MLP53b_brier <- train(formula, data=uk_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP53b_brier$results
uk_model_MLP53b_brier$resample
uk_pred_MLP53b_brier <- predict(uk_model_MLP53b_brier, newdata = uk_woe_train3, type='prob')
uk_woe_train3$Censorb <- uk_woe_train3$Censor
levels(uk_woe_train3$Censorb) <- c('0','1')
uk_MLP53b.bs <- Brier(as.numeric(as.character(uk_woe_train3$Censorb)), uk_pred_MLP53b_brier$X1)
uk_MLP53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP54a_roc <- train(formula, data=uk_woe_train4, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP54a_roc <- predict(uk_model_MLP54a_roc,uk_woe_test4,type="prob")
uk_MLP54a.ROC <- roc(predictor=ukpredb_MLP54a_roc$X0,
                     response=uk_woe_test4$Censor,
                     levels=rev(levels(uk_woe_test4$Censor)))
uk_MLP54a.ROC

#normalizedGini
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_woe_test4$Censorb <- as.numeric(levels(uk_woe_test4$Censorb))[uk_woe_test4$Censorb]
set.seed(123); uk_model_MLP54a_gini <- train(formula, data=uk_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP54a_gini$results
uk_model_MLP54a_gini$resample
uk_pred_MLP54a_gini<- predict(uk_model_MLP54a_gini, newdata = uk_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test4$Censorb, uk_pred_MLP54a_gini$X1)
Gini(uk_woe_test4$Censorb, uk_pred_MLP54a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP54a_gini$X1[uk_pred_MLP54a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test4$Censorb[uk_pred_MLP54a_gini$X1<=0.4]))
uk_MLP54a.ngini <- normalizedGini(a, p)
uk_MLP54a.ngini
uk_MLP54a.gini <-Gini(a, p)
uk_MLP54a.gini

#Brier score
set.seed(123); uk_model_MLP54a_brier <- train(formula, data=uk_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP54a_brier$results
uk_model_MLP54a_brier$resample
uk_pred_MLP54a_brier <- predict(uk_model_MLP54a_brier, newdata = uk_woe_test4, type='prob')
uk_woe_test4$Censorb <- uk_woe_test4$Censor
levels(uk_woe_test4$Censorb) <- c('0','1')
uk_MLP54a.bs <- Brier(as.numeric(as.character(uk_woe_test4$Censorb)), uk_pred_MLP54a_brier$X1)
uk_MLP54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_MLP54b_roc <- train(formula, data=uk_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP54b_roc <- predict(uk_model_MLP54b_roc, uk_woe_train4,type="prob")
uk_MLP54b.ROC <- roc(predictor=ukpredb_MLP54b_roc$X0,
                     response=uk_woe_train4$Censor,
                     levels=rev(levels(uk_woe_train4$Censor)))
uk_MLP54b.ROC

#normalizedGini
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_woe_train4$Censorb <- as.numeric(levels(uk_woe_train4$Censorb))[uk_woe_train4$Censorb]
set.seed(123); uk_model_MLP54b_gini <- train(formula, data=uk_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP54b_gini$results
uk_model_MLP54b_gini$resample
uk_pred_MLP54b_gini<- predict(uk_model_MLP54b_gini, newdata = uk_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train4$Censorb, uk_pred_MLP54b_gini$X1)
Gini(uk_woe_train4$Censorb, uk_pred_MLP54b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP54b_gini$X1[uk_pred_MLP54b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train4$Censorb[uk_pred_MLP54b_gini$X1<=0.4]))
uk_MLP54b.ngini <- normalizedGini(a, p)
uk_MLP54b.ngini
uk_MLP54b.gini <-Gini(a, p)
uk_MLP54b.gini

#Brier score
set.seed(123); uk_model_MLP54b_brier <- train(formula, data=uk_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP54b_brier$results
uk_model_MLP54b_brier$resample
uk_pred_MLP54b_brier <- predict(uk_model_MLP54b_brier, newdata = uk_woe_train4, type='prob')
uk_woe_train4$Censorb <- uk_woe_train4$Censor
levels(uk_woe_train4$Censorb) <- c('0','1')
uk_MLP54b.bs <- Brier(as.numeric(as.character(uk_woe_train4$Censorb)), uk_pred_MLP54b_brier$X1)
uk_MLP54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_MLP55a_roc <- train(formula, data=uk_woe_train5, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP55a_roc <- predict(uk_model_MLP55a_roc,uk_woe_test5,type="prob")
uk_MLP55a.ROC <- roc(predictor=ukpredb_MLP55a_roc$X0,
                     response=uk_woe_test5$Censor,
                     levels=rev(levels(uk_woe_test5$Censor)))
uk_MLP55a.ROC

#normalizedGini
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_woe_test5$Censorb <- as.numeric(levels(uk_woe_test5$Censorb))[uk_woe_test5$Censorb]
set.seed(123); uk_model_MLP55a_gini <- train(formula, data=uk_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP55a_gini$results
uk_model_MLP55a_gini$resample
uk_pred_MLP55a_gini<- predict(uk_model_MLP55a_gini, newdata = uk_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_test5$Censorb, uk_pred_MLP55a_gini$X1)
Gini(uk_woe_test5$Censorb, uk_pred_MLP55a_gini$X1)
#b <= 0.4
p <- uk_pred_MLP55a_gini$X1[uk_pred_MLP55a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_test5$Censorb[uk_pred_MLP55a_gini$X1<=0.4]))
uk_MLP55a.ngini <- normalizedGini(a, p)
uk_MLP55a.ngini
uk_MLP55a.gini <-Gini(a, p)
uk_MLP55a.gini

#Brier score
set.seed(123); uk_model_MLP55a_brier <- train(formula, data=uk_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP55a_brier$results
uk_model_MLP55a_brier$resample
uk_pred_MLP55a_brier <- predict(uk_model_MLP55a_brier, newdata = uk_woe_test5, type='prob')
uk_woe_test5$Censorb <- uk_woe_test5$Censor
levels(uk_woe_test5$Censorb) <- c('0','1')
uk_MLP55a.bs <- Brier(as.numeric(as.character(uk_woe_test5$Censorb)), uk_pred_MLP55a_brier$X1)
uk_MLP55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_MLP55b_roc <- train(formula, data=uk_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_MLP55b_roc <- predict(uk_model_MLP55b_roc, uk_woe_train5,type="prob")
uk_MLP55b.ROC <- roc(predictor=ukpredb_MLP55b_roc$X0,
                     response=uk_woe_train5$Censor,
                     levels=rev(levels(uk_woe_train5$Censor)))
uk_MLP55b.ROC

#normalizedGini
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_woe_train5$Censorb <- as.numeric(levels(uk_woe_train5$Censorb))[uk_woe_train5$Censorb]
set.seed(123); uk_model_MLP55b_gini <- train(formula, data=uk_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_MLP55b_gini$results
uk_model_MLP55b_gini$resample
uk_pred_MLP55b_gini<- predict(uk_model_MLP55b_gini, newdata = uk_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_woe_train5$Censorb, uk_pred_MLP55b_gini$X1)
Gini(uk_woe_train5$Censorb, uk_pred_MLP55b_gini$X1)
#b <= 0.4
p <- uk_pred_MLP55b_gini$X1[uk_pred_MLP55b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_woe_train5$Censorb[uk_pred_MLP55b_gini$X1<=0.4]))
uk_MLP55b.ngini <- normalizedGini(a, p)
uk_MLP55b.ngini
uk_MLP55b.gini <-Gini(a, p)
uk_MLP55b.gini

#Brier score
set.seed(123); uk_model_MLP55b_brier <- train(formula, data=uk_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_MLP55b_brier$results
uk_model_MLP55b_brier$resample
uk_pred_MLP55b_brier <- predict(uk_model_MLP55b_brier, newdata = uk_woe_train5, type='prob')
uk_woe_train5$Censorb <- uk_woe_train5$Censor
levels(uk_woe_train5$Censorb) <- c('0','1')
uk_MLP55b.bs <- Brier(as.numeric(as.character(uk_woe_train5$Censorb)), uk_pred_MLP55b_brier$X1)
uk_MLP55b.bs


##Restults RF!
uk_results_MLP5_AUC <- cbind(uk_MLP51a.ROC$auc,uk_MLP51b.ROC$auc,uk_MLP52a.ROC$auc,uk_MLP52b.ROC$auc,uk_MLP53a.ROC$auc,uk_MLP53b.ROC$auc,uk_MLP54a.ROC$auc,
                             uk_MLP54b.ROC$auc,uk_MLP55a.ROC$auc,uk_MLP55b.ROC$auc)
uk_results_MLP5_bs <- cbind(uk_MLP51a.bs,uk_MLP51b.bs,uk_MLP52a.bs,uk_MLP52b.bs,uk_MLP53a.bs,uk_MLP53b.bs,uk_MLP54a.bs,uk_MLP54b.bs,uk_MLP55a.bs,uk_MLP55b.bs)
uk_results_MLP5_ngini <- cbind(uk_MLP51a.ngini,uk_MLP51b.ngini,uk_MLP52a.ngini,uk_MLP52b.ngini,uk_MLP53a.ngini,uk_MLP53b.ngini,uk_MLP54a.ngini,uk_MLP54b.ngini,
                               uk_MLP55a.ngini,uk_MLP55b.ngini)
uk_results_MLP5_gini <- cbind(uk_MLP51a.gini,uk_MLP51b.gini,uk_MLP52a.gini,uk_MLP52b.gini,uk_MLP53a.gini,uk_MLP53b.gini,uk_MLP54a.gini,uk_MLP54b.gini,
                              uk_MLP55a.gini,uk_MLP55b.gini)
mean(uk_results_MLP5_AUC)
mean(uk_results_MLP5_bs)
mean(uk_results_MLP5_ngini)
mean(uk_results_MLP5_gini)

################################
#######Deep learning (DBN)######
################################

DBN1 <- list(label = "Deep Belife Network",
             library = "deepnet",
             loop = NULL,
             type = c("Classification", "Regression"),
             parameters = data.frame(parameter = c("layer1", "hidden_dropout", "visible_dropout", "lr"),
                                     class = rep("numeric", 4),
                                     label = c("Hidden Layer 1", "Hidden Dropouts", "Visible Dropout", "Learning rate")),
             grid = function(x, y, len = NULL, search = "grid") {
               if(search == "grid") {
                 out <- expand.grid(layer1 = 1:len,
                                    hidden_dropout = seq(0, .7, length = len), 
                                    visible_dropout = seq(0, .7, length = len),
                                    lr=0.8)
               } else {
                 out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                   hidden_dropout = runif(len, min = 0, max = .7),
                                   visible_dropout = runif(len, min = 0, max = .7),
                                   lr=0.8)
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               if(!is.matrix(x)) x <- as.matrix(x)
               is_class <- is.factor(y)
               if (is_class) y <- caret:::class2ind(y)
               layers <- c(param$layer1)
               layers <- layers[layers > 0]
               deepnet::dbn.dnn.train(x, y, hidden = layers, activationfun = "sigm", learningrate = param$lr,
                                      output = if(is_class) "sigm" else "linear", batchsize = 100, 
                                      hidden_dropout = param$hidden_dropout, visible_dropout = param$visable_dropout, cd = 1)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               pred <- deepnet::nn.predict(modelFit, as.matrix(newdata))
               if(ncol(pred) > 1)
                 pred <- modelFit$obsLevels[apply(pred, 1, which.max)]
               pred
             },
             prob = function(modelFit, newdata, submodels = NULL) {
               out <- exp(deepnet::nn.predict(modelFit, as.matrix(newdata)))
               out <- apply(out, 1, function(x) x/sum(x))
               t(out)
             },
             predictors = function(x, ...) {
               NULL
             },
             varImp = NULL,
             levels = function(x) x$classes,
             tags = c("Neural Network"),
             sort = function(x) x[order(x[,1]),])


DBN3 <- list(label = "Deep Belife Network",
             library = "deepnet",
             loop = NULL,
             type = c("Classification", "Regression"),
             parameters = data.frame(parameter = c("layer1", "layer2", "layer3", "hidden_dropout", "visible_dropout", "lr"),
                                     class = rep("numeric", 6),
                                     label = c("Hidden Layer 1", "Hidden Layer 2", "Hidden Layer 3", 
                                               "Hidden Dropouts", "Visible Dropout", "Learning rate")),
             grid = function(x, y, len = NULL, search = "grid") {
               if(search == "grid") {
                 out <- expand.grid(layer1 = 1:len, layer2 = 0:(len -1), layer3 = 0:(len -1),
                                    hidden_dropout = seq(0, .7, length = len), 
                                    visible_dropout = seq(0, .7, length = len),
                                    lr=0.8)
               } else {
                 out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                   layer2 = sample(2:20, replace = TRUE, size = len),
                                   layer3 = sample(2:20, replace = TRUE, size = len),
                                   hidden_dropout = runif(len, min = 0, max = .7),
                                   visible_dropout = runif(len, min = 0, max = .7),
                                   lr=0.8)
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               if(!is.matrix(x)) x <- as.matrix(x)
               is_class <- is.factor(y)
               if (is_class) y <- caret:::class2ind(y)
               layers <- c(param$layer1, param$layer2, param$layer3)
               layers <- layers[layers > 0]
               deepnet::dbn.dnn.train(x, y, hidden = layers, activationfun = "sigm", learningrate = param$lr,
                                      output = if(is_class) "sigm" else "linear", batchsize = 520, 
                                      hidden_dropout = param$hidden_dropout, visible_dropout = param$visable_dropout, cd = 1)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               pred <- deepnet::nn.predict(modelFit, as.matrix(newdata))
               if(ncol(pred) > 1)
                 pred <- modelFit$obsLevels[apply(pred, 1, which.max)]
               pred
             },
             prob = function(modelFit, newdata, submodels = NULL) {
               out <- exp(deepnet::nn.predict(modelFit, as.matrix(newdata)))
               out <- apply(out, 1, function(x) x/sum(x))
               t(out)
             },
             predictors = function(x, ...) {
               NULL
             },
             varImp = NULL,
             levels = function(x) x$classes,
             tags = c("Neural Network"),
             sort = function(x) x[order(x[,1]),])

DBN5 <- list(label = "Deep Belife Network",
             library = "deepnet",
             loop = NULL,
             type = c("Classification", "Regression"),
             parameters = data.frame(parameter = c("layer1", "layer2", "layer3", "layer4", "layer5",
                                                   "hidden_dropout", "visible_dropout", "lr"),
                                     class = rep("numeric", 8),
                                     label = c("Hidden Layer 1", "Hidden Layer 2", "Hidden Layer 3",
                                               "Hidden Layer 4", "Hidden Layer 5", "Hidden Dropouts", 
                                               "Visible Dropout", "Learning rate")),
             grid = function(x, y, len = NULL, search = "grid") {
               if(search == "grid") {
                 out <- expand.grid(layer1 = 1:len, layer2 = 0:(len -1), layer3 = 0:(len -1),
                                    hidden_dropout = seq(0, .7, length = len), 
                                    visible_dropout = seq(0, .7, length = len),
                                    lr=0.8)
               } else {
                 out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                   layer2 = sample(2:20, replace = TRUE, size = len),
                                   layer3 = sample(2:20, replace = TRUE, size = len),
                                   layer4 = sample(2:20, replace = TRUE, size = len),
                                   layer5 = sample(2:20, replace = TRUE, size = len),
                                   hidden_dropout = runif(len, min = 0, max = .7),
                                   visible_dropout = runif(len, min = 0, max = .7),
                                   lr=0.8)
               }
               out
             },
             fit = function(x, y, wts, param, lev, last, classProbs, ...) {
               if(!is.matrix(x)) x <- as.matrix(x)
               is_class <- is.factor(y)
               if (is_class) y <- caret:::class2ind(y)
               layers <- c(param$layer1, param$layer2, param$layer3, param$layer4, param$layer5)
               layers <- layers[layers > 0]
               deepnet::dbn.dnn.train(x, y, hidden = layers, activationfun = "sigm", learningrate = param$lr,
                                      output = if(is_class) "sigm" else "linear", batchsize = 100, 
                                      hidden_dropout = param$hidden_dropout, visible_dropout = param$visable_dropout, cd = 1)
             },
             predict = function(modelFit, newdata, submodels = NULL) {
               pred <- deepnet::nn.predict(modelFit, as.matrix(newdata))
               if(ncol(pred) > 1)
                 pred <- modelFit$obsLevels[apply(pred, 1, which.max)]
               pred
             },
             prob = function(modelFit, newdata, submodels = NULL) {
               out <- exp(deepnet::nn.predict(modelFit, as.matrix(newdata)))
               out <- apply(out, 1, function(x) x/sum(x))
               t(out)
             },
             predictors = function(x, ...) {
               NULL
             },
             varImp = NULL,
             levels = function(x) x$classes,
             tags = c("Neural Network"),
             sort = function(x) x[order(x[,1]),])



##############################
###########DBN1###############
##############################

DBN1_grid <- expand.grid(.layer1=c(5,10,15,20), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0))
set.seed(123); uk_model_DBN11a_roc <- train(formula, data=uk_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
DBN1grid <- expand.grid(.layer1=c(10), .hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(1.5))
###data 1, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN11a_roc <- train(formula, data=uk_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN11a_roc <- predict(uk_model_DBN11a_roc,uk_test1,type="prob")
uk_DBN11a.ROC <- roc(predictor=ukpredb_DBN11a_roc$X0,
                     response=uk_test1$Censor,
                     levels=rev(levels(uk_test1$Censor)))
uk_DBN11a.ROC

#normalizedGini
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_test1$Censorb <- as.numeric(levels(uk_test1$Censorb))[uk_test1$Censorb]
set.seed(123); uk_model_DBN11a_gini <- train(formula, data=uk_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN11a_gini$results
uk_model_DBN11a_gini$resample
uk_pred_DBN11a_gini<- predict(uk_model_DBN11a_gini, newdata = uk_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test1$Censorb, uk_pred_DBN11a_gini$X1)
Gini(uk_test1$Censorb, uk_pred_DBN11a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN11a_gini$X1[uk_pred_DBN11a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test1$Censorb[uk_pred_DBN11a_gini$X1<=0.4]))
uk_DBN11a.ngini <- normalizedGini(a, p)
uk_DBN11a.ngini
uk_DBN11a.gini <-Gini(a, p)
uk_DBN11a.gini

#Brier score
set.seed(123); uk_model_DBN11a_brier <- train(formula, data=uk_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN11a_brier$results
uk_model_DBN11a_brier$resample
uk_pred_DBN11a_brier <- predict(uk_model_DBN11a_brier, newdata = uk_test1, type='prob')
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_DBN11a.bs <- Brier(as.numeric(as.character(uk_test1$Censorb)), uk_pred_DBN11a_brier$X1)
uk_DBN11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_DBN11b_roc <- train(formula, data=uk_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN11b_roc <- predict(uk_model_DBN11b_roc, uk_train1,type="prob")
uk_DBN11b.ROC <- roc(predictor=ukpredb_DBN11b_roc$X0,
                     response=uk_train1$Censor,
                     levels=rev(levels(uk_train1$Censor)))
uk_DBN11b.ROC

#normalizedGini
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_train1$Censorb <- as.numeric(levels(uk_train1$Censorb))[uk_train1$Censorb]
set.seed(123); uk_model_DBN11b_gini <- train(formula, data=uk_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN11b_gini$results
uk_model_DBN11b_gini$resample
uk_pred_DBN11b_gini<- predict(uk_model_DBN11b_gini, newdata = uk_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train1$Censorb, uk_pred_DBN11b_gini$X1)
Gini(uk_train1$Censorb, uk_pred_DBN11b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN11b_gini$X1[uk_pred_DBN11b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train1$Censorb[uk_pred_DBN11b_gini$X1<=0.4]))
uk_DBN11b.ngini <- normalizedGini(a, p)
uk_DBN11b.ngini
uk_DBN11b.gini <-Gini(a, p)
uk_DBN11b.gini

#Brier score
set.seed(123); uk_model_DBN11b_brier <- train(formula, data=uk_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN11b_brier$results
uk_model_DBN11b_brier$resample
uk_pred_DBN11b_brier <- predict(uk_model_DBN11b_brier, newdata = uk_train1, type='prob')
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_DBN11b.bs <- Brier(as.numeric(as.character(uk_train1$Censorb)), uk_pred_DBN11b_brier$X1)
uk_DBN11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN12a_roc <- train(formula, data=uk_train2, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN12a_roc <- predict(uk_model_DBN12a_roc,uk_test2,type="prob")
uk_DBN12a.ROC <- roc(predictor=ukpredb_DBN12a_roc$X0,
                     response=uk_test2$Censor,
                     levels=rev(levels(uk_test2$Censor)))
uk_DBN12a.ROC

#normalizedGini
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_test2$Censorb <- as.numeric(levels(uk_test2$Censorb))[uk_test2$Censorb]
set.seed(123); uk_model_DBN12a_gini <- train(formula, data=uk_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN12a_gini$results
uk_model_DBN12a_gini$resample
uk_pred_DBN12a_gini<- predict(uk_model_DBN12a_gini, newdata = uk_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test2$Censorb, uk_pred_DBN12a_gini$X1)
Gini(uk_test2$Censorb, uk_pred_DBN12a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN12a_gini$X1[uk_pred_DBN12a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test2$Censorb[uk_pred_DBN12a_gini$X1<=0.4]))
uk_DBN12a.ngini <- normalizedGini(a, p)
uk_DBN12a.ngini
uk_DBN12a.gini <-Gini(a, p)
uk_DBN12a.gini

#Brier score
set.seed(123); uk_model_DBN12a_brier <- train(formula, data=uk_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN12a_brier$results
uk_model_DBN12a_brier$resample
uk_pred_DBN12a_brier <- predict(uk_model_DBN12a_brier, newdata = uk_test2, type='prob')
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_DBN12a.bs <- Brier(as.numeric(as.character(uk_test2$Censorb)), uk_pred_DBN12a_brier$X1)
uk_DBN12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_DBN12b_roc <- train(formula, data=uk_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN12b_roc <- predict(uk_model_DBN12b_roc, uk_train2,type="prob")
uk_DBN12b.ROC <- roc(predictor=ukpredb_DBN12b_roc$X0,
                     response=uk_train2$Censor,
                     levels=rev(levels(uk_train2$Censor)))
uk_DBN12b.ROC

#normalizedGini
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_train2$Censorb <- as.numeric(levels(uk_train2$Censorb))[uk_train2$Censorb]
set.seed(123); uk_model_DBN12b_gini <- train(formula, data=uk_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN12b_gini$results
uk_model_DBN12b_gini$resample
uk_pred_DBN12b_gini<- predict(uk_model_DBN12b_gini, newdata = uk_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train2$Censorb, uk_pred_DBN12b_gini$X1)
Gini(uk_train2$Censorb, uk_pred_DBN12b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN12b_gini$X1[uk_pred_DBN12b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train2$Censorb[uk_pred_DBN12b_gini$X1<=0.4]))
uk_DBN12b.ngini <- normalizedGini(a, p)
uk_DBN12b.ngini
uk_DBN12b.gini <-Gini(a, p)
uk_DBN12b.gini

#Brier score
set.seed(123); uk_model_DBN12b_brier <- train(formula, data=uk_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN12b_brier$results
uk_model_DBN12b_brier$resample
uk_pred_DBN12b_brier <- predict(uk_model_DBN12b_brier, newdata = uk_train2, type='prob')
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_DBN12b.bs <- Brier(as.numeric(as.character(uk_train2$Censorb)), uk_pred_DBN12b_brier$X1)
uk_DBN12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN13a_roc <- train(formula, data=uk_train3, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN13a_roc <- predict(uk_model_DBN13a_roc,uk_test3,type="prob")
uk_DBN13a.ROC <- roc(predictor=ukpredb_DBN13a_roc$X0,
                     response=uk_test3$Censor,
                     levels=rev(levels(uk_test3$Censor)))
uk_DBN13a.ROC

#normalizedGini
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_test3$Censorb <- as.numeric(levels(uk_test3$Censorb))[uk_test3$Censorb]
set.seed(123); uk_model_DBN13a_gini <- train(formula, data=uk_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN13a_gini$results
uk_model_DBN13a_gini$resample
uk_pred_DBN13a_gini<- predict(uk_model_DBN13a_gini, newdata = uk_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test3$Censorb, uk_pred_DBN13a_gini$X1)
Gini(uk_test3$Censorb, uk_pred_DBN13a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN13a_gini$X1[uk_pred_DBN13a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test3$Censorb[uk_pred_DBN13a_gini$X1<=0.4]))
uk_DBN13a.ngini <- normalizedGini(a, p)
uk_DBN13a.ngini
uk_DBN13a.gini <-Gini(a, p)
uk_DBN13a.gini

#Brier score
set.seed(123); uk_model_DBN13a_brier <- train(formula, data=uk_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN13a_brier$results
uk_model_DBN13a_brier$resample
uk_pred_DBN13a_brier <- predict(uk_model_DBN13a_brier, newdata = uk_test3, type='prob')
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_DBN13a.bs <- Brier(as.numeric(as.character(uk_test3$Censorb)), uk_pred_DBN13a_brier$X1)
uk_DBN13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_DBN13b_roc <- train(formula, data=uk_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN13b_roc <- predict(uk_model_DBN13b_roc, uk_train3,type="prob")
uk_DBN13b.ROC <- roc(predictor=ukpredb_DBN13b_roc$X0,
                     response=uk_train3$Censor,
                     levels=rev(levels(uk_train3$Censor)))
uk_DBN13b.ROC

#normalizedGini
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_train3$Censorb <- as.numeric(levels(uk_train3$Censorb))[uk_train3$Censorb]
set.seed(123); uk_model_DBN13b_gini <- train(formula, data=uk_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN13b_gini$results
uk_model_DBN13b_gini$resample
uk_pred_DBN13b_gini<- predict(uk_model_DBN13b_gini, newdata = uk_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train3$Censorb, uk_pred_DBN13b_gini$X1)
Gini(uk_train3$Censorb, uk_pred_DBN13b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN13b_gini$X1[uk_pred_DBN13b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train3$Censorb[uk_pred_DBN13b_gini$X1<=0.4]))
uk_DBN13b.ngini <- normalizedGini(a, p)
uk_DBN13b.ngini
uk_DBN13b.gini <-Gini(a, p)
uk_DBN13b.gini

#Brier score
set.seed(123); uk_model_DBN13b_brier <- train(formula, data=uk_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN13b_brier$results
uk_model_DBN13b_brier$resample
uk_pred_DBN13b_brier <- predict(uk_model_DBN13b_brier, newdata = uk_train3, type='prob')
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_DBN13b.bs <- Brier(as.numeric(as.character(uk_train3$Censorb)), uk_pred_DBN13b_brier$X1)
uk_DBN13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN14a_roc <- train(formula, data=uk_train4, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN14a_roc <- predict(uk_model_DBN14a_roc,uk_test4,type="prob")
uk_DBN14a.ROC <- roc(predictor=ukpredb_DBN14a_roc$X0,
                     response=uk_test4$Censor,
                     levels=rev(levels(uk_test4$Censor)))
uk_DBN14a.ROC

#normalizedGini
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_test4$Censorb <- as.numeric(levels(uk_test4$Censorb))[uk_test4$Censorb]
set.seed(123); uk_model_DBN14a_gini <- train(formula, data=uk_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN14a_gini$results
uk_model_DBN14a_gini$resample
uk_pred_DBN14a_gini<- predict(uk_model_DBN14a_gini, newdata = uk_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test4$Censorb, uk_pred_DBN14a_gini$X1)
Gini(uk_test4$Censorb, uk_pred_DBN14a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN14a_gini$X1[uk_pred_DBN14a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test4$Censorb[uk_pred_DBN14a_gini$X1<=0.4]))
uk_DBN14a.ngini <- normalizedGini(a, p)
uk_DBN14a.ngini
uk_DBN14a.gini <-Gini(a, p)
uk_DBN14a.gini

#Brier score
set.seed(123); uk_model_DBN14a_brier <- train(formula, data=uk_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN14a_brier$results
uk_model_DBN14a_brier$resample
uk_pred_DBN14a_brier <- predict(uk_model_DBN14a_brier, newdata = uk_test4, type='prob')
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_DBN14a.bs <- Brier(as.numeric(as.character(uk_test4$Censorb)), uk_pred_DBN14a_brier$X1)
uk_DBN14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_DBN14b_roc <- train(formula, data=uk_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN14b_roc <- predict(uk_model_DBN14b_roc, uk_train4,type="prob")
uk_DBN14b.ROC <- roc(predictor=ukpredb_DBN14b_roc$X0,
                     response=uk_train4$Censor,
                     levels=rev(levels(uk_train4$Censor)))
uk_DBN14b.ROC

#normalizedGini
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_train4$Censorb <- as.numeric(levels(uk_train4$Censorb))[uk_train4$Censorb]
set.seed(123); uk_model_DBN14b_gini <- train(formula, data=uk_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN14b_gini$results
uk_model_DBN14b_gini$resample
uk_pred_DBN14b_gini<- predict(uk_model_DBN14b_gini, newdata = uk_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train4$Censorb, uk_pred_DBN14b_gini$X1)
Gini(uk_train4$Censorb, uk_pred_DBN14b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN14b_gini$X1[uk_pred_DBN14b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train4$Censorb[uk_pred_DBN14b_gini$X1<=0.4]))
uk_DBN14b.ngini <- normalizedGini(a, p)
uk_DBN14b.ngini
uk_DBN14b.gini <-Gini(a, p)
uk_DBN14b.gini

#Brier score
set.seed(123); uk_model_DBN14b_brier <- train(formula, data=uk_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN14b_brier$results
uk_model_DBN14b_brier$resample
uk_pred_DBN14b_brier <- predict(uk_model_DBN14b_brier, newdata = uk_train4, type='prob')
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_DBN14b.bs <- Brier(as.numeric(as.character(uk_train4$Censorb)), uk_pred_DBN14b_brier$X1)
uk_DBN14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN15a_roc <- train(formula, data=uk_train5, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN15a_roc <- predict(uk_model_DBN15a_roc,uk_test5,type="prob")
uk_DBN15a.ROC <- roc(predictor=ukpredb_DBN15a_roc$X0,
                     response=uk_test5$Censor,
                     levels=rev(levels(uk_test5$Censor)))
uk_DBN15a.ROC

#normalizedGini
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_test5$Censorb <- as.numeric(levels(uk_test5$Censorb))[uk_test5$Censorb]
set.seed(123); uk_model_DBN15a_gini <- train(formula, data=uk_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN15a_gini$results
uk_model_DBN15a_gini$resample
uk_pred_DBN15a_gini<- predict(uk_model_DBN15a_gini, newdata = uk_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test5$Censorb, uk_pred_DBN15a_gini$X1)
Gini(uk_test5$Censorb, uk_pred_DBN15a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN15a_gini$X1[uk_pred_DBN15a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test5$Censorb[uk_pred_DBN15a_gini$X1<=0.4]))
uk_DBN15a.ngini <- normalizedGini(a, p)
uk_DBN15a.ngini
uk_DBN15a.gini <-Gini(a, p)
uk_DBN15a.gini

#Brier score
set.seed(123); uk_model_DBN15a_brier <- train(formula, data=uk_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN15a_brier$results
uk_model_DBN15a_brier$resample
uk_pred_DBN15a_brier <- predict(uk_model_DBN15a_brier, newdata = uk_test5, type='prob')
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_DBN15a.bs <- Brier(as.numeric(as.character(uk_test5$Censorb)), uk_pred_DBN15a_brier$X1)
uk_DBN15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_DBN15b_roc <- train(formula, data=uk_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN15b_roc <- predict(uk_model_DBN15b_roc, uk_train5,type="prob")
uk_DBN15b.ROC <- roc(predictor=ukpredb_DBN15b_roc$X0,
                     response=uk_train5$Censor,
                     levels=rev(levels(uk_train5$Censor)))
uk_DBN15b.ROC

#normalizedGini
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_train5$Censorb <- as.numeric(levels(uk_train5$Censorb))[uk_train5$Censorb]
set.seed(123); uk_model_DBN15b_gini <- train(formula, data=uk_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN15b_gini$results
uk_model_DBN15b_gini$resample
uk_pred_DBN15b_gini<- predict(uk_model_DBN15b_gini, newdata = uk_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train5$Censorb, uk_pred_DBN15b_gini$X1)
Gini(uk_train5$Censorb, uk_pred_DBN15b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN15b_gini$X1[uk_pred_DBN15b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train5$Censorb[uk_pred_DBN15b_gini$X1<=0.4]))
uk_DBN15b.ngini <- normalizedGini(a, p)
uk_DBN15b.ngini
uk_DBN15b.gini <-Gini(a, p)
uk_DBN15b.gini

#Brier score
set.seed(123); uk_model_DBN15b_brier <- train(formula, data=uk_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN15b_brier$results
uk_model_DBN15b_brier$resample
uk_pred_DBN15b_brier <- predict(uk_model_DBN15b_brier, newdata = uk_train5, type='prob')
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_DBN15b.bs <- Brier(as.numeric(as.character(uk_train5$Censorb)), uk_pred_DBN15b_brier$X1)
uk_DBN15b.bs

##Restults RF!
uk_results_DBN1_AUC <- cbind(uk_DBN11a.ROC$auc,uk_DBN11b.ROC$auc,uk_DBN12a.ROC$auc,uk_DBN12b.ROC$auc,uk_DBN13a.ROC$auc,uk_DBN13b.ROC$auc,uk_DBN14a.ROC$auc,
                             uk_DBN14b.ROC$auc,uk_DBN15a.ROC$auc,uk_DBN15b.ROC$auc)
uk_results_DBN1_bs <- cbind(uk_DBN11a.bs,uk_DBN11b.bs,uk_DBN12a.bs,uk_DBN12b.bs,uk_DBN13a.bs,uk_DBN13b.bs,uk_DBN14a.bs,uk_DBN14b.bs,uk_DBN15a.bs,uk_DBN15b.bs)
uk_results_DBN1_ngini <- cbind(uk_DBN11a.ngini,uk_DBN11b.ngini,uk_DBN12a.ngini,uk_DBN12b.ngini,uk_DBN13a.ngini,uk_DBN13b.ngini,uk_DBN14a.ngini,uk_DBN14b.ngini,
                               uk_DBN15a.ngini,uk_DBN15b.ngini)
uk_results_DBN1_gini <- cbind(uk_DBN11a.gini,uk_DBN11b.gini,uk_DBN12a.gini,uk_DBN12b.gini,uk_DBN13a.gini,uk_DBN13b.gini,uk_DBN14a.gini,uk_DBN14b.gini,
                              uk_DBN15a.gini,uk_DBN15b.gini)
mean(uk_results_DBN1_AUC)
mean(uk_results_DBN1_bs)
mean(uk_results_DBN1_ngini)
mean(uk_results_DBN1_gini)

##############################
###########DBN3###############
##############################
DBN3_grid <- expand.grid(.layer1=c(5,10,15,20), .layer2=c(5,10,15,20), .layer3=c(5,10,15,20), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0))
set.seed(123); uk_model_DBN31a_roc <- train(formula, data=uk_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN3grid <- expand.grid(.layer1=c(15), .layer2=c(15), .layer3=c(20),.hidden_dropout=c(0.25), .visible_dropout=c(0.5), .lr=c(1.5))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN31a_roc <- train(formula, data=uk_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN31a_roc <- predict(uk_model_DBN31a_roc,uk_test1,type="prob")
uk_DBN31a.ROC <- roc(predictor=ukpredb_DBN31a_roc$X0,
                     response=uk_test1$Censor,
                     levels=rev(levels(uk_test1$Censor)))
uk_DBN31a.ROC

#normalizedGini
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_test1$Censorb <- as.numeric(levels(uk_test1$Censorb))[uk_test1$Censorb]
set.seed(123); uk_model_DBN31a_gini <- train(formula, data=uk_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN31a_gini$results
uk_model_DBN31a_gini$resample
uk_pred_DBN31a_gini<- predict(uk_model_DBN31a_gini, newdata = uk_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test1$Censorb, uk_pred_DBN31a_gini$X1)
Gini(uk_test1$Censorb, uk_pred_DBN31a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN31a_gini$X1[uk_pred_DBN31a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test1$Censorb[uk_pred_DBN31a_gini$X1<=0.4]))
uk_DBN31a.ngini <- normalizedGini(a, p)
uk_DBN31a.ngini
uk_DBN31a.gini <-Gini(a, p)
uk_DBN31a.gini

#Brier score
set.seed(123); uk_model_DBN31a_brier <- train(formula, data=uk_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN31a_brier$results
uk_model_DBN31a_brier$resample
uk_pred_DBN31a_brier <- predict(uk_model_DBN31a_brier, newdata = uk_test1, type='prob')
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_DBN31a.bs <- Brier(as.numeric(as.character(uk_test1$Censorb)), uk_pred_DBN31a_brier$X1)
uk_DBN31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_DBN31b_roc <- train(formula, data=uk_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN31b_roc <- predict(uk_model_DBN31b_roc, uk_train1,type="prob")
uk_DBN31b.ROC <- roc(predictor=ukpredb_DBN31b_roc$X0,
                     response=uk_train1$Censor,
                     levels=rev(levels(uk_train1$Censor)))
uk_DBN31b.ROC

#normalizedGini
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_train1$Censorb <- as.numeric(levels(uk_train1$Censorb))[uk_train1$Censorb]
set.seed(123); uk_model_DBN31b_gini <- train(formula, data=uk_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN31b_gini$results
uk_model_DBN31b_gini$resample
uk_pred_DBN31b_gini<- predict(uk_model_DBN31b_gini, newdata = uk_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train1$Censorb, uk_pred_DBN31b_gini$X1)
Gini(uk_train1$Censorb, uk_pred_DBN31b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN31b_gini$X1[uk_pred_DBN31b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train1$Censorb[uk_pred_DBN31b_gini$X1<=0.4]))
uk_DBN31b.ngini <- normalizedGini(a, p)
uk_DBN31b.ngini
uk_DBN31b.gini <-Gini(a, p)
uk_DBN31b.gini

#Brier score
set.seed(123); uk_model_DBN31b_brier <- train(formula, data=uk_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN31b_brier$results
uk_model_DBN31b_brier$resample
uk_pred_DBN31b_brier <- predict(uk_model_DBN31b_brier, newdata = uk_train1, type='prob')
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_DBN31b.bs <- Brier(as.numeric(as.character(uk_train1$Censorb)), uk_pred_DBN31b_brier$X1)
uk_DBN31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN32a_roc <- train(formula, data=uk_train2, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN32a_roc <- predict(uk_model_DBN32a_roc,uk_test2,type="prob")
uk_DBN32a.ROC <- roc(predictor=ukpredb_DBN32a_roc$X0,
                     response=uk_test2$Censor,
                     levels=rev(levels(uk_test2$Censor)))
uk_DBN32a.ROC

#normalizedGini
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_test2$Censorb <- as.numeric(levels(uk_test2$Censorb))[uk_test2$Censorb]
set.seed(123); uk_model_DBN32a_gini <- train(formula, data=uk_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN32a_gini$results
uk_model_DBN32a_gini$resample
uk_pred_DBN32a_gini<- predict(uk_model_DBN32a_gini, newdata = uk_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test2$Censorb, uk_pred_DBN32a_gini$X1)
Gini(uk_test2$Censorb, uk_pred_DBN32a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN32a_gini$X1[uk_pred_DBN32a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test2$Censorb[uk_pred_DBN32a_gini$X1<=0.4]))
uk_DBN32a.ngini <- normalizedGini(a, p)
uk_DBN32a.ngini
uk_DBN32a.gini <-Gini(a, p)
uk_DBN32a.gini

#Brier score
set.seed(123); uk_model_DBN32a_brier <- train(formula, data=uk_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN32a_brier$results
uk_model_DBN32a_brier$resample
uk_pred_DBN32a_brier <- predict(uk_model_DBN32a_brier, newdata = uk_test2, type='prob')
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_DBN32a.bs <- Brier(as.numeric(as.character(uk_test2$Censorb)), uk_pred_DBN32a_brier$X1)
uk_DBN32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_DBN32b_roc <- train(formula, data=uk_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN32b_roc <- predict(uk_model_DBN32b_roc, uk_train2,type="prob")
uk_DBN32b.ROC <- roc(predictor=ukpredb_DBN32b_roc$X0,
                     response=uk_train2$Censor,
                     levels=rev(levels(uk_train2$Censor)))
uk_DBN32b.ROC

#normalizedGini
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_train2$Censorb <- as.numeric(levels(uk_train2$Censorb))[uk_train2$Censorb]
set.seed(123); uk_model_DBN32b_gini <- train(formula, data=uk_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN32b_gini$results
uk_model_DBN32b_gini$resample
uk_pred_DBN32b_gini<- predict(uk_model_DBN32b_gini, newdata = uk_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train2$Censorb, uk_pred_DBN32b_gini$X1)
Gini(uk_train2$Censorb, uk_pred_DBN32b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN32b_gini$X1[uk_pred_DBN32b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train2$Censorb[uk_pred_DBN32b_gini$X1<=0.4]))
uk_DBN32b.ngini <- normalizedGini(a, p)
uk_DBN32b.ngini
uk_DBN32b.gini <-Gini(a, p)
uk_DBN32b.gini

#Brier score
set.seed(123); uk_model_DBN32b_brier <- train(formula, data=uk_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN32b_brier$results
uk_model_DBN32b_brier$resample
uk_pred_DBN32b_brier <- predict(uk_model_DBN32b_brier, newdata = uk_train2, type='prob')
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_DBN32b.bs <- Brier(as.numeric(as.character(uk_train2$Censorb)), uk_pred_DBN32b_brier$X1)
uk_DBN32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN33a_roc <- train(formula, data=uk_train3, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN33a_roc <- predict(uk_model_DBN33a_roc,uk_test3,type="prob")
uk_DBN33a.ROC <- roc(predictor=ukpredb_DBN33a_roc$X0,
                     response=uk_test3$Censor,
                     levels=rev(levels(uk_test3$Censor)))
uk_DBN33a.ROC

#normalizedGini
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_test3$Censorb <- as.numeric(levels(uk_test3$Censorb))[uk_test3$Censorb]
set.seed(123); uk_model_DBN33a_gini <- train(formula, data=uk_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN33a_gini$results
uk_model_DBN33a_gini$resample
uk_pred_DBN33a_gini<- predict(uk_model_DBN33a_gini, newdata = uk_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test3$Censorb, uk_pred_DBN33a_gini$X1)
Gini(uk_test3$Censorb, uk_pred_DBN33a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN33a_gini$X1[uk_pred_DBN33a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test3$Censorb[uk_pred_DBN33a_gini$X1<=0.4]))
uk_DBN33a.ngini <- normalizedGini(a, p)
uk_DBN33a.ngini
uk_DBN33a.gini <-Gini(a, p)
uk_DBN33a.gini

#Brier score
set.seed(123); uk_model_DBN33a_brier <- train(formula, data=uk_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN33a_brier$results
uk_model_DBN33a_brier$resample
uk_pred_DBN33a_brier <- predict(uk_model_DBN33a_brier, newdata = uk_test3, type='prob')
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_DBN33a.bs <- Brier(as.numeric(as.character(uk_test3$Censorb)), uk_pred_DBN33a_brier$X1)
uk_DBN33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_DBN33b_roc <- train(formula, data=uk_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN33b_roc <- predict(uk_model_DBN33b_roc, uk_train3,type="prob")
uk_DBN33b.ROC <- roc(predictor=ukpredb_DBN33b_roc$X0,
                     response=uk_train3$Censor,
                     levels=rev(levels(uk_train3$Censor)))
uk_DBN33b.ROC

#normalizedGini
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_train3$Censorb <- as.numeric(levels(uk_train3$Censorb))[uk_train3$Censorb]
set.seed(123); uk_model_DBN33b_gini <- train(formula, data=uk_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN33b_gini$results
uk_model_DBN33b_gini$resample
uk_pred_DBN33b_gini<- predict(uk_model_DBN33b_gini, newdata = uk_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train3$Censorb, uk_pred_DBN33b_gini$X1)
Gini(uk_train3$Censorb, uk_pred_DBN33b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN33b_gini$X1[uk_pred_DBN33b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train3$Censorb[uk_pred_DBN33b_gini$X1<=0.4]))
uk_DBN33b.ngini <- normalizedGini(a, p)
uk_DBN33b.ngini
uk_DBN33b.gini <-Gini(a, p)
uk_DBN33b.gini

#Brier score
set.seed(123); uk_model_DBN33b_brier <- train(formula, data=uk_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN33b_brier$results
uk_model_DBN33b_brier$resample
uk_pred_DBN33b_brier <- predict(uk_model_DBN33b_brier, newdata = uk_train3, type='prob')
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_DBN33b.bs <- Brier(as.numeric(as.character(uk_train3$Censorb)), uk_pred_DBN33b_brier$X1)
uk_DBN33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN34a_roc <- train(formula, data=uk_train4, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN34a_roc <- predict(uk_model_DBN34a_roc,uk_test4,type="prob")
uk_DBN34a.ROC <- roc(predictor=ukpredb_DBN34a_roc$X0,
                     response=uk_test4$Censor,
                     levels=rev(levels(uk_test4$Censor)))
uk_DBN34a.ROC

#normalizedGini
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_test4$Censorb <- as.numeric(levels(uk_test4$Censorb))[uk_test4$Censorb]
set.seed(123); uk_model_DBN34a_gini <- train(formula, data=uk_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN34a_gini$results
uk_model_DBN34a_gini$resample
uk_pred_DBN34a_gini<- predict(uk_model_DBN34a_gini, newdata = uk_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test4$Censorb, uk_pred_DBN34a_gini$X1)
Gini(uk_test4$Censorb, uk_pred_DBN34a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN34a_gini$X1[uk_pred_DBN34a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test4$Censorb[uk_pred_DBN34a_gini$X1<=0.4]))
uk_DBN34a.ngini <- normalizedGini(a, p)
uk_DBN34a.ngini
uk_DBN34a.gini <-Gini(a, p)
uk_DBN34a.gini

#Brier score
set.seed(123); uk_model_DBN34a_brier <- train(formula, data=uk_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN34a_brier$results
uk_model_DBN34a_brier$resample
uk_pred_DBN34a_brier <- predict(uk_model_DBN34a_brier, newdata = uk_test4, type='prob')
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_DBN34a.bs <- Brier(as.numeric(as.character(uk_test4$Censorb)), uk_pred_DBN34a_brier$X1)
uk_DBN34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_DBN34b_roc <- train(formula, data=uk_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN34b_roc <- predict(uk_model_DBN34b_roc, uk_train4,type="prob")
uk_DBN34b.ROC <- roc(predictor=ukpredb_DBN34b_roc$X0,
                     response=uk_train4$Censor,
                     levels=rev(levels(uk_train4$Censor)))
uk_DBN34b.ROC

#normalizedGini
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_train4$Censorb <- as.numeric(levels(uk_train4$Censorb))[uk_train4$Censorb]
set.seed(123); uk_model_DBN34b_gini <- train(formula, data=uk_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN34b_gini$results
uk_model_DBN34b_gini$resample
uk_pred_DBN34b_gini<- predict(uk_model_DBN34b_gini, newdata = uk_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train4$Censorb, uk_pred_DBN34b_gini$X1)
Gini(uk_train4$Censorb, uk_pred_DBN34b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN34b_gini$X1[uk_pred_DBN34b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train4$Censorb[uk_pred_DBN34b_gini$X1<=0.4]))
uk_DBN34b.ngini <- normalizedGini(a, p)
uk_DBN34b.ngini
uk_DBN34b.gini <-Gini(a, p)
uk_DBN34b.gini

#Brier score
set.seed(123); uk_model_DBN34b_brier <- train(formula, data=uk_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN34b_brier$results
uk_model_DBN34b_brier$resample
uk_pred_DBN34b_brier <- predict(uk_model_DBN34b_brier, newdata = uk_train4, type='prob')
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_DBN34b.bs <- Brier(as.numeric(as.character(uk_train4$Censorb)), uk_pred_DBN34b_brier$X1)
uk_DBN34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN35a_roc <- train(formula, data=uk_train5, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN35a_roc <- predict(uk_model_DBN35a_roc,uk_test5,type="prob")
uk_DBN35a.ROC <- roc(predictor=ukpredb_DBN35a_roc$X0,
                     response=uk_test5$Censor,
                     levels=rev(levels(uk_test5$Censor)))
uk_DBN35a.ROC

#normalizedGini
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_test5$Censorb <- as.numeric(levels(uk_test5$Censorb))[uk_test5$Censorb]
set.seed(123); uk_model_DBN35a_gini <- train(formula, data=uk_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN35a_gini$results
uk_model_DBN35a_gini$resample
uk_pred_DBN35a_gini<- predict(uk_model_DBN35a_gini, newdata = uk_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test5$Censorb, uk_pred_DBN35a_gini$X1)
Gini(uk_test5$Censorb, uk_pred_DBN35a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN35a_gini$X1[uk_pred_DBN35a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test5$Censorb[uk_pred_DBN35a_gini$X1<=0.4]))
uk_DBN35a.ngini <- normalizedGini(a, p)
uk_DBN35a.ngini
uk_DBN35a.gini <-Gini(a, p)
uk_DBN35a.gini

#Brier score
set.seed(123); uk_model_DBN35a_brier <- train(formula, data=uk_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN35a_brier$results
uk_model_DBN35a_brier$resample
uk_pred_DBN35a_brier <- predict(uk_model_DBN35a_brier, newdata = uk_test5, type='prob')
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_DBN35a.bs <- Brier(as.numeric(as.character(uk_test5$Censorb)), uk_pred_DBN35a_brier$X1)
uk_DBN35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_DBN35b_roc <- train(formula, data=uk_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN35b_roc <- predict(uk_model_DBN35b_roc, uk_train5,type="prob")
uk_DBN35b.ROC <- roc(predictor=ukpredb_DBN35b_roc$X0,
                     response=uk_train5$Censor,
                     levels=rev(levels(uk_train5$Censor)))
uk_DBN35b.ROC

#normalizedGini
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_train5$Censorb <- as.numeric(levels(uk_train5$Censorb))[uk_train5$Censorb]
set.seed(123); uk_model_DBN35b_gini <- train(formula, data=uk_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN35b_gini$results
uk_model_DBN35b_gini$resample
uk_pred_DBN35b_gini<- predict(uk_model_DBN35b_gini, newdata = uk_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train5$Censorb, uk_pred_DBN35b_gini$X1)
Gini(uk_train5$Censorb, uk_pred_DBN35b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN35b_gini$X1[uk_pred_DBN35b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train5$Censorb[uk_pred_DBN35b_gini$X1<=0.4]))
uk_DBN35b.ngini <- normalizedGini(a, p)
uk_DBN35b.ngini
uk_DBN35b.gini <-Gini(a, p)
uk_DBN35b.gini

#Brier score
set.seed(123); uk_model_DBN35b_brier <- train(formula, data=uk_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN35b_brier$results
uk_model_DBN35b_brier$resample
uk_pred_DBN35b_brier <- predict(uk_model_DBN35b_brier, newdata = uk_train5, type='prob')
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_DBN35b.bs <- Brier(as.numeric(as.character(uk_train5$Censorb)), uk_pred_DBN35b_brier$X1)
uk_DBN35b.bs

##Restults RF!
uk_results_DBN3_AUC <- cbind(uk_DBN31a.ROC$auc,uk_DBN31b.ROC$auc,uk_DBN32a.ROC$auc,uk_DBN32b.ROC$auc,uk_DBN33a.ROC$auc,uk_DBN33b.ROC$auc,uk_DBN34a.ROC$auc,
                             uk_DBN34b.ROC$auc,uk_DBN35a.ROC$auc,uk_DBN35b.ROC$auc)
uk_results_DBN3_bs <- cbind(uk_DBN31a.bs,uk_DBN31b.bs,uk_DBN32a.bs,uk_DBN32b.bs,uk_DBN33a.bs,uk_DBN33b.bs,uk_DBN34a.bs,uk_DBN34b.bs,uk_DBN35a.bs,uk_DBN35b.bs)
uk_results_DBN3_ngini <- cbind(uk_DBN31a.ngini,uk_DBN31b.ngini,uk_DBN32a.ngini,uk_DBN32b.ngini,uk_DBN33a.ngini,uk_DBN33b.ngini,uk_DBN34a.ngini,uk_DBN34b.ngini,
                               uk_DBN35a.ngini,uk_DBN35b.ngini)
uk_results_DBN3_gini <- cbind(uk_DBN31a.gini,uk_DBN31b.gini,uk_DBN32a.gini,uk_DBN32b.gini,uk_DBN33a.gini,uk_DBN33b.gini,uk_DBN34a.gini,uk_DBN34b.gini,
                              uk_DBN35a.gini,uk_DBN35b.gini)
mean(uk_results_DBN3_AUC)
mean(uk_results_DBN3_bs)
mean(uk_results_DBN3_ngini)
mean(uk_results_DBN3_gini)


##############################
###########DBN5###############
##############################
DBN5_grid <- expand.grid(.layer1=c(5,10,15), .layer2=c(5,10,15), .layer3=c(5,10,15),.layer4=c(5,10,15),.layer5=c(5,10,15), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0))
set.seed(123); uk_model_DBN51a_roc <- train(formula, data=uk_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
DBN5grid <- expand.grid(.layer1=c(15),.layer2=c(15),.layer3=c(15),.layer4=c(15),.layer5=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0.25), .lr=c(1.5))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN51a_roc <- train(formula, data=uk_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN51a_roc <- predict(uk_model_DBN51a_roc,uk_test1,type="prob")
uk_DBN51a.ROC <- roc(predictor=ukpredb_DBN51a_roc$X0,
                     response=uk_test1$Censor,
                     levels=rev(levels(uk_test1$Censor)))
uk_DBN51a.ROC

#normalizedGini
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_test1$Censorb <- as.numeric(levels(uk_test1$Censorb))[uk_test1$Censorb]
set.seed(123); uk_model_DBN51a_gini <- train(formula, data=uk_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN51a_gini$results
uk_model_DBN51a_gini$resample
uk_pred_DBN51a_gini<- predict(uk_model_DBN51a_gini, newdata = uk_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test1$Censorb, uk_pred_DBN51a_gini$X1)
Gini(uk_test1$Censorb, uk_pred_DBN51a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN51a_gini$X1[uk_pred_DBN51a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test1$Censorb[uk_pred_DBN51a_gini$X1<=0.4]))
uk_DBN51a.ngini <- normalizedGini(a, p)
uk_DBN51a.ngini
uk_DBN51a.gini <-Gini(a, p)
uk_DBN51a.gini

#Brier score
set.seed(123); uk_model_DBN51a_brier <- train(formula, data=uk_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN51a_brier$results
uk_model_DBN51a_brier$resample
uk_pred_DBN51a_brier <- predict(uk_model_DBN51a_brier, newdata = uk_test1, type='prob')
uk_test1$Censorb <- uk_test1$Censor
levels(uk_test1$Censorb) <- c('0','1')
uk_DBN51a.bs <- Brier(as.numeric(as.character(uk_test1$Censorb)), uk_pred_DBN51a_brier$X1)
uk_DBN51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); uk_model_DBN51b_roc <- train(formula, data=uk_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN51b_roc <- predict(uk_model_DBN51b_roc, uk_train1,type="prob")
uk_DBN51b.ROC <- roc(predictor=ukpredb_DBN51b_roc$X0,
                     response=uk_train1$Censor,
                     levels=rev(levels(uk_train1$Censor)))
uk_DBN51b.ROC

#normalizedGini
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_train1$Censorb <- as.numeric(levels(uk_train1$Censorb))[uk_train1$Censorb]
set.seed(123); uk_model_DBN51b_gini <- train(formula, data=uk_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN51b_gini$results
uk_model_DBN51b_gini$resample
uk_pred_DBN51b_gini<- predict(uk_model_DBN51b_gini, newdata = uk_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train1$Censorb, uk_pred_DBN51b_gini$X1)
Gini(uk_train1$Censorb, uk_pred_DBN51b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN51b_gini$X1[uk_pred_DBN51b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train1$Censorb[uk_pred_DBN51b_gini$X1<=0.4]))
uk_DBN51b.ngini <- normalizedGini(a, p)
uk_DBN51b.ngini
uk_DBN51b.gini <-Gini(a, p)
uk_DBN51b.gini

#Brier score
set.seed(123); uk_model_DBN51b_brier <- train(formula, data=uk_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN51b_brier$results
uk_model_DBN51b_brier$resample
uk_pred_DBN51b_brier <- predict(uk_model_DBN51b_brier, newdata = uk_train1, type='prob')
uk_train1$Censorb <- uk_train1$Censor
levels(uk_train1$Censorb) <- c('0','1')
uk_DBN51b.bs <- Brier(as.numeric(as.character(uk_train1$Censorb)), uk_pred_DBN51b_brier$X1)
uk_DBN51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN52a_roc <- train(formula, data=uk_train2, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN52a_roc <- predict(uk_model_DBN52a_roc,uk_test2,type="prob")
uk_DBN52a.ROC <- roc(predictor=ukpredb_DBN52a_roc$X0,
                     response=uk_test2$Censor,
                     levels=rev(levels(uk_test2$Censor)))
uk_DBN52a.ROC

#normalizedGini
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_test2$Censorb <- as.numeric(levels(uk_test2$Censorb))[uk_test2$Censorb]
set.seed(123); uk_model_DBN52a_gini <- train(formula, data=uk_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN52a_gini$results
uk_model_DBN52a_gini$resample
uk_pred_DBN52a_gini<- predict(uk_model_DBN52a_gini, newdata = uk_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test2$Censorb, uk_pred_DBN52a_gini$X1)
Gini(uk_test2$Censorb, uk_pred_DBN52a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN52a_gini$X1[uk_pred_DBN52a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test2$Censorb[uk_pred_DBN52a_gini$X1<=0.4]))
uk_DBN52a.ngini <- normalizedGini(a, p)
uk_DBN52a.ngini
uk_DBN52a.gini <-Gini(a, p)
uk_DBN52a.gini

#Brier score
set.seed(123); uk_model_DBN52a_brier <- train(formula, data=uk_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN52a_brier$results
uk_model_DBN52a_brier$resample
uk_pred_DBN52a_brier <- predict(uk_model_DBN52a_brier, newdata = uk_test2, type='prob')
uk_test2$Censorb <- uk_test2$Censor
levels(uk_test2$Censorb) <- c('0','1')
uk_DBN52a.bs <- Brier(as.numeric(as.character(uk_test2$Censorb)), uk_pred_DBN52a_brier$X1)
uk_DBN52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); uk_model_DBN52b_roc <- train(formula, data=uk_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN52b_roc <- predict(uk_model_DBN52b_roc, uk_train2,type="prob")
uk_DBN52b.ROC <- roc(predictor=ukpredb_DBN52b_roc$X0,
                     response=uk_train2$Censor,
                     levels=rev(levels(uk_train2$Censor)))
uk_DBN52b.ROC

#normalizedGini
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_train2$Censorb <- as.numeric(levels(uk_train2$Censorb))[uk_train2$Censorb]
set.seed(123); uk_model_DBN52b_gini <- train(formula, data=uk_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN52b_gini$results
uk_model_DBN52b_gini$resample
uk_pred_DBN52b_gini<- predict(uk_model_DBN52b_gini, newdata = uk_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train2$Censorb, uk_pred_DBN52b_gini$X1)
Gini(uk_train2$Censorb, uk_pred_DBN52b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN52b_gini$X1[uk_pred_DBN52b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train2$Censorb[uk_pred_DBN52b_gini$X1<=0.4]))
uk_DBN52b.ngini <- normalizedGini(a, p)
uk_DBN52b.ngini
uk_DBN52b.gini <-Gini(a, p)
uk_DBN52b.gini

#Brier score
set.seed(123); uk_model_DBN52b_brier <- train(formula, data=uk_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN52b_brier$results
uk_model_DBN52b_brier$resample
uk_pred_DBN52b_brier <- predict(uk_model_DBN52b_brier, newdata = uk_train2, type='prob')
uk_train2$Censorb <- uk_train2$Censor
levels(uk_train2$Censorb) <- c('0','1')
uk_DBN52b.bs <- Brier(as.numeric(as.character(uk_train2$Censorb)), uk_pred_DBN52b_brier$X1)
uk_DBN52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN53a_roc <- train(formula, data=uk_train3, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN53a_roc <- predict(uk_model_DBN53a_roc,uk_test3,type="prob")
uk_DBN53a.ROC <- roc(predictor=ukpredb_DBN53a_roc$X0,
                     response=uk_test3$Censor,
                     levels=rev(levels(uk_test3$Censor)))
uk_DBN53a.ROC

#normalizedGini
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_test3$Censorb <- as.numeric(levels(uk_test3$Censorb))[uk_test3$Censorb]
set.seed(123); uk_model_DBN53a_gini <- train(formula, data=uk_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN53a_gini$results
uk_model_DBN53a_gini$resample
uk_pred_DBN53a_gini<- predict(uk_model_DBN53a_gini, newdata = uk_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test3$Censorb, uk_pred_DBN53a_gini$X1)
Gini(uk_test3$Censorb, uk_pred_DBN53a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN53a_gini$X1[uk_pred_DBN53a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test3$Censorb[uk_pred_DBN53a_gini$X1<=0.4]))
uk_DBN53a.ngini <- normalizedGini(a, p)
uk_DBN53a.ngini
uk_DBN53a.gini <-Gini(a, p)
uk_DBN53a.gini

#Brier score
set.seed(123); uk_model_DBN53a_brier <- train(formula, data=uk_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN53a_brier$results
uk_model_DBN53a_brier$resample
uk_pred_DBN53a_brier <- predict(uk_model_DBN53a_brier, newdata = uk_test3, type='prob')
uk_test3$Censorb <- uk_test3$Censor
levels(uk_test3$Censorb) <- c('0','1')
uk_DBN53a.bs <- Brier(as.numeric(as.character(uk_test3$Censorb)), uk_pred_DBN53a_brier$X1)
uk_DBN53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); uk_model_DBN53b_roc <- train(formula, data=uk_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN53b_roc <- predict(uk_model_DBN53b_roc, uk_train3,type="prob")
uk_DBN53b.ROC <- roc(predictor=ukpredb_DBN53b_roc$X0,
                     response=uk_train3$Censor,
                     levels=rev(levels(uk_train3$Censor)))
uk_DBN53b.ROC

#normalizedGini
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_train3$Censorb <- as.numeric(levels(uk_train3$Censorb))[uk_train3$Censorb]
set.seed(123); uk_model_DBN53b_gini <- train(formula, data=uk_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN53b_gini$results
uk_model_DBN53b_gini$resample
uk_pred_DBN53b_gini<- predict(uk_model_DBN53b_gini, newdata = uk_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train3$Censorb, uk_pred_DBN53b_gini$X1)
Gini(uk_train3$Censorb, uk_pred_DBN53b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN53b_gini$X1[uk_pred_DBN53b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train3$Censorb[uk_pred_DBN53b_gini$X1<=0.4]))
uk_DBN53b.ngini <- normalizedGini(a, p)
uk_DBN53b.ngini
uk_DBN53b.gini <-Gini(a, p)
uk_DBN53b.gini

#Brier score
set.seed(123); uk_model_DBN53b_brier <- train(formula, data=uk_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN53b_brier$results
uk_model_DBN53b_brier$resample
uk_pred_DBN53b_brier <- predict(uk_model_DBN53b_brier, newdata = uk_train3, type='prob')
uk_train3$Censorb <- uk_train3$Censor
levels(uk_train3$Censorb) <- c('0','1')
uk_DBN53b.bs <- Brier(as.numeric(as.character(uk_train3$Censorb)), uk_pred_DBN53b_brier$X1)
uk_DBN53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN54a_roc <- train(formula, data=uk_train4, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN54a_roc <- predict(uk_model_DBN54a_roc,uk_test4,type="prob")
uk_DBN54a.ROC <- roc(predictor=ukpredb_DBN54a_roc$X0,
                     response=uk_test4$Censor,
                     levels=rev(levels(uk_test4$Censor)))
uk_DBN54a.ROC

#normalizedGini
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_test4$Censorb <- as.numeric(levels(uk_test4$Censorb))[uk_test4$Censorb]
set.seed(123); uk_model_DBN54a_gini <- train(formula, data=uk_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN54a_gini$results
uk_model_DBN54a_gini$resample
uk_pred_DBN54a_gini<- predict(uk_model_DBN54a_gini, newdata = uk_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test4$Censorb, uk_pred_DBN54a_gini$X1)
Gini(uk_test4$Censorb, uk_pred_DBN54a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN54a_gini$X1[uk_pred_DBN54a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test4$Censorb[uk_pred_DBN54a_gini$X1<=0.4]))
uk_DBN54a.ngini <- normalizedGini(a, p)
uk_DBN54a.ngini
uk_DBN54a.gini <-Gini(a, p)
uk_DBN54a.gini

#Brier score
set.seed(123); uk_model_DBN54a_brier <- train(formula, data=uk_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN54a_brier$results
uk_model_DBN54a_brier$resample
uk_pred_DBN54a_brier <- predict(uk_model_DBN54a_brier, newdata = uk_test4, type='prob')
uk_test4$Censorb <- uk_test4$Censor
levels(uk_test4$Censorb) <- c('0','1')
uk_DBN54a.bs <- Brier(as.numeric(as.character(uk_test4$Censorb)), uk_pred_DBN54a_brier$X1)
uk_DBN54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); uk_model_DBN54b_roc <- train(formula, data=uk_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN54b_roc <- predict(uk_model_DBN54b_roc, uk_train4,type="prob")
uk_DBN54b.ROC <- roc(predictor=ukpredb_DBN54b_roc$X0,
                     response=uk_train4$Censor,
                     levels=rev(levels(uk_train4$Censor)))
uk_DBN54b.ROC

#normalizedGini
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_train4$Censorb <- as.numeric(levels(uk_train4$Censorb))[uk_train4$Censorb]
set.seed(123); uk_model_DBN54b_gini <- train(formula, data=uk_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN54b_gini$results
uk_model_DBN54b_gini$resample
uk_pred_DBN54b_gini<- predict(uk_model_DBN54b_gini, newdata = uk_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train4$Censorb, uk_pred_DBN54b_gini$X1)
Gini(uk_train4$Censorb, uk_pred_DBN54b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN54b_gini$X1[uk_pred_DBN54b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train4$Censorb[uk_pred_DBN54b_gini$X1<=0.4]))
uk_DBN54b.ngini <- normalizedGini(a, p)
uk_DBN54b.ngini
uk_DBN54b.gini <-Gini(a, p)
uk_DBN54b.gini

#Brier score
set.seed(123); uk_model_DBN54b_brier <- train(formula, data=uk_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN54b_brier$results
uk_model_DBN54b_brier$resample
uk_pred_DBN54b_brier <- predict(uk_model_DBN54b_brier, newdata = uk_train4, type='prob')
uk_train4$Censorb <- uk_train4$Censor
levels(uk_train4$Censorb) <- c('0','1')
uk_DBN54b.bs <- Brier(as.numeric(as.character(uk_train4$Censorb)), uk_pred_DBN54b_brier$X1)
uk_DBN54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); uk_model_DBN55a_roc <- train(formula, data=uk_train5, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN55a_roc <- predict(uk_model_DBN55a_roc,uk_test5,type="prob")
uk_DBN55a.ROC <- roc(predictor=ukpredb_DBN55a_roc$X0,
                     response=uk_test5$Censor,
                     levels=rev(levels(uk_test5$Censor)))
uk_DBN55a.ROC

#normalizedGini
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_test5$Censorb <- as.numeric(levels(uk_test5$Censorb))[uk_test5$Censorb]
set.seed(123); uk_model_DBN55a_gini <- train(formula, data=uk_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN55a_gini$results
uk_model_DBN55a_gini$resample
uk_pred_DBN55a_gini<- predict(uk_model_DBN55a_gini, newdata = uk_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_test5$Censorb, uk_pred_DBN55a_gini$X1)
Gini(uk_test5$Censorb, uk_pred_DBN55a_gini$X1)
#b <= 0.4
p <- uk_pred_DBN55a_gini$X1[uk_pred_DBN55a_gini$X1<=0.4]
a <- as.numeric(as.character(uk_test5$Censorb[uk_pred_DBN55a_gini$X1<=0.4]))
uk_DBN55a.ngini <- normalizedGini(a, p)
uk_DBN55a.ngini
uk_DBN55a.gini <-Gini(a, p)
uk_DBN55a.gini

#Brier score
set.seed(123); uk_model_DBN55a_brier <- train(formula, data=uk_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN55a_brier$results
uk_model_DBN55a_brier$resample
uk_pred_DBN55a_brier <- predict(uk_model_DBN55a_brier, newdata = uk_test5, type='prob')
uk_test5$Censorb <- uk_test5$Censor
levels(uk_test5$Censorb) <- c('0','1')
uk_DBN55a.bs <- Brier(as.numeric(as.character(uk_test5$Censorb)), uk_pred_DBN55a_brier$X1)
uk_DBN55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); uk_model_DBN55b_roc <- train(formula, data=uk_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
ukpredb_DBN55b_roc <- predict(uk_model_DBN55b_roc, uk_train5,type="prob")
uk_DBN55b.ROC <- roc(predictor=ukpredb_DBN55b_roc$X0,
                     response=uk_train5$Censor,
                     levels=rev(levels(uk_train5$Censor)))
uk_DBN55b.ROC

#normalizedGini
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_train5$Censorb <- as.numeric(levels(uk_train5$Censorb))[uk_train5$Censorb]
set.seed(123); uk_model_DBN55b_gini <- train(formula, data=uk_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
uk_model_DBN55b_gini$results
uk_model_DBN55b_gini$resample
uk_pred_DBN55b_gini<- predict(uk_model_DBN55b_gini, newdata = uk_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(uk_train5$Censorb, uk_pred_DBN55b_gini$X1)
Gini(uk_train5$Censorb, uk_pred_DBN55b_gini$X1)
#b <= 0.4
p <- uk_pred_DBN55b_gini$X1[uk_pred_DBN55b_gini$X1<=0.4]
a <- as.numeric(as.character(uk_train5$Censorb[uk_pred_DBN55b_gini$X1<=0.4]))
uk_DBN55b.ngini <- normalizedGini(a, p)
uk_DBN55b.ngini
uk_DBN55b.gini <-Gini(a, p)
uk_DBN55b.gini

#Brier score
set.seed(123); uk_model_DBN55b_brier <- train(formula, data=uk_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
uk_model_DBN55b_brier$results
uk_model_DBN55b_brier$resample
uk_pred_DBN55b_brier <- predict(uk_model_DBN55b_brier, newdata = uk_train5, type='prob')
uk_train5$Censorb <- uk_train5$Censor
levels(uk_train5$Censorb) <- c('0','1')
uk_DBN55b.bs <- Brier(as.numeric(as.character(uk_train5$Censorb)), uk_pred_DBN55b_brier$X1)
uk_DBN55b.bs

##Restults RF!
uk_results_DBN5_AUC <- cbind(uk_DBN51a.ROC$auc,uk_DBN51b.ROC$auc,uk_DBN52a.ROC$auc,uk_DBN52b.ROC$auc,uk_DBN53a.ROC$auc,uk_DBN53b.ROC$auc,uk_DBN54a.ROC$auc,
                             uk_DBN54b.ROC$auc,uk_DBN55a.ROC$auc,uk_DBN55b.ROC$auc)
uk_results_DBN5_bs <- cbind(uk_DBN51a.bs,uk_DBN51b.bs,uk_DBN52a.bs,uk_DBN52b.bs,uk_DBN53a.bs,uk_DBN53b.bs,uk_DBN54a.bs,uk_DBN54b.bs,uk_DBN55a.bs,uk_DBN55b.bs)
uk_results_DBN5_ngini <- cbind(uk_DBN51a.ngini,uk_DBN51b.ngini,uk_DBN52a.ngini,uk_DBN52b.ngini,uk_DBN53a.ngini,uk_DBN53b.ngini,uk_DBN54a.ngini,uk_DBN54b.ngini,
                               uk_DBN55a.ngini,uk_DBN55b.ngini)
uk_results_DBN5_gini <- cbind(uk_DBN51a.gini,uk_DBN51b.gini,uk_DBN52a.gini,uk_DBN52b.gini,uk_DBN53a.gini,uk_DBN53b.gini,uk_DBN54a.gini,uk_DBN54b.gini,
                              uk_DBN55a.gini,uk_DBN55b.gini)
mean(uk_results_DBN5_AUC)
mean(uk_results_DBN5_bs)
mean(uk_results_DBN5_ngini)
mean(uk_results_DBN5_gini)

#EMP 
#uk - logistic
formula <- Censor~Open+Age+Amount+Curradd+Curremp+Custgend+Depchild+Freqpaid+Homephon+Insprem+Loantype+Marstat+Term+Homwowns+Purpose
set.seed(123); uk_model_log1a_emp <- train(formula, data=uk_woe_train1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log1a_emp <- predict(uk_model_log1a_emp, newdata = uk_woe_test1, type='prob')
ukemp_log1a <- empCreditScoring(ukpredb_log1a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_log1b_emp <- train(formula, data=uk_woe_test1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log1b_emp <- predict(uk_model_log1b_emp, newdata = uk_woe_train1, type='prob')
ukemp_log1b <- empCreditScoring(ukpredb_log1b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_log2a_emp <- train(formula, data=uk_woe_train2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log2a_emp <- predict(uk_model_log2a_emp, newdata = uk_woe_test2, type='prob')
ukemp_log2a <- empCreditScoring(ukpredb_log2a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_log2b_emp <- train(formula, data=uk_woe_test2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log2b_emp <- predict(uk_model_log2b_emp, newdata = uk_woe_train2, type='prob')
ukemp_log2b <- empCreditScoring(ukpredb_log2b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_log3a_emp <- train(formula, data=uk_woe_train3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log3a_emp <- predict(uk_model_log3a_emp, newdata = uk_woe_test3, type='prob')
ukemp_log3a <- empCreditScoring(ukpredb_log3a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_log3b_emp <- train(formula, data=uk_woe_test3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log3b_emp <- predict(uk_model_log3b_emp, newdata = uk_woe_train3, type='prob')
ukemp_log3b <- empCreditScoring(ukpredb_log3b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_log4a_emp <- train(formula, data=uk_woe_train4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log4a_emp <- predict(uk_model_log4a_emp, newdata = uk_woe_test4, type='prob')
ukemp_log4a <- empCreditScoring(ukpredb_log4a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_log4b_emp <- train(formula, data=uk_woe_test4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log4b_emp <- predict(uk_model_log4b_emp, newdata = uk_woe_train4, type='prob')
ukemp_log4b <- empCreditScoring(ukpredb_log4b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_log5a_emp <- train(formula, data=uk_woe_train5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log5a_emp <- predict(uk_model_log5a_emp, newdata = uk_woe_test5, type='prob')
ukemp_log5a <- empCreditScoring(ukpredb_log5a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_log5b_emp <- train(formula, data=uk_woe_test5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
ukpredb_log5b_emp <- predict(uk_model_log5b_emp, newdata = uk_woe_train5, type='prob')
ukemp_log5b <- empCreditScoring(ukpredb_log5b_emp$X1,uk_woe_train5$Censor)

#uk - dt
DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
set.seed(123); ukreg_model_DT1a_emp <- train(formula, data=uk_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT1a_emp <- predict(ukreg_model_DT1a_emp, newdata = uk_test1, type='prob')
ukemp_dt1a <- empCreditScoring(ukregpredb_DT1a_emp$X1,uk_test1$Censor)
set.seed(123); ukreg_model_DT1b_emp <- train(formula, data=uk_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT1b_emp <- predict(ukreg_model_DT1b_emp, newdata = uk_train1, type='prob')
ukemp_dt1b <- empCreditScoring(ukregpredb_DT1b_emp$X1,uk_train1$Censor)
set.seed(123); ukreg_model_DT2a_emp <- train(formula, data=uk_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT2a_emp <- predict(ukreg_model_DT2a_emp, newdata = uk_test2, type='prob')
ukemp_dt2a <- empCreditScoring(ukregpredb_DT2a_emp$X1,uk_test2$Censor)
set.seed(123); ukreg_model_DT2b_emp <- train(formula, data=uk_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT2b_emp <- predict(ukreg_model_DT2b_emp, newdata = uk_train2, type='prob')
ukemp_dt2b <- empCreditScoring(ukregpredb_DT2b_emp$X1,uk_train2$Censor)
set.seed(123); ukreg_model_DT3a_emp <- train(formula, data=uk_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT3a_emp <- predict(ukreg_model_DT3a_emp, newdata = uk_test3, type='prob')
ukemp_dt3a <- empCreditScoring(ukregpredb_DT3a_emp$X1,uk_test3$Censor)
set.seed(123); ukreg_model_DT3b_emp <- train(formula, data=uk_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT3b_emp <- predict(ukreg_model_DT3b_emp, newdata = uk_train3, type='prob')
ukemp_dt3b <- empCreditScoring(ukregpredb_DT3b_emp$X1,uk_train3$Censor)
set.seed(123); ukreg_model_DT4a_emp <- train(formula, data=uk_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT4a_emp <- predict(ukreg_model_DT4a_emp, newdata = uk_test4, type='prob')
ukemp_dt4a <- empCreditScoring(ukregpredb_DT4a_emp$X1,uk_test4$Censor)
set.seed(123); ukreg_model_DT4b_emp <- train(formula, data=uk_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT4b_emp <- predict(ukreg_model_DT4b_emp, newdata = uk_train4, type='prob')
ukemp_dt4b <- empCreditScoring(ukregpredb_DT4b_emp$X1,uk_train4$Censor)
set.seed(123); ukreg_model_DT5a_emp <- train(formula, data=uk_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT5a_emp <- predict(ukreg_model_DT5a_emp, newdata = uk_test5, type='prob')
ukemp_dt5a <- empCreditScoring(ukregpredb_DT5a_emp$X1,uk_test5$Censor)
set.seed(123); ukreg_model_DT5b_emp <- train(formula, data=uk_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
ukregpredb_DT5b_emp <- predict(ukreg_model_DT5b_emp, newdata = uk_train5, type='prob')
ukemp_dt5b <- empCreditScoring(ukregpredb_DT5b_emp$X1,uk_train5$Censor)

#uk - RF
m <- floor(log2(length(uk_train1$Censor)+1))
RFgrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))
set.seed(123); ukreg_model_RF1a_emp <- train(formula, data=uk_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF1a_emp <- predict(ukreg_model_RF1a_emp, newdata = uk_test1, type='prob')
ukemp_RF1a <- empCreditScoring(ukregpredb_RF1a_emp$X1,uk_test1$Censor)
set.seed(123); ukreg_model_RF1b_emp <- train(formula, data=uk_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF1b_emp <- predict(ukreg_model_RF1b_emp, newdata = uk_train1, type='prob')
ukemp_RF1b <- empCreditScoring(ukregpredb_RF1b_emp$X1,uk_train1$Censor)
set.seed(123); ukreg_model_RF2a_emp <- train(formula, data=uk_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF2a_emp <- predict(ukreg_model_RF2a_emp, newdata = uk_test2, type='prob')
ukemp_RF2a <- empCreditScoring(ukregpredb_RF2a_emp$X1,uk_test2$Censor)
set.seed(123); ukreg_model_RF2b_emp <- train(formula, data=uk_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF2b_emp <- predict(ukreg_model_RF2b_emp, newdata = uk_train2, type='prob')
ukemp_RF2b <- empCreditScoring(ukregpredb_RF2b_emp$X1,uk_train2$Censor)
set.seed(123); ukreg_model_RF3a_emp <- train(formula, data=uk_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF3a_emp <- predict(ukreg_model_RF3a_emp, newdata = uk_test3, type='prob')
ukemp_RF3a <- empCreditScoring(ukregpredb_RF3a_emp$X1,uk_test3$Censor)
set.seed(123); ukreg_model_RF3b_emp <- train(formula, data=uk_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF3b_emp <- predict(ukreg_model_RF3b_emp, newdata = uk_train3, type='prob')
ukemp_RF3b <- empCreditScoring(ukregpredb_RF3b_emp$X1,uk_train3$Censor)
set.seed(123); ukreg_model_RF4a_emp <- train(formula, data=uk_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF4a_emp <- predict(ukreg_model_RF4a_emp, newdata = uk_test4, type='prob')
ukemp_RF4a <- empCreditScoring(ukregpredb_RF4a_emp$X1,uk_test4$Censor)
set.seed(123); ukreg_model_RF4b_emp <- train(formula, data=uk_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF4b_emp <- predict(ukreg_model_RF4b_emp, newdata = uk_train4, type='prob')
ukemp_RF4b <- empCreditScoring(ukregpredb_RF4b_emp$X1,uk_train4$Censor)
set.seed(123); ukreg_model_RF5a_emp <- train(formula, data=uk_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF5a_emp <- predict(ukreg_model_RF5a_emp, newdata = uk_test5, type='prob')
ukemp_RF5a <- empCreditScoring(ukregpredb_RF5a_emp$X1,uk_test5$Censor)
set.seed(123); ukreg_model_RF5b_emp <- train(formula, data=uk_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
ukregpredb_RF5b_emp <- predict(ukreg_model_RF5b_emp, newdata = uk_train5, type='prob')
ukemp_RF5b <- empCreditScoring(ukregpredb_RF5b_emp$X1,uk_train5$Censor)

#uk - MLP1
MLP1grid <- expand.grid(.size=c(15), .dropout=c(0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP11a_emp <- train(formula, data=uk_woe_train1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP11a_emp <- predict(uk_model_MLP11a_emp, newdata = uk_woe_test1, type='prob')
ukemp_MLP11a <- empCreditScoring(ukpredb_MLP11a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_MLP11b_emp <- train(formula, data=uk_woe_test1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP11b_emp <- predict(uk_model_MLP11b_emp, newdata = uk_woe_train1, type='prob')
ukemp_MLP11b <- empCreditScoring(ukpredb_MLP11b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_MLP12a_emp <- train(formula, data=uk_woe_train2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP12a_emp <- predict(uk_model_MLP12a_emp, newdata = uk_woe_test2, type='prob')
ukemp_MLP12a <- empCreditScoring(ukpredb_MLP12a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_MLP12b_emp <- train(formula, data=uk_woe_test2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP12b_emp <- predict(uk_model_MLP12b_emp, newdata = uk_woe_train2, type='prob')
ukemp_MLP12b <- empCreditScoring(ukpredb_MLP12b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_MLP13a_emp <- train(formula, data=uk_woe_train3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP13a_emp <- predict(uk_model_MLP13a_emp, newdata = uk_woe_test3, type='prob')
ukemp_MLP13a <- empCreditScoring(ukpredb_MLP13a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_MLP13b_emp <- train(formula, data=uk_woe_test3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP13b_emp <- predict(uk_model_MLP13b_emp, newdata = uk_woe_train3, type='prob')
ukemp_MLP13b <- empCreditScoring(ukpredb_MLP13b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_MLP14a_emp <- train(formula, data=uk_woe_train4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP14a_emp <- predict(uk_model_MLP14a_emp, newdata = uk_woe_test4, type='prob')
ukemp_MLP14a <- empCreditScoring(ukpredb_MLP14a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_MLP14b_emp <- train(formula, data=uk_woe_test4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP14b_emp <- predict(uk_model_MLP14b_emp, newdata = uk_woe_train4, type='prob')
ukemp_MLP14b <- empCreditScoring(ukpredb_MLP14b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_MLP15a_emp <- train(formula, data=uk_woe_train5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP15a_emp <- predict(uk_model_MLP15a_emp, newdata = uk_woe_test5, type='prob')
ukemp_MLP15a <- empCreditScoring(ukpredb_MLP15a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_MLP15b_emp <- train(formula, data=uk_woe_test5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP15b_emp <- predict(uk_model_MLP15b_emp, newdata = uk_woe_train5, type='prob')
ukemp_MLP15b <- empCreditScoring(ukpredb_MLP15b_emp$X1,uk_woe_train5$Censor)

#uk - MLP3
MLP3grid <- expand.grid(.size1=c(20), .size2=c(20), .size3=c(15), .dropout=c(0.25), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP31a_emp <- train(formula, data=uk_woe_train1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP31a_emp <- predict(uk_model_MLP31a_emp, newdata = uk_woe_test1, type='prob')
ukemp_MLP31a <- empCreditScoring(ukpredb_MLP31a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_MLP31b_emp <- train(formula, data=uk_woe_test1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP31b_emp <- predict(uk_model_MLP31b_emp, newdata = uk_woe_train1, type='prob')
ukemp_MLP31b <- empCreditScoring(ukpredb_MLP31b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_MLP32a_emp <- train(formula, data=uk_woe_train2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP32a_emp <- predict(uk_model_MLP32a_emp, newdata = uk_woe_test2, type='prob')
ukemp_MLP32a <- empCreditScoring(ukpredb_MLP32a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_MLP32b_emp <- train(formula, data=uk_woe_test2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP32b_emp <- predict(uk_model_MLP32b_emp, newdata = uk_woe_train2, type='prob')
ukemp_MLP32b <- empCreditScoring(ukpredb_MLP32b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_MLP33a_emp <- train(formula, data=uk_woe_train3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP33a_emp <- predict(uk_model_MLP33a_emp, newdata = uk_woe_test3, type='prob')
ukemp_MLP33a <- empCreditScoring(ukpredb_MLP33a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_MLP33b_emp <- train(formula, data=uk_woe_test3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP33b_emp <- predict(uk_model_MLP33b_emp, newdata = uk_woe_train3, type='prob')
ukemp_MLP33b <- empCreditScoring(ukpredb_MLP33b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_MLP34a_emp <- train(formula, data=uk_woe_train4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP34a_emp <- predict(uk_model_MLP34a_emp, newdata = uk_woe_test4, type='prob')
ukemp_MLP34a <- empCreditScoring(ukpredb_MLP34a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_MLP34b_emp <- train(formula, data=uk_woe_test4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP34b_emp <- predict(uk_model_MLP34b_emp, newdata = uk_woe_train4, type='prob')
ukemp_MLP34b <- empCreditScoring(ukpredb_MLP34b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_MLP35a_emp <- train(formula, data=uk_woe_train5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP35a_emp <- predict(uk_model_MLP35a_emp, newdata = uk_woe_test5, type='prob')
ukemp_MLP35a <- empCreditScoring(ukpredb_MLP35a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_MLP35b_emp <- train(formula, data=uk_woe_test5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP35b_emp <- predict(uk_model_MLP35b_emp, newdata = uk_woe_train5, type='prob')
ukemp_MLP35b <- empCreditScoring(ukpredb_MLP35b_emp$X1,uk_woe_train5$Censor)

#uk - MLP5
MLP5grid <- expand.grid(.size1=c(15), .size2=c(15), .size3=c(10), .size4=c(10), .size5=c(5), .dropout=c(0.0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); uk_model_MLP51a_emp <- train(formula, data=uk_woe_train1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP51a_emp <- predict(uk_model_MLP51a_emp, newdata = uk_woe_test1, type='prob')
ukemp_MLP51a <- empCreditScoring(ukpredb_MLP51a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_MLP51b_emp <- train(formula, data=uk_woe_test1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP51b_emp <- predict(uk_model_MLP51b_emp, newdata = uk_woe_train1, type='prob')
ukemp_MLP51b <- empCreditScoring(ukpredb_MLP51b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_MLP52a_emp <- train(formula, data=uk_woe_train2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP52a_emp <- predict(uk_model_MLP52a_emp, newdata = uk_woe_test2, type='prob')
ukemp_MLP52a <- empCreditScoring(ukpredb_MLP52a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_MLP52b_emp <- train(formula, data=uk_woe_test2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP52b_emp <- predict(uk_model_MLP52b_emp, newdata = uk_woe_train2, type='prob')
ukemp_MLP52b <- empCreditScoring(ukpredb_MLP52b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_MLP53a_emp <- train(formula, data=uk_woe_train3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP53a_emp <- predict(uk_model_MLP53a_emp, newdata = uk_woe_test3, type='prob')
ukemp_MLP53a <- empCreditScoring(ukpredb_MLP53a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_MLP53b_emp <- train(formula, data=uk_woe_test3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP53b_emp <- predict(uk_model_MLP53b_emp, newdata = uk_woe_train3, type='prob')
ukemp_MLP53b <- empCreditScoring(ukpredb_MLP53b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_MLP54a_emp <- train(formula, data=uk_woe_train4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP54a_emp <- predict(uk_model_MLP54a_emp, newdata = uk_woe_test4, type='prob')
ukemp_MLP54a <- empCreditScoring(ukpredb_MLP54a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_MLP54b_emp <- train(formula, data=uk_woe_test4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP54b_emp <- predict(uk_model_MLP54b_emp, newdata = uk_woe_train4, type='prob')
ukemp_MLP54b <- empCreditScoring(ukpredb_MLP54b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_MLP55a_emp <- train(formula, data=uk_woe_train5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP55a_emp <- predict(uk_model_MLP55a_emp, newdata = uk_woe_test5, type='prob')
ukemp_MLP55a <- empCreditScoring(ukpredb_MLP55a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_MLP55b_emp <- train(formula, data=uk_woe_test5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_MLP55b_emp <- predict(uk_model_MLP55b_emp, newdata = uk_woe_train5, type='prob')
ukemp_MLP55b <- empCreditScoring(ukpredb_MLP55b_emp$X1,uk_woe_train5$Censor)

#uk - DBN1  
DBN1grid <- expand.grid(.layer1=c(5), .hidden_dropout=c(0), .visible_dropout=c(0.25), .lr=c(1.0))
set.seed(123); uk_model_DBN11a_emp <- train(formula, data=uk_woe_train1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN11a_emp <- predict(uk_model_DBN11a_emp, newdata = uk_woe_test1, type='prob')
ukemp_DBN11a <- empCreditScoring(ukpredb_DBN11a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_DBN11b_emp <- train(formula, data=uk_woe_test1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN11b_emp <- predict(uk_model_DBN11b_emp, newdata = uk_woe_train1, type='prob')
ukemp_DBN11b <- empCreditScoring(ukpredb_DBN11b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_DBN12a_emp <- train(formula, data=uk_woe_train2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN12a_emp <- predict(uk_model_DBN12a_emp, newdata = uk_woe_test2, type='prob')
ukemp_DBN12a <- empCreditScoring(ukpredb_DBN12a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_DBN12b_emp <- train(formula, data=uk_woe_test2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN12b_emp <- predict(uk_model_DBN12b_emp, newdata = uk_woe_train2, type='prob')
ukemp_DBN12b <- empCreditScoring(ukpredb_DBN12b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_DBN13a_emp <- train(formula, data=uk_woe_train3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN13a_emp <- predict(uk_model_DBN13a_emp, newdata = uk_woe_test3, type='prob')
ukemp_DBN13a <- empCreditScoring(ukpredb_DBN13a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_DBN13b_emp <- train(formula, data=uk_woe_test3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN13b_emp <- predict(uk_model_DBN13b_emp, newdata = uk_woe_train3, type='prob')
ukemp_DBN13b <- empCreditScoring(ukpredb_DBN13b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_DBN14a_emp <- train(formula, data=uk_woe_train4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN14a_emp <- predict(uk_model_DBN14a_emp, newdata = uk_woe_test4, type='prob')
ukemp_DBN14a <- empCreditScoring(ukpredb_DBN14a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_DBN14b_emp <- train(formula, data=uk_woe_test4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN14b_emp <- predict(uk_model_DBN14b_emp, newdata = uk_woe_train4, type='prob')
ukemp_DBN14b <- empCreditScoring(ukpredb_DBN14b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_DBN15a_emp <- train(formula, data=uk_woe_train5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN15a_emp <- predict(uk_model_DBN15a_emp, newdata = uk_woe_test5, type='prob')
ukemp_DBN15a <- empCreditScoring(ukpredb_DBN15a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_DBN15b_emp <- train(formula, data=uk_woe_test5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN15b_emp <- predict(uk_model_DBN15b_emp, newdata = uk_woe_train5, type='prob')
ukemp_DBN15b <- empCreditScoring(ukpredb_DBN15b_emp$X1,uk_woe_train5$Censor)

#uk - DBN3 
DBN3grid <- expand.grid(.layer1=c(10), .layer2=c(5), .layer3=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0), .lr=c(2))
set.seed(123); uk_model_DBN31a_emp <- train(formula, data=uk_woe_train1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN31a_emp <- predict(uk_model_DBN31a_emp, newdata = uk_woe_test1, type='prob')
ukemp_DBN31a <- empCreditScoring(ukpredb_DBN31a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_DBN31b_emp <- train(formula, data=uk_woe_test1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN31b_emp <- predict(uk_model_DBN31b_emp, newdata = uk_woe_train1, type='prob')
ukemp_DBN31b <- empCreditScoring(ukpredb_DBN31b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_DBN32a_emp <- train(formula, data=uk_woe_train2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN32a_emp <- predict(uk_model_DBN32a_emp, newdata = uk_woe_test2, type='prob')
ukemp_DBN32a <- empCreditScoring(ukpredb_DBN32a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_DBN32b_emp <- train(formula, data=uk_woe_test2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN32b_emp <- predict(uk_model_DBN32b_emp, newdata = uk_woe_train2, type='prob')
ukemp_DBN32b <- empCreditScoring(ukpredb_DBN32b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_DBN33a_emp <- train(formula, data=uk_woe_train3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN33a_emp <- predict(uk_model_DBN33a_emp, newdata = uk_woe_test3, type='prob')
ukemp_DBN33a <- empCreditScoring(ukpredb_DBN33a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_DBN33b_emp <- train(formula, data=uk_woe_test3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN33b_emp <- predict(uk_model_DBN33b_emp, newdata = uk_woe_train3, type='prob')
ukemp_DBN33b <- empCreditScoring(ukpredb_DBN33b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_DBN34a_emp <- train(formula, data=uk_woe_train4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN34a_emp <- predict(uk_model_DBN34a_emp, newdata = uk_woe_test4, type='prob')
ukemp_DBN34a <- empCreditScoring(ukpredb_DBN34a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_DBN34b_emp <- train(formula, data=uk_woe_test4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN34b_emp <- predict(uk_model_DBN34b_emp, newdata = uk_woe_train4, type='prob')
ukemp_DBN34b <- empCreditScoring(ukpredb_DBN34b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_DBN35a_emp <- train(formula, data=uk_woe_train5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN35a_emp <- predict(uk_model_DBN35a_emp, newdata = uk_woe_test5, type='prob')
ukemp_DBN35a <- empCreditScoring(ukpredb_DBN35a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_DBN35b_emp <- train(formula, data=uk_woe_test5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN35b_emp <- predict(uk_model_DBN35b_emp, newdata = uk_woe_train5, type='prob')
ukemp_DBN35b <- empCreditScoring(ukpredb_DBN35b_emp$X1,uk_woe_train5$Censor)

#uk - DBN5   
DBN5grid <- expand.grid(.layer1=c(10),.layer2=c(10),.layer3=c(10),.layer4=c(10),.layer5=c(10),.hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(1.5))
set.seed(123); uk_model_DBN51a_emp <- train(formula, data=uk_woe_train1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN51a_emp <- predict(uk_model_DBN51a_emp, newdata = uk_woe_test1, type='prob')
ukemp_DBN51a <- empCreditScoring(ukpredb_DBN51a_emp$X1,uk_woe_test1$Censor)
set.seed(123); uk_model_DBN51b_emp <- train(formula, data=uk_woe_test1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN51b_emp <- predict(uk_model_DBN51b_emp, newdata = uk_woe_train1, type='prob')
ukemp_DBN51b <- empCreditScoring(ukpredb_DBN51b_emp$X1,uk_woe_train1$Censor)
set.seed(123); uk_model_DBN52a_emp <- train(formula, data=uk_woe_train2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN52a_emp <- predict(uk_model_DBN52a_emp, newdata = uk_woe_test2, type='prob')
ukemp_DBN52a <- empCreditScoring(ukpredb_DBN52a_emp$X1,uk_woe_test2$Censor)
set.seed(123); uk_model_DBN52b_emp <- train(formula, data=uk_woe_test2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN52b_emp <- predict(uk_model_DBN52b_emp, newdata = uk_woe_train2, type='prob')
ukemp_DBN52b <- empCreditScoring(ukpredb_DBN52b_emp$X1,uk_woe_train2$Censor)
set.seed(123); uk_model_DBN53a_emp <- train(formula, data=uk_woe_train3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN53a_emp <- predict(uk_model_DBN53a_emp, newdata = uk_woe_test3, type='prob')
ukemp_DBN53a <- empCreditScoring(ukpredb_DBN53a_emp$X1,uk_woe_test3$Censor)
set.seed(123); uk_model_DBN53b_emp <- train(formula, data=uk_woe_test3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN53b_emp <- predict(uk_model_DBN53b_emp, newdata = uk_woe_train3, type='prob')
ukemp_DBN53b <- empCreditScoring(ukpredb_DBN53b_emp$X1,uk_woe_train3$Censor)
set.seed(123); uk_model_DBN54a_emp <- train(formula, data=uk_woe_train4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN54a_emp <- predict(uk_model_DBN54a_emp, newdata = uk_woe_test4, type='prob')
ukemp_DBN54a <- empCreditScoring(ukpredb_DBN54a_emp$X1,uk_woe_test4$Censor)
set.seed(123); uk_model_DBN54b_emp <- train(formula, data=uk_woe_test4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN54b_emp <- predict(uk_model_DBN54b_emp, newdata = uk_woe_train4, type='prob')
ukemp_DBN54b <- empCreditScoring(ukpredb_DBN54b_emp$X1,uk_woe_train4$Censor)
set.seed(123); uk_model_DBN55a_emp <- train(formula, data=uk_woe_train5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN55a_emp <- predict(uk_model_DBN55a_emp, newdata = uk_woe_test5, type='prob')
ukemp_DBN55a <- empCreditScoring(ukpredb_DBN55a_emp$X1,uk_woe_test5$Censor)
set.seed(123); uk_model_DBN55b_emp <- train(formula, data=uk_woe_test5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
ukpredb_DBN55b_emp <- predict(uk_model_DBN55b_emp, newdata = uk_woe_train5, type='prob')
ukemp_DBN55b <- empCreditScoring(ukpredb_DBN55b_emp$X1,uk_woe_train5$Censor)

ukemp_log <- cbind(ukemp_log1a$EMPC,ukemp_log1b$EMPC,ukemp_log2a$EMPC,ukemp_log2b$EMPC,ukemp_log3a$EMPC,ukemp_log3b$EMPC,
                   ukemp_log4a$EMPC,ukemp_log4b$EMPC,ukemp_log5a$EMPC,ukemp_log5b$EMPC)
ukemp_dt <- cbind(ukemp_dt1a$EMPC,ukemp_dt1b$EMPC,ukemp_dt2a$EMPC,ukemp_dt2b$EMPC,ukemp_dt3a$EMPC,ukemp_dt3b$EMPC,
                  ukemp_dt4a$EMPC,ukemp_dt4b$EMPC,ukemp_dt5a$EMPC,ukemp_dt5b$EMPC)
ukemp_rf <- cbind(ukemp_RF1a$EMPC,ukemp_RF1b$EMPC,ukemp_RF2a$EMPC,ukemp_RF2b$EMPC,ukemp_RF3a$EMPC,ukemp_RF3b$EMPC,
                  ukemp_RF4a$EMPC,ukemp_RF4b$EMPC,ukemp_RF5a$EMPC,ukemp_RF5b$EMPC)
ukemp_MLP1 <- cbind(ukemp_MLP11a$EMPC,ukemp_MLP11b$EMPC,ukemp_MLP12a$EMPC,ukemp_MLP12b$EMPC,ukemp_MLP13a$EMPC,ukemp_MLP13b$EMPC,
                    ukemp_MLP14a$EMPC,ukemp_MLP14b$EMPC,ukemp_MLP15a$EMPC,ukemp_MLP15b$EMPC)
ukemp_MLP3 <- cbind(ukemp_MLP31a$EMPC,ukemp_MLP31b$EMPC,ukemp_MLP32a$EMPC,ukemp_MLP32b$EMPC,ukemp_MLP33a$EMPC,ukemp_MLP33b$EMPC,
                    ukemp_MLP34a$EMPC,ukemp_MLP34b$EMPC,ukemp_MLP35a$EMPC,ukemp_MLP35b$EMPC)
ukemp_MLP5 <- cbind(ukemp_MLP51a$EMPC,ukemp_MLP51b$EMPC,ukemp_MLP52a$EMPC,ukemp_MLP52b$EMPC,ukemp_MLP53a$EMPC,ukemp_MLP53b$EMPC,
                    ukemp_MLP54a$EMPC,ukemp_MLP54b$EMPC,ukemp_MLP55a$EMPC,ukemp_MLP55b$EMPC)
ukemp_DBN1 <- cbind(ukemp_DBN11a$EMPC,ukemp_DBN11b$EMPC,ukemp_DBN12a$EMPC,ukemp_DBN12b$EMPC,ukemp_DBN13a$EMPC,ukemp_DBN13b$EMPC,
                    ukemp_DBN14a$EMPC,ukemp_DBN14b$EMPC,ukemp_DBN15a$EMPC,ukemp_DBN15b$EMPC)
ukemp_DBN3 <- cbind(ukemp_DBN31a$EMPC,ukemp_DBN31b$EMPC,ukemp_DBN32a$EMPC,ukemp_DBN32b$EMPC,ukemp_DBN33a$EMPC,ukemp_DBN33b$EMPC,
                    ukemp_DBN34a$EMPC,ukemp_DBN34b$EMPC,ukemp_DBN35a$EMPC,ukemp_DBN35b$EMPC)
ukemp_DBN5 <- cbind(ukemp_DBN51a$EMPC,ukemp_DBN51b$EMPC,ukemp_DBN52a$EMPC,ukemp_DBN52b$EMPC,ukemp_DBN53a$EMPC,ukemp_DBN53b$EMPC,
                    ukemp_DBN54a$EMPC,ukemp_DBN54b$EMPC,ukemp_DBN55a$EMPC,ukemp_DBN55b$EMPC)
mean(ukemp_log)
mean(ukemp_dt)
mean(ukemp_rf)
mean(ukemp_MLP1)
mean(ukemp_MLP3)
mean(ukemp_MLP5)
mean(ukemp_DBN1)
mean(ukemp_DBN3)
mean(ukemp_DBN5)