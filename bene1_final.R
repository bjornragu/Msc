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
bene1 <- read.delim("D:/Users/r0648841/Desktop/thesis/Data/input_bene1.txt", header=FALSE)
colnames(bene1) <- c("NUMLENI", "BLENIA1", "BFACTA1", "PLASTA1", "PDUURA2", "CDOELA2", "CGEBRA1", "BMENSA1",
                     "BSPARA11", "BUITGA21", "BINKOA11", "DBEGIA21", "DVERBA11", "DGEBOA11", "CPRIVA11", "CBURGA11", 
                     "DVERBA21", "CTROSA11", "CEIGEA11", "DCLIEA11", "DLLENA11", "ACONTCV1", "ACONTTM1", "ACONTHY1",
                     "APERSA11", "CECOTA11", "CVOLTA11", "CAANSA11", "TARGET")
bene1 <- subset(bene1, select = -c(NUMLENI))
##mis val
any(is.na(bene1)) 
vif = function(df){diag(solve(cor(df)))}
vif(bene1)
bene1[,"TARGET"] <- ifelse(bene1[,"TARGET"] == 1, 0, 1)

########################
########-WOE-########### 
########################
b1_woe <- Information::create_infotables(data=bene1, y ="TARGET", parallel = TRUE)
bene1_woedata <- bene1
#CDOELA2 (7)
b1_woe$Tables$CDOELA2
table(bene1$CDOELA2)
bene1_woedata[,"CDOELA2"] <- ifelse(bene1[,"CDOELA2"] == 1, as.matrix(b1_woe$Tables$CDOELA2$WOE)[1,],  
                                    ifelse(bene1[,"CDOELA2"] == 2, as.matrix(b1_woe$Tables$CDOELA2$WOE)[1,],
                                           ifelse(bene1[,"CDOELA2"] == 3, as.matrix(b1_woe$Tables$CDOELA2$WOE)[2,],
                                                  ifelse(bene1[,"CDOELA2"] == 4, as.matrix(b1_woe$Tables$CDOELA2$WOE)[2,],
                                                         ifelse(bene1[,"CDOELA2"] == 5, as.matrix(b1_woe$Tables$CDOELA2$WOE)[3,],
                                                                as.matrix(b1_woe$Tables$CDOELA2$WOE)[4,])))))
table(bene1$CDOELA2, bene1_woedata$CDOELA2)

#CGEBRA1 (2)
b1_woe$Tables$CGEBRA1
table(bene1$CGEBRA1)
bene1_woedata[,"CGEBRA1"] <- ifelse(bene1[,"CGEBRA1"] == 1, as.matrix(b1_woe$Tables$CGEBRA1$WOE)[1,], as.matrix(b1_woe$Tables$CGEBRA1$WOE)[2,])
table(bene1$CGEBRA1, bene1_woedata$CGEBRA1)

#newbuts(2)
b1_woe$Tables$CPRIVA11
table(bene1$CPRIVA11)
bene1_woedata[,"CPRIVA11"] <- ifelse(bene1[,"CPRIVA11"] == 1, as.matrix(b1_woe$Tables$CPRIVA11$WOE)[1,], as.matrix(b1_woe$Tables$CPRIVA11$WOE)[2,])
table(bene1$CPRIVA11, bene1_woedata$CPRIVA11)

#CBURGA11(29)
b1_woe$Tables$CBURGA11
table(bene1$CBURGA11)
bene1_woedata[,"CBURGA11"] <- ifelse(bene1[,"CBURGA11"] == 1, as.matrix(b1_woe$Tables$CBURGA11$WOE)[1,],  
                                     ifelse(bene1[,"CBURGA11"] == 2, as.matrix(b1_woe$Tables$CBURGA11$WOE)[2,],
                                            ifelse(bene1[,"CBURGA11"] == 3, as.matrix(b1_woe$Tables$CBURGA11$WOE)[2,],
                                                   as.matrix(b1_woe$Tables$CBURGA11$WOE)[3,])))
table(bene1$CBURGA11, bene1_woedata$CBURGA11)

#CTROSA11 (2)
b1_woe$Tables$CTROSA11
table(bene1$CTROSA11)
bene1_woedata[,"CTROSA11"] <- ifelse(bene1[,"CTROSA11"] == 1, as.matrix(b1_woe$Tables$CTROSA11$WOE)[1,], as.matrix(b1_woe$Tables$CTROSA11$WOE)[2,])
table(bene1$CTROSA11, bene1_woedata$CTROSA11)

#CEIGEA11(4)
b1_woe$Tables$CEIGEA11
table(bene1$CEIGEA11)
bene1_woedata[,"CEIGEA11"] <- ifelse(bene1[,"CEIGEA11"] == 3, as.matrix(b1_woe$Tables$CEIGEA11$WOE)[1,],  
                                     ifelse(bene1[,"CEIGEA11"] == 4, as.matrix(b1_woe$Tables$CEIGEA11$WOE)[1,],
                                            ifelse(bene1[,"CEIGEA11"] == 8, as.matrix(b1_woe$Tables$CEIGEA11$WOE)[1,],
                                                   as.matrix(b1_woe$Tables$CEIGEA11$WOE)[2,])))
table(bene1$CEIGEA11, bene1_woedata$CEIGEA11)

#CECOTA11(2)
b1_woe$Tables$CECOTA11
table(bene1$CECOTA11)
bene1_woedata[,"CECOTA11"] <- ifelse(bene1[,"CECOTA11"] == 10, as.matrix(b1_woe$Tables$CECOTA11$WOE)[1,],  
                                     ifelse(bene1[,"CECOTA11"] == 30, as.matrix(b1_woe$Tables$CECOTA11$WOE)[2,],
                                            ifelse(bene1[,"CECOTA11"] == 40, as.matrix(b1_woe$Tables$CECOTA11$WOE)[2,],
                                                   ifelse(bene1[,"CECOTA11"] == 50, as.matrix(b1_woe$Tables$CECOTA11$WOE)[3,],
                                                          ifelse(bene1[,"CECOTA11"] == 60, as.matrix(b1_woe$Tables$CECOTA11$WOE)[4,],
                                                                 ifelse(bene1[,"CECOTA11"] == 70, as.matrix(b1_woe$Tables$CECOTA11$WOE)[4,],
                                                                        as.matrix(b1_woe$Tables$CECOTA11$WOE)[5,]))))))
table(bene1$CECOTA11, bene1_woedata$CECOTA11)

#Prop (5)
b1_woe$Tables$CVOLTA11
table(bene1$CVOLTA11)
bene1_woedata[,"CVOLTA11"] <- ifelse(bene1[,"CVOLTA11"] == 1, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[1,],  
                                     ifelse(bene1[,"CVOLTA11"] == 2, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[2,],
                                            ifelse(bene1[,"CVOLTA11"] == 3, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[2,],
                                                   ifelse(bene1[,"CVOLTA11"] == 4, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[2,],
                                                          ifelse(bene1[,"CVOLTA11"] == 5, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[2,],
                                                                 ifelse(bene1[,"CVOLTA11"] == 6, as.matrix(b1_woe$Tables$CVOLTA11$WOE)[2,],
                                                                        as.matrix(b1_woe$Tables$CVOLTA11$WOE)[3,]))))))
table(bene1$CVOLTA11, bene1_woedata$CVOLTA11)

##nom var: CAANSA11 (2)
b1_woe$Tables$CAANSA11
table(bene1$CAANSA11)
bene1_woedata[,"CAANSA11"] <- ifelse(bene1[,"CAANSA11"] == 1, as.matrix(b1_woe$Tables$CAANSA11$WOE)[1,],
                                     as.matrix(b1_woe$Tables$CAANSA11$WOE)[2,])
table(bene1$CAANSA11, bene1_woedata$CAANSA11)

####creating factors#####
#nom var: CDOELA2, CGEBRA1, CPRIVA11, CBURGA11, CTROSA11, CEIGEA11, CECOTA11, CVOLTA11, CAANSA11
bene1_woedata$TARGET <- as.factor(bene1_woedata$TARGET)
bene1$TARGET <- as.factor(bene1$TARGET)
bene1$CDOELA2 <- as.factor(bene1$CDOELA2)
bene1$CGEBRA1 <- as.factor(bene1$CGEBRA1)
bene1$CPRIVA11 <- as.factor(bene1$CPRIVA11)
bene1$CBURGA11 <- as.factor(bene1$CBURGA11)
bene1$CTROSA11 <- as.factor(bene1$CTROSA11)
bene1$CEIGEA11 <- as.factor(bene1$CEIGEA11)
bene1$CECOTA11 <- as.factor(bene1$CECOTA11)
bene1$CVOLTA11 <- as.factor(bene1$CVOLTA11)
bene1$CAANSA11 <- as.factor(bene1$CAANSA11)

levels(bene1_woedata$TARGET) <- make.names(levels(factor(bene1_woedata$TARGET)))
levels(bene1$TARGET) <- make.names(levels(factor(bene1$TARGET)))
levels(bene1$CDOELA2) <- make.names(levels(factor(bene1$CDOELA2)))
levels(bene1$CGEBRA1) <- make.names(levels(factor(bene1$CGEBRA1)))
levels(bene1$CPRIVA11) <- make.names(levels(factor(bene1$CPRIVA11)))
levels(bene1$CBURGA11) <- make.names(levels(factor(bene1$CBURGA11)))
levels(bene1$CTROSA11) <- make.names(levels(factor(bene1$CTROSA11)))
levels(bene1$CEIGEA11) <- make.names(levels(factor(bene1$CEIGEA11)))
levels(bene1$CECOTA11) <- make.names(levels(factor(bene1$CECOTA11)))
levels(bene1$CVOLTA11) <- make.names(levels(factor(bene1$CVOLTA11)))
levels(bene1$CAANSA11) <- make.names(levels(factor(bene1$CAANSA11)))

summary(bene1)
summary(bene1_woedata)

#################################################################
##DATA PARTITIONING.Use Nx2-foldcross-validation(Dietterich,1998)
##################################################################
##bene_data
k=10
a <- createDataPartition(bene1$TARGET, times = k, p = 0.5, list = FALSE) #https://dataanalyticsblog.com/2016/03/27/splitting-data-into-train-and-test-using-caret-package-in-r/
bene1_train1 <- bene1[a[,1],]
bene1_test1 <- bene1[-a[,1],]
bene1_train2 <- bene1[a[,2],]
bene1_test2 <- bene1[-a[,2],]
bene1_train3 <- bene1[a[,3],]
bene1_test3 <- bene1[-a[,3],]
bene1_train4 <- bene1[a[,4],]
bene1_test4 <- bene1[-a[,4],]
bene1_train5 <- bene1[a[,5],]
bene1_test5 <- bene1[-a[,5],]
bene1_train6 <- bene1[a[,6],]
bene1_test6 <- bene1[-a[,6],]
bene1_train7 <- bene1[a[,7],]
bene1_test7 <- bene1[-a[,7],]
bene1_train8 <- bene1[a[,8],]
bene1_test8 <- bene1[-a[,8],]
bene1_train9 <- bene1[a[,9],]
bene1_test9 <- bene1[-a[,9],]
bene1_train10 <- bene1[a[,10],]
bene1_test10 <- bene1[-a[,10],]

b <- createDataPartition(bene1_woedata$TARGET, times = k, p = 0.5, list = FALSE) 
bene1_woe_train1 <- bene1_woedata[b[,1],]
bene1_woe_test1 <- bene1_woedata[-b[,1],]
bene1_woe_train2 <- bene1_woedata[b[,2],]
bene1_woe_test2 <- bene1_woedata[-b[,2],]
bene1_woe_train3 <- bene1_woedata[b[,3],]
bene1_woe_test3 <- bene1_woedata[-b[,3],]
bene1_woe_train4 <- bene1_woedata[b[,4],]
bene1_woe_test4 <- bene1_woedata[-b[,4],]
bene1_woe_train5 <- bene1_woedata[b[,5],]
bene1_woe_test5 <- bene1_woedata[-b[,5],]
bene1_woe_train6 <- bene1_woedata[b[,6],]
bene1_woe_test6 <- bene1_woedata[-b[,6],]
bene1_woe_train7 <- bene1_woedata[b[,7],]
bene1_woe_test7 <- bene1_woedata[-b[,7],]
bene1_woe_train8 <- bene1_woedata[b[,8],]
bene1_woe_test8 <- bene1_woedata[-b[,8],]
bene1_woe_train9 <- bene1_woedata[b[,9],]
bene1_woe_test9 <- bene1_woedata[-b[,9],]
bene1_woe_train10 <- bene1_woedata[b[,10],]
bene1_woe_test10 <- bene1_woedata[-b[,10],]

formula <- TARGET~BLENIA1+BFACTA1+PLASTA1+PDUURA2+CDOELA2+CGEBRA1+BMENSA1+BSPARA11+BUITGA21+BINKOA11+DBEGIA21+DVERBA11+DGEBOA11+CPRIVA11+CBURGA11+DVERBA21+CTROSA11+CEIGEA11+DCLIEA11+DLLENA11+ACONTCV1+ACONTTM1+ACONTHY1+APERSA11+CECOTA11+CVOLTA11+CAANSA11  

####################################################################################
##custom summary functions, reference recomends AUC, the PG, and the BS#############
####################################################################################
hmeasure<-function (data, lev = NULL, model = NULL,...) 
{ 
  # adaptation of twoClassSummary
  require(hmeasure)
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  hObject <- try(hmeasure::HMeasure(data$obs, data[, lev[1]]),silent=TRUE)
  hmeasH <- if (class(hObject)[1] == "try-error") {
    NA
  } else {hObject$metrics[[1]] 
  }
  out<-hmeasH 
  names(out) <- c("hmeasH")
  out 
}

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
set.seed(123); b1_model_log1a_roc <- train(TARGET~., data=bene1_woe_train1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log1a_roc <- predict(b1_model_log1a_roc,bene1_woe_test1,type="prob")
b1_log1a.ROC <- roc(predictor=b1predb_log1a_roc$X0,
                    response=bene1_woe_test1$TARGET,
                    levels=rev(levels(bene1_woe_test1$TARGET)))
b1_log1a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_log1a_gini <- train(formula, data=bene1_woe_train1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log1a_gini$results
b1_model_log1a_gini$resample
b1_pred_log1a_gini<- predict(b1_model_log1a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_log1a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_log1a_gini$X1)
#b <= 0.4
p <- b1_pred_log1a_gini$X1[b1_pred_log1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_log1a_gini$X1<=0.4]))
b1_log1a.ngini <- normalizedGini(a, p)
b1_log1a.ngini
b1_log1a.gini <-Gini(a, p)
b1_log1a.gini

#Brier score
set.seed(123); b1_model_log1a_brier <- train(formula, data=bene1_woe_train1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log1a_brier$results
b1_model_log1a_brier$resample
b1_pred_log1a_brier <- predict(b1_model_log1a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_log1a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_log1a_brier$X1)
b1_log1a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_log1b_roc <- train(formula, data=bene1_woe_test1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log1b_roc <- predict(b1_model_log1b_roc, bene1_woe_train1,type="prob")
b1_log1b.ROC <- roc(predictor=b1predb_log1b_roc$X0,
                    response=bene1_woe_train1$TARGET,
                    levels=rev(levels(bene1_woe_train1$TARGET)))
b1_log1b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_log1b_gini <- train(formula, data=bene1_woe_test1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log1b_gini$results
b1_model_log1b_gini$resample
b1_pred_log1b_gini<- predict(b1_model_log1b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_log1b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_log1b_gini$X1)
#b <= 0.4
p <- b1_pred_log1b_gini$X1[b1_pred_log1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_log1b_gini$X1<=0.4]))
b1_log1b.ngini <- normalizedGini(a, p)
b1_log1b.ngini
b1_log1b.gini <-Gini(a, p)
b1_log1b.gini

#Brier score
set.seed(123); b1_model_log1b_brier <- train(formula, data=bene1_woe_test1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log1b_brier$results
b1_model_log1b_brier$resample
b1_pred_log1b_brier <- predict(b1_model_log1b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_log1b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_log1b_brier$X1)
b1_log1b.bs

###data 2, train-test
#ROC curve 
set.seed(123); b1_model_log2a_roc <- train(formula, data=bene1_woe_train2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log2a_roc <- predict(b1_model_log2a_roc,bene1_woe_test2,type="prob")
b1_log2a.ROC <- roc(predictor=b1predb_log2a_roc$X0,
                    response=bene1_woe_test2$TARGET,
                    levels=rev(levels(bene1_woe_test2$TARGET)))
b1_log2a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_log2a_gini <- train(formula, data=bene1_woe_train2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log2a_gini$results
b1_model_log2a_gini$resample
b1_pred_log2a_gini<- predict(b1_model_log2a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_log2a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_log2a_gini$X1)
#b <= 0.4
p <- b1_pred_log2a_gini$X1[b1_pred_log2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_log2a_gini$X1<=0.4]))
b1_log2a.ngini <- normalizedGini(a, p)
b1_log2a.ngini
b1_log2a.gini <-Gini(a, p)
b1_log2a.gini

#Brier score
set.seed(123); b1_model_log2a_brier <- train(formula, data=bene1_woe_train2, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log2a_brier$results
b1_model_log2a_brier$resample
b1_pred_log2a_brier <- predict(b1_model_log2a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_log2a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_log2a_brier$X1)
b1_log2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_log2b_roc <- train(formula, data=bene1_woe_test2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log2b_roc <- predict(b1_model_log2b_roc, bene1_woe_train2,type="prob")
b1_log2b.ROC <- roc(predictor=b1predb_log2b_roc$X0,
                    response=bene1_woe_train2$TARGET,
                    levels=rev(levels(bene1_woe_train2$TARGET)))
b1_log2b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_log2b_gini <- train(formula, data=bene1_woe_test2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log2b_gini$results
b1_model_log2b_gini$resample
b1_pred_log2b_gini<- predict(b1_model_log2b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_log2b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_log2b_gini$X1)
#b <= 0.4
p <- b1_pred_log2b_gini$X1[b1_pred_log2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_log2b_gini$X1<=0.4]))
b1_log2b.ngini <- normalizedGini(a, p)
b1_log2b.ngini
b1_log2b.gini <-Gini(a, p)
b1_log2b.gini

#Brier score
set.seed(123); b1_model_log2b_brier <- train(formula, data=bene1_woe_test2, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log2b_brier$results
b1_model_log2b_brier$resample
b1_pred_log2b_brier <- predict(b1_model_log2b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_log2b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_log2b_brier$X1)
b1_log2b.bs

###data 3, train-test
#ROC curve 
set.seed(123); b1_model_log3a_roc <- train(formula, data=bene1_woe_train3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log3a_roc <- predict(b1_model_log3a_roc,bene1_woe_test3,type="prob")
b1_log3a.ROC <- roc(predictor=b1predb_log3a_roc$X0,
                    response=bene1_woe_test3$TARGET,
                    levels=rev(levels(bene1_woe_test3$TARGET)))
b1_log3a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_log3a_gini <- train(formula, data=bene1_woe_train3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log3a_gini$results
b1_model_log3a_gini$resample
b1_pred_log3a_gini<- predict(b1_model_log3a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_log3a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_log3a_gini$X1)
#b <= 0.4
p <- b1_pred_log3a_gini$X1[b1_pred_log3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_log3a_gini$X1<=0.4]))
b1_log3a.ngini <- normalizedGini(a, p)
b1_log3a.ngini
b1_log3a.gini <-Gini(a, p)
b1_log3a.gini

#Brier score
set.seed(123); b1_model_log3a_brier <- train(formula, data=bene1_woe_train3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log3a_brier$results
b1_model_log3a_brier$resample
b1_pred_log3a_brier <- predict(b1_model_log3a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_log3a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_log3a_brier$X1)
b1_log3a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_log3b_roc <- train(formula, data=bene1_woe_test3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log3b_roc <- predict(b1_model_log3b_roc, bene1_woe_train3,type="prob")
b1_log3b.ROC <- roc(predictor=b1predb_log3b_roc$X0,
                    response=bene1_woe_train3$TARGET,
                    levels=rev(levels(bene1_woe_train3$TARGET)))
b1_log3b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_log3b_gini <- train(formula, data=bene1_woe_test3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log3b_gini$results
b1_model_log3b_gini$resample
b1_pred_log3b_gini<- predict(b1_model_log3b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_log3b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_log3b_gini$X1)
#b <= 0.4
p <- b1_pred_log3b_gini$X1[b1_pred_log3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_log3b_gini$X1<=0.4]))
b1_log3b.ngini <- normalizedGini(a, p)
b1_log3b.ngini
b1_log3b.gini <-Gini(a, p)
b1_log3b.gini

#Brier score
set.seed(123); b1_model_log3b_brier <- train(formula, data=bene1_woe_test3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log3b_brier$results
b1_model_log3b_brier$resample
b1_pred_log3b_brier <- predict(b1_model_log3b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_log3b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_log3b_brier$X1)
b1_log3b.bs

###data 4, train-test
#ROC curve 
set.seed(123); b1_model_log4a_roc <- train(formula, data=bene1_woe_train4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log4a_roc <- predict(b1_model_log4a_roc,bene1_woe_test4,type="prob")
b1_log4a.ROC <- roc(predictor=b1predb_log4a_roc$X0,
                    response=bene1_woe_test4$TARGET,
                    levels=rev(levels(bene1_woe_test4$TARGET)))
b1_log4a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_log4a_gini <- train(formula, data=bene1_woe_train4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log4a_gini$results
b1_model_log4a_gini$resample
b1_pred_log4a_gini<- predict(b1_model_log4a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_log4a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_log4a_gini$X1)
#b <= 0.4
p <- b1_pred_log4a_gini$X1[b1_pred_log4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_log4a_gini$X1<=0.4]))
b1_log4a.ngini <- normalizedGini(a, p)
b1_log4a.ngini
b1_log4a.gini <-Gini(a, p)
b1_log4a.gini

#Brier score
set.seed(123); b1_model_log4a_brier <- train(formula, data=bene1_woe_train4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log4a_brier$results
b1_model_log4a_brier$resample
b1_pred_log4a_brier <- predict(b1_model_log4a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_log4a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_log4a_brier$X1)
b1_log4a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_log4b_roc <- train(formula, data=bene1_woe_test4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log4b_roc <- predict(b1_model_log4b_roc, bene1_woe_train4,type="prob")
b1_log4b.ROC <- roc(predictor=b1predb_log4b_roc$X0,
                    response=bene1_woe_train4$TARGET,
                    levels=rev(levels(bene1_woe_train4$TARGET)))
b1_log4b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_log4b_gini <- train(formula, data=bene1_woe_test4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log4b_gini$results
b1_model_log4b_gini$resample
b1_pred_log4b_gini<- predict(b1_model_log4b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_log4b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_log4b_gini$X1)
#b <= 0.4
p <- b1_pred_log4b_gini$X1[b1_pred_log4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_log4b_gini$X1<=0.4]))
b1_log4b.ngini <- normalizedGini(a, p)
b1_log4b.ngini
b1_log4b.gini <-Gini(a, p)
b1_log4b.gini

#Brier score
set.seed(123); b1_model_log4b_brier <- train(formula, data=bene1_woe_test4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log4b_brier$results
b1_model_log4b_brier$resample
b1_pred_log4b_brier <- predict(b1_model_log4b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_log4b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_log4b_brier$X1)
b1_log4b.bs

###data 5, train-test
#ROC curve 
set.seed(123); b1_model_log5a_roc <- train(formula, data=bene1_woe_train5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log5a_roc <- predict(b1_model_log5a_roc,bene1_woe_test5,type="prob")
b1_log5a.ROC <- roc(predictor=b1predb_log5a_roc$X0,
                    response=bene1_woe_test5$TARGET,
                    levels=rev(levels(bene1_woe_test5$TARGET)))
b1_log5a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_log5a_gini <- train(formula, data=bene1_woe_train5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log5a_gini$results
b1_model_log5a_gini$resample
b1_pred_log5a_gini<- predict(b1_model_log5a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_log5a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_log5a_gini$X1)
#b <= 0.4
p <- b1_pred_log5a_gini$X1[b1_pred_log5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_log5a_gini$X1<=0.4]))
b1_log5a.ngini <- normalizedGini(a, p)
b1_log5a.ngini
b1_log5a.gini <-Gini(a, p)
b1_log5a.gini

#Brier score
set.seed(123); b1_model_log5a_brier <- train(formula, data=bene1_woe_train5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log5a_brier$results
b1_model_log5a_brier$resample
b1_pred_log5a_brier <- predict(b1_model_log5a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_log5a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_log5a_brier$X1)
b1_log5a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_log5b_roc <- train(formula, data=bene1_woe_test5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log5b_roc <- predict(b1_model_log5b_roc, bene1_woe_train5,type="prob")
b1_log5b.ROC <- roc(predictor=b1predb_log5b_roc$X0,
                    response=bene1_woe_train5$TARGET,
                    levels=rev(levels(bene1_woe_train5$TARGET)))
b1_log5b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_log5b_gini <- train(formula, data=bene1_woe_test5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log5b_gini$results
b1_model_log5b_gini$resample
b1_pred_log5b_gini<- predict(b1_model_log5b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_log5b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_log5b_gini$X1)
#b <= 0.4
p <- b1_pred_log5b_gini$X1[b1_pred_log5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_log5b_gini$X1<=0.4]))
b1_log5b.ngini <- normalizedGini(a, p)
b1_log5b.ngini
b1_log5b.gini <-Gini(a, p)
b1_log5b.gini

#Brier score
set.seed(123); b1_model_log5b_brier <- train(formula, data=bene1_woe_test5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log5b_brier$results
b1_model_log5b_brier$resample
b1_pred_log5b_brier <- predict(b1_model_log5b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_log5b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_log5b_brier$X1)
b1_log5b.bs

###data 6, train-test
#ROC curve 
set.seed(123); b1_model_log6a_roc <- train(formula, data=bene1_woe_train6, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log6a_roc <- predict(b1_model_log6a_roc,bene1_woe_test6,type="prob")
b1_log6a.ROC <- roc(predictor=b1predb_log6a_roc$X0,
                    response=bene1_woe_test6$TARGET,
                    levels=rev(levels(bene1_woe_test6$TARGET)))
b1_log6a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_log6a_gini <- train(formula, data=bene1_woe_train6, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log6a_gini$results
b1_model_log6a_gini$resample
b1_pred_log6a_gini<- predict(b1_model_log6a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_log6a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_log6a_gini$X1)
#b <= 0.4
p <- b1_pred_log6a_gini$X1[b1_pred_log6a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_log6a_gini$X1<=0.4]))
b1_log6a.ngini <- normalizedGini(a, p)
b1_log6a.ngini
b1_log6a.gini <-Gini(a, p)
b1_log6a.gini

#Brier score
set.seed(123); b1_model_log6a_brier <- train(formula, data=bene1_woe_train6, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log6a_brier$results
b1_model_log6a_brier$resample
b1_pred_log6a_brier <- predict(b1_model_log6a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_log6a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_log6a_brier$X1)
b1_log6a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_log6b_roc <- train(formula, data=bene1_woe_test6, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log6b_roc <- predict(b1_model_log6b_roc, bene1_woe_train6,type="prob")
b1_log6b.ROC <- roc(predictor=b1predb_log6b_roc$X0,
                    response=bene1_woe_train6$TARGET,
                    levels=rev(levels(bene1_woe_train6$TARGET)))
b1_log6b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_log6b_gini <- train(formula, data=bene1_woe_test6, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log6b_gini$results
b1_model_log6b_gini$resample
b1_pred_log6b_gini<- predict(b1_model_log6b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_log6b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_log6b_gini$X1)
#b <= 0.4
p <- b1_pred_log6b_gini$X1[b1_pred_log6b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_log6b_gini$X1<=0.4]))
b1_log6b.ngini <- normalizedGini(a, p)
b1_log6b.ngini
b1_log6b.gini <-Gini(a, p)
b1_log6b.gini

#Brier score
set.seed(123); b1_model_log6b_brier <- train(formula, data=bene1_woe_test6, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log6b_brier$results
b1_model_log6b_brier$resample
b1_pred_log6b_brier <- predict(b1_model_log6b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_log6b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_log6b_brier$X1)
b1_log6b.bs

###data 7, train-test
#ROC curve 
set.seed(123); b1_model_log7a_roc <- train(formula, data=bene1_woe_train7, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log7a_roc <- predict(b1_model_log7a_roc,bene1_woe_test7,type="prob")
b1_log7a.ROC <- roc(predictor=b1predb_log7a_roc$X0,
                    response=bene1_woe_test7$TARGET,
                    levels=rev(levels(bene1_woe_test7$TARGET)))
b1_log7a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_log7a_gini <- train(formula, data=bene1_woe_train7, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log7a_gini$results
b1_model_log7a_gini$resample
b1_pred_log7a_gini<- predict(b1_model_log7a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_log7a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_log7a_gini$X1)
#b <= 0.4
p <- b1_pred_log7a_gini$X1[b1_pred_log7a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_log7a_gini$X1<=0.4]))
b1_log7a.ngini <- normalizedGini(a, p)
b1_log7a.ngini
b1_log7a.gini <-Gini(a, p)
b1_log7a.gini

#Brier score
set.seed(123); b1_model_log7a_brier <- train(formula, data=bene1_woe_train7, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log7a_brier$results
b1_model_log7a_brier$resample
b1_pred_log7a_brier <- predict(b1_model_log7a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_log7a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_log7a_brier$X1)
b1_log7a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_log7b_roc <- train(formula, data=bene1_woe_test7, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log7b_roc <- predict(b1_model_log7b_roc, bene1_woe_train7,type="prob")
b1_log7b.ROC <- roc(predictor=b1predb_log7b_roc$X0,
                    response=bene1_woe_train7$TARGET,
                    levels=rev(levels(bene1_woe_train7$TARGET)))
b1_log7b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_log7b_gini <- train(formula, data=bene1_woe_test7, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log7b_gini$results
b1_model_log7b_gini$resample
b1_pred_log7b_gini<- predict(b1_model_log7b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_log7b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_log7b_gini$X1)
#b <= 0.4
p <- b1_pred_log7b_gini$X1[b1_pred_log7b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_log7b_gini$X1<=0.4]))
b1_log7b.ngini <- normalizedGini(a, p)
b1_log7b.ngini
b1_log7b.gini <-Gini(a, p)
b1_log7b.gini

#Brier score
set.seed(123); b1_model_log7b_brier <- train(formula, data=bene1_woe_test7, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log7b_brier$results
b1_model_log7b_brier$resample
b1_pred_log7b_brier <- predict(b1_model_log7b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_log7b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_log7b_brier$X1)
b1_log7b.bs

###data 8, train-test
#ROC curve 
set.seed(123); b1_model_log8a_roc <- train(formula, data=bene1_woe_train8, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log8a_roc <- predict(b1_model_log8a_roc,bene1_woe_test8,type="prob")
b1_log8a.ROC <- roc(predictor=b1predb_log8a_roc$X0,
                    response=bene1_woe_test8$TARGET,
                    levels=rev(levels(bene1_woe_test8$TARGET)))
b1_log8a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_log8a_gini <- train(formula, data=bene1_woe_train8, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log8a_gini$results
b1_model_log8a_gini$resample
b1_pred_log8a_gini<- predict(b1_model_log8a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_log8a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_log8a_gini$X1)
#b <= 0.4
p <- b1_pred_log8a_gini$X1[b1_pred_log8a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_log8a_gini$X1<=0.4]))
b1_log8a.ngini <- normalizedGini(a, p)
b1_log8a.ngini
b1_log8a.gini <-Gini(a, p)
b1_log8a.gini

#Brier score
set.seed(123); b1_model_log8a_brier <- train(formula, data=bene1_woe_train8, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log8a_brier$results
b1_model_log8a_brier$resample
b1_pred_log8a_brier <- predict(b1_model_log8a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_log8a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_log8a_brier$X1)
b1_log8a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_log8b_roc <- train(formula, data=bene1_woe_test8, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log8b_roc <- predict(b1_model_log8b_roc, bene1_woe_train8,type="prob")
b1_log8b.ROC <- roc(predictor=b1predb_log8b_roc$X0,
                    response=bene1_woe_train8$TARGET,
                    levels=rev(levels(bene1_woe_train8$TARGET)))
b1_log8b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_log8b_gini <- train(formula, data=bene1_woe_test8, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log8b_gini$results
b1_model_log8b_gini$resample
b1_pred_log8b_gini<- predict(b1_model_log8b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_log8b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_log8b_gini$X1)
#b <= 0.4
p <- b1_pred_log8b_gini$X1[b1_pred_log8b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_log8b_gini$X1<=0.4]))
b1_log8b.ngini <- normalizedGini(a, p)
b1_log8b.ngini
b1_log8b.gini <-Gini(a, p)
b1_log8b.gini

#Brier score
set.seed(123); b1_model_log8b_brier <- train(formula, data=bene1_woe_test8, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log8b_brier$results
b1_model_log8b_brier$resample
b1_pred_log8b_brier <- predict(b1_model_log8b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_log8b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_log8b_brier$X1)
b1_log8b.bs

###data 9, train-test
#ROC curve 
set.seed(123); b1_model_log9a_roc <- train(formula, data=bene1_woe_train9, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log9a_roc <- predict(b1_model_log9a_roc,bene1_woe_test9,type="prob")
b1_log9a.ROC <- roc(predictor=b1predb_log9a_roc$X0,
                    response=bene1_woe_test9$TARGET,
                    levels=rev(levels(bene1_woe_test9$TARGET)))
b1_log9a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_log9a_gini <- train(formula, data=bene1_woe_train9, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log9a_gini$results
b1_model_log9a_gini$resample
b1_pred_log9a_gini<- predict(b1_model_log9a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_log9a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_log9a_gini$X1)
#b <= 0.4
p <- b1_pred_log9a_gini$X1[b1_pred_log9a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_log9a_gini$X1<=0.4]))
b1_log9a.ngini <- normalizedGini(a, p)
b1_log9a.ngini
b1_log9a.gini <-Gini(a, p)
b1_log9a.gini

#Brier score
set.seed(123); b1_model_log9a_brier <- train(formula, data=bene1_woe_train9, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log9a_brier$results
b1_model_log9a_brier$resample
b1_pred_log9a_brier <- predict(b1_model_log9a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_log9a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_log9a_brier$X1)
b1_log9a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_log9b_roc <- train(formula, data=bene1_woe_test9, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log9b_roc <- predict(b1_model_log9b_roc, bene1_woe_train9,type="prob")
b1_log9b.ROC <- roc(predictor=b1predb_log9b_roc$X0,
                    response=bene1_woe_train9$TARGET,
                    levels=rev(levels(bene1_woe_train9$TARGET)))
b1_log9b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_log9b_gini <- train(formula, data=bene1_woe_test9, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log9b_gini$results
b1_model_log9b_gini$resample
b1_pred_log9b_gini<- predict(b1_model_log9b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_log9b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_log9b_gini$X1)
#b <= 0.4
p <- b1_pred_log9b_gini$X1[b1_pred_log9b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_log9b_gini$X1<=0.4]))
b1_log9b.ngini <- normalizedGini(a, p)
b1_log9b.ngini
b1_log9b.gini <-Gini(a, p)
b1_log9b.gini

#Brier score
set.seed(123); b1_model_log9b_brier <- train(formula, data=bene1_woe_test9, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log9b_brier$results
b1_model_log9b_brier$resample
b1_pred_log9b_brier <- predict(b1_model_log9b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_log9b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_log9b_brier$X1)
b1_log9b.bs

###data 10, train-test
#ROC curve 
set.seed(123); b1_model_log10a_roc <- train(formula, data=bene1_woe_train10, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log10a_roc <- predict(b1_model_log10a_roc,bene1_woe_test10,type="prob")
b1_log10a.ROC <- roc(predictor=b1predb_log10a_roc$X0,
                     response=bene1_woe_test10$TARGET,
                     levels=rev(levels(bene1_woe_test10$TARGET)))
b1_log10a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_log10a_gini <- train(formula, data=bene1_woe_train10, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log10a_gini$results
b1_model_log10a_gini$resample
b1_pred_log10a_gini<- predict(b1_model_log10a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_log10a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_log10a_gini$X1)
#b <= 0.4
p <- b1_pred_log10a_gini$X1[b1_pred_log10a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_log10a_gini$X1<=0.4]))
b1_log10a.ngini <- normalizedGini(a, p)
b1_log10a.ngini
b1_log10a.gini <-Gini(a, p)
b1_log10a.gini

#Brier score
set.seed(123); b1_model_log10a_brier <- train(formula, data=bene1_woe_train10, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log10a_brier$results
b1_model_log10a_brier$resample
b1_pred_log10a_brier <- predict(b1_model_log10a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_log10a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_log10a_brier$X1)
b1_log10a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_log10b_roc <- train(formula, data=bene1_woe_test10, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc = c("center", "scale"))
b1predb_log10b_roc <- predict(b1_model_log10b_roc, bene1_woe_train10,type="prob")
b1_log10b.ROC <- roc(predictor=b1predb_log10b_roc$X0,
                     response=bene1_woe_train10$TARGET,
                     levels=rev(levels(bene1_woe_train10$TARGET)))
b1_log10b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_log10b_gini <- train(formula, data=bene1_woe_test10, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc = c("center", "scale"))
b1_model_log10b_gini$results
b1_model_log10b_gini$resample
b1_pred_log10b_gini<- predict(b1_model_log10b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_log10b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_log10b_gini$X1)
#b <= 0.4
p <- b1_pred_log10b_gini$X1[b1_pred_log10b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_log10b_gini$X1<=0.4]))
b1_log10b.ngini <- normalizedGini(a, p)
b1_log10b.ngini
b1_log10b.gini <-Gini(a, p)
b1_log10b.gini

#Brier score
set.seed(123); b1_model_log10b_brier <- train(formula, data=bene1_woe_test10, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc = c("center", "scale"))
b1_model_log10b_brier$results
b1_model_log10b_brier$resample
b1_pred_log10b_brier <- predict(b1_model_log10b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_log10b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_log10b_brier$X1)
b1_log10b.bs

##Restults logistic regression!
b1.results_log_AUC <- cbind(b1_log1a.ROC$auc,b1_log1b.ROC$auc,b1_log2a.ROC$auc,b1_log2b.ROC$auc,b1_log3a.ROC$auc,b1_log3b.ROC$auc,
                            b1_log4a.ROC$auc,b1_log4b.ROC$auc,b1_log5a.ROC$auc,b1_log5b.ROC$auc, b1_log6a.ROC$auc,b1_log6b.ROC$auc, 
                            b1_log7a.ROC$auc,b1_log7b.ROC$auc, b1_log8a.ROC$auc,b1_log8b.ROC$auc, b1_log9a.ROC$auc,b1_log9b.ROC$auc,
                            b1_log10a.ROC$auc,b1_log10b.ROC$auc)
b1.results_log_bs <- cbind(b1_log1a.bs,b1_log1b.bs,b1_log2a.bs,b1_log2b.bs,b1_log3a.bs,b1_log3b.bs,b1_log4a.bs,b1_log4b.bs,b1_log5a.bs,b1_log5b.bs,
                           b1_log6a.bs,b1_log6b.bs, b1_log7a.bs,b1_log7b.bs, b1_log7a.bs,b1_log7b.bs, b1_log8a.bs,b1_log8b.bs, b1_log9a.bs,b1_log9b.bs,
                           b1_log10a.bs,b1_log10b.bs)
b1.results_log_ngini <- cbind(b1_log1a.ngini,b1_log1b.ngini,b1_log2a.ngini,b1_log2b.ngini,b1_log3a.ngini,b1_log3b.ngini,b1_log4a.ngini,b1_log4b.ngini,
                              b1_log5a.ngini,b1_log5b.ngini, b1_log6a.ngini,b1_log6b.ngini, b1_log7a.ngini,b1_log7b.ngini, b1_log8a.ngini,b1_log8b.ngini,
                              b1_log9a.ngini,b1_log9b.ngini, b1_log10a.ngini, b1_log10b.ngini)
b1.results_log_gini <- cbind(b1_log1a.gini,b1_log1b.gini,b1_log2a.gini,b1_log2b.gini,b1_log3a.gini,b1_log3b.gini,b1_log4a.gini,b1_log4b.gini,
                             b1_log5a.gini,b1_log5b.gini, b1_log6a.gini,b1_log6b.gini,b1_log7a.gini,b1_log7b.gini, b1_log8a.gini,b1_log8b.gini,
                             b1_log9a.gini,b1_log9b.gini, b1_log10a.gini,b1_log10b.gini)
mean(b1.results_log_AUC)
mean(b1.results_log_bs)
mean(b1.results_log_ngini)
mean(b1.results_log_gini)

#########################################
#######J4.8 Decission tree###############
#########################################

DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
DTgrid

train_control_roc <- trainControl(method="cv", number=5, savePredictions=TRUE,classProbs=TRUE, summaryFunction=twoClassSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_gini <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=giniSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_brier <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=brierSummary, verboseIter = TRUE, allowParallel = TRUE)


###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1reg_model_DT1a_roc <- train(formula, data=bene1_train1, method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT1a_roc <- predict(b1reg_model_DT1a_roc,bene1_test1,type="prob")
b1reg_DT1a.ROC <- roc(predictor=b1regpredb_DT1a_roc$X0,
                      response=bene1_test1$TARGET,
                      levels=rev(levels(bene1_test1$TARGET)))
b1reg_DT1a.ROC

#normalizedGini
bene1_test1$TARGETb <- bene1_test1$TARGET
levels(bene1_test1$TARGETb) <- c('0','1')
bene1_test1$TARGETb <- as.numeric(levels(bene1_test1$TARGETb))[bene1_test1$TARGETb]
set.seed(123); b1reg_model_DT1a_gini <- train(formula, data=bene1_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT1a_gini$results
b1reg_model_DT1a_gini$resample
b1reg_pred_DT1a_gini<- predict(b1reg_model_DT1a_gini, newdata = bene1_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test1$TARGETb, b1reg_pred_DT1a_gini$X1)
Gini(bene1_test1$TARGETb, b1reg_pred_DT1a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT1a_gini$X1[b1reg_pred_DT1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test1$TARGETb[b1reg_pred_DT1a_gini$X1<=0.4]))
b1reg_DT1a.ngini <- normalizedGini(a, p)
b1reg_DT1a.ngini
b1reg_DT1a.gini <-Gini(a, p)
b1reg_DT1a.gini

#Brier score
set.seed(123); b1reg_model_DT1a_brier <- train(formula, data=bene1_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT1a_brier$results
b1reg_model_DT1a_brier$resample
b1reg_pred_DT1a_brier <- predict(b1reg_model_DT1a_brier, newdata = bene1_test1, type='prob')
bene1_test1$TARGETb <- bene1_test1$TARGET
levels(bene1_test1$TARGETb) <- c('0','1')
b1reg_DT1a.bs <- Brier(as.numeric(as.character(bene1_test1$TARGETb)), b1reg_pred_DT1a_brier$X1)
b1reg_DT1a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT1b_roc <- train(formula, data=bene1_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT1b_roc <- predict(b1reg_model_DT1b_roc, bene1_train1,type="prob")
b1reg_DT1b.ROC <- roc(predictor=b1regpredb_DT1b_roc$X0,
                      response=bene1_train1$TARGET,
                      levels=rev(levels(bene1_train1$TARGET)))
b1reg_DT1b.ROC

#normalizedGini
bene1_train1$TARGETb <- bene1_train1$TARGET
levels(bene1_train1$TARGETb) <- c('0','1')
bene1_train1$TARGETb <- as.numeric(levels(bene1_train1$TARGETb))[bene1_train1$TARGETb]
set.seed(123); b1reg_model_DT1b_gini <- train(formula, data=bene1_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT1b_gini$results
b1reg_model_DT1b_gini$resample
b1reg_pred_DT1b_gini<- predict(b1reg_model_DT1b_gini, newdata = bene1_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train1$TARGETb, b1reg_pred_DT1b_gini$X1)
Gini(bene1_train1$TARGETb, b1reg_pred_DT1b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT1b_gini$X1[b1reg_pred_DT1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train1$TARGETb[b1reg_pred_DT1b_gini$X1<=0.4]))
b1reg_DT1b.ngini <- normalizedGini(a, p)
b1reg_DT1b.ngini
b1reg_DT1b.gini <-Gini(a, p)
b1reg_DT1b.gini

#Brier score
set.seed(123); b1reg_model_DT1b_brier <- train(formula, data=bene1_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT1b_brier$results
b1reg_model_DT1b_brier$resample
b1reg_pred_DT1b_brier <- predict(b1reg_model_DT1b_brier, newdata = bene1_train1, type='prob')
bene1_train1$TARGETb <- bene1_train1$TARGET
levels(bene1_train1$TARGETb) <- c('0','1')
b1reg_DT1b.bs <- Brier(as.numeric(as.character(bene1_train1$TARGETb)), b1reg_pred_DT1b_brier$X1)
b1reg_DT1b.bs

###data 2, train-test
#ROC curve 
set.seed(123); b1reg_model_DT2a_roc <- train(formula, data=bene1_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT2a_roc <- predict(b1reg_model_DT2a_roc,bene1_test2,type="prob")
b1reg_DT2a.ROC <- roc(predictor=b1regpredb_DT2a_roc$X0,
                      response=bene1_test2$TARGET,
                      levels=rev(levels(bene1_test2$TARGET)))
b1reg_DT2a.ROC

#normalizedGini
bene1_test2$TARGETb <- bene1_test2$TARGET
levels(bene1_test2$TARGETb) <- c('0','1')
bene1_test2$TARGETb <- as.numeric(levels(bene1_test2$TARGETb))[bene1_test2$TARGETb]
set.seed(123); b1reg_model_DT2a_gini <- train(formula, data=bene1_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT2a_gini$results
b1reg_model_DT2a_gini$resample
b1reg_pred_DT2a_gini<- predict(b1reg_model_DT2a_gini, newdata = bene1_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test2$TARGETb, b1reg_pred_DT2a_gini$X1)
Gini(bene1_test2$TARGETb, b1reg_pred_DT2a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT2a_gini$X1[b1reg_pred_DT2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test2$TARGETb[b1reg_pred_DT2a_gini$X1<=0.4]))
b1reg_DT2a.ngini <- normalizedGini(a, p)
b1reg_DT2a.ngini
b1reg_DT2a.gini <-Gini(a, p)
b1reg_DT2a.gini

#Brier score
set.seed(123); b1reg_model_DT2a_brier <- train(formula, data=bene1_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT2a_brier$results
b1reg_model_DT2a_brier$resample
b1reg_pred_DT2a_brier <- predict(b1reg_model_DT2a_brier, newdata = bene1_test2, type='prob')
bene1_test2$TARGETb <- bene1_test2$TARGET
levels(bene1_test2$TARGETb) <- c('0','1')
b1reg_DT2a.bs <- Brier(as.numeric(as.character(bene1_test2$TARGETb)), b1reg_pred_DT2a_brier$X1)
b1reg_DT2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT2b_roc <- train(formula, data=bene1_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT2b_roc <- predict(b1reg_model_DT2b_roc, bene1_train2,type="prob")
b1reg_DT2b.ROC <- roc(predictor=b1regpredb_DT2b_roc$X0,
                      response=bene1_train2$TARGET,
                      levels=rev(levels(bene1_train2$TARGET)))
b1reg_DT2b.ROC

#normalizedGini
bene1_train2$TARGETb <- bene1_train2$TARGET
levels(bene1_train2$TARGETb) <- c('0','1')
bene1_train2$TARGETb <- as.numeric(levels(bene1_train2$TARGETb))[bene1_train2$TARGETb]
set.seed(123); b1reg_model_DT2b_gini <- train(formula, data=bene1_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT2b_gini$results
b1reg_model_DT2b_gini$resample
b1reg_pred_DT2b_gini<- predict(b1reg_model_DT2b_gini, newdata = bene1_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train2$TARGETb, b1reg_pred_DT2b_gini$X1)
Gini(bene1_train2$TARGETb, b1reg_pred_DT2b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT2b_gini$X1[b1reg_pred_DT2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train2$TARGETb[b1reg_pred_DT2b_gini$X1<=0.4]))
b1reg_DT2b.ngini <- normalizedGini(a, p)
b1reg_DT2b.ngini
b1reg_DT2b.gini <-Gini(a, p)
b1reg_DT2b.gini

#Brier score
set.seed(123); b1reg_model_DT2b_brier <- train(formula, data=bene1_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT2b_brier$results
b1reg_model_DT2b_brier$resample
b1reg_pred_DT2b_brier <- predict(b1reg_model_DT2b_brier, newdata = bene1_train2, type='prob')
bene1_train2$TARGETb <- bene1_train2$TARGET
levels(bene1_train2$TARGETb) <- c('0','1')
b1reg_DT2b.bs <- Brier(as.numeric(as.character(bene1_train2$TARGETb)), b1reg_pred_DT2b_brier$X1)
b1reg_DT2b.bs

###data 3, train-test
#ROC curve 
set.seed(123); b1reg_model_DT3a_roc <- train(formula, data=bene1_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT3a_roc <- predict(b1reg_model_DT3a_roc,bene1_test3,type="prob")
b1reg_DT3a.ROC <- roc(predictor=b1regpredb_DT3a_roc$X0,
                      response=bene1_test3$TARGET,
                      levels=rev(levels(bene1_test3$TARGET)))
b1reg_DT3a.ROC

#normalizedGini
bene1_test3$TARGETb <- bene1_test3$TARGET
levels(bene1_test3$TARGETb) <- c('0','1')
bene1_test3$TARGETb <- as.numeric(levels(bene1_test3$TARGETb))[bene1_test3$TARGETb]
set.seed(123); b1reg_model_DT3a_gini <- train(formula, data=bene1_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT3a_gini$results
b1reg_model_DT3a_gini$resample
b1reg_pred_DT3a_gini<- predict(b1reg_model_DT3a_gini, newdata = bene1_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test3$TARGETb, b1reg_pred_DT3a_gini$X1)
Gini(bene1_test3$TARGETb, b1reg_pred_DT3a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT3a_gini$X1[b1reg_pred_DT3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test3$TARGETb[b1reg_pred_DT3a_gini$X1<=0.4]))
b1reg_DT3a.ngini <- normalizedGini(a, p)
b1reg_DT3a.ngini
b1reg_DT3a.gini <-Gini(a, p)
b1reg_DT3a.gini

#Brier score
set.seed(123); b1reg_model_DT3a_brier <- train(formula, data=bene1_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT3a_brier$results
b1reg_model_DT3a_brier$resample
b1reg_pred_DT3a_brier <- predict(b1reg_model_DT3a_brier, newdata = bene1_test3, type='prob')
bene1_test3$TARGETb <- bene1_test3$TARGET
levels(bene1_test3$TARGETb) <- c('0','1')
b1reg_DT3a.bs <- Brier(as.numeric(as.character(bene1_test3$TARGETb)), b1reg_pred_DT3a_brier$X1)
b1reg_DT3a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT3b_roc <- train(formula, data=bene1_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT3b_roc <- predict(b1reg_model_DT3b_roc, bene1_train3,type="prob")
b1reg_DT3b.ROC <- roc(predictor=b1regpredb_DT3b_roc$X0,
                      response=bene1_train3$TARGET,
                      levels=rev(levels(bene1_train3$TARGET)))
b1reg_DT3b.ROC

#normalizedGini
bene1_train3$TARGETb <- bene1_train3$TARGET
levels(bene1_train3$TARGETb) <- c('0','1')
bene1_train3$TARGETb <- as.numeric(levels(bene1_train3$TARGETb))[bene1_train3$TARGETb]
set.seed(123); b1reg_model_DT3b_gini <- train(formula, data=bene1_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT3b_gini$results
b1reg_model_DT3b_gini$resample
b1reg_pred_DT3b_gini<- predict(b1reg_model_DT3b_gini, newdata = bene1_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train3$TARGETb, b1reg_pred_DT3b_gini$X1)
Gini(bene1_train3$TARGETb, b1reg_pred_DT3b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT3b_gini$X1[b1reg_pred_DT3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train3$TARGETb[b1reg_pred_DT3b_gini$X1<=0.4]))
b1reg_DT3b.ngini <- normalizedGini(a, p)
b1reg_DT3b.ngini
b1reg_DT3b.gini <-Gini(a, p)
b1reg_DT3b.gini

#Brier score
set.seed(123); b1reg_model_DT3b_brier <- train(formula, data=bene1_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT3b_brier$results
b1reg_model_DT3b_brier$resample
b1reg_pred_DT3b_brier <- predict(b1reg_model_DT3b_brier, newdata = bene1_train3, type='prob')
bene1_train3$TARGETb <- bene1_train3$TARGET
levels(bene1_train3$TARGETb) <- c('0','1')
b1reg_DT3b.bs <- Brier(as.numeric(as.character(bene1_train3$TARGETb)), b1reg_pred_DT3b_brier$X1)
b1reg_DT3b.bs

###data 4, train-test
#ROC curve 
set.seed(123); b1reg_model_DT4a_roc <- train(formula, data=bene1_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT4a_roc <- predict(b1reg_model_DT4a_roc,bene1_test4,type="prob")
b1reg_DT4a.ROC <- roc(predictor=b1regpredb_DT4a_roc$X0,
                      response=bene1_test4$TARGET,
                      levels=rev(levels(bene1_test4$TARGET)))
b1reg_DT4a.ROC

#normalizedGini
bene1_test4$TARGETb <- bene1_test4$TARGET
levels(bene1_test4$TARGETb) <- c('0','1')
bene1_test4$TARGETb <- as.numeric(levels(bene1_test4$TARGETb))[bene1_test4$TARGETb]
set.seed(123); b1reg_model_DT4a_gini <- train(formula, data=bene1_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT4a_gini$results
b1reg_model_DT4a_gini$resample
b1reg_pred_DT4a_gini<- predict(b1reg_model_DT4a_gini, newdata = bene1_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test4$TARGETb, b1reg_pred_DT4a_gini$X1)
Gini(bene1_test4$TARGETb, b1reg_pred_DT4a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT4a_gini$X1[b1reg_pred_DT4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test4$TARGETb[b1reg_pred_DT4a_gini$X1<=0.4]))
b1reg_DT4a.ngini <- normalizedGini(a, p)
b1reg_DT4a.ngini
b1reg_DT4a.gini <-Gini(a, p)
b1reg_DT4a.gini

#Brier score
set.seed(123); b1reg_model_DT4a_brier <- train(formula, data=bene1_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT4a_brier$results
b1reg_model_DT4a_brier$resample
b1reg_pred_DT4a_brier <- predict(b1reg_model_DT4a_brier, newdata = bene1_test4, type='prob')
bene1_test4$TARGETb <- bene1_test4$TARGET
levels(bene1_test4$TARGETb) <- c('0','1')
b1reg_DT4a.bs <- Brier(as.numeric(as.character(bene1_test4$TARGETb)), b1reg_pred_DT4a_brier$X1)
b1reg_DT4a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT4b_roc <- train(formula, data=bene1_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT4b_roc <- predict(b1reg_model_DT4b_roc, bene1_train4,type="prob")
b1reg_DT4b.ROC <- roc(predictor=b1regpredb_DT4b_roc$X0,
                      response=bene1_train4$TARGET,
                      levels=rev(levels(bene1_train4$TARGET)))
b1reg_DT4b.ROC

#normalizedGini
bene1_train4$TARGETb <- bene1_train4$TARGET
levels(bene1_train4$TARGETb) <- c('0','1')
bene1_train4$TARGETb <- as.numeric(levels(bene1_train4$TARGETb))[bene1_train4$TARGETb]
set.seed(123); b1reg_model_DT4b_gini <- train(formula, data=bene1_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT4b_gini$results
b1reg_model_DT4b_gini$resample
b1reg_pred_DT4b_gini<- predict(b1reg_model_DT4b_gini, newdata = bene1_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train4$TARGETb, b1reg_pred_DT4b_gini$X1)
Gini(bene1_train4$TARGETb, b1reg_pred_DT4b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT4b_gini$X1[b1reg_pred_DT4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train4$TARGETb[b1reg_pred_DT4b_gini$X1<=0.4]))
b1reg_DT4b.ngini <- normalizedGini(a, p)
b1reg_DT4b.ngini
b1reg_DT4b.gini <-Gini(a, p)
b1reg_DT4b.gini

#Brier score
set.seed(123); b1reg_model_DT4b_brier <- train(formula, data=bene1_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT4b_brier$results
b1reg_model_DT4b_brier$resample
b1reg_pred_DT4b_brier <- predict(b1reg_model_DT4b_brier, newdata = bene1_train4, type='prob')
bene1_train4$TARGETb <- bene1_train4$TARGET
levels(bene1_train4$TARGETb) <- c('0','1')
b1reg_DT4b.bs <- Brier(as.numeric(as.character(bene1_train4$TARGETb)), b1reg_pred_DT4b_brier$X1)
b1reg_DT4b.bs

###data 5, train-test
#ROC curve 
set.seed(123); b1reg_model_DT5a_roc <- train(formula, data=bene1_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT5a_roc <- predict(b1reg_model_DT5a_roc,bene1_test5,type="prob")
b1reg_DT5a.ROC <- roc(predictor=b1regpredb_DT5a_roc$X0,
                      response=bene1_test5$TARGET,
                      levels=rev(levels(bene1_test5$TARGET)))
b1reg_DT5a.ROC

#normalizedGini
bene1_test5$TARGETb <- bene1_test5$TARGET
levels(bene1_test5$TARGETb) <- c('0','1')
bene1_test5$TARGETb <- as.numeric(levels(bene1_test5$TARGETb))[bene1_test5$TARGETb]
set.seed(123); b1reg_model_DT5a_gini <- train(formula, data=bene1_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT5a_gini$results
b1reg_model_DT5a_gini$resample

b1reg_pred_DT5a_gini<- predict(b1reg_model_DT5a_gini, newdata = bene1_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test5$TARGETb, b1reg_pred_DT5a_gini$X1)
Gini(bene1_test5$TARGETb, b1reg_pred_DT5a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT5a_gini$X1[b1reg_pred_DT5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test5$TARGETb[b1reg_pred_DT5a_gini$X1<=0.4]))
b1reg_DT5a.ngini <- normalizedGini(a, p)
b1reg_DT5a.ngini
b1reg_DT5a.gini <-Gini(a, p)
b1reg_DT5a.gini

#Brier score
set.seed(123); b1reg_model_DT5a_brier <- train(formula, data=bene1_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT5a_brier$results
b1reg_model_DT5a_brier$resample
b1reg_pred_DT5a_brier <- predict(b1reg_model_DT5a_brier, newdata = bene1_test5, type='prob')
bene1_test5$TARGETb <- bene1_test5$TARGET
levels(bene1_test5$TARGETb) <- c('0','1')
b1reg_DT5a.bs <- Brier(as.numeric(as.character(bene1_test5$TARGETb)), b1reg_pred_DT5a_brier$X1)
b1reg_DT5a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT5b_roc <- train(formula, data=bene1_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT5b_roc <- predict(b1reg_model_DT5b_roc, bene1_train5,type="prob")
b1reg_DT5b.ROC <- roc(predictor=b1regpredb_DT5b_roc$X0,
                      response=bene1_train5$TARGET,
                      levels=rev(levels(bene1_train5$TARGET)))
b1reg_DT5b.ROC

#normalizedGini
bene1_train5$TARGETb <- bene1_train5$TARGET
levels(bene1_train5$TARGETb) <- c('0','1')
bene1_train5$TARGETb <- as.numeric(levels(bene1_train5$TARGETb))[bene1_train5$TARGETb]
set.seed(123); b1reg_model_DT5b_gini <- train(formula, data=bene1_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT5b_gini$results
b1reg_model_DT5b_gini$resample
b1reg_pred_DT5b_gini<- predict(b1reg_model_DT5b_gini, newdata = bene1_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train5$TARGETb, b1reg_pred_DT5b_gini$X1)
Gini(bene1_train5$TARGETb, b1reg_pred_DT5b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT5b_gini$X1[b1reg_pred_DT5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train5$TARGETb[b1reg_pred_DT5b_gini$X1<=0.4]))
b1reg_DT5b.ngini <- normalizedGini(a, p)
b1reg_DT5b.ngini
b1reg_DT5b.gini <-Gini(a, p)
b1reg_DT5b.gini

#Brier score
set.seed(123); b1reg_model_DT5b_brier <- train(formula, data=bene1_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT5b_brier$results
b1reg_model_DT5b_brier$resample
b1reg_pred_DT5b_brier <- predict(b1reg_model_DT5b_brier, newdata = bene1_train5, type='prob')
bene1_train5$TARGETb <- bene1_train5$TARGET
levels(bene1_train5$TARGETb) <- c('0','1')
b1reg_DT5b.bs <- Brier(as.numeric(as.character(bene1_train5$TARGETb)), b1reg_pred_DT5b_brier$X1)
b1reg_DT5b.bs

###data 6, train-test
#ROC curve 
set.seed(123); b1reg_model_DT6a_roc <- train(formula, data=bene1_train6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT6a_roc <- predict(b1reg_model_DT6a_roc,bene1_test6,type="prob")
b1reg_DT6a.ROC <- roc(predictor=b1regpredb_DT6a_roc$X0,
                      response=bene1_test6$TARGET,
                      levels=rev(levels(bene1_test6$TARGET)))
b1reg_DT6a.ROC

#normalizedGini
bene1_test6$TARGETb <- bene1_test6$TARGET
levels(bene1_test6$TARGETb) <- c('0','1')
bene1_test6$TARGETb <- as.numeric(levels(bene1_test6$TARGETb))[bene1_test6$TARGETb]
set.seed(123); b1reg_model_DT6a_gini <- train(formula, data=bene1_train6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT6a_gini$results
b1reg_model_DT6a_gini$resample

b1reg_pred_DT6a_gini<- predict(b1reg_model_DT6a_gini, newdata = bene1_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test6$TARGETb, b1reg_pred_DT6a_gini$X1)
Gini(bene1_test6$TARGETb, b1reg_pred_DT6a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT6a_gini$X1[b1reg_pred_DT6a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test6$TARGETb[b1reg_pred_DT6a_gini$X1<=0.4]))
b1reg_DT6a.ngini <- normalizedGini(a, p)
b1reg_DT6a.ngini
b1reg_DT6a.gini <-Gini(a, p)
b1reg_DT6a.gini

#Brier score
set.seed(123); b1reg_model_DT6a_brier <- train(formula, data=bene1_train6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT6a_brier$results
b1reg_model_DT6a_brier$resample
b1reg_pred_DT6a_brier <- predict(b1reg_model_DT6a_brier, newdata = bene1_test6, type='prob')
bene1_test6$TARGETb <- bene1_test6$TARGET
levels(bene1_test6$TARGETb) <- c('0','1')
b1reg_DT6a.bs <- Brier(as.numeric(as.character(bene1_test6$TARGETb)), b1reg_pred_DT6a_brier$X1)
b1reg_DT6a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT6b_roc <- train(formula, data=bene1_test6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT6b_roc <- predict(b1reg_model_DT6b_roc, bene1_train6,type="prob")
b1reg_DT6b.ROC <- roc(predictor=b1regpredb_DT6b_roc$X0,
                      response=bene1_train6$TARGET,
                      levels=rev(levels(bene1_train6$TARGET)))
b1reg_DT6b.ROC

#normalizedGini
bene1_train6$TARGETb <- bene1_train6$TARGET
levels(bene1_train6$TARGETb) <- c('0','1')
bene1_train6$TARGETb <- as.numeric(levels(bene1_train6$TARGETb))[bene1_train6$TARGETb]
set.seed(123); b1reg_model_DT6b_gini <- train(formula, data=bene1_test6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT6b_gini$results
b1reg_model_DT6b_gini$resample
b1reg_pred_DT6b_gini<- predict(b1reg_model_DT6b_gini, newdata = bene1_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train6$TARGETb, b1reg_pred_DT6b_gini$X1)
Gini(bene1_train6$TARGETb, b1reg_pred_DT6b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT6b_gini$X1[b1reg_pred_DT6b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train6$TARGETb[b1reg_pred_DT6b_gini$X1<=0.4]))
b1reg_DT6b.ngini <- normalizedGini(a, p)
b1reg_DT6b.ngini
b1reg_DT6b.gini <-Gini(a, p)
b1reg_DT6b.gini

#Brier score
set.seed(123); b1reg_model_DT6b_brier <- train(formula, data=bene1_test6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT6b_brier$results
b1reg_model_DT6b_brier$resample
b1reg_pred_DT6b_brier <- predict(b1reg_model_DT6b_brier, newdata = bene1_train6, type='prob')
bene1_train6$TARGETb <- bene1_train6$TARGET
levels(bene1_train6$TARGETb) <- c('0','1')
b1reg_DT6b.bs <- Brier(as.numeric(as.character(bene1_train6$TARGETb)), b1reg_pred_DT6b_brier$X1)
b1reg_DT6b.bs

###data 7, train-test
#ROC curve 
set.seed(123); b1reg_model_DT7a_roc <- train(formula, data=bene1_train7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT7a_roc <- predict(b1reg_model_DT7a_roc,bene1_test7,type="prob")
b1reg_DT7a.ROC <- roc(predictor=b1regpredb_DT7a_roc$X0,
                      response=bene1_test7$TARGET,
                      levels=rev(levels(bene1_test7$TARGET)))
b1reg_DT7a.ROC

#normalizedGini
bene1_test7$TARGETb <- bene1_test7$TARGET
levels(bene1_test7$TARGETb) <- c('0','1')
bene1_test7$TARGETb <- as.numeric(levels(bene1_test7$TARGETb))[bene1_test7$TARGETb]
set.seed(123); b1reg_model_DT7a_gini <- train(formula, data=bene1_train7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT7a_gini$results
b1reg_model_DT7a_gini$resample

b1reg_pred_DT7a_gini<- predict(b1reg_model_DT7a_gini, newdata = bene1_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test7$TARGETb, b1reg_pred_DT7a_gini$X1)
Gini(bene1_test7$TARGETb, b1reg_pred_DT7a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT7a_gini$X1[b1reg_pred_DT7a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test7$TARGETb[b1reg_pred_DT7a_gini$X1<=0.4]))
b1reg_DT7a.ngini <- normalizedGini(a, p)
b1reg_DT7a.ngini
b1reg_DT7a.gini <-Gini(a, p)
b1reg_DT7a.gini

#Brier score
set.seed(123); b1reg_model_DT7a_brier <- train(formula, data=bene1_train7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT7a_brier$results
b1reg_model_DT7a_brier$resample
b1reg_pred_DT7a_brier <- predict(b1reg_model_DT7a_brier, newdata = bene1_test7, type='prob')
bene1_test7$TARGETb <- bene1_test7$TARGET
levels(bene1_test7$TARGETb) <- c('0','1')
b1reg_DT7a.bs <- Brier(as.numeric(as.character(bene1_test7$TARGETb)), b1reg_pred_DT7a_brier$X1)
b1reg_DT7a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT7b_roc <- train(formula, data=bene1_test7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT7b_roc <- predict(b1reg_model_DT7b_roc, bene1_train7,type="prob")
b1reg_DT7b.ROC <- roc(predictor=b1regpredb_DT7b_roc$X0,
                      response=bene1_train7$TARGET,
                      levels=rev(levels(bene1_train7$TARGET)))
b1reg_DT7b.ROC

#normalizedGini
bene1_train7$TARGETb <- bene1_train7$TARGET
levels(bene1_train7$TARGETb) <- c('0','1')
bene1_train7$TARGETb <- as.numeric(levels(bene1_train7$TARGETb))[bene1_train7$TARGETb]
set.seed(123); b1reg_model_DT7b_gini <- train(formula, data=bene1_test7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT7b_gini$results
b1reg_model_DT7b_gini$resample
b1reg_pred_DT7b_gini<- predict(b1reg_model_DT7b_gini, newdata = bene1_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train7$TARGETb, b1reg_pred_DT7b_gini$X1)
Gini(bene1_train7$TARGETb, b1reg_pred_DT7b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT7b_gini$X1[b1reg_pred_DT7b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train7$TARGETb[b1reg_pred_DT7b_gini$X1<=0.4]))
b1reg_DT7b.ngini <- normalizedGini(a, p)
b1reg_DT7b.ngini
b1reg_DT7b.gini <-Gini(a, p)
b1reg_DT7b.gini

#Brier score
set.seed(123); b1reg_model_DT7b_brier <- train(formula, data=bene1_test7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT7b_brier$results
b1reg_model_DT7b_brier$resample
b1reg_pred_DT7b_brier <- predict(b1reg_model_DT7b_brier, newdata = bene1_train7, type='prob')
bene1_train7$TARGETb <- bene1_train7$TARGET
levels(bene1_train7$TARGETb) <- c('0','1')
b1reg_DT7b.bs <- Brier(as.numeric(as.character(bene1_train7$TARGETb)), b1reg_pred_DT7b_brier$X1)
b1reg_DT7b.bs

###data 8, train-test
#ROC curve 
set.seed(123); b1reg_model_DT8a_roc <- train(formula, data=bene1_train8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT8a_roc <- predict(b1reg_model_DT8a_roc,bene1_test8,type="prob")
b1reg_DT8a.ROC <- roc(predictor=b1regpredb_DT8a_roc$X0,
                      response=bene1_test8$TARGET,
                      levels=rev(levels(bene1_test8$TARGET)))
b1reg_DT8a.ROC

#normalizedGini
bene1_test8$TARGETb <- bene1_test8$TARGET
levels(bene1_test8$TARGETb) <- c('0','1')
bene1_test8$TARGETb <- as.numeric(levels(bene1_test8$TARGETb))[bene1_test8$TARGETb]
set.seed(123); b1reg_model_DT8a_gini <- train(formula, data=bene1_train8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT8a_gini$results
b1reg_model_DT8a_gini$resample

b1reg_pred_DT8a_gini<- predict(b1reg_model_DT8a_gini, newdata = bene1_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test8$TARGETb, b1reg_pred_DT8a_gini$X1)
Gini(bene1_test8$TARGETb, b1reg_pred_DT8a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT8a_gini$X1[b1reg_pred_DT8a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test8$TARGETb[b1reg_pred_DT8a_gini$X1<=0.4]))
b1reg_DT8a.ngini <- normalizedGini(a, p)
b1reg_DT8a.ngini
b1reg_DT8a.gini <-Gini(a, p)
b1reg_DT8a.gini

#Brier score
set.seed(123); b1reg_model_DT8a_brier <- train(formula, data=bene1_train8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT8a_brier$results
b1reg_model_DT8a_brier$resample
b1reg_pred_DT8a_brier <- predict(b1reg_model_DT8a_brier, newdata = bene1_test8, type='prob')
bene1_test8$TARGETb <- bene1_test8$TARGET
levels(bene1_test8$TARGETb) <- c('0','1')
b1reg_DT8a.bs <- Brier(as.numeric(as.character(bene1_test8$TARGETb)), b1reg_pred_DT8a_brier$X1)
b1reg_DT8a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT8b_roc <- train(formula, data=bene1_test8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT8b_roc <- predict(b1reg_model_DT8b_roc, bene1_train8,type="prob")
b1reg_DT8b.ROC <- roc(predictor=b1regpredb_DT8b_roc$X0,
                      response=bene1_train8$TARGET,
                      levels=rev(levels(bene1_train8$TARGET)))
b1reg_DT8b.ROC

#normalizedGini
bene1_train8$TARGETb <- bene1_train8$TARGET
levels(bene1_train8$TARGETb) <- c('0','1')
bene1_train8$TARGETb <- as.numeric(levels(bene1_train8$TARGETb))[bene1_train8$TARGETb]
set.seed(123); b1reg_model_DT8b_gini <- train(formula, data=bene1_test8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT8b_gini$results
b1reg_model_DT8b_gini$resample
b1reg_pred_DT8b_gini<- predict(b1reg_model_DT8b_gini, newdata = bene1_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train8$TARGETb, b1reg_pred_DT8b_gini$X1)
Gini(bene1_train8$TARGETb, b1reg_pred_DT8b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT8b_gini$X1[b1reg_pred_DT8b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train8$TARGETb[b1reg_pred_DT8b_gini$X1<=0.4]))
b1reg_DT8b.ngini <- normalizedGini(a, p)
b1reg_DT8b.ngini
b1reg_DT8b.gini <-Gini(a, p)
b1reg_DT8b.gini

#Brier score
set.seed(123); b1reg_model_DT8b_brier <- train(formula, data=bene1_test8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT8b_brier$results
b1reg_model_DT8b_brier$resample
b1reg_pred_DT8b_brier <- predict(b1reg_model_DT8b_brier, newdata = bene1_train8, type='prob')
bene1_train8$TARGETb <- bene1_train8$TARGET
levels(bene1_train8$TARGETb) <- c('0','1')
b1reg_DT8b.bs <- Brier(as.numeric(as.character(bene1_train8$TARGETb)), b1reg_pred_DT8b_brier$X1)
b1reg_DT8b.bs

###data 9, train-test
#ROC curve 
set.seed(123); b1reg_model_DT9a_roc <- train(formula, data=bene1_train9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT9a_roc <- predict(b1reg_model_DT9a_roc,bene1_test9,type="prob")
b1reg_DT9a.ROC <- roc(predictor=b1regpredb_DT9a_roc$X0,
                      response=bene1_test9$TARGET,
                      levels=rev(levels(bene1_test9$TARGET)))
b1reg_DT9a.ROC

#normalizedGini
bene1_test9$TARGETb <- bene1_test9$TARGET
levels(bene1_test9$TARGETb) <- c('0','1')
bene1_test9$TARGETb <- as.numeric(levels(bene1_test9$TARGETb))[bene1_test9$TARGETb]
set.seed(123); b1reg_model_DT9a_gini <- train(formula, data=bene1_train9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT9a_gini$results
b1reg_model_DT9a_gini$resample

b1reg_pred_DT9a_gini<- predict(b1reg_model_DT9a_gini, newdata = bene1_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test9$TARGETb, b1reg_pred_DT9a_gini$X1)
Gini(bene1_test9$TARGETb, b1reg_pred_DT9a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT9a_gini$X1[b1reg_pred_DT9a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test9$TARGETb[b1reg_pred_DT9a_gini$X1<=0.4]))
b1reg_DT9a.ngini <- normalizedGini(a, p)
b1reg_DT9a.ngini
b1reg_DT9a.gini <-Gini(a, p)
b1reg_DT9a.gini

#Brier score
set.seed(123); b1reg_model_DT9a_brier <- train(formula, data=bene1_train9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT9a_brier$results
b1reg_model_DT9a_brier$resample
b1reg_pred_DT9a_brier <- predict(b1reg_model_DT9a_brier, newdata = bene1_test9, type='prob')
bene1_test9$TARGETb <- bene1_test9$TARGET
levels(bene1_test9$TARGETb) <- c('0','1')
b1reg_DT9a.bs <- Brier(as.numeric(as.character(bene1_test9$TARGETb)), b1reg_pred_DT9a_brier$X1)
b1reg_DT9a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT9b_roc <- train(formula, data=bene1_test9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT9b_roc <- predict(b1reg_model_DT9b_roc, bene1_train9,type="prob")
b1reg_DT9b.ROC <- roc(predictor=b1regpredb_DT9b_roc$X0,
                      response=bene1_train9$TARGET,
                      levels=rev(levels(bene1_train9$TARGET)))
b1reg_DT9b.ROC

#normalizedGini
bene1_train9$TARGETb <- bene1_train9$TARGET
levels(bene1_train9$TARGETb) <- c('0','1')
bene1_train9$TARGETb <- as.numeric(levels(bene1_train9$TARGETb))[bene1_train9$TARGETb]
set.seed(123); b1reg_model_DT9b_gini <- train(formula, data=bene1_test9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT9b_gini$results
b1reg_model_DT9b_gini$resample
b1reg_pred_DT9b_gini<- predict(b1reg_model_DT9b_gini, newdata = bene1_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train9$TARGETb, b1reg_pred_DT9b_gini$X1)
Gini(bene1_train9$TARGETb, b1reg_pred_DT9b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT9b_gini$X1[b1reg_pred_DT9b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train9$TARGETb[b1reg_pred_DT9b_gini$X1<=0.4]))
b1reg_DT9b.ngini <- normalizedGini(a, p)
b1reg_DT9b.ngini
b1reg_DT9b.gini <-Gini(a, p)
b1reg_DT9b.gini

#Brier score
set.seed(123); b1reg_model_DT9b_brier <- train(formula, data=bene1_test9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT9b_brier$results
b1reg_model_DT9b_brier$resample
b1reg_pred_DT9b_brier <- predict(b1reg_model_DT9b_brier, newdata = bene1_train9, type='prob')
bene1_train9$TARGETb <- bene1_train9$TARGET
levels(bene1_train9$TARGETb) <- c('0','1')
b1reg_DT9b.bs <- Brier(as.numeric(as.character(bene1_train9$TARGETb)), b1reg_pred_DT9b_brier$X1)
b1reg_DT9b.bs

###data 10, train-test
#ROC curve 
set.seed(123); b1reg_model_DT10a_roc <- train(formula, data=bene1_train10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT10a_roc <- predict(b1reg_model_DT10a_roc,bene1_test10,type="prob")
b1reg_DT10a.ROC <- roc(predictor=b1regpredb_DT10a_roc$X0,
                       response=bene1_test10$TARGET,
                       levels=rev(levels(bene1_test10$TARGET)))
b1reg_DT10a.ROC

#normalizedGini
bene1_test10$TARGETb <- bene1_test10$TARGET
levels(bene1_test10$TARGETb) <- c('0','1')
bene1_test10$TARGETb <- as.numeric(levels(bene1_test10$TARGETb))[bene1_test10$TARGETb]
set.seed(123); b1reg_model_DT10a_gini <- train(formula, data=bene1_train10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT10a_gini$results
b1reg_model_DT10a_gini$resample

b1reg_pred_DT10a_gini<- predict(b1reg_model_DT10a_gini, newdata = bene1_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test10$TARGETb, b1reg_pred_DT10a_gini$X1)
Gini(bene1_test10$TARGETb, b1reg_pred_DT10a_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT10a_gini$X1[b1reg_pred_DT10a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test10$TARGETb[b1reg_pred_DT10a_gini$X1<=0.4]))
b1reg_DT10a.ngini <- normalizedGini(a, p)
b1reg_DT10a.ngini
b1reg_DT10a.gini <-Gini(a, p)
b1reg_DT10a.gini

#Brier score
set.seed(123); b1reg_model_DT10a_brier <- train(formula, data=bene1_train10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT10a_brier$results
b1reg_model_DT10a_brier$resample
b1reg_pred_DT10a_brier <- predict(b1reg_model_DT10a_brier, newdata = bene1_test10, type='prob')
bene1_test10$TARGETb <- bene1_test10$TARGET
levels(bene1_test10$TARGETb) <- c('0','1')
b1reg_DT10a.bs <- Brier(as.numeric(as.character(bene1_test10$TARGETb)), b1reg_pred_DT10a_brier$X1)
b1reg_DT10a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1reg_model_DT10b_roc <- train(formula, data=bene1_test10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_DT10b_roc <- predict(b1reg_model_DT10b_roc, bene1_train10,type="prob")
b1reg_DT10b.ROC <- roc(predictor=b1regpredb_DT10b_roc$X0,
                       response=bene1_train10$TARGET,
                       levels=rev(levels(bene1_train10$TARGET)))
b1reg_DT10b.ROC

#normalizedGini
bene1_train10$TARGETb <- bene1_train10$TARGET
levels(bene1_train10$TARGETb) <- c('0','1')
bene1_train10$TARGETb <- as.numeric(levels(bene1_train10$TARGETb))[bene1_train10$TARGETb]
set.seed(123); b1reg_model_DT10b_gini <- train(formula, data=bene1_test10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_DT10b_gini$results
b1reg_model_DT10b_gini$resample
b1reg_pred_DT10b_gini<- predict(b1reg_model_DT10b_gini, newdata = bene1_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train10$TARGETb, b1reg_pred_DT10b_gini$X1)
Gini(bene1_train10$TARGETb, b1reg_pred_DT10b_gini$X1)
#b <= 0.4
p <- b1reg_pred_DT10b_gini$X1[b1reg_pred_DT10b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train10$TARGETb[b1reg_pred_DT10b_gini$X1<=0.4]))
b1reg_DT10b.ngini <- normalizedGini(a, p)
b1reg_DT10b.ngini
b1reg_DT10b.gini <-Gini(a, p)
b1reg_DT10b.gini

#Brier score
set.seed(123); b1reg_model_DT10b_brier <- train(formula, data=bene1_test10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_DT10b_brier$results
b1reg_model_DT10b_brier$resample
b1reg_pred_DT10b_brier <- predict(b1reg_model_DT10b_brier, newdata = bene1_train10, type='prob')
bene1_train10$TARGETb <- bene1_train10$TARGET
levels(bene1_train10$TARGETb) <- c('0','1')
b1reg_DT10b.bs <- Brier(as.numeric(as.character(bene1_train10$TARGETb)), b1reg_pred_DT10b_brier$X1)
b1reg_DT10b.bs

##Restults DT!
b1reg_results_DT_AUC <- cbind(b1reg_DT1a.ROC$auc,b1reg_DT1b.ROC$auc,b1reg_DT2a.ROC$auc,b1reg_DT2b.ROC$auc,b1reg_DT3a.ROC$auc,b1reg_DT3b.ROC$auc,
                              b1reg_DT4a.ROC$auc,b1reg_DT4b.ROC$auc,b1reg_DT5a.ROC$auc,b1reg_DT5b.ROC$auc,b1reg_DT6a.ROC$auc,b1reg_DT6b.ROC$auc,
                              b1reg_DT7a.ROC$auc,b1reg_DT7b.ROC$auc,b1reg_DT8a.ROC$auc,b1reg_DT8b.ROC$auc,b1reg_DT9a.ROC$auc,b1reg_DT9b.ROC$auc,
                              b1reg_DT10a.ROC$auc,b1reg_DT10b.ROC$auc)
b1reg_results_DT_bs <- cbind(b1reg_DT1a.bs,b1reg_DT1b.bs,b1reg_DT2a.bs,b1reg_DT2b.bs,b1reg_DT3a.bs,b1reg_DT3b.bs,b1reg_DT4a.bs,b1reg_DT4b.bs,b1reg_DT5a.bs,b1reg_DT5b.bs,
                             b1reg_DT6a.bs,b1reg_DT6b.bs,b1reg_DT7a.bs,b1reg_DT7b.bs,b1reg_DT8a.bs,b1reg_DT8b.bs,b1reg_DT9a.bs,b1reg_DT9b.bs,b1reg_DT10a.bs,b1reg_DT10b.bs)
b1reg_results_DT_ngini <- cbind(b1reg_DT1a.ngini,b1reg_DT1b.ngini,b1reg_DT2a.ngini,b1reg_DT2b.ngini,b1reg_DT3a.ngini,b1reg_DT3b.ngini,b1reg_DT4a.ngini,b1reg_DT4b.ngini,
                                b1reg_DT5a.ngini,b1reg_DT5b.ngini, b1reg_DT6a.ngini,b1reg_DT6b.ngini,b1reg_DT7a.ngini,b1reg_DT7b.ngini,b1reg_DT8a.ngini,b1reg_DT8b.ngini,
                                b1reg_DT9a.ngini,b1reg_DT9b.ngini,b1reg_DT10a.ngini,b1reg_DT10b.ngini)
b1reg_results_DT_gini <- cbind(b1reg_DT1a.gini,b1reg_DT1b.gini,b1reg_DT2a.gini,b1reg_DT2b.gini,b1reg_DT3a.gini,b1reg_DT3b.gini,b1reg_DT4a.gini,b1reg_DT4b.gini,
                               b1reg_DT5a.gini,b1reg_DT5b.gini,b1reg_DT6a.gini,b1reg_DT6b.gini,b1reg_DT7a.gini,b1reg_DT7b.gini,b1reg_DT8a.gini,b1reg_DT8b.gini,
                               b1reg_DT9a.gini,b1reg_DT9b.gini,b1reg_DT10a.gini,b1reg_DT10b.gini)

mean(b1reg_results_DT_AUC)
mean(b1reg_results_DT_bs)
mean(b1reg_results_DT_ngini)
mean(b1reg_results_DT_gini)

##############################
#######Random forest######### 
#############################
#create a custom function to tune over both meta parameters
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

m <- floor(log2(length(bene1_train1$TARGET)+1))
RFgrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1reg_model_RF1a_roc <- train(formula, data=bene1_train1, method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF1a_roc <- predict(b1reg_model_RF1a_roc,bene1_test1,type="prob")
b1reg_RF1a.ROC <- roc(predictor=b1regpredb_RF1a_roc$X0,
                      response=bene1_test1$TARGET,
                      levels=rev(levels(bene1_test1$TARGET)))
b1reg_RF1a.ROC

#normalizedGini
bene1_test1$TARGETb <- bene1_test1$TARGET
levels(bene1_test1$TARGETb) <- c('0','1')
bene1_test1$TARGETb <- as.numeric(levels(bene1_test1$TARGETb))[bene1_test1$TARGETb]
set.seed(123); b1reg_model_RF1a_gini <- train(formula, data=bene1_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF1a_gini$results
b1reg_model_RF1a_gini$resample
b1reg_pred_RF1a_gini<- predict(b1reg_model_RF1a_gini, newdata = bene1_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test1$TARGETb, b1reg_pred_RF1a_gini$X1)
Gini(bene1_test1$TARGETb, b1reg_pred_RF1a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF1a_gini$X1[b1reg_pred_RF1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test1$TARGETb[b1reg_pred_RF1a_gini$X1<=0.4]))
b1reg_RF1a.ngini <- normalizedGini(a, p)
b1reg_RF1a.ngini
b1reg_RF1a.gini <-Gini(a, p)
b1reg_RF1a.gini

#Brier score
set.seed(123); b1reg_model_RF1a_brier <- train(formula, data=bene1_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore')
b1reg_model_RF1a_brier$results
b1reg_model_RF1a_brier$resample
b1reg_pred_RF1a_brier <- predict(b1reg_model_RF1a_brier, newdata = bene1_test1, type='prob')
bene1_test1$TARGETb <- bene1_test1$TARGET
levels(bene1_test1$TARGETb) <- c('0','1')
b1reg_RF1a.bs <- Brier(as.numeric(as.character(bene1_test1$TARGETb)), b1reg_pred_RF1a_brier$X1)
b1reg_RF1a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF1b_roc <- train(formula, data=bene1_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF1b_roc <- predict(b1reg_model_RF1b_roc, bene1_train1,type="prob")
b1reg_RF1b.ROC <- roc(predictor=b1regpredb_RF1b_roc$X0,
                      response=bene1_train1$TARGET,
                      levels=rev(levels(bene1_train1$TARGET)))
b1reg_RF1b.ROC

#normalizedGini
bene1_train1$TARGETb <- bene1_train1$TARGET
levels(bene1_train1$TARGETb) <- c('0','1')
bene1_train1$TARGETb <- as.numeric(levels(bene1_train1$TARGETb))[bene1_train1$TARGETb]
set.seed(123); b1reg_model_RF1b_gini <- train(formula, data=bene1_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF1b_gini$results
b1reg_model_RF1b_gini$resample
b1reg_pred_RF1b_gini<- predict(b1reg_model_RF1b_gini, newdata = bene1_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train1$TARGETb, b1reg_pred_RF1b_gini$X1)
Gini(bene1_train1$TARGETb, b1reg_pred_RF1b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF1b_gini$X1[b1reg_pred_RF1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train1$TARGETb[b1reg_pred_RF1b_gini$X1<=0.4]))
b1reg_RF1b.ngini <- normalizedGini(a, p)
b1reg_RF1b.ngini
b1reg_RF1b.gini <-Gini(a, p)
b1reg_RF1b.gini

#Brier score
set.seed(123); b1reg_model_RF1b_brier <- train(formula, data=bene1_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF1b_brier$results
b1reg_model_RF1b_brier$resample
b1reg_pred_RF1b_brier <- predict(b1reg_model_RF1b_brier, newdata = bene1_train1, type='prob')
bene1_train1$TARGETb <- bene1_train1$TARGET
levels(bene1_train1$TARGETb) <- c('0','1')
b1reg_RF1b.bs <- Brier(as.numeric(as.character(bene1_train1$TARGETb)), b1reg_pred_RF1b_brier$X1)
b1reg_RF1b.bs

###data 2, train-test
#ROC curve 
set.seed(123); b1reg_model_RF2a_roc <- train(formula, data=bene1_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF2a_roc <- predict(b1reg_model_RF2a_roc,bene1_test2,type="prob")
b1reg_RF2a.ROC <- roc(predictor=b1regpredb_RF2a_roc$X0,
                      response=bene1_test2$TARGET,
                      levels=rev(levels(bene1_test2$TARGET)))
b1reg_RF2a.ROC

#normalizedGini
bene1_test2$TARGETb <- bene1_test2$TARGET
levels(bene1_test2$TARGETb) <- c('0','1')
bene1_test2$TARGETb <- as.numeric(levels(bene1_test2$TARGETb))[bene1_test2$TARGETb]
set.seed(123); b1reg_model_RF2a_gini <- train(formula, data=bene1_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF2a_gini$results
b1reg_model_RF2a_gini$resample
b1reg_pred_RF2a_gini<- predict(b1reg_model_RF2a_gini, newdata = bene1_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test2$TARGETb, b1reg_pred_RF2a_gini$X1)
Gini(bene1_test2$TARGETb, b1reg_pred_RF2a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF2a_gini$X1[b1reg_pred_RF2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test2$TARGETb[b1reg_pred_RF2a_gini$X1<=0.4]))
b1reg_RF2a.ngini <- normalizedGini(a, p)
b1reg_RF2a.ngini
b1reg_RF2a.gini <-Gini(a, p)
b1reg_RF2a.gini

#Brier score
set.seed(123); b1reg_model_RF2a_brier <- train(formula, data=bene1_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF2a_brier$results
b1reg_model_RF2a_brier$resample
b1reg_pred_RF2a_brier <- predict(b1reg_model_RF2a_brier, newdata = bene1_test2, type='prob')
bene1_test2$TARGETb <- bene1_test2$TARGET
levels(bene1_test2$TARGETb) <- c('0','1')
b1reg_RF2a.bs <- Brier(as.numeric(as.character(bene1_test2$TARGETb)), b1reg_pred_RF2a_brier$X1)
b1reg_RF2a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF2b_roc <- train(formula, data=bene1_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF2b_roc <- predict(b1reg_model_RF2b_roc, bene1_train2,type="prob")
b1reg_RF2b.ROC <- roc(predictor=b1regpredb_RF2b_roc$X0,
                      response=bene1_train2$TARGET,
                      levels=rev(levels(bene1_train2$TARGET)))
b1reg_RF2b.ROC

#normalizedGini
bene1_train2$TARGETb <- bene1_train2$TARGET
levels(bene1_train2$TARGETb) <- c('0','1')
bene1_train2$TARGETb <- as.numeric(levels(bene1_train2$TARGETb))[bene1_train2$TARGETb]
set.seed(123); b1reg_model_RF2b_gini <- train(formula, data=bene1_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF2b_gini$results
b1reg_model_RF2b_gini$resample
b1reg_pred_RF2b_gini<- predict(b1reg_model_RF2b_gini, newdata = bene1_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train2$TARGETb, b1reg_pred_RF2b_gini$X1)
Gini(bene1_train2$TARGETb, b1reg_pred_RF2b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF2b_gini$X1[b1reg_pred_RF2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train2$TARGETb[b1reg_pred_RF2b_gini$X1<=0.4]))
b1reg_RF2b.ngini <- normalizedGini(a, p)
b1reg_RF2b.ngini
b1reg_RF2b.gini <-Gini(a, p)
b1reg_RF2b.gini

#Brier score
set.seed(123); b1reg_model_RF2b_brier <- train(formula, data=bene1_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF2b_brier$results
b1reg_model_RF2b_brier$resample
b1reg_pred_RF2b_brier <- predict(b1reg_model_RF2b_brier, newdata = bene1_train2, type='prob')
bene1_train2$TARGETb <- bene1_train2$TARGET
levels(bene1_train2$TARGETb) <- c('0','1')
b1reg_RF2b.bs <- Brier(as.numeric(as.character(bene1_train2$TARGETb)), b1reg_pred_RF2b_brier$X1)
b1reg_RF2b.bs

###data 3, train-test
#ROC curve 
set.seed(123); b1reg_model_RF3a_roc <- train(formula, data=bene1_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF3a_roc <- predict(b1reg_model_RF3a_roc,bene1_test3,type="prob")
b1reg_RF3a.ROC <- roc(predictor=b1regpredb_RF3a_roc$X0,
                      response=bene1_test3$TARGET,
                      levels=rev(levels(bene1_test3$TARGET)))
b1reg_RF3a.ROC

#normalizedGini
bene1_test3$TARGETb <- bene1_test3$TARGET
levels(bene1_test3$TARGETb) <- c('0','1')
bene1_test3$TARGETb <- as.numeric(levels(bene1_test3$TARGETb))[bene1_test3$TARGETb]
set.seed(123); b1reg_model_RF3a_gini <- train(formula, data=bene1_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF3a_gini$results
b1reg_model_RF3a_gini$resample
b1reg_pred_RF3a_gini<- predict(b1reg_model_RF3a_gini, newdata = bene1_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test3$TARGETb, b1reg_pred_RF3a_gini$X1)
Gini(bene1_test3$TARGETb, b1reg_pred_RF3a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF3a_gini$X1[b1reg_pred_RF3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test3$TARGETb[b1reg_pred_RF3a_gini$X1<=0.4]))
b1reg_RF3a.ngini <- normalizedGini(a, p)
b1reg_RF3a.ngini
b1reg_RF3a.gini <-Gini(a, p)
b1reg_RF3a.gini

#Brier score
set.seed(123); b1reg_model_RF3a_brier <- train(formula, data=bene1_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF3a_brier$results
b1reg_model_RF3a_brier$resample
b1reg_pred_RF3a_brier <- predict(b1reg_model_RF3a_brier, newdata = bene1_test3, type='prob')
bene1_test3$TARGETb <- bene1_test3$TARGET
levels(bene1_test3$TARGETb) <- c('0','1')
b1reg_RF3a.bs <- Brier(as.numeric(as.character(bene1_test3$TARGETb)), b1reg_pred_RF3a_brier$X1)
b1reg_RF3a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF3b_roc <- train(formula, data=bene1_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF3b_roc <- predict(b1reg_model_RF3b_roc, bene1_train3,type="prob")
b1reg_RF3b.ROC <- roc(predictor=b1regpredb_RF3b_roc$X0,
                      response=bene1_train3$TARGET,
                      levels=rev(levels(bene1_train3$TARGET)))
b1reg_RF3b.ROC

#normalizedGini
bene1_train3$TARGETb <- bene1_train3$TARGET
levels(bene1_train3$TARGETb) <- c('0','1')
bene1_train3$TARGETb <- as.numeric(levels(bene1_train3$TARGETb))[bene1_train3$TARGETb]
set.seed(123); b1reg_model_RF3b_gini <- train(formula, data=bene1_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF3b_gini$results
b1reg_model_RF3b_gini$resample
b1reg_pred_RF3b_gini<- predict(b1reg_model_RF3b_gini, newdata = bene1_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train3$TARGETb, b1reg_pred_RF3b_gini$X1)
Gini(bene1_train3$TARGETb, b1reg_pred_RF3b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF3b_gini$X1[b1reg_pred_RF3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train3$TARGETb[b1reg_pred_RF3b_gini$X1<=0.4]))
b1reg_RF3b.ngini <- normalizedGini(a, p)
b1reg_RF3b.ngini
b1reg_RF3b.gini <-Gini(a, p)
b1reg_RF3b.gini

#Brier score
set.seed(123); b1reg_model_RF3b_brier <- train(formula, data=bene1_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF3b_brier$results
b1reg_model_RF3b_brier$resample
b1reg_pred_RF3b_brier <- predict(b1reg_model_RF3b_brier, newdata = bene1_train3, type='prob')
bene1_train3$TARGETb <- bene1_train3$TARGET
levels(bene1_train3$TARGETb) <- c('0','1')
b1reg_RF3b.bs <- Brier(as.numeric(as.character(bene1_train3$TARGETb)), b1reg_pred_RF3b_brier$X1)
b1reg_RF3b.bs

###data 4, train-test
#ROC curve 
set.seed(123); b1reg_model_RF4a_roc <- train(formula, data=bene1_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF4a_roc <- predict(b1reg_model_RF4a_roc,bene1_test4,type="prob")
b1reg_RF4a.ROC <- roc(predictor=b1regpredb_RF4a_roc$X0,
                      response=bene1_test4$TARGET,
                      levels=rev(levels(bene1_test4$TARGET)))
b1reg_RF4a.ROC

#normalizedGini
bene1_test4$TARGETb <- bene1_test4$TARGET
levels(bene1_test4$TARGETb) <- c('0','1')
bene1_test4$TARGETb <- as.numeric(levels(bene1_test4$TARGETb))[bene1_test4$TARGETb]
set.seed(123); b1reg_model_RF4a_gini <- train(formula, data=bene1_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF4a_gini$results
b1reg_model_RF4a_gini$resample
b1reg_pred_RF4a_gini<- predict(b1reg_model_RF4a_gini, newdata = bene1_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test4$TARGETb, b1reg_pred_RF4a_gini$X1)
Gini(bene1_test4$TARGETb, b1reg_pred_RF4a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF4a_gini$X1[b1reg_pred_RF4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test4$TARGETb[b1reg_pred_RF4a_gini$X1<=0.4]))
b1reg_RF4a.ngini <- normalizedGini(a, p)
b1reg_RF4a.ngini
b1reg_RF4a.gini <-Gini(a, p)
b1reg_RF4a.gini

#Brier score
set.seed(123); b1reg_model_RF4a_brier <- train(formula, data=bene1_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF4a_brier$results
b1reg_model_RF4a_brier$resample
b1reg_pred_RF4a_brier <- predict(b1reg_model_RF4a_brier, newdata = bene1_test4, type='prob')
bene1_test4$TARGETb <- bene1_test4$TARGET
levels(bene1_test4$TARGETb) <- c('0','1')
b1reg_RF4a.bs <- Brier(as.numeric(as.character(bene1_test4$TARGETb)), b1reg_pred_RF4a_brier$X1)
b1reg_RF4a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF4b_roc <- train(formula, data=bene1_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF4b_roc <- predict(b1reg_model_RF4b_roc, bene1_train4,type="prob")
b1reg_RF4b.ROC <- roc(predictor=b1regpredb_RF4b_roc$X0,
                      response=bene1_train4$TARGET,
                      levels=rev(levels(bene1_train4$TARGET)))
b1reg_RF4b.ROC

#normalizedGini
bene1_train4$TARGETb <- bene1_train4$TARGET
levels(bene1_train4$TARGETb) <- c('0','1')
bene1_train4$TARGETb <- as.numeric(levels(bene1_train4$TARGETb))[bene1_train4$TARGETb]
set.seed(123); b1reg_model_RF4b_gini <- train(formula, data=bene1_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF4b_gini$results
b1reg_model_RF4b_gini$resample
b1reg_pred_RF4b_gini<- predict(b1reg_model_RF4b_gini, newdata = bene1_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train4$TARGETb, b1reg_pred_RF4b_gini$X1)
Gini(bene1_train4$TARGETb, b1reg_pred_RF4b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF4b_gini$X1[b1reg_pred_RF4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train4$TARGETb[b1reg_pred_RF4b_gini$X1<=0.4]))
b1reg_RF4b.ngini <- normalizedGini(a, p)
b1reg_RF4b.ngini
b1reg_RF4b.gini <-Gini(a, p)
b1reg_RF4b.gini

#Brier score
set.seed(123); b1reg_model_RF4b_brier <- train(formula, data=bene1_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF4b_brier$results
b1reg_model_RF4b_brier$resample
b1reg_pred_RF4b_brier <- predict(b1reg_model_RF4b_brier, newdata = bene1_train4, type='prob')
bene1_train4$TARGETb <- bene1_train4$TARGET
levels(bene1_train4$TARGETb) <- c('0','1')
b1reg_RF4b.bs <- Brier(as.numeric(as.character(bene1_train4$TARGETb)), b1reg_pred_RF4b_brier$X1)
b1reg_RF4b.bs

###data 5, train-test
#ROC curve 
set.seed(123); b1reg_model_RF5a_roc <- train(formula, data=bene1_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF5a_roc <- predict(b1reg_model_RF5a_roc,bene1_test5,type="prob")
b1reg_RF5a.ROC <- roc(predictor=b1regpredb_RF5a_roc$X0,
                      response=bene1_test5$TARGET,
                      levels=rev(levels(bene1_test5$TARGET)))
b1reg_RF5a.ROC

#normalizedGini
bene1_test5$TARGETb <- bene1_test5$TARGET
levels(bene1_test5$TARGETb) <- c('0','1')
bene1_test5$TARGETb <- as.numeric(levels(bene1_test5$TARGETb))[bene1_test5$TARGETb]
set.seed(123); b1reg_model_RF5a_gini <- train(formula, data=bene1_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF5a_gini$results
b1reg_model_RF5a_gini$resample

b1reg_pred_RF5a_gini<- predict(b1reg_model_RF5a_gini, newdata = bene1_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test5$TARGETb, b1reg_pred_RF5a_gini$X1)
Gini(bene1_test5$TARGETb, b1reg_pred_RF5a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF5a_gini$X1[b1reg_pred_RF5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test5$TARGETb[b1reg_pred_RF5a_gini$X1<=0.4]))
b1reg_RF5a.ngini <- normalizedGini(a, p)
b1reg_RF5a.ngini
b1reg_RF5a.gini <-Gini(a, p)
b1reg_RF5a.gini

#Brier score
set.seed(123); b1reg_model_RF5a_brier <- train(formula, data=bene1_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF5a_brier$results
b1reg_model_RF5a_brier$resample
b1reg_pred_RF5a_brier <- predict(b1reg_model_RF5a_brier, newdata = bene1_test5, type='prob')
bene1_test5$TARGETb <- bene1_test5$TARGET
levels(bene1_test5$TARGETb) <- c('0','1')
b1reg_RF5a.bs <- Brier(as.numeric(as.character(bene1_test5$TARGETb)), b1reg_pred_RF5a_brier$X1)
b1reg_RF5a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF5b_roc <- train(formula, data=bene1_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF5b_roc <- predict(b1reg_model_RF5b_roc, bene1_train5,type="prob")
b1reg_RF5b.ROC <- roc(predictor=b1regpredb_RF5b_roc$X0,
                      response=bene1_train5$TARGET,
                      levels=rev(levels(bene1_train5$TARGET)))
b1reg_RF5b.ROC

#normalizedGini
bene1_train5$TARGETb <- bene1_train5$TARGET
levels(bene1_train5$TARGETb) <- c('0','1')
bene1_train5$TARGETb <- as.numeric(levels(bene1_train5$TARGETb))[bene1_train5$TARGETb]
set.seed(123); b1reg_model_RF5b_gini <- train(formula, data=bene1_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF5b_gini$results
b1reg_model_RF5b_gini$resample
b1reg_pred_RF5b_gini<- predict(b1reg_model_RF5b_gini, newdata = bene1_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train5$TARGETb, b1reg_pred_RF5b_gini$X1)
Gini(bene1_train5$TARGETb, b1reg_pred_RF5b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF5b_gini$X1[b1reg_pred_RF5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train5$TARGETb[b1reg_pred_RF5b_gini$X1<=0.4]))
b1reg_RF5b.ngini <- normalizedGini(a, p)
b1reg_RF5b.ngini
b1reg_RF5b.gini <-Gini(a, p)
b1reg_RF5b.gini

#Brier score
set.seed(123); b1reg_model_RF5b_brier <- train(formula, data=bene1_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF5b_brier$results
b1reg_model_RF5b_brier$resample
b1reg_pred_RF5b_brier <- predict(b1reg_model_RF5b_brier, newdata = bene1_train5, type='prob')
bene1_train5$TARGETb <- bene1_train5$TARGET
levels(bene1_train5$TARGETb) <- c('0','1')
b1reg_RF5b.bs <- Brier(as.numeric(as.character(bene1_train5$TARGETb)), b1reg_pred_RF5b_brier$X1)
b1reg_RF5b.bs

###data 6, train-test
#ROC curve 
set.seed(123); b1reg_model_RF6a_roc <- train(formula, data=bene1_train6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF6a_roc <- predict(b1reg_model_RF6a_roc,bene1_test6,type="prob")
b1reg_RF6a.ROC <- roc(predictor=b1regpredb_RF6a_roc$X0,
                      response=bene1_test6$TARGET,
                      levels=rev(levels(bene1_test6$TARGET)))
b1reg_RF6a.ROC

#normalizedGini
bene1_test6$TARGETb <- bene1_test6$TARGET
levels(bene1_test6$TARGETb) <- c('0','1')
bene1_test6$TARGETb <- as.numeric(levels(bene1_test6$TARGETb))[bene1_test6$TARGETb]
set.seed(123); b1reg_model_RF6a_gini <- train(formula, data=bene1_train6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF6a_gini$results
b1reg_model_RF6a_gini$resample

b1reg_pred_RF6a_gini<- predict(b1reg_model_RF6a_gini, newdata = bene1_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test6$TARGETb, b1reg_pred_RF6a_gini$X1)
Gini(bene1_test6$TARGETb, b1reg_pred_RF6a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF6a_gini$X1[b1reg_pred_RF6a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test6$TARGETb[b1reg_pred_RF6a_gini$X1<=0.4]))
b1reg_RF6a.ngini <- normalizedGini(a, p)
b1reg_RF6a.ngini
b1reg_RF6a.gini <-Gini(a, p)
b1reg_RF6a.gini

#Brier score
set.seed(123); b1reg_model_RF6a_brier <- train(formula, data=bene1_train6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF6a_brier$results
b1reg_model_RF6a_brier$resample
b1reg_pred_RF6a_brier <- predict(b1reg_model_RF6a_brier, newdata = bene1_test6, type='prob')
bene1_test6$TARGETb <- bene1_test6$TARGET
levels(bene1_test6$TARGETb) <- c('0','1')
b1reg_RF6a.bs <- Brier(as.numeric(as.character(bene1_test6$TARGETb)), b1reg_pred_RF6a_brier$X1)
b1reg_RF6a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF6b_roc <- train(formula, data=bene1_test6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF6b_roc <- predict(b1reg_model_RF6b_roc, bene1_train6,type="prob")
b1reg_RF6b.ROC <- roc(predictor=b1regpredb_RF6b_roc$X0,
                      response=bene1_train6$TARGET,
                      levels=rev(levels(bene1_train6$TARGET)))
b1reg_RF6b.ROC

#normalizedGini
bene1_train6$TARGETb <- bene1_train6$TARGET
levels(bene1_train6$TARGETb) <- c('0','1')
bene1_train6$TARGETb <- as.numeric(levels(bene1_train6$TARGETb))[bene1_train6$TARGETb]
set.seed(123); b1reg_model_RF6b_gini <- train(formula, data=bene1_test6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF6b_gini$results
b1reg_model_RF6b_gini$resample
b1reg_pred_RF6b_gini<- predict(b1reg_model_RF6b_gini, newdata = bene1_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train6$TARGETb, b1reg_pred_RF6b_gini$X1)
Gini(bene1_train6$TARGETb, b1reg_pred_RF6b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF6b_gini$X1[b1reg_pred_RF6b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train6$TARGETb[b1reg_pred_RF6b_gini$X1<=0.4]))
b1reg_RF6b.ngini <- normalizedGini(a, p)
b1reg_RF6b.ngini
b1reg_RF6b.gini <-Gini(a, p)
b1reg_RF6b.gini

#Brier score
set.seed(123); b1reg_model_RF6b_brier <- train(formula, data=bene1_test6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF6b_brier$results
b1reg_model_RF6b_brier$resample
b1reg_pred_RF6b_brier <- predict(b1reg_model_RF6b_brier, newdata = bene1_train6, type='prob')
bene1_train6$TARGETb <- bene1_train6$TARGET
levels(bene1_train6$TARGETb) <- c('0','1')
b1reg_RF6b.bs <- Brier(as.numeric(as.character(bene1_train6$TARGETb)), b1reg_pred_RF6b_brier$X1)
b1reg_RF6b.bs

###data 7, train-test
#ROC curve 
set.seed(123); b1reg_model_RF7a_roc <- train(formula, data=bene1_train7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF7a_roc <- predict(b1reg_model_RF7a_roc,bene1_test7,type="prob")
b1reg_RF7a.ROC <- roc(predictor=b1regpredb_RF7a_roc$X0,
                      response=bene1_test7$TARGET,
                      levels=rev(levels(bene1_test7$TARGET)))
b1reg_RF7a.ROC

#normalizedGini
bene1_test7$TARGETb <- bene1_test7$TARGET
levels(bene1_test7$TARGETb) <- c('0','1')
bene1_test7$TARGETb <- as.numeric(levels(bene1_test7$TARGETb))[bene1_test7$TARGETb]
set.seed(123); b1reg_model_RF7a_gini <- train(formula, data=bene1_train7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF7a_gini$results
b1reg_model_RF7a_gini$resample

b1reg_pred_RF7a_gini<- predict(b1reg_model_RF7a_gini, newdata = bene1_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test7$TARGETb, b1reg_pred_RF7a_gini$X1)
Gini(bene1_test7$TARGETb, b1reg_pred_RF7a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF7a_gini$X1[b1reg_pred_RF7a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test7$TARGETb[b1reg_pred_RF7a_gini$X1<=0.4]))
b1reg_RF7a.ngini <- normalizedGini(a, p)
b1reg_RF7a.ngini
b1reg_RF7a.gini <-Gini(a, p)
b1reg_RF7a.gini

#Brier score
set.seed(123); b1reg_model_RF7a_brier <- train(formula, data=bene1_train7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF7a_brier$results
b1reg_model_RF7a_brier$resample
b1reg_pred_RF7a_brier <- predict(b1reg_model_RF7a_brier, newdata = bene1_test7, type='prob')
bene1_test7$TARGETb <- bene1_test7$TARGET
levels(bene1_test7$TARGETb) <- c('0','1')
b1reg_RF7a.bs <- Brier(as.numeric(as.character(bene1_test7$TARGETb)), b1reg_pred_RF7a_brier$X1)
b1reg_RF7a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF7b_roc <- train(formula, data=bene1_test7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF7b_roc <- predict(b1reg_model_RF7b_roc, bene1_train7,type="prob")
b1reg_RF7b.ROC <- roc(predictor=b1regpredb_RF7b_roc$X0,
                      response=bene1_train7$TARGET,
                      levels=rev(levels(bene1_train7$TARGET)))
b1reg_RF7b.ROC

#normalizedGini
bene1_train7$TARGETb <- bene1_train7$TARGET
levels(bene1_train7$TARGETb) <- c('0','1')
bene1_train7$TARGETb <- as.numeric(levels(bene1_train7$TARGETb))[bene1_train7$TARGETb]
set.seed(123); b1reg_model_RF7b_gini <- train(formula, data=bene1_test7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF7b_gini$results
b1reg_model_RF7b_gini$resample
b1reg_pred_RF7b_gini<- predict(b1reg_model_RF7b_gini, newdata = bene1_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train7$TARGETb, b1reg_pred_RF7b_gini$X1)
Gini(bene1_train7$TARGETb, b1reg_pred_RF7b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF7b_gini$X1[b1reg_pred_RF7b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train7$TARGETb[b1reg_pred_RF7b_gini$X1<=0.4]))
b1reg_RF7b.ngini <- normalizedGini(a, p)
b1reg_RF7b.ngini
b1reg_RF7b.gini <-Gini(a, p)
b1reg_RF7b.gini

#Brier score
set.seed(123); b1reg_model_RF7b_brier <- train(formula, data=bene1_test7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF7b_brier$results
b1reg_model_RF7b_brier$resample
b1reg_pred_RF7b_brier <- predict(b1reg_model_RF7b_brier, newdata = bene1_train7, type='prob')
bene1_train7$TARGETb <- bene1_train7$TARGET
levels(bene1_train7$TARGETb) <- c('0','1')
b1reg_RF7b.bs <- Brier(as.numeric(as.character(bene1_train7$TARGETb)), b1reg_pred_RF7b_brier$X1)
b1reg_RF7b.bs

###data 8, train-test
#ROC curve 
set.seed(123); b1reg_model_RF8a_roc <- train(formula, data=bene1_train8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF8a_roc <- predict(b1reg_model_RF8a_roc,bene1_test8,type="prob")
b1reg_RF8a.ROC <- roc(predictor=b1regpredb_RF8a_roc$X0,
                      response=bene1_test8$TARGET,
                      levels=rev(levels(bene1_test8$TARGET)))
b1reg_RF8a.ROC

#normalizedGini
bene1_test8$TARGETb <- bene1_test8$TARGET
levels(bene1_test8$TARGETb) <- c('0','1')
bene1_test8$TARGETb <- as.numeric(levels(bene1_test8$TARGETb))[bene1_test8$TARGETb]
set.seed(123); b1reg_model_RF8a_gini <- train(formula, data=bene1_train8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF8a_gini$results
b1reg_model_RF8a_gini$resample

b1reg_pred_RF8a_gini<- predict(b1reg_model_RF8a_gini, newdata = bene1_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test8$TARGETb, b1reg_pred_RF8a_gini$X1)
Gini(bene1_test8$TARGETb, b1reg_pred_RF8a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF8a_gini$X1[b1reg_pred_RF8a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test8$TARGETb[b1reg_pred_RF8a_gini$X1<=0.4]))
b1reg_RF8a.ngini <- normalizedGini(a, p)
b1reg_RF8a.ngini
b1reg_RF8a.gini <-Gini(a, p)
b1reg_RF8a.gini

#Brier score
set.seed(123); b1reg_model_RF8a_brier <- train(formula, data=bene1_train8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF8a_brier$results
b1reg_model_RF8a_brier$resample
b1reg_pred_RF8a_brier <- predict(b1reg_model_RF8a_brier, newdata = bene1_test8, type='prob')
bene1_test8$TARGETb <- bene1_test8$TARGET
levels(bene1_test8$TARGETb) <- c('0','1')
b1reg_RF8a.bs <- Brier(as.numeric(as.character(bene1_test8$TARGETb)), b1reg_pred_RF8a_brier$X1)
b1reg_RF8a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF8b_roc <- train(formula, data=bene1_test8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF8b_roc <- predict(b1reg_model_RF8b_roc, bene1_train8,type="prob")
b1reg_RF8b.ROC <- roc(predictor=b1regpredb_RF8b_roc$X0,
                      response=bene1_train8$TARGET,
                      levels=rev(levels(bene1_train8$TARGET)))
b1reg_RF8b.ROC

#normalizedGini
bene1_train8$TARGETb <- bene1_train8$TARGET
levels(bene1_train8$TARGETb) <- c('0','1')
bene1_train8$TARGETb <- as.numeric(levels(bene1_train8$TARGETb))[bene1_train8$TARGETb]
set.seed(123); b1reg_model_RF8b_gini <- train(formula, data=bene1_test8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF8b_gini$results
b1reg_model_RF8b_gini$resample
b1reg_pred_RF8b_gini<- predict(b1reg_model_RF8b_gini, newdata = bene1_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train8$TARGETb, b1reg_pred_RF8b_gini$X1)
Gini(bene1_train8$TARGETb, b1reg_pred_RF8b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF8b_gini$X1[b1reg_pred_RF8b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train8$TARGETb[b1reg_pred_RF8b_gini$X1<=0.4]))
b1reg_RF8b.ngini <- normalizedGini(a, p)
b1reg_RF8b.ngini
b1reg_RF8b.gini <-Gini(a, p)
b1reg_RF8b.gini

#Brier score
set.seed(123); b1reg_model_RF8b_brier <- train(formula, data=bene1_test8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF8b_brier$results
b1reg_model_RF8b_brier$resample
b1reg_pred_RF8b_brier <- predict(b1reg_model_RF8b_brier, newdata = bene1_train8, type='prob')
bene1_train8$TARGETb <- bene1_train8$TARGET
levels(bene1_train8$TARGETb) <- c('0','1')
b1reg_RF8b.bs <- Brier(as.numeric(as.character(bene1_train8$TARGETb)), b1reg_pred_RF8b_brier$X1)
b1reg_RF8b.bs

###data 9, train-test
#ROC curve 
set.seed(123); b1reg_model_RF9a_roc <- train(formula, data=bene1_train9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF9a_roc <- predict(b1reg_model_RF9a_roc,bene1_test9,type="prob")
b1reg_RF9a.ROC <- roc(predictor=b1regpredb_RF9a_roc$X0,
                      response=bene1_test9$TARGET,
                      levels=rev(levels(bene1_test9$TARGET)))
b1reg_RF9a.ROC

#normalizedGini
bene1_test9$TARGETb <- bene1_test9$TARGET
levels(bene1_test9$TARGETb) <- c('0','1')
bene1_test9$TARGETb <- as.numeric(levels(bene1_test9$TARGETb))[bene1_test9$TARGETb]
set.seed(123); b1reg_model_RF9a_gini <- train(formula, data=bene1_train9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF9a_gini$results
b1reg_model_RF9a_gini$resample

b1reg_pred_RF9a_gini<- predict(b1reg_model_RF9a_gini, newdata = bene1_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test9$TARGETb, b1reg_pred_RF9a_gini$X1)
Gini(bene1_test9$TARGETb, b1reg_pred_RF9a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF9a_gini$X1[b1reg_pred_RF9a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test9$TARGETb[b1reg_pred_RF9a_gini$X1<=0.4]))
b1reg_RF9a.ngini <- normalizedGini(a, p)
b1reg_RF9a.ngini
b1reg_RF9a.gini <-Gini(a, p)
b1reg_RF9a.gini

#Brier score
set.seed(123); b1reg_model_RF9a_brier <- train(formula, data=bene1_train9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF9a_brier$results
b1reg_model_RF9a_brier$resample
b1reg_pred_RF9a_brier <- predict(b1reg_model_RF9a_brier, newdata = bene1_test9, type='prob')
bene1_test9$TARGETb <- bene1_test9$TARGET
levels(bene1_test9$TARGETb) <- c('0','1')
b1reg_RF9a.bs <- Brier(as.numeric(as.character(bene1_test9$TARGETb)), b1reg_pred_RF9a_brier$X1)
b1reg_RF9a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF9b_roc <- train(formula, data=bene1_test9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF9b_roc <- predict(b1reg_model_RF9b_roc, bene1_train9,type="prob")
b1reg_RF9b.ROC <- roc(predictor=b1regpredb_RF9b_roc$X0,
                      response=bene1_train9$TARGET,
                      levels=rev(levels(bene1_train9$TARGET)))
b1reg_RF9b.ROC

#normalizedGini
bene1_train9$TARGETb <- bene1_train9$TARGET
levels(bene1_train9$TARGETb) <- c('0','1')
bene1_train9$TARGETb <- as.numeric(levels(bene1_train9$TARGETb))[bene1_train9$TARGETb]
set.seed(123); b1reg_model_RF9b_gini <- train(formula, data=bene1_test9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF9b_gini$results
b1reg_model_RF9b_gini$resample
b1reg_pred_RF9b_gini<- predict(b1reg_model_RF9b_gini, newdata = bene1_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train9$TARGETb, b1reg_pred_RF9b_gini$X1)
Gini(bene1_train9$TARGETb, b1reg_pred_RF9b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF9b_gini$X1[b1reg_pred_RF9b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train9$TARGETb[b1reg_pred_RF9b_gini$X1<=0.4]))
b1reg_RF9b.ngini <- normalizedGini(a, p)
b1reg_RF9b.ngini
b1reg_RF9b.gini <-Gini(a, p)
b1reg_RF9b.gini

#Brier score
set.seed(123); b1reg_model_RF9b_brier <- train(formula, data=bene1_test9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF9b_brier$results
b1reg_model_RF9b_brier$resample
b1reg_pred_RF9b_brier <- predict(b1reg_model_RF9b_brier, newdata = bene1_train9, type='prob')
bene1_train9$TARGETb <- bene1_train9$TARGET
levels(bene1_train9$TARGETb) <- c('0','1')
b1reg_RF9b.bs <- Brier(as.numeric(as.character(bene1_train9$TARGETb)), b1reg_pred_RF9b_brier$X1)
b1reg_RF9b.bs

###data 10, train-test
#ROC curve 
set.seed(123); b1reg_model_RF10a_roc <- train(formula, data=bene1_train10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF10a_roc <- predict(b1reg_model_RF10a_roc,bene1_test10,type="prob")
b1reg_RF10a.ROC <- roc(predictor=b1regpredb_RF10a_roc$X0,
                       response=bene1_test10$TARGET,
                       levels=rev(levels(bene1_test10$TARGET)))
b1reg_RF10a.ROC

#normalizedGini
bene1_test10$TARGETb <- bene1_test10$TARGET
levels(bene1_test10$TARGETb) <- c('0','1')
bene1_test10$TARGETb <- as.numeric(levels(bene1_test10$TARGETb))[bene1_test10$TARGETb]
set.seed(123); b1reg_model_RF10a_gini <- train(formula, data=bene1_train10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF10a_gini$results
b1reg_model_RF10a_gini$resample

b1reg_pred_RF10a_gini<- predict(b1reg_model_RF10a_gini, newdata = bene1_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_test10$TARGETb, b1reg_pred_RF10a_gini$X1)
Gini(bene1_test10$TARGETb, b1reg_pred_RF10a_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF10a_gini$X1[b1reg_pred_RF10a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_test10$TARGETb[b1reg_pred_RF10a_gini$X1<=0.4]))
b1reg_RF10a.ngini <- normalizedGini(a, p)
b1reg_RF10a.ngini
b1reg_RF10a.gini <-Gini(a, p)
b1reg_RF10a.gini

#Brier score
set.seed(123); b1reg_model_RF10a_brier <- train(formula, data=bene1_train10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF10a_brier$results
b1reg_model_RF10a_brier$resample
b1reg_pred_RF10a_brier <- predict(b1reg_model_RF10a_brier, newdata = bene1_test10, type='prob')
bene1_test10$TARGETb <- bene1_test10$TARGET
levels(bene1_test10$TARGETb) <- c('0','1')
b1reg_RF10a.bs <- Brier(as.numeric(as.character(bene1_test10$TARGETb)), b1reg_pred_RF10a_brier$X1)
b1reg_RF10a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1reg_model_RF10b_roc <- train(formula, data=bene1_test10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b1regpredb_RF10b_roc <- predict(b1reg_model_RF10b_roc, bene1_train10,type="prob")
b1reg_RF10b.ROC <- roc(predictor=b1regpredb_RF10b_roc$X0,
                       response=bene1_train10$TARGET,
                       levels=rev(levels(bene1_train10$TARGET)))
b1reg_RF10b.ROC

#normalizedGini
bene1_train10$TARGETb <- bene1_train10$TARGET
levels(bene1_train10$TARGETb) <- c('0','1')
bene1_train10$TARGETb <- as.numeric(levels(bene1_train10$TARGETb))[bene1_train10$TARGETb]
set.seed(123); b1reg_model_RF10b_gini <- train(formula, data=bene1_test10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b1reg_model_RF10b_gini$results
b1reg_model_RF10b_gini$resample
b1reg_pred_RF10b_gini<- predict(b1reg_model_RF10b_gini, newdata = bene1_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_train10$TARGETb, b1reg_pred_RF10b_gini$X1)
Gini(bene1_train10$TARGETb, b1reg_pred_RF10b_gini$X1)
#b <= 0.4
p <- b1reg_pred_RF10b_gini$X1[b1reg_pred_RF10b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_train10$TARGETb[b1reg_pred_RF10b_gini$X1<=0.4]))
b1reg_RF10b.ngini <- normalizedGini(a, p)
b1reg_RF10b.ngini
b1reg_RF10b.gini <-Gini(a, p)
b1reg_RF10b.gini

#Brier score
set.seed(123); b1reg_model_RF10b_brier <- train(formula, data=bene1_test10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1reg_model_RF10b_brier$results
b1reg_model_RF10b_brier$resample
b1reg_pred_RF10b_brier <- predict(b1reg_model_RF10b_brier, newdata = bene1_train10, type='prob')
bene1_train10$TARGETb <- bene1_train10$TARGET
levels(bene1_train10$TARGETb) <- c('0','1')
b1reg_RF10b.bs <- Brier(as.numeric(as.character(bene1_train10$TARGETb)), b1reg_pred_RF10b_brier$X1)
b1reg_RF10b.bs



##Restults RF!
b1reg_results_RF_AUC <- cbind(b1reg_RF1a.ROC$auc,b1reg_RF1b.ROC$auc,b1reg_RF2a.ROC$auc,b1reg_RF2b.ROC$auc,b1reg_RF3a.ROC$auc,b1reg_RF3b.ROC$auc,b1reg_RF4a.ROC$auc,
                              b1reg_RF4b.ROC$auc,b1reg_RF5a.ROC$auc,b1reg_RF5b.ROC$auc,b1reg_RF6a.ROC$auc,b1reg_RF6b.ROC$auc,b1reg_RF7a.ROC$auc,b1reg_RF7b.ROC$auc,
                              b1reg_RF8a.ROC$auc,b1reg_RF8b.ROC$auc,b1reg_RF9a.ROC$auc,b1reg_RF9b.ROC$auc,b1reg_RF10a.ROC$auc,b1reg_RF10b.ROC$auc)
b1reg_results_RF_bs <- cbind(b1reg_RF1a.bs,b1reg_RF1b.bs,b1reg_RF2a.bs,b1reg_RF2b.bs,b1reg_RF3a.bs,b1reg_RF3b.bs,b1reg_RF4a.bs,b1reg_RF4b.bs,b1reg_RF5a.bs,b1reg_RF5b.bs,
                             b1reg_RF6a.bs,b1reg_RF6b.bs,b1reg_RF7a.bs,b1reg_RF7b.bs,b1reg_RF8a.bs,b1reg_RF8b.bs,b1reg_RF9a.bs,b1reg_RF9b.bs,b1reg_RF10a.bs,b1reg_RF10b.bs)
b1reg_results_RF_ngini <- cbind(b1reg_RF1a.ngini,b1reg_RF1b.ngini,b1reg_RF2a.ngini,b1reg_RF2b.ngini,b1reg_RF3a.ngini,b1reg_RF3b.ngini,b1reg_RF4a.ngini,b1reg_RF4b.ngini,
                                b1reg_RF5a.ngini,b1reg_RF5b.ngini,b1reg_RF6a.ngini,b1reg_RF6b.ngini,b1reg_RF7a.ngini,b1reg_RF7b.ngini,b1reg_RF8a.ngini,b1reg_RF8b.ngini,
                                b1reg_RF9a.ngini,b1reg_RF9b.ngini,b1reg_RF10a.ngini,b1reg_RF10b.ngini)
b1reg_results_RF_gini <- cbind(b1reg_RF1a.gini, b1reg_RF1b.gini, b1reg_RF2a.gini, b1reg_RF2b.gini, b1reg_RF3a.gini,b1reg_RF3b.gini,b1reg_RF4a.gini,b1reg_RF4b.gini,
                               b1reg_RF5a.gini,b1reg_RF5b.gini,b1reg_RF6a.gini,b1reg_RF6b.gini,b1reg_RF7a.gini,b1reg_RF7b.gini,b1reg_RF8a.gini,b1reg_RF8b.gini,
                               b1reg_RF9a.gini,b1reg_RF9b.gini,b1reg_RF10a.gini,b1reg_RF10b.gini)
mean(b1reg_results_RF_AUC)
mean(b1reg_results_RF_bs)
mean(b1reg_results_RF_ngini)
mean(b1reg_results_RF_gini)


####################################
########Deep learning (MLP)#########
####################################
#functions
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


train_control_roc <- trainControl(method="cv", number=5, savePredictions=TRUE,classProbs=TRUE, summaryFunction=twoClassSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_gini <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=giniSummary, verboseIter = TRUE, allowParallel = TRUE)
train_control_brier <-  trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE, summaryFunction=brierSummary, verboseIter = TRUE, allowParallel = TRUE)
batch <- floor(nrow(bene1_woe_train1)/3)

##############################
###########MLP1###############
##############################

MLP1grid <- expand.grid(.size=c(5,10,15,20), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP11a_roc <- train(formula, data=bene1_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP1grid <- expand.grid(.size=c(10), .dropout=c(0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP11a_roc <- train(formula, data=bene1_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP11a_roc <- predict(b1_model_MLP11a_roc,bene1_woe_test1,type="prob")
b1_MLP11a.ROC <- roc(predictor=b1predb_MLP11a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_MLP11a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_MLP11a_gini <- train(formula, data=bene1_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP11a_gini$results
b1_model_MLP11a_gini$resample
b1_pred_MLP11a_gini<- predict(b1_model_MLP11a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_MLP11a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_MLP11a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP11a_gini$X1[b1_pred_MLP11a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_MLP11a_gini$X1<=0.4]))
b1_MLP11a.ngini <- normalizedGini(a, p)
b1_MLP11a.ngini
b1_MLP11a.gini <-Gini(a, p)
b1_MLP11a.gini

#Brier score
set.seed(123); b1_model_MLP11a_brier <- train(formula, data=bene1_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP11a_brier$results
b1_model_MLP11a_brier$resample
b1_pred_MLP11a_brier <- predict(b1_model_MLP11a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_MLP11a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_MLP11a_brier$X1)
b1_MLP11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_MLP11b_roc <- train(formula, data=bene1_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP11b_roc <- predict(b1_model_MLP11b_roc, bene1_woe_train1,type="prob")
b1_MLP11b.ROC <- roc(predictor=b1predb_MLP11b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_MLP11b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_MLP11b_gini <- train(formula, data=bene1_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP11b_gini$results
b1_model_MLP11b_gini$resample
b1_pred_MLP11b_gini<- predict(b1_model_MLP11b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_MLP11b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_MLP11b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP11b_gini$X1[b1_pred_MLP11b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_MLP11b_gini$X1<=0.4]))
b1_MLP11b.ngini <- normalizedGini(a, p)
b1_MLP11b.ngini
b1_MLP11b.gini <-Gini(a, p)
b1_MLP11b.gini

#Brier score
set.seed(123); b1_model_MLP11b_brier <- train(formula, data=bene1_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP11b_brier$results
b1_model_MLP11b_brier$resample
b1_pred_MLP11b_brier <- predict(b1_model_MLP11b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_MLP11b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_MLP11b_brier$X1)
b1_MLP11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP12a_roc <- train(formula, data=bene1_woe_train2, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP12a_roc <- predict(b1_model_MLP12a_roc,bene1_woe_test2,type="prob")
b1_MLP12a.ROC <- roc(predictor=b1predb_MLP12a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_MLP12a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_MLP12a_gini <- train(formula, data=bene1_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP12a_gini$results
b1_model_MLP12a_gini$resample
b1_pred_MLP12a_gini<- predict(b1_model_MLP12a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_MLP12a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_MLP12a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP12a_gini$X1[b1_pred_MLP12a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_MLP12a_gini$X1<=0.4]))
b1_MLP12a.ngini <- normalizedGini(a, p)
b1_MLP12a.ngini
b1_MLP12a.gini <-Gini(a, p)
b1_MLP12a.gini

#Brier score
set.seed(123); b1_model_MLP12a_brier <- train(formula, data=bene1_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP12a_brier$results
b1_model_MLP12a_brier$resample
b1_pred_MLP12a_brier <- predict(b1_model_MLP12a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_MLP12a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_MLP12a_brier$X1)
b1_MLP12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_MLP12b_roc <- train(formula, data=bene1_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP12b_roc <- predict(b1_model_MLP12b_roc, bene1_woe_train2,type="prob")
b1_MLP12b.ROC <- roc(predictor=b1predb_MLP12b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_MLP12b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_MLP12b_gini <- train(formula, data=bene1_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP12b_gini$results
b1_model_MLP12b_gini$resample
b1_pred_MLP12b_gini<- predict(b1_model_MLP12b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_MLP12b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_MLP12b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP12b_gini$X1[b1_pred_MLP12b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_MLP12b_gini$X1<=0.4]))
b1_MLP12b.ngini <- normalizedGini(a, p)
b1_MLP12b.ngini
b1_MLP12b.gini <-Gini(a, p)
b1_MLP12b.gini

#Brier score
set.seed(123); b1_model_MLP12b_brier <- train(formula, data=bene1_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP12b_brier$results
b1_model_MLP12b_brier$resample
b1_pred_MLP12b_brier <- predict(b1_model_MLP12b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_MLP12b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_MLP12b_brier$X1)
b1_MLP12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP13a_roc <- train(formula, data=bene1_woe_train3, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP13a_roc <- predict(b1_model_MLP13a_roc,bene1_woe_test3,type="prob")
b1_MLP13a.ROC <- roc(predictor=b1predb_MLP13a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_MLP13a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_MLP13a_gini <- train(formula, data=bene1_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP13a_gini$results
b1_model_MLP13a_gini$resample
b1_pred_MLP13a_gini<- predict(b1_model_MLP13a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_MLP13a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_MLP13a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP13a_gini$X1[b1_pred_MLP13a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_MLP13a_gini$X1<=0.4]))
b1_MLP13a.ngini <- normalizedGini(a, p)
b1_MLP13a.ngini
b1_MLP13a.gini <-Gini(a, p)
b1_MLP13a.gini

#Brier score
set.seed(123); b1_model_MLP13a_brier <- train(formula, data=bene1_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP13a_brier$results
b1_model_MLP13a_brier$resample
b1_pred_MLP13a_brier <- predict(b1_model_MLP13a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_MLP13a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_MLP13a_brier$X1)
b1_MLP13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_MLP13b_roc <- train(formula, data=bene1_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP13b_roc <- predict(b1_model_MLP13b_roc, bene1_woe_train3,type="prob")
b1_MLP13b.ROC <- roc(predictor=b1predb_MLP13b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_MLP13b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_MLP13b_gini <- train(formula, data=bene1_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP13b_gini$results
b1_model_MLP13b_gini$resample
b1_pred_MLP13b_gini<- predict(b1_model_MLP13b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_MLP13b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_MLP13b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP13b_gini$X1[b1_pred_MLP13b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_MLP13b_gini$X1<=0.4]))
b1_MLP13b.ngini <- normalizedGini(a, p)
b1_MLP13b.ngini
b1_MLP13b.gini <-Gini(a, p)
b1_MLP13b.gini

#Brier score
set.seed(123); b1_model_MLP13b_brier <- train(formula, data=bene1_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP13b_brier$results
b1_model_MLP13b_brier$resample
b1_pred_MLP13b_brier <- predict(b1_model_MLP13b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_MLP13b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_MLP13b_brier$X1)
b1_MLP13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP14a_roc <- train(formula, data=bene1_woe_train4, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP14a_roc <- predict(b1_model_MLP14a_roc,bene1_woe_test4,type="prob")
b1_MLP14a.ROC <- roc(predictor=b1predb_MLP14a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_MLP14a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_MLP14a_gini <- train(formula, data=bene1_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP14a_gini$results
b1_model_MLP14a_gini$resample
b1_pred_MLP14a_gini<- predict(b1_model_MLP14a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_MLP14a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_MLP14a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP14a_gini$X1[b1_pred_MLP14a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_MLP14a_gini$X1<=0.4]))
b1_MLP14a.ngini <- normalizedGini(a, p)
b1_MLP14a.ngini
b1_MLP14a.gini <-Gini(a, p)
b1_MLP14a.gini

#Brier score
set.seed(123); b1_model_MLP14a_brier <- train(formula, data=bene1_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP14a_brier$results
b1_model_MLP14a_brier$resample
b1_pred_MLP14a_brier <- predict(b1_model_MLP14a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_MLP14a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_MLP14a_brier$X1)
b1_MLP14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_MLP14b_roc <- train(formula, data=bene1_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP14b_roc <- predict(b1_model_MLP14b_roc, bene1_woe_train4,type="prob")
b1_MLP14b.ROC <- roc(predictor=b1predb_MLP14b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_MLP14b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_MLP14b_gini <- train(formula, data=bene1_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP14b_gini$results
b1_model_MLP14b_gini$resample
b1_pred_MLP14b_gini<- predict(b1_model_MLP14b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_MLP14b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_MLP14b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP14b_gini$X1[b1_pred_MLP14b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_MLP14b_gini$X1<=0.4]))
b1_MLP14b.ngini <- normalizedGini(a, p)
b1_MLP14b.ngini
b1_MLP14b.gini <-Gini(a, p)
b1_MLP14b.gini

#Brier score
set.seed(123); b1_model_MLP14b_brier <- train(formula, data=bene1_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP14b_brier$results
b1_model_MLP14b_brier$resample
b1_pred_MLP14b_brier <- predict(b1_model_MLP14b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_MLP14b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_MLP14b_brier$X1)
b1_MLP14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP15a_roc <- train(formula, data=bene1_woe_train5, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP15a_roc <- predict(b1_model_MLP15a_roc,bene1_woe_test5,type="prob")
b1_MLP15a.ROC <- roc(predictor=b1predb_MLP15a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_MLP15a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_MLP15a_gini <- train(formula, data=bene1_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP15a_gini$results
b1_model_MLP15a_gini$resample
b1_pred_MLP15a_gini<- predict(b1_model_MLP15a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_MLP15a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_MLP15a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP15a_gini$X1[b1_pred_MLP15a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_MLP15a_gini$X1<=0.4]))
b1_MLP15a.ngini <- normalizedGini(a, p)
b1_MLP15a.ngini
b1_MLP15a.gini <-Gini(a, p)
b1_MLP15a.gini

#Brier score
set.seed(123); b1_model_MLP15a_brier <- train(formula, data=bene1_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP15a_brier$results
b1_model_MLP15a_brier$resample
b1_pred_MLP15a_brier <- predict(b1_model_MLP15a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_MLP15a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_MLP15a_brier$X1)
b1_MLP15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_MLP15b_roc <- train(formula, data=bene1_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP15b_roc <- predict(b1_model_MLP15b_roc, bene1_woe_train5,type="prob")
b1_MLP15b.ROC <- roc(predictor=b1predb_MLP15b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_MLP15b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_MLP15b_gini <- train(formula, data=bene1_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP15b_gini$results
b1_model_MLP15b_gini$resample
b1_pred_MLP15b_gini<- predict(b1_model_MLP15b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_MLP15b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_MLP15b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP15b_gini$X1[b1_pred_MLP15b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_MLP15b_gini$X1<=0.4]))
b1_MLP15b.ngini <- normalizedGini(a, p)
b1_MLP15b.ngini
b1_MLP15b.gini <-Gini(a, p)
b1_MLP15b.gini

#Brier score
set.seed(123); b1_model_MLP15b_brier <- train(formula, data=bene1_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP15b_brier$results
b1_model_MLP15b_brier$resample
b1_pred_MLP15b_brier <- predict(b1_model_MLP15b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_MLP15b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_MLP15b_brier$X1)
b1_MLP15b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP16a_roc <- train(formula, data=bene1_woe_train6, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP16a_roc <- predict(b1_model_MLP16a_roc,bene1_woe_test6,type="prob")
b1_MLP16a.ROC <- roc(predictor=b1predb_MLP16a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_MLP16a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_MLP16a_gini <- train(formula, data=bene1_woe_train6,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP16a_gini$results
b1_model_MLP16a_gini$resample
b1_pred_MLP16a_gini<- predict(b1_model_MLP16a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_MLP16a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_MLP16a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP16a_gini$X1[b1_pred_MLP16a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_MLP16a_gini$X1<=0.4]))
b1_MLP16a.ngini <- normalizedGini(a, p)
b1_MLP16a.ngini
b1_MLP16a.gini <-Gini(a, p)
b1_MLP16a.gini

#Brier score
set.seed(123); b1_model_MLP16a_brier <- train(formula, data=bene1_woe_train6,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP16a_brier$results
b1_model_MLP16a_brier$resample
b1_pred_MLP16a_brier <- predict(b1_model_MLP16a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_MLP16a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_MLP16a_brier$X1)
b1_MLP16a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_MLP16b_roc <- train(formula, data=bene1_woe_test6,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP16b_roc <- predict(b1_model_MLP16b_roc, bene1_woe_train6,type="prob")
b1_MLP16b.ROC <- roc(predictor=b1predb_MLP16b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_MLP16b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_MLP16b_gini <- train(formula, data=bene1_woe_test6,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP16b_gini$results
b1_model_MLP16b_gini$resample
b1_pred_MLP16b_gini<- predict(b1_model_MLP16b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_MLP16b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_MLP16b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP16b_gini$X1[b1_pred_MLP16b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_MLP16b_gini$X1<=0.4]))
b1_MLP16b.ngini <- normalizedGini(a, p)
b1_MLP16b.ngini
b1_MLP16b.gini <-Gini(a, p)
b1_MLP16b.gini

#Brier score
set.seed(123); b1_model_MLP16b_brier <- train(formula, data=bene1_woe_test6,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP16b_brier$results
b1_model_MLP16b_brier$resample
b1_pred_MLP16b_brier <- predict(b1_model_MLP16b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_MLP16b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_MLP16b_brier$X1)
b1_MLP16b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP17a_roc <- train(formula, data=bene1_woe_train7, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP17a_roc <- predict(b1_model_MLP17a_roc,bene1_woe_test7,type="prob")
b1_MLP17a.ROC <- roc(predictor=b1predb_MLP17a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_MLP17a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_MLP17a_gini <- train(formula, data=bene1_woe_train7,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP17a_gini$results
b1_model_MLP17a_gini$resample
b1_pred_MLP17a_gini<- predict(b1_model_MLP17a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_MLP17a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_MLP17a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP17a_gini$X1[b1_pred_MLP17a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_MLP17a_gini$X1<=0.4]))
b1_MLP17a.ngini <- normalizedGini(a, p)
b1_MLP17a.ngini
b1_MLP17a.gini <-Gini(a, p)
b1_MLP17a.gini

#Brier score
set.seed(123); b1_model_MLP17a_brier <- train(formula, data=bene1_woe_train7,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP17a_brier$results
b1_model_MLP17a_brier$resample
b1_pred_MLP17a_brier <- predict(b1_model_MLP17a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_MLP17a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_MLP17a_brier$X1)
b1_MLP17a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_MLP17b_roc <- train(formula, data=bene1_woe_test7,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP17b_roc <- predict(b1_model_MLP17b_roc, bene1_woe_train7,type="prob")
b1_MLP17b.ROC <- roc(predictor=b1predb_MLP17b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_MLP17b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_MLP17b_gini <- train(formula, data=bene1_woe_test7,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP17b_gini$results
b1_model_MLP17b_gini$resample
b1_pred_MLP17b_gini<- predict(b1_model_MLP17b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_MLP17b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_MLP17b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP17b_gini$X1[b1_pred_MLP17b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_MLP17b_gini$X1<=0.4]))
b1_MLP17b.ngini <- normalizedGini(a, p)
b1_MLP17b.ngini
b1_MLP17b.gini <-Gini(a, p)
b1_MLP17b.gini

#Brier score
set.seed(123); b1_model_MLP17b_brier <- train(formula, data=bene1_woe_test7,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP17b_brier$results
b1_model_MLP17b_brier$resample
b1_pred_MLP17b_brier <- predict(b1_model_MLP17b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_MLP17b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_MLP17b_brier$X1)
b1_MLP17b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP18a_roc <- train(formula, data=bene1_woe_train8, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP18a_roc <- predict(b1_model_MLP18a_roc,bene1_woe_test8,type="prob")
b1_MLP18a.ROC <- roc(predictor=b1predb_MLP18a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_MLP18a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_MLP18a_gini <- train(formula, data=bene1_woe_train8,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP18a_gini$results
b1_model_MLP18a_gini$resample
b1_pred_MLP18a_gini<- predict(b1_model_MLP18a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_MLP18a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_MLP18a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP18a_gini$X1[b1_pred_MLP18a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_MLP18a_gini$X1<=0.4]))
b1_MLP18a.ngini <- normalizedGini(a, p)
b1_MLP18a.ngini
b1_MLP18a.gini <-Gini(a, p)
b1_MLP18a.gini

#Brier score
set.seed(123); b1_model_MLP18a_brier <- train(formula, data=bene1_woe_train8,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP18a_brier$results
b1_model_MLP18a_brier$resample
b1_pred_MLP18a_brier <- predict(b1_model_MLP18a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_MLP18a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_MLP18a_brier$X1)
b1_MLP18a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_MLP18b_roc <- train(formula, data=bene1_woe_test8,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP18b_roc <- predict(b1_model_MLP18b_roc, bene1_woe_train8,type="prob")
b1_MLP18b.ROC <- roc(predictor=b1predb_MLP18b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_MLP18b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_MLP18b_gini <- train(formula, data=bene1_woe_test8,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP18b_gini$results
b1_model_MLP18b_gini$resample
b1_pred_MLP18b_gini<- predict(b1_model_MLP18b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_MLP18b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_MLP18b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP18b_gini$X1[b1_pred_MLP18b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_MLP18b_gini$X1<=0.4]))
b1_MLP18b.ngini <- normalizedGini(a, p)
b1_MLP18b.ngini
b1_MLP18b.gini <-Gini(a, p)
b1_MLP18b.gini

#Brier score
set.seed(123); b1_model_MLP18b_brier <- train(formula, data=bene1_woe_test8,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP18b_brier$results
b1_model_MLP18b_brier$resample
b1_pred_MLP18b_brier <- predict(b1_model_MLP18b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_MLP18b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_MLP18b_brier$X1)
b1_MLP18b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP19a_roc <- train(formula, data=bene1_woe_train9, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP19a_roc <- predict(b1_model_MLP19a_roc,bene1_woe_test9,type="prob")
b1_MLP19a.ROC <- roc(predictor=b1predb_MLP19a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_MLP19a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_MLP19a_gini <- train(formula, data=bene1_woe_train9,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP19a_gini$results
b1_model_MLP19a_gini$resample
b1_pred_MLP19a_gini<- predict(b1_model_MLP19a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_MLP19a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_MLP19a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP19a_gini$X1[b1_pred_MLP19a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_MLP19a_gini$X1<=0.4]))
b1_MLP19a.ngini <- normalizedGini(a, p)
b1_MLP19a.ngini
b1_MLP19a.gini <-Gini(a, p)
b1_MLP19a.gini

#Brier score
set.seed(123); b1_model_MLP19a_brier <- train(formula, data=bene1_woe_train9,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP19a_brier$results
b1_model_MLP19a_brier$resample
b1_pred_MLP19a_brier <- predict(b1_model_MLP19a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_MLP19a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_MLP19a_brier$X1)
b1_MLP19a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_MLP19b_roc <- train(formula, data=bene1_woe_test9,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP19b_roc <- predict(b1_model_MLP19b_roc, bene1_woe_train9,type="prob")
b1_MLP19b.ROC <- roc(predictor=b1predb_MLP19b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_MLP19b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_MLP19b_gini <- train(formula, data=bene1_woe_test9,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP19b_gini$results
b1_model_MLP19b_gini$resample
b1_pred_MLP19b_gini<- predict(b1_model_MLP19b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_MLP19b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_MLP19b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP19b_gini$X1[b1_pred_MLP19b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_MLP19b_gini$X1<=0.4]))
b1_MLP19b.ngini <- normalizedGini(a, p)
b1_MLP19b.ngini
b1_MLP19b.gini <-Gini(a, p)
b1_MLP19b.gini

#Brier score
set.seed(123); b1_model_MLP19b_brier <- train(formula, data=bene1_woe_test9,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP19b_brier$results
b1_model_MLP19b_brier$resample
b1_pred_MLP19b_brier <- predict(b1_model_MLP19b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_MLP19b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_MLP19b_brier$X1)
b1_MLP19b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP110a_roc <- train(formula, data=bene1_woe_train10, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP110a_roc <- predict(b1_model_MLP110a_roc,bene1_woe_test10,type="prob")
b1_MLP110a.ROC <- roc(predictor=b1predb_MLP110a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_MLP110a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_MLP110a_gini <- train(formula, data=bene1_woe_train10,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP110a_gini$results
b1_model_MLP110a_gini$resample
b1_pred_MLP110a_gini<- predict(b1_model_MLP110a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_MLP110a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_MLP110a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP110a_gini$X1[b1_pred_MLP110a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_MLP110a_gini$X1<=0.4]))
b1_MLP110a.ngini <- normalizedGini(a, p)
b1_MLP110a.ngini
b1_MLP110a.gini <-Gini(a, p)
b1_MLP110a.gini

#Brier score
set.seed(123); b1_model_MLP110a_brier <- train(formula, data=bene1_woe_train10,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP110a_brier$results
b1_model_MLP110a_brier$resample
b1_pred_MLP110a_brier <- predict(b1_model_MLP110a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_MLP110a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_MLP110a_brier$X1)
b1_MLP110a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_MLP110b_roc <- train(formula, data=bene1_woe_test10,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP110b_roc <- predict(b1_model_MLP110b_roc, bene1_woe_train10,type="prob")
b1_MLP110b.ROC <- roc(predictor=b1predb_MLP110b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_MLP110b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_MLP110b_gini <- train(formula, data=bene1_woe_test10,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP110b_gini$results
b1_model_MLP110b_gini$resample
b1_pred_MLP110b_gini<- predict(b1_model_MLP110b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_MLP110b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_MLP110b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP110b_gini$X1[b1_pred_MLP110b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_MLP110b_gini$X1<=0.4]))
b1_MLP110b.ngini <- normalizedGini(a, p)
b1_MLP110b.ngini
b1_MLP110b.gini <-Gini(a, p)
b1_MLP110b.gini

#Brier score
set.seed(123); b1_model_MLP110b_brier <- train(formula, data=bene1_woe_test10,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP110b_brier$results
b1_model_MLP110b_brier$resample
b1_pred_MLP110b_brier <- predict(b1_model_MLP110b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_MLP110b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_MLP110b_brier$X1)
b1_MLP110b.bs

##Restults MLP1!
b1_results_MLP1_AUC <- cbind(b1_MLP11a.ROC$auc,b1_MLP11b.ROC$auc,b1_MLP12a.ROC$auc,b1_MLP12b.ROC$auc,b1_MLP13a.ROC$auc,b1_MLP13b.ROC$auc,b1_MLP14a.ROC$auc,
                             b1_MLP14b.ROC$auc,b1_MLP15a.ROC$auc,b1_MLP15b.ROC$auc,b1_MLP16a.ROC$auc,b1_MLP16b.ROC$auc,b1_MLP17a.ROC$auc,b1_MLP17b.ROC$auc,
                             b1_MLP18a.ROC$auc,b1_MLP18b.ROC$auc,b1_MLP19a.ROC$auc,b1_MLP19b.ROC$auc,b1_MLP110a.ROC$auc,b1_MLP110b.ROC$auc)
b1_results_MLP1_bs <- cbind(b1_MLP11a.bs,b1_MLP11b.bs,b1_MLP12a.bs,b1_MLP12b.bs,b1_MLP13a.bs,b1_MLP13b.bs,b1_MLP14a.bs,b1_MLP14b.bs,b1_MLP15a.bs,b1_MLP15b.bs,
                            b1_MLP16a.bs,b1_MLP16b.bs,b1_MLP17a.bs,b1_MLP17b.bs,b1_MLP18a.bs,b1_MLP18b.bs,b1_MLP19a.bs,b1_MLP19b.bs,b1_MLP110a.bs,b1_MLP110b.bs)
b1_results_MLP1_ngini <- cbind(b1_MLP11a.ngini,b1_MLP11b.ngini,b1_MLP12a.ngini,b1_MLP12b.ngini,b1_MLP13a.ngini,b1_MLP13b.ngini,b1_MLP14a.ngini,b1_MLP14b.ngini,
                               b1_MLP15a.ngini,b1_MLP15b.ngini,b1_MLP16a.ngini,b1_MLP16b.ngini,b1_MLP17a.ngini,b1_MLP17b.ngini,b1_MLP18a.ngini,b1_MLP18b.ngini,
                               b1_MLP19a.ngini,b1_MLP19b.ngini,b1_MLP110a.ngini,b1_MLP110b.ngini)
b1_results_MLP1_gini <- cbind(b1_MLP11a.gini,b1_MLP11b.gini,b1_MLP12a.gini,b1_MLP12b.gini,b1_MLP13a.gini,b1_MLP13b.gini,b1_MLP14a.gini,b1_MLP14b.gini,
                              b1_MLP15a.gini,b1_MLP15b.gini,b1_MLP16a.gini,b1_MLP16b.gini,b1_MLP17a.gini,b1_MLP17b.gini,b1_MLP18a.gini,b1_MLP18b.gini,
                              b1_MLP19a.gini,b1_MLP19b.gini,b1_MLP110a.gini,b1_MLP110b.gini)
mean(b1_results_MLP1_AUC)
mean(b1_results_MLP1_bs)
mean(b1_results_MLP1_ngini)
mean(b1_results_MLP1_gini)

##############################
###########MLP3###############
##############################

MLP3grid <- expand.grid(.size1=c(5,10,15,20), .size2=c(5,10,15,20), .size3=c(5,10,15,20), .dropout=c(0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP31a_roc <- train(formula, data=bene1_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best_tune
MLP3grid <- expand.grid(.size1=c(15), .size2=c(10), .size3=c(20), .dropout=c(0.25), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP31a_roc <- train(formula, data=bene1_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP31a_roc <- predict(b1_model_MLP31a_roc,bene1_woe_test1,type="prob")
b1_MLP31a.ROC <- roc(predictor=b1predb_MLP31a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_MLP31a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_MLP31a_gini <- train(formula, data=bene1_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP31a_gini$results
b1_model_MLP31a_gini$resample
b1_pred_MLP31a_gini<- predict(b1_model_MLP31a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_MLP31a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_MLP31a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP31a_gini$X1[b1_pred_MLP31a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_MLP31a_gini$X1<=0.4]))
b1_MLP31a.ngini <- normalizedGini(a, p)
b1_MLP31a.ngini
b1_MLP31a.gini <-Gini(a, p)
b1_MLP31a.gini

#Brier score
set.seed(123); b1_model_MLP31a_brier <- train(formula, data=bene1_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP31a_brier$results
b1_model_MLP31a_brier$resample
b1_pred_MLP31a_brier <- predict(b1_model_MLP31a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_MLP31a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_MLP31a_brier$X1)
b1_MLP31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_MLP31b_roc <- train(formula, data=bene1_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP31b_roc <- predict(b1_model_MLP31b_roc, bene1_woe_train1,type="prob")
b1_MLP31b.ROC <- roc(predictor=b1predb_MLP31b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_MLP31b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_MLP31b_gini <- train(formula, data=bene1_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP31b_gini$results
b1_model_MLP31b_gini$resample
b1_pred_MLP31b_gini<- predict(b1_model_MLP31b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_MLP31b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_MLP31b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP31b_gini$X1[b1_pred_MLP31b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_MLP31b_gini$X1<=0.4]))
b1_MLP31b.ngini <- normalizedGini(a, p)
b1_MLP31b.ngini
b1_MLP31b.gini <-Gini(a, p)
b1_MLP31b.gini

#Brier score
set.seed(123); b1_model_MLP31b_brier <- train(formula, data=bene1_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP31b_brier$results
b1_model_MLP31b_brier$resample
b1_pred_MLP31b_brier <- predict(b1_model_MLP31b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_MLP31b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_MLP31b_brier$X1)
b1_MLP31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP32a_roc <- train(formula, data=bene1_woe_train2, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP32a_roc <- predict(b1_model_MLP32a_roc,bene1_woe_test2,type="prob")
b1_MLP32a.ROC <- roc(predictor=b1predb_MLP32a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_MLP32a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_MLP32a_gini <- train(formula, data=bene1_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP32a_gini$results
b1_model_MLP32a_gini$resample
b1_pred_MLP32a_gini<- predict(b1_model_MLP32a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_MLP32a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_MLP32a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP32a_gini$X1[b1_pred_MLP32a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_MLP32a_gini$X1<=0.4]))
b1_MLP32a.ngini <- normalizedGini(a, p)
b1_MLP32a.ngini
b1_MLP32a.gini <-Gini(a, p)
b1_MLP32a.gini

#Brier score
set.seed(123); b1_model_MLP32a_brier <- train(formula, data=bene1_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP32a_brier$results
b1_model_MLP32a_brier$resample
b1_pred_MLP32a_brier <- predict(b1_model_MLP32a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_MLP32a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_MLP32a_brier$X1)
b1_MLP32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_MLP32b_roc <- train(formula, data=bene1_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP32b_roc <- predict(b1_model_MLP32b_roc, bene1_woe_train2,type="prob")
b1_MLP32b.ROC <- roc(predictor=b1predb_MLP32b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_MLP32b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_MLP32b_gini <- train(formula, data=bene1_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP32b_gini$results
b1_model_MLP32b_gini$resample
b1_pred_MLP32b_gini<- predict(b1_model_MLP32b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_MLP32b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_MLP32b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP32b_gini$X1[b1_pred_MLP32b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_MLP32b_gini$X1<=0.4]))
b1_MLP32b.ngini <- normalizedGini(a, p)
b1_MLP32b.ngini
b1_MLP32b.gini <-Gini(a, p)
b1_MLP32b.gini

#Brier score
set.seed(123); b1_model_MLP32b_brier <- train(formula, data=bene1_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP32b_brier$results
b1_model_MLP32b_brier$resample
b1_pred_MLP32b_brier <- predict(b1_model_MLP32b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_MLP32b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_MLP32b_brier$X1)
b1_MLP32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP33a_roc <- train(formula, data=bene1_woe_train3, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP33a_roc <- predict(b1_model_MLP33a_roc,bene1_woe_test3,type="prob")
b1_MLP33a.ROC <- roc(predictor=b1predb_MLP33a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_MLP33a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_MLP33a_gini <- train(formula, data=bene1_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP33a_gini$results
b1_model_MLP33a_gini$resample
b1_pred_MLP33a_gini<- predict(b1_model_MLP33a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_MLP33a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_MLP33a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP33a_gini$X1[b1_pred_MLP33a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_MLP33a_gini$X1<=0.4]))
b1_MLP33a.ngini <- normalizedGini(a, p)
b1_MLP33a.ngini
b1_MLP33a.gini <-Gini(a, p)
b1_MLP33a.gini

#Brier score
set.seed(123); b1_model_MLP33a_brier <- train(formula, data=bene1_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP33a_brier$results
b1_model_MLP33a_brier$resample
b1_pred_MLP33a_brier <- predict(b1_model_MLP33a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_MLP33a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_MLP33a_brier$X1)
b1_MLP33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_MLP33b_roc <- train(formula, data=bene1_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP33b_roc <- predict(b1_model_MLP33b_roc, bene1_woe_train3,type="prob")
b1_MLP33b.ROC <- roc(predictor=b1predb_MLP33b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_MLP33b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_MLP33b_gini <- train(formula, data=bene1_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP33b_gini$results
b1_model_MLP33b_gini$resample
b1_pred_MLP33b_gini<- predict(b1_model_MLP33b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_MLP33b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_MLP33b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP33b_gini$X1[b1_pred_MLP33b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_MLP33b_gini$X1<=0.4]))
b1_MLP33b.ngini <- normalizedGini(a, p)
b1_MLP33b.ngini
b1_MLP33b.gini <-Gini(a, p)
b1_MLP33b.gini

#Brier score
set.seed(123); b1_model_MLP33b_brier <- train(formula, data=bene1_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP33b_brier$results
b1_model_MLP33b_brier$resample
b1_pred_MLP33b_brier <- predict(b1_model_MLP33b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_MLP33b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_MLP33b_brier$X1)
b1_MLP33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP34a_roc <- train(formula, data=bene1_woe_train4, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP34a_roc <- predict(b1_model_MLP34a_roc,bene1_woe_test4,type="prob")
b1_MLP34a.ROC <- roc(predictor=b1predb_MLP34a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_MLP34a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_MLP34a_gini <- train(formula, data=bene1_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP34a_gini$results
b1_model_MLP34a_gini$resample
b1_pred_MLP34a_gini<- predict(b1_model_MLP34a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_MLP34a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_MLP34a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP34a_gini$X1[b1_pred_MLP34a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_MLP34a_gini$X1<=0.4]))
b1_MLP34a.ngini <- normalizedGini(a, p)
b1_MLP34a.ngini
b1_MLP34a.gini <-Gini(a, p)
b1_MLP34a.gini

#Brier score
set.seed(123); b1_model_MLP34a_brier <- train(formula, data=bene1_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP34a_brier$results
b1_model_MLP34a_brier$resample
b1_pred_MLP34a_brier <- predict(b1_model_MLP34a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_MLP34a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_MLP34a_brier$X1)
b1_MLP34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_MLP34b_roc <- train(formula, data=bene1_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP34b_roc <- predict(b1_model_MLP34b_roc, bene1_woe_train4,type="prob")
b1_MLP34b.ROC <- roc(predictor=b1predb_MLP34b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_MLP34b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_MLP34b_gini <- train(formula, data=bene1_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP34b_gini$results
b1_model_MLP34b_gini$resample
b1_pred_MLP34b_gini<- predict(b1_model_MLP34b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_MLP34b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_MLP34b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP34b_gini$X1[b1_pred_MLP34b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_MLP34b_gini$X1<=0.4]))
b1_MLP34b.ngini <- normalizedGini(a, p)
b1_MLP34b.ngini
b1_MLP34b.gini <-Gini(a, p)
b1_MLP34b.gini

#Brier score
set.seed(123); b1_model_MLP34b_brier <- train(formula, data=bene1_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP34b_brier$results
b1_model_MLP34b_brier$resample
b1_pred_MLP34b_brier <- predict(b1_model_MLP34b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_MLP34b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_MLP34b_brier$X1)
b1_MLP34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP35a_roc <- train(formula, data=bene1_woe_train5, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP35a_roc <- predict(b1_model_MLP35a_roc,bene1_woe_test5,type="prob")
b1_MLP35a.ROC <- roc(predictor=b1predb_MLP35a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_MLP35a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_MLP35a_gini <- train(formula, data=bene1_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP35a_gini$results
b1_model_MLP35a_gini$resample
b1_pred_MLP35a_gini<- predict(b1_model_MLP35a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_MLP35a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_MLP35a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP35a_gini$X1[b1_pred_MLP35a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_MLP35a_gini$X1<=0.4]))
b1_MLP35a.ngini <- normalizedGini(a, p)
b1_MLP35a.ngini
b1_MLP35a.gini <-Gini(a, p)
b1_MLP35a.gini

#Brier score
set.seed(123); b1_model_MLP35a_brier <- train(formula, data=bene1_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP35a_brier$results
b1_model_MLP35a_brier$resample
b1_pred_MLP35a_brier <- predict(b1_model_MLP35a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_MLP35a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_MLP35a_brier$X1)
b1_MLP35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_MLP35b_roc <- train(formula, data=bene1_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP35b_roc <- predict(b1_model_MLP35b_roc, bene1_woe_train5,type="prob")
b1_MLP35b.ROC <- roc(predictor=b1predb_MLP35b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_MLP35b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_MLP35b_gini <- train(formula, data=bene1_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP35b_gini$results
b1_model_MLP35b_gini$resample
b1_pred_MLP35b_gini<- predict(b1_model_MLP35b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_MLP35b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_MLP35b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP35b_gini$X1[b1_pred_MLP35b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_MLP35b_gini$X1<=0.4]))
b1_MLP35b.ngini <- normalizedGini(a, p)
b1_MLP35b.ngini
b1_MLP35b.gini <-Gini(a, p)
b1_MLP35b.gini

#Brier score
set.seed(123); b1_model_MLP35b_brier <- train(formula, data=bene1_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP35b_brier$results
b1_model_MLP35b_brier$resample
b1_pred_MLP35b_brier <- predict(b1_model_MLP35b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_MLP35b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_MLP35b_brier$X1)
b1_MLP35b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP36a_roc <- train(formula, data=bene1_woe_train6, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP36a_roc <- predict(b1_model_MLP36a_roc,bene1_woe_test6,type="prob")
b1_MLP36a.ROC <- roc(predictor=b1predb_MLP36a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_MLP36a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_MLP36a_gini <- train(formula, data=bene1_woe_train6,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP36a_gini$results
b1_model_MLP36a_gini$resample
b1_pred_MLP36a_gini<- predict(b1_model_MLP36a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_MLP36a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_MLP36a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP36a_gini$X1[b1_pred_MLP36a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_MLP36a_gini$X1<=0.4]))
b1_MLP36a.ngini <- normalizedGini(a, p)
b1_MLP36a.ngini
b1_MLP36a.gini <-Gini(a, p)
b1_MLP36a.gini

#Brier score
set.seed(123); b1_model_MLP36a_brier <- train(formula, data=bene1_woe_train6,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP36a_brier$results
b1_model_MLP36a_brier$resample
b1_pred_MLP36a_brier <- predict(b1_model_MLP36a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_MLP36a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_MLP36a_brier$X1)
b1_MLP36a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_MLP36b_roc <- train(formula, data=bene1_woe_test6,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP36b_roc <- predict(b1_model_MLP36b_roc, bene1_woe_train6,type="prob")
b1_MLP36b.ROC <- roc(predictor=b1predb_MLP36b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_MLP36b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_MLP36b_gini <- train(formula, data=bene1_woe_test6,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP36b_gini$results
b1_model_MLP36b_gini$resample
b1_pred_MLP36b_gini<- predict(b1_model_MLP36b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_MLP36b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_MLP36b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP36b_gini$X1[b1_pred_MLP36b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_MLP36b_gini$X1<=0.4]))
b1_MLP36b.ngini <- normalizedGini(a, p)
b1_MLP36b.ngini
b1_MLP36b.gini <-Gini(a, p)
b1_MLP36b.gini

#Brier score
set.seed(123); b1_model_MLP36b_brier <- train(formula, data=bene1_woe_test6,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP36b_brier$results
b1_model_MLP36b_brier$resample
b1_pred_MLP36b_brier <- predict(b1_model_MLP36b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_MLP36b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_MLP36b_brier$X1)
b1_MLP36b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP37a_roc <- train(formula, data=bene1_woe_train7, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP37a_roc <- predict(b1_model_MLP37a_roc,bene1_woe_test7,type="prob")
b1_MLP37a.ROC <- roc(predictor=b1predb_MLP37a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_MLP37a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_MLP37a_gini <- train(formula, data=bene1_woe_train7,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP37a_gini$results
b1_model_MLP37a_gini$resample
b1_pred_MLP37a_gini<- predict(b1_model_MLP37a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_MLP37a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_MLP37a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP37a_gini$X1[b1_pred_MLP37a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_MLP37a_gini$X1<=0.4]))
b1_MLP37a.ngini <- normalizedGini(a, p)
b1_MLP37a.ngini
b1_MLP37a.gini <-Gini(a, p)
b1_MLP37a.gini

#Brier score
set.seed(123); b1_model_MLP37a_brier <- train(formula, data=bene1_woe_train7,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP37a_brier$results
b1_model_MLP37a_brier$resample
b1_pred_MLP37a_brier <- predict(b1_model_MLP37a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_MLP37a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_MLP37a_brier$X1)
b1_MLP37a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_MLP37b_roc <- train(formula, data=bene1_woe_test7,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP37b_roc <- predict(b1_model_MLP37b_roc, bene1_woe_train7,type="prob")
b1_MLP37b.ROC <- roc(predictor=b1predb_MLP37b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_MLP37b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_MLP37b_gini <- train(formula, data=bene1_woe_test7,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP37b_gini$results
b1_model_MLP37b_gini$resample
b1_pred_MLP37b_gini<- predict(b1_model_MLP37b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_MLP37b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_MLP37b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP37b_gini$X1[b1_pred_MLP37b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_MLP37b_gini$X1<=0.4]))
b1_MLP37b.ngini <- normalizedGini(a, p)
b1_MLP37b.ngini
b1_MLP37b.gini <-Gini(a, p)
b1_MLP37b.gini

#Brier score
set.seed(123); b1_model_MLP37b_brier <- train(formula, data=bene1_woe_test7,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP37b_brier$results
b1_model_MLP37b_brier$resample
b1_pred_MLP37b_brier <- predict(b1_model_MLP37b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_MLP37b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_MLP37b_brier$X1)
b1_MLP37b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP38a_roc <- train(formula, data=bene1_woe_train8, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP38a_roc <- predict(b1_model_MLP38a_roc,bene1_woe_test8,type="prob")
b1_MLP38a.ROC <- roc(predictor=b1predb_MLP38a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_MLP38a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_MLP38a_gini <- train(formula, data=bene1_woe_train8,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP38a_gini$results
b1_model_MLP38a_gini$resample
b1_pred_MLP38a_gini<- predict(b1_model_MLP38a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_MLP38a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_MLP38a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP38a_gini$X1[b1_pred_MLP38a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_MLP38a_gini$X1<=0.4]))
b1_MLP38a.ngini <- normalizedGini(a, p)
b1_MLP38a.ngini
b1_MLP38a.gini <-Gini(a, p)
b1_MLP38a.gini

#Brier score
set.seed(123); b1_model_MLP38a_brier <- train(formula, data=bene1_woe_train8,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP38a_brier$results
b1_model_MLP38a_brier$resample
b1_pred_MLP38a_brier <- predict(b1_model_MLP38a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_MLP38a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_MLP38a_brier$X1)
b1_MLP38a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_MLP38b_roc <- train(formula, data=bene1_woe_test8,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP38b_roc <- predict(b1_model_MLP38b_roc, bene1_woe_train8,type="prob")
b1_MLP38b.ROC <- roc(predictor=b1predb_MLP38b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_MLP38b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_MLP38b_gini <- train(formula, data=bene1_woe_test8,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP38b_gini$results
b1_model_MLP38b_gini$resample
b1_pred_MLP38b_gini<- predict(b1_model_MLP38b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_MLP38b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_MLP38b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP38b_gini$X1[b1_pred_MLP38b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_MLP38b_gini$X1<=0.4]))
b1_MLP38b.ngini <- normalizedGini(a, p)
b1_MLP38b.ngini
b1_MLP38b.gini <-Gini(a, p)
b1_MLP38b.gini

#Brier score
set.seed(123); b1_model_MLP38b_brier <- train(formula, data=bene1_woe_test8,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP38b_brier$results
b1_model_MLP38b_brier$resample
b1_pred_MLP38b_brier <- predict(b1_model_MLP38b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_MLP38b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_MLP38b_brier$X1)
b1_MLP38b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP39a_roc <- train(formula, data=bene1_woe_train9, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP39a_roc <- predict(b1_model_MLP39a_roc,bene1_woe_test9,type="prob")
b1_MLP39a.ROC <- roc(predictor=b1predb_MLP39a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_MLP39a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_MLP39a_gini <- train(formula, data=bene1_woe_train9,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP39a_gini$results
b1_model_MLP39a_gini$resample
b1_pred_MLP39a_gini<- predict(b1_model_MLP39a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_MLP39a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_MLP39a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP39a_gini$X1[b1_pred_MLP39a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_MLP39a_gini$X1<=0.4]))
b1_MLP39a.ngini <- normalizedGini(a, p)
b1_MLP39a.ngini
b1_MLP39a.gini <-Gini(a, p)
b1_MLP39a.gini

#Brier score
set.seed(123); b1_model_MLP39a_brier <- train(formula, data=bene1_woe_train9,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP39a_brier$results
b1_model_MLP39a_brier$resample
b1_pred_MLP39a_brier <- predict(b1_model_MLP39a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_MLP39a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_MLP39a_brier$X1)
b1_MLP39a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_MLP39b_roc <- train(formula, data=bene1_woe_test9,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP39b_roc <- predict(b1_model_MLP39b_roc, bene1_woe_train9,type="prob")
b1_MLP39b.ROC <- roc(predictor=b1predb_MLP39b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_MLP39b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_MLP39b_gini <- train(formula, data=bene1_woe_test9,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP39b_gini$results
b1_model_MLP39b_gini$resample
b1_pred_MLP39b_gini<- predict(b1_model_MLP39b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_MLP39b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_MLP39b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP39b_gini$X1[b1_pred_MLP39b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_MLP39b_gini$X1<=0.4]))
b1_MLP39b.ngini <- normalizedGini(a, p)
b1_MLP39b.ngini
b1_MLP39b.gini <-Gini(a, p)
b1_MLP39b.gini

#Brier score
set.seed(123); b1_model_MLP39b_brier <- train(formula, data=bene1_woe_test9,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP39b_brier$results
b1_model_MLP39b_brier$resample
b1_pred_MLP39b_brier <- predict(b1_model_MLP39b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_MLP39b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_MLP39b_brier$X1)
b1_MLP39b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP310a_roc <- train(formula, data=bene1_woe_train10, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP310a_roc <- predict(b1_model_MLP310a_roc,bene1_woe_test10,type="prob")
b1_MLP310a.ROC <- roc(predictor=b1predb_MLP310a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_MLP310a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_MLP310a_gini <- train(formula, data=bene1_woe_train10,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP310a_gini$results
b1_model_MLP310a_gini$resample
b1_pred_MLP310a_gini<- predict(b1_model_MLP310a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_MLP310a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_MLP310a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP310a_gini$X1[b1_pred_MLP310a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_MLP310a_gini$X1<=0.4]))
b1_MLP310a.ngini <- normalizedGini(a, p)
b1_MLP310a.ngini
b1_MLP310a.gini <-Gini(a, p)
b1_MLP310a.gini

#Brier score
set.seed(123); b1_model_MLP310a_brier <- train(formula, data=bene1_woe_train10,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP310a_brier$results
b1_model_MLP310a_brier$resample
b1_pred_MLP310a_brier <- predict(b1_model_MLP310a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_MLP310a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_MLP310a_brier$X1)
b1_MLP310a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_MLP310b_roc <- train(formula, data=bene1_woe_test10,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP310b_roc <- predict(b1_model_MLP310b_roc, bene1_woe_train10,type="prob")
b1_MLP310b.ROC <- roc(predictor=b1predb_MLP310b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_MLP310b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_MLP310b_gini <- train(formula, data=bene1_woe_test10,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP310b_gini$results
b1_model_MLP310b_gini$resample
b1_pred_MLP310b_gini<- predict(b1_model_MLP310b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_MLP310b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_MLP310b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP310b_gini$X1[b1_pred_MLP310b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_MLP310b_gini$X1<=0.4]))
b1_MLP310b.ngini <- normalizedGini(a, p)
b1_MLP310b.ngini
b1_MLP310b.gini <-Gini(a, p)
b1_MLP310b.gini

#Brier score
set.seed(123); b1_model_MLP310b_brier <- train(formula, data=bene1_woe_test10,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP310b_brier$results
b1_model_MLP310b_brier$resample
b1_pred_MLP310b_brier <- predict(b1_model_MLP310b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_MLP310b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_MLP310b_brier$X1)
b1_MLP310b.bs

##Restults RF!
b1_results_MLP3_AUC <- cbind(b1_MLP31a.ROC$auc,b1_MLP31b.ROC$auc,b1_MLP32a.ROC$auc,b1_MLP32b.ROC$auc,b1_MLP33a.ROC$auc,b1_MLP33b.ROC$auc,b1_MLP34a.ROC$auc,
                             b1_MLP34b.ROC$auc,b1_MLP35a.ROC$auc,b1_MLP35b.ROC$auc, b1_MLP36a.ROC$auc,b1_MLP36b.ROC$auc,b1_MLP37a.ROC$auc,b1_MLP37b.ROC$auc,
                             b1_MLP38a.ROC$auc,b1_MLP38b.ROC$auc,b1_MLP39a.ROC$auc,b1_MLP39b.ROC$auc,b1_MLP310a.ROC$auc,b1_MLP310b.ROC$auc)
b1_results_MLP3_bs <- cbind(b1_MLP31a.bs,b1_MLP31b.bs,b1_MLP32a.bs,b1_MLP32b.bs,b1_MLP33a.bs,b1_MLP33b.bs,b1_MLP34a.bs,b1_MLP34b.bs,b1_MLP35a.bs,b1_MLP35b.bs,
                            b1_MLP36a.bs,b1_MLP36b.bs,b1_MLP37a.bs,b1_MLP37b.bs,b1_MLP38a.bs,b1_MLP38b.bs,b1_MLP39a.bs,b1_MLP39b.bs,b1_MLP310a.bs,b1_MLP310b.bs)
b1_results_MLP3_ngini <- cbind(b1_MLP31a.ngini,b1_MLP31b.ngini,b1_MLP32a.ngini,b1_MLP32b.ngini,b1_MLP33a.ngini,b1_MLP33b.ngini,b1_MLP34a.ngini,b1_MLP34b.ngini,
                               b1_MLP35a.ngini,b1_MLP35b.ngini,b1_MLP36a.ngini,b1_MLP36b.ngini,b1_MLP37a.ngini,b1_MLP37b.ngini,b1_MLP38a.ngini,b1_MLP38b.ngini,
                               b1_MLP39a.ngini,b1_MLP39b.ngini,b1_MLP310a.ngini,b1_MLP310b.ngini)
b1_results_MLP3_gini <- cbind(b1_MLP31a.gini,b1_MLP31b.gini,b1_MLP32a.gini,b1_MLP32b.gini,b1_MLP33a.gini,b1_MLP33b.gini,b1_MLP34a.gini,b1_MLP34b.gini,
                              b1_MLP35a.gini,b1_MLP35b.gini,b1_MLP36a.gini,b1_MLP36b.gini,b1_MLP37a.gini,b1_MLP37b.gini,b1_MLP38a.gini,b1_MLP38b.gini,
                              b1_MLP39a.gini,b1_MLP39b.gini,b1_MLP310a.gini,b1_MLP310b.gini)
mean(b1_results_MLP3_AUC)
mean(b1_results_MLP3_bs)
mean(b1_results_MLP3_ngini)
mean(b1_results_MLP3_gini)

##############################
###########MLP5###############
##############################

MLP5grid <- expand.grid(.size1=c(5,10,15), .size2=c(5,10,15), .size3=c(5,10,15), .size4=c(5,10,15), .size5=c(5,10,15), .dropout=c(0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP51a_roc <- train(formula, data=bene1_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP5grid <- expand.grid(.size1=c(10), .size2=c(15), .size3=c(10), .size4=c(10), .size5=c(15), .dropout=c(0), .batch_size=batch, .lr=c(0.05), .rho=0.9, .decay=0, .activation='relu')

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP51a_roc <- train(formula, data=bene1_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP51a_roc <- predict(b1_model_MLP51a_roc,bene1_woe_test1,type="prob")
b1_MLP51a.ROC <- roc(predictor=b1predb_MLP51a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_MLP51a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_MLP51a_gini <- train(formula, data=bene1_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP51a_gini$results
b1_model_MLP51a_gini$resample
b1_pred_MLP51a_gini<- predict(b1_model_MLP51a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_MLP51a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_MLP51a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP51a_gini$X1[b1_pred_MLP51a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_MLP51a_gini$X1<=0.4]))
b1_MLP51a.ngini <- normalizedGini(a, p)
b1_MLP51a.ngini
b1_MLP51a.gini <-Gini(a, p)
b1_MLP51a.gini

#Brier score
set.seed(123); b1_model_MLP51a_brier <- train(formula, data=bene1_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP51a_brier$results
b1_model_MLP51a_brier$resample
b1_pred_MLP51a_brier <- predict(b1_model_MLP51a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_MLP51a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_MLP51a_brier$X1)
b1_MLP51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_MLP51b_roc <- train(formula, data=bene1_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP51b_roc <- predict(b1_model_MLP51b_roc, bene1_woe_train1,type="prob")
b1_MLP51b.ROC <- roc(predictor=b1predb_MLP51b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_MLP51b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_MLP51b_gini <- train(formula, data=bene1_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP51b_gini$results
b1_model_MLP51b_gini$resample
b1_pred_MLP51b_gini<- predict(b1_model_MLP51b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_MLP51b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_MLP51b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP51b_gini$X1[b1_pred_MLP51b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_MLP51b_gini$X1<=0.4]))
b1_MLP51b.ngini <- normalizedGini(a, p)
b1_MLP51b.ngini
b1_MLP51b.gini <-Gini(a, p)
b1_MLP51b.gini

#Brier score
set.seed(123); b1_model_MLP51b_brier <- train(formula, data=bene1_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP51b_brier$results
b1_model_MLP51b_brier$resample
b1_pred_MLP51b_brier <- predict(b1_model_MLP51b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_MLP51b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_MLP51b_brier$X1)
b1_MLP51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP52a_roc <- train(formula, data=bene1_woe_train2, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP52a_roc <- predict(b1_model_MLP52a_roc,bene1_woe_test2,type="prob")
b1_MLP52a.ROC <- roc(predictor=b1predb_MLP52a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_MLP52a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_MLP52a_gini <- train(formula, data=bene1_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP52a_gini$results
b1_model_MLP52a_gini$resample
b1_pred_MLP52a_gini<- predict(b1_model_MLP52a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_MLP52a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_MLP52a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP52a_gini$X1[b1_pred_MLP52a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_MLP52a_gini$X1<=0.4]))
b1_MLP52a.ngini <- normalizedGini(a, p)
b1_MLP52a.ngini
b1_MLP52a.gini <-Gini(a, p)
b1_MLP52a.gini

#Brier score
set.seed(123); b1_model_MLP52a_brier <- train(formula, data=bene1_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP52a_brier$results
b1_model_MLP52a_brier$resample
b1_pred_MLP52a_brier <- predict(b1_model_MLP52a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_MLP52a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_MLP52a_brier$X1)
b1_MLP52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_MLP52b_roc <- train(formula, data=bene1_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP52b_roc <- predict(b1_model_MLP52b_roc, bene1_woe_train2,type="prob")
b1_MLP52b.ROC <- roc(predictor=b1predb_MLP52b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_MLP52b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_MLP52b_gini <- train(formula, data=bene1_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP52b_gini$results
b1_model_MLP52b_gini$resample
b1_pred_MLP52b_gini<- predict(b1_model_MLP52b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_MLP52b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_MLP52b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP52b_gini$X1[b1_pred_MLP52b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_MLP52b_gini$X1<=0.4]))
b1_MLP52b.ngini <- normalizedGini(a, p)
b1_MLP52b.ngini
b1_MLP52b.gini <-Gini(a, p)
b1_MLP52b.gini

#Brier score
set.seed(123); b1_model_MLP52b_brier <- train(formula, data=bene1_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP52b_brier$results
b1_model_MLP52b_brier$resample
b1_pred_MLP52b_brier <- predict(b1_model_MLP52b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_MLP52b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_MLP52b_brier$X1)
b1_MLP52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP53a_roc <- train(formula, data=bene1_woe_train3, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP53a_roc <- predict(b1_model_MLP53a_roc,bene1_woe_test3,type="prob")
b1_MLP53a.ROC <- roc(predictor=b1predb_MLP53a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_MLP53a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_MLP53a_gini <- train(formula, data=bene1_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP53a_gini$results
b1_model_MLP53a_gini$resample
b1_pred_MLP53a_gini<- predict(b1_model_MLP53a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_MLP53a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_MLP53a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP53a_gini$X1[b1_pred_MLP53a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_MLP53a_gini$X1<=0.4]))
b1_MLP53a.ngini <- normalizedGini(a, p)
b1_MLP53a.ngini
b1_MLP53a.gini <-Gini(a, p)
b1_MLP53a.gini

#Brier score
set.seed(123); b1_model_MLP53a_brier <- train(formula, data=bene1_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP53a_brier$results
b1_model_MLP53a_brier$resample
b1_pred_MLP53a_brier <- predict(b1_model_MLP53a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_MLP53a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_MLP53a_brier$X1)
b1_MLP53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_MLP53b_roc <- train(formula, data=bene1_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP53b_roc <- predict(b1_model_MLP53b_roc, bene1_woe_train3,type="prob")
b1_MLP53b.ROC <- roc(predictor=b1predb_MLP53b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_MLP53b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_MLP53b_gini <- train(formula, data=bene1_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP53b_gini$results
b1_model_MLP53b_gini$resample
b1_pred_MLP53b_gini<- predict(b1_model_MLP53b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_MLP53b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_MLP53b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP53b_gini$X1[b1_pred_MLP53b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_MLP53b_gini$X1<=0.4]))
b1_MLP53b.ngini <- normalizedGini(a, p)
b1_MLP53b.ngini
b1_MLP53b.gini <-Gini(a, p)
b1_MLP53b.gini

#Brier score
set.seed(123); b1_model_MLP53b_brier <- train(formula, data=bene1_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP53b_brier$results
b1_model_MLP53b_brier$resample
b1_pred_MLP53b_brier <- predict(b1_model_MLP53b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_MLP53b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_MLP53b_brier$X1)
b1_MLP53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP54a_roc <- train(formula, data=bene1_woe_train4, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP54a_roc <- predict(b1_model_MLP54a_roc,bene1_woe_test4,type="prob")
b1_MLP54a.ROC <- roc(predictor=b1predb_MLP54a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_MLP54a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_MLP54a_gini <- train(formula, data=bene1_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP54a_gini$results
b1_model_MLP54a_gini$resample
b1_pred_MLP54a_gini<- predict(b1_model_MLP54a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_MLP54a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_MLP54a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP54a_gini$X1[b1_pred_MLP54a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_MLP54a_gini$X1<=0.4]))
b1_MLP54a.ngini <- normalizedGini(a, p)
b1_MLP54a.ngini
b1_MLP54a.gini <-Gini(a, p)
b1_MLP54a.gini

#Brier score
set.seed(123); b1_model_MLP54a_brier <- train(formula, data=bene1_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP54a_brier$results
b1_model_MLP54a_brier$resample
b1_pred_MLP54a_brier <- predict(b1_model_MLP54a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_MLP54a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_MLP54a_brier$X1)
b1_MLP54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_MLP54b_roc <- train(formula, data=bene1_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP54b_roc <- predict(b1_model_MLP54b_roc, bene1_woe_train4,type="prob")
b1_MLP54b.ROC <- roc(predictor=b1predb_MLP54b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_MLP54b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_MLP54b_gini <- train(formula, data=bene1_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP54b_gini$results
b1_model_MLP54b_gini$resample
b1_pred_MLP54b_gini<- predict(b1_model_MLP54b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_MLP54b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_MLP54b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP54b_gini$X1[b1_pred_MLP54b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_MLP54b_gini$X1<=0.4]))
b1_MLP54b.ngini <- normalizedGini(a, p)
b1_MLP54b.ngini
b1_MLP54b.gini <-Gini(a, p)
b1_MLP54b.gini

#Brier score
set.seed(123); b1_model_MLP54b_brier <- train(formula, data=bene1_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP54b_brier$results
b1_model_MLP54b_brier$resample
b1_pred_MLP54b_brier <- predict(b1_model_MLP54b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_MLP54b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_MLP54b_brier$X1)
b1_MLP54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP55a_roc <- train(formula, data=bene1_woe_train5, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP55a_roc <- predict(b1_model_MLP55a_roc,bene1_woe_test5,type="prob")
b1_MLP55a.ROC <- roc(predictor=b1predb_MLP55a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_MLP55a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_MLP55a_gini <- train(formula, data=bene1_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP55a_gini$results
b1_model_MLP55a_gini$resample
b1_pred_MLP55a_gini<- predict(b1_model_MLP55a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_MLP55a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_MLP55a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP55a_gini$X1[b1_pred_MLP55a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_MLP55a_gini$X1<=0.4]))
b1_MLP55a.ngini <- normalizedGini(a, p)
b1_MLP55a.ngini
b1_MLP55a.gini <-Gini(a, p)
b1_MLP55a.gini

#Brier score
set.seed(123); b1_model_MLP55a_brier <- train(formula, data=bene1_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP55a_brier$results
b1_model_MLP55a_brier$resample
b1_pred_MLP55a_brier <- predict(b1_model_MLP55a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_MLP55a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_MLP55a_brier$X1)
b1_MLP55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_MLP55b_roc <- train(formula, data=bene1_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP55b_roc <- predict(b1_model_MLP55b_roc, bene1_woe_train5,type="prob")
b1_MLP55b.ROC <- roc(predictor=b1predb_MLP55b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_MLP55b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_MLP55b_gini <- train(formula, data=bene1_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP55b_gini$results
b1_model_MLP55b_gini$resample
b1_pred_MLP55b_gini<- predict(b1_model_MLP55b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_MLP55b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_MLP55b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP55b_gini$X1[b1_pred_MLP55b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_MLP55b_gini$X1<=0.4]))
b1_MLP55b.ngini <- normalizedGini(a, p)
b1_MLP55b.ngini
b1_MLP55b.gini <-Gini(a, p)
b1_MLP55b.gini

#Brier score
set.seed(123); b1_model_MLP55b_brier <- train(formula, data=bene1_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP55b_brier$results
b1_model_MLP55b_brier$resample
b1_pred_MLP55b_brier <- predict(b1_model_MLP55b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_MLP55b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_MLP55b_brier$X1)
b1_MLP55b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP56a_roc <- train(formula, data=bene1_woe_train6, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP56a_roc <- predict(b1_model_MLP56a_roc,bene1_woe_test6,type="prob")
b1_MLP56a.ROC <- roc(predictor=b1predb_MLP56a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_MLP56a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_MLP56a_gini <- train(formula, data=bene1_woe_train6,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP56a_gini$results
b1_model_MLP56a_gini$resample
b1_pred_MLP56a_gini<- predict(b1_model_MLP56a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_MLP56a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_MLP56a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP56a_gini$X1[b1_pred_MLP56a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_MLP56a_gini$X1<=0.4]))
b1_MLP56a.ngini <- normalizedGini(a, p)
b1_MLP56a.ngini
b1_MLP56a.gini <-Gini(a, p)
b1_MLP56a.gini

#Brier score
set.seed(123); b1_model_MLP56a_brier <- train(formula, data=bene1_woe_train6,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP56a_brier$results
b1_model_MLP56a_brier$resample
b1_pred_MLP56a_brier <- predict(b1_model_MLP56a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_MLP56a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_MLP56a_brier$X1)
b1_MLP56a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_MLP56b_roc <- train(formula, data=bene1_woe_test6,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP56b_roc <- predict(b1_model_MLP56b_roc, bene1_woe_train6,type="prob")
b1_MLP56b.ROC <- roc(predictor=b1predb_MLP56b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_MLP56b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_MLP56b_gini <- train(formula, data=bene1_woe_test6,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP56b_gini$results
b1_model_MLP56b_gini$resample
b1_pred_MLP56b_gini<- predict(b1_model_MLP56b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_MLP56b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_MLP56b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP56b_gini$X1[b1_pred_MLP56b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_MLP56b_gini$X1<=0.4]))
b1_MLP56b.ngini <- normalizedGini(a, p)
b1_MLP56b.ngini
b1_MLP56b.gini <-Gini(a, p)
b1_MLP56b.gini

#Brier score
set.seed(123); b1_model_MLP56b_brier <- train(formula, data=bene1_woe_test6,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP56b_brier$results
b1_model_MLP56b_brier$resample
b1_pred_MLP56b_brier <- predict(b1_model_MLP56b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_MLP56b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_MLP56b_brier$X1)
b1_MLP56b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP57a_roc <- train(formula, data=bene1_woe_train7, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP57a_roc <- predict(b1_model_MLP57a_roc,bene1_woe_test7,type="prob")
b1_MLP57a.ROC <- roc(predictor=b1predb_MLP57a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_MLP57a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_MLP57a_gini <- train(formula, data=bene1_woe_train7,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP57a_gini$results
b1_model_MLP57a_gini$resample
b1_pred_MLP57a_gini<- predict(b1_model_MLP57a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_MLP57a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_MLP57a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP57a_gini$X1[b1_pred_MLP57a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_MLP57a_gini$X1<=0.4]))
b1_MLP57a.ngini <- normalizedGini(a, p)
b1_MLP57a.ngini
b1_MLP57a.gini <-Gini(a, p)
b1_MLP57a.gini

#Brier score
set.seed(123); b1_model_MLP57a_brier <- train(formula, data=bene1_woe_train7,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP57a_brier$results
b1_model_MLP57a_brier$resample
b1_pred_MLP57a_brier <- predict(b1_model_MLP57a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_MLP57a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_MLP57a_brier$X1)
b1_MLP57a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_MLP57b_roc <- train(formula, data=bene1_woe_test7,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP57b_roc <- predict(b1_model_MLP57b_roc, bene1_woe_train7,type="prob")
b1_MLP57b.ROC <- roc(predictor=b1predb_MLP57b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_MLP57b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_MLP57b_gini <- train(formula, data=bene1_woe_test7,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP57b_gini$results
b1_model_MLP57b_gini$resample
b1_pred_MLP57b_gini<- predict(b1_model_MLP57b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_MLP57b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_MLP57b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP57b_gini$X1[b1_pred_MLP57b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_MLP57b_gini$X1<=0.4]))
b1_MLP57b.ngini <- normalizedGini(a, p)
b1_MLP57b.ngini
b1_MLP57b.gini <-Gini(a, p)
b1_MLP57b.gini

#Brier score
set.seed(123); b1_model_MLP57b_brier <- train(formula, data=bene1_woe_test7,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP57b_brier$results
b1_model_MLP57b_brier$resample
b1_pred_MLP57b_brier <- predict(b1_model_MLP57b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_MLP57b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_MLP57b_brier$X1)
b1_MLP57b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP58a_roc <- train(formula, data=bene1_woe_train8, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP58a_roc <- predict(b1_model_MLP58a_roc,bene1_woe_test8,type="prob")
b1_MLP58a.ROC <- roc(predictor=b1predb_MLP58a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_MLP58a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_MLP58a_gini <- train(formula, data=bene1_woe_train8,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP58a_gini$results
b1_model_MLP58a_gini$resample
b1_pred_MLP58a_gini<- predict(b1_model_MLP58a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_MLP58a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_MLP58a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP58a_gini$X1[b1_pred_MLP58a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_MLP58a_gini$X1<=0.4]))
b1_MLP58a.ngini <- normalizedGini(a, p)
b1_MLP58a.ngini
b1_MLP58a.gini <-Gini(a, p)
b1_MLP58a.gini

#Brier score
set.seed(123); b1_model_MLP58a_brier <- train(formula, data=bene1_woe_train8,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP58a_brier$results
b1_model_MLP58a_brier$resample
b1_pred_MLP58a_brier <- predict(b1_model_MLP58a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_MLP58a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_MLP58a_brier$X1)
b1_MLP58a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_MLP58b_roc <- train(formula, data=bene1_woe_test8,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP58b_roc <- predict(b1_model_MLP58b_roc, bene1_woe_train8,type="prob")
b1_MLP58b.ROC <- roc(predictor=b1predb_MLP58b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_MLP58b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_MLP58b_gini <- train(formula, data=bene1_woe_test8,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP58b_gini$results
b1_model_MLP58b_gini$resample
b1_pred_MLP58b_gini<- predict(b1_model_MLP58b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_MLP58b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_MLP58b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP58b_gini$X1[b1_pred_MLP58b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_MLP58b_gini$X1<=0.4]))
b1_MLP58b.ngini <- normalizedGini(a, p)
b1_MLP58b.ngini
b1_MLP58b.gini <-Gini(a, p)
b1_MLP58b.gini

#Brier score
set.seed(123); b1_model_MLP58b_brier <- train(formula, data=bene1_woe_test8,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP58b_brier$results
b1_model_MLP58b_brier$resample
b1_pred_MLP58b_brier <- predict(b1_model_MLP58b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_MLP58b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_MLP58b_brier$X1)
b1_MLP58b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP59a_roc <- train(formula, data=bene1_woe_train9, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP59a_roc <- predict(b1_model_MLP59a_roc,bene1_woe_test9,type="prob")
b1_MLP59a.ROC <- roc(predictor=b1predb_MLP59a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_MLP59a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_MLP59a_gini <- train(formula, data=bene1_woe_train9,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP59a_gini$results
b1_model_MLP59a_gini$resample
b1_pred_MLP59a_gini<- predict(b1_model_MLP59a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_MLP59a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_MLP59a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP59a_gini$X1[b1_pred_MLP59a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_MLP59a_gini$X1<=0.4]))
b1_MLP59a.ngini <- normalizedGini(a, p)
b1_MLP59a.ngini
b1_MLP59a.gini <-Gini(a, p)
b1_MLP59a.gini

#Brier score
set.seed(123); b1_model_MLP59a_brier <- train(formula, data=bene1_woe_train9,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP59a_brier$results
b1_model_MLP59a_brier$resample
b1_pred_MLP59a_brier <- predict(b1_model_MLP59a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_MLP59a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_MLP59a_brier$X1)
b1_MLP59a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_MLP59b_roc <- train(formula, data=bene1_woe_test9,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP59b_roc <- predict(b1_model_MLP59b_roc, bene1_woe_train9,type="prob")
b1_MLP59b.ROC <- roc(predictor=b1predb_MLP59b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_MLP59b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_MLP59b_gini <- train(formula, data=bene1_woe_test9,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP59b_gini$results
b1_model_MLP59b_gini$resample
b1_pred_MLP59b_gini<- predict(b1_model_MLP59b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_MLP59b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_MLP59b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP59b_gini$X1[b1_pred_MLP59b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_MLP59b_gini$X1<=0.4]))
b1_MLP59b.ngini <- normalizedGini(a, p)
b1_MLP59b.ngini
b1_MLP59b.gini <-Gini(a, p)
b1_MLP59b.gini

#Brier score
set.seed(123); b1_model_MLP59b_brier <- train(formula, data=bene1_woe_test9,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP59b_brier$results
b1_model_MLP59b_brier$resample
b1_pred_MLP59b_brier <- predict(b1_model_MLP59b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_MLP59b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_MLP59b_brier$X1)
b1_MLP59b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_MLP510a_roc <- train(formula, data=bene1_woe_train10, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP510a_roc <- predict(b1_model_MLP510a_roc,bene1_woe_test10,type="prob")
b1_MLP510a.ROC <- roc(predictor=b1predb_MLP510a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_MLP510a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_MLP510a_gini <- train(formula, data=bene1_woe_train10,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP510a_gini$results
b1_model_MLP510a_gini$resample
b1_pred_MLP510a_gini<- predict(b1_model_MLP510a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_MLP510a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_MLP510a_gini$X1)
#b <= 0.4
p <- b1_pred_MLP510a_gini$X1[b1_pred_MLP510a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_MLP510a_gini$X1<=0.4]))
b1_MLP510a.ngini <- normalizedGini(a, p)
b1_MLP510a.ngini
b1_MLP510a.gini <-Gini(a, p)
b1_MLP510a.gini

#Brier score
set.seed(123); b1_model_MLP510a_brier <- train(formula, data=bene1_woe_train10,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP510a_brier$results
b1_model_MLP510a_brier$resample
b1_pred_MLP510a_brier <- predict(b1_model_MLP510a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_MLP510a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_MLP510a_brier$X1)
b1_MLP510a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_MLP510b_roc <- train(formula, data=bene1_woe_test10,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_MLP510b_roc <- predict(b1_model_MLP510b_roc, bene1_woe_train10,type="prob")
b1_MLP510b.ROC <- roc(predictor=b1predb_MLP510b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_MLP510b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_MLP510b_gini <- train(formula, data=bene1_woe_test10,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_MLP510b_gini$results
b1_model_MLP510b_gini$resample
b1_pred_MLP510b_gini<- predict(b1_model_MLP510b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_MLP510b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_MLP510b_gini$X1)
#b <= 0.4
p <- b1_pred_MLP510b_gini$X1[b1_pred_MLP510b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_MLP510b_gini$X1<=0.4]))
b1_MLP510b.ngini <- normalizedGini(a, p)
b1_MLP510b.ngini
b1_MLP510b.gini <-Gini(a, p)
b1_MLP510b.gini

#Brier score
set.seed(123); b1_model_MLP510b_brier <- train(formula, data=bene1_woe_test10,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_MLP510b_brier$results
b1_model_MLP510b_brier$resample
b1_pred_MLP510b_brier <- predict(b1_model_MLP510b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_MLP510b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_MLP510b_brier$X1)
b1_MLP510b.bs

##Restults RF!
b1_results_MLP5_AUC <- cbind(b1_MLP51a.ROC$auc,b1_MLP51b.ROC$auc,b1_MLP52a.ROC$auc,b1_MLP52b.ROC$auc,b1_MLP53a.ROC$auc,b1_MLP53b.ROC$auc,b1_MLP54a.ROC$auc,
                             b1_MLP54b.ROC$auc,b1_MLP55a.ROC$auc,b1_MLP55b.ROC$auc,b1_MLP56a.ROC$auc,b1_MLP56b.ROC$auc,b1_MLP57a.ROC$auc,b1_MLP57b.ROC$auc,
                             b1_MLP58a.ROC$auc,b1_MLP58b.ROC$auc,b1_MLP59a.ROC$auc,b1_MLP59b.ROC$auc,b1_MLP510a.ROC$auc,b1_MLP510b.ROC$auc)
b1_results_MLP5_bs <- cbind(b1_MLP51a.bs,b1_MLP51b.bs,b1_MLP52a.bs,b1_MLP52b.bs,b1_MLP53a.bs,b1_MLP53b.bs,b1_MLP54a.bs,b1_MLP54b.bs,b1_MLP55a.bs,b1_MLP55b.bs,
                            b1_MLP56a.bs,b1_MLP56b.bs,b1_MLP57a.bs,b1_MLP57b.bs,b1_MLP58a.bs,b1_MLP58b.bs,b1_MLP59a.bs,b1_MLP59b.bs,b1_MLP510a.bs,b1_MLP510b.bs)
b1_results_MLP5_ngini <- cbind(b1_MLP51a.ngini,b1_MLP51b.ngini,b1_MLP52a.ngini,b1_MLP52b.ngini,b1_MLP53a.ngini,b1_MLP53b.ngini,b1_MLP54a.ngini,b1_MLP54b.ngini,
                               b1_MLP55a.ngini,b1_MLP55b.ngini,b1_MLP56a.ngini,b1_MLP56b.ngini,b1_MLP57a.ngini,b1_MLP57b.ngini,b1_MLP58a.ngini,b1_MLP58b.ngini,
                               b1_MLP59a.ngini,b1_MLP59b.ngini,b1_MLP510a.ngini,b1_MLP510b.ngini)
b1_results_MLP5_gini <- cbind(b1_MLP51a.gini,b1_MLP51b.gini,b1_MLP52a.gini,b1_MLP52b.gini,b1_MLP53a.gini,b1_MLP53b.gini,b1_MLP54a.gini,b1_MLP54b.gini,
                              b1_MLP55a.gini,b1_MLP55b.gini,b1_MLP56a.gini,b1_MLP56b.gini,b1_MLP57a.gini,b1_MLP57b.gini,b1_MLP58a.gini,b1_MLP58b.gini,
                              b1_MLP59a.gini,b1_MLP59b.gini,b1_MLP510a.gini,b1_MLP510b.gini)
b1_results_MLP5_gini <- cbind(b1_MLP51a.gini,b1_MLP51b.gini,b1_MLP52a.gini,b1_MLP52b.gini,b1_MLP53a.gini,b1_MLP53b.gini,b1_MLP54a.gini,b1_MLP54b.gini,
                              b1_MLP55a.gini,b1_MLP55b.gini,b1_MLP56a.gini,b1_MLP56b.gini,b1_MLP57b.gini,b1_MLP58a.gini,b1_MLP58b.gini,
                              b1_MLP59a.gini,b1_MLP59b.gini,b1_MLP510a.gini,b1_MLP510b.gini)
mean(b1_results_MLP5_AUC)
mean(b1_results_MLP5_bs)
#mean(b1_results_MLP5_ngini)
mean(b1_results_MLP5_gini)

##############################
###########DBN################
##############################

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


DBN1grid <- expand.grid(.layer1=c(5,10,15,20), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0,0.5))
set.seed(123); b1_model_DBN11a_roc <- train(formula, data=bene1_woe_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN1grid <- expand.grid(.layer1=c(20), .hidden_dropout=c(0), .visible_dropout=c(0.25), .lr=c(1.0))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN11a_roc <- train(formula, data=bene1_woe_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN11a_roc <- predict(b1_model_DBN11a_roc,bene1_woe_test1,type="prob")
b1_DBN11a.ROC <- roc(predictor=b1predb_DBN11a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_DBN11a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_DBN11a_gini <- train(formula, data=bene1_woe_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN11a_gini$results
b1_model_DBN11a_gini$resample
b1_pred_DBN11a_gini<- predict(b1_model_DBN11a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_DBN11a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_DBN11a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN11a_gini$X1[b1_pred_DBN11a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_DBN11a_gini$X1<=0.4]))
b1_DBN11a.ngini <- normalizedGini(a, p)
b1_DBN11a.ngini
b1_DBN11a.gini <-Gini(a, p)
b1_DBN11a.gini

#Brier score
set.seed(123); b1_model_DBN11a_brier <- train(formula, data=bene1_woe_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN11a_brier$results
b1_model_DBN11a_brier$resample
b1_pred_DBN11a_brier <- predict(b1_model_DBN11a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_DBN11a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_DBN11a_brier$X1)
b1_DBN11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_DBN11b_roc <- train(formula, data=bene1_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN11b_roc <- predict(b1_model_DBN11b_roc, bene1_woe_train1,type="prob")
b1_DBN11b.ROC <- roc(predictor=b1predb_DBN11b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_DBN11b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_DBN11b_gini <- train(formula, data=bene1_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN11b_gini$results
b1_model_DBN11b_gini$resample
b1_pred_DBN11b_gini<- predict(b1_model_DBN11b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_DBN11b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_DBN11b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN11b_gini$X1[b1_pred_DBN11b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_DBN11b_gini$X1<=0.4]))
b1_DBN11b.ngini <- normalizedGini(a, p)
b1_DBN11b.ngini
b1_DBN11b.gini <-Gini(a, p)
b1_DBN11b.gini

#Brier score
set.seed(123); b1_model_DBN11b_brier <- train(formula, data=bene1_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN11b_brier$results
b1_model_DBN11b_brier$resample
b1_pred_DBN11b_brier <- predict(b1_model_DBN11b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_DBN11b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_DBN11b_brier$X1)
b1_DBN11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN12a_roc <- train(formula, data=bene1_woe_train2, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN12a_roc <- predict(b1_model_DBN12a_roc,bene1_woe_test2,type="prob")
b1_DBN12a.ROC <- roc(predictor=b1predb_DBN12a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_DBN12a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_DBN12a_gini <- train(formula, data=bene1_woe_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN12a_gini$results
b1_model_DBN12a_gini$resample
b1_pred_DBN12a_gini<- predict(b1_model_DBN12a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_DBN12a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_DBN12a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN12a_gini$X1[b1_pred_DBN12a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_DBN12a_gini$X1<=0.4]))
b1_DBN12a.ngini <- normalizedGini(a, p)
b1_DBN12a.ngini
b1_DBN12a.gini <-Gini(a, p)
b1_DBN12a.gini

#Brier score
set.seed(123); b1_model_DBN12a_brier <- train(formula, data=bene1_woe_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN12a_brier$results
b1_model_DBN12a_brier$resample
b1_pred_DBN12a_brier <- predict(b1_model_DBN12a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_DBN12a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_DBN12a_brier$X1)
b1_DBN12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_DBN12b_roc <- train(formula, data=bene1_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN12b_roc <- predict(b1_model_DBN12b_roc, bene1_woe_train2,type="prob")
b1_DBN12b.ROC <- roc(predictor=b1predb_DBN12b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_DBN12b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_DBN12b_gini <- train(formula, data=bene1_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN12b_gini$results
b1_model_DBN12b_gini$resample
b1_pred_DBN12b_gini<- predict(b1_model_DBN12b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_DBN12b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_DBN12b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN12b_gini$X1[b1_pred_DBN12b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_DBN12b_gini$X1<=0.4]))
b1_DBN12b.ngini <- normalizedGini(a, p)
b1_DBN12b.ngini
b1_DBN12b.gini <-Gini(a, p)
b1_DBN12b.gini

#Brier score
set.seed(123); b1_model_DBN12b_brier <- train(formula, data=bene1_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN12b_brier$results
b1_model_DBN12b_brier$resample
b1_pred_DBN12b_brier <- predict(b1_model_DBN12b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_DBN12b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_DBN12b_brier$X1)
b1_DBN12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN13a_roc <- train(formula, data=bene1_woe_train3, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN13a_roc <- predict(b1_model_DBN13a_roc,bene1_woe_test3,type="prob")
b1_DBN13a.ROC <- roc(predictor=b1predb_DBN13a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_DBN13a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_DBN13a_gini <- train(formula, data=bene1_woe_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN13a_gini$results
b1_model_DBN13a_gini$resample
b1_pred_DBN13a_gini<- predict(b1_model_DBN13a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_DBN13a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_DBN13a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN13a_gini$X1[b1_pred_DBN13a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_DBN13a_gini$X1<=0.4]))
b1_DBN13a.ngini <- normalizedGini(a, p)
b1_DBN13a.ngini
b1_DBN13a.gini <-Gini(a, p)
b1_DBN13a.gini

#Brier score
set.seed(123); b1_model_DBN13a_brier <- train(formula, data=bene1_woe_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN13a_brier$results
b1_model_DBN13a_brier$resample
b1_pred_DBN13a_brier <- predict(b1_model_DBN13a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_DBN13a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_DBN13a_brier$X1)
b1_DBN13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_DBN13b_roc <- train(formula, data=bene1_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN13b_roc <- predict(b1_model_DBN13b_roc, bene1_woe_train3,type="prob")
b1_DBN13b.ROC <- roc(predictor=b1predb_DBN13b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_DBN13b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_DBN13b_gini <- train(formula, data=bene1_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN13b_gini$results
b1_model_DBN13b_gini$resample
b1_pred_DBN13b_gini<- predict(b1_model_DBN13b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_DBN13b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_DBN13b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN13b_gini$X1[b1_pred_DBN13b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_DBN13b_gini$X1<=0.4]))
b1_DBN13b.ngini <- normalizedGini(a, p)
b1_DBN13b.ngini
b1_DBN13b.gini <-Gini(a, p)
b1_DBN13b.gini

#Brier score
set.seed(123); b1_model_DBN13b_brier <- train(formula, data=bene1_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN13b_brier$results
b1_model_DBN13b_brier$resample
b1_pred_DBN13b_brier <- predict(b1_model_DBN13b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_DBN13b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_DBN13b_brier$X1)
b1_DBN13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN14a_roc <- train(formula, data=bene1_woe_train4, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN14a_roc <- predict(b1_model_DBN14a_roc,bene1_woe_test4,type="prob")
b1_DBN14a.ROC <- roc(predictor=b1predb_DBN14a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_DBN14a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_DBN14a_gini <- train(formula, data=bene1_woe_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN14a_gini$results
b1_model_DBN14a_gini$resample
b1_pred_DBN14a_gini<- predict(b1_model_DBN14a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_DBN14a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_DBN14a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN14a_gini$X1[b1_pred_DBN14a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_DBN14a_gini$X1<=0.4]))
b1_DBN14a.ngini <- normalizedGini(a, p)
b1_DBN14a.ngini
b1_DBN14a.gini <-Gini(a, p)
b1_DBN14a.gini

#Brier score
set.seed(123); b1_model_DBN14a_brier <- train(formula, data=bene1_woe_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN14a_brier$results
b1_model_DBN14a_brier$resample
b1_pred_DBN14a_brier <- predict(b1_model_DBN14a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_DBN14a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_DBN14a_brier$X1)
b1_DBN14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_DBN14b_roc <- train(formula, data=bene1_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN14b_roc <- predict(b1_model_DBN14b_roc, bene1_woe_train4,type="prob")
b1_DBN14b.ROC <- roc(predictor=b1predb_DBN14b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_DBN14b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_DBN14b_gini <- train(formula, data=bene1_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN14b_gini$results
b1_model_DBN14b_gini$resample
b1_pred_DBN14b_gini<- predict(b1_model_DBN14b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_DBN14b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_DBN14b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN14b_gini$X1[b1_pred_DBN14b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_DBN14b_gini$X1<=0.4]))
b1_DBN14b.ngini <- normalizedGini(a, p)
b1_DBN14b.ngini
b1_DBN14b.gini <-Gini(a, p)
b1_DBN14b.gini

#Brier score
set.seed(123); b1_model_DBN14b_brier <- train(formula, data=bene1_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN14b_brier$results
b1_model_DBN14b_brier$resample
b1_pred_DBN14b_brier <- predict(b1_model_DBN14b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_DBN14b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_DBN14b_brier$X1)
b1_DBN14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN15a_roc <- train(formula, data=bene1_woe_train5, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN15a_roc <- predict(b1_model_DBN15a_roc,bene1_woe_test5,type="prob")
b1_DBN15a.ROC <- roc(predictor=b1predb_DBN15a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_DBN15a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_DBN15a_gini <- train(formula, data=bene1_woe_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN15a_gini$results
b1_model_DBN15a_gini$resample
b1_pred_DBN15a_gini<- predict(b1_model_DBN15a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_DBN15a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_DBN15a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN15a_gini$X1[b1_pred_DBN15a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_DBN15a_gini$X1<=0.4]))
b1_DBN15a.ngini <- normalizedGini(a, p)
b1_DBN15a.ngini
b1_DBN15a.gini <-Gini(a, p)
b1_DBN15a.gini

#Brier score
set.seed(123); b1_model_DBN15a_brier <- train(formula, data=bene1_woe_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN15a_brier$results
b1_model_DBN15a_brier$resample
b1_pred_DBN15a_brier <- predict(b1_model_DBN15a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_DBN15a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_DBN15a_brier$X1)
b1_DBN15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_DBN15b_roc <- train(formula, data=bene1_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN15b_roc <- predict(b1_model_DBN15b_roc, bene1_woe_train5,type="prob")
b1_DBN15b.ROC <- roc(predictor=b1predb_DBN15b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_DBN15b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_DBN15b_gini <- train(formula, data=bene1_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN15b_gini$results
b1_model_DBN15b_gini$resample
b1_pred_DBN15b_gini<- predict(b1_model_DBN15b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_DBN15b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_DBN15b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN15b_gini$X1[b1_pred_DBN15b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_DBN15b_gini$X1<=0.4]))
b1_DBN15b.ngini <- normalizedGini(a, p)
b1_DBN15b.ngini
b1_DBN15b.gini <-Gini(a, p)
b1_DBN15b.gini

#Brier score
set.seed(123); b1_model_DBN15b_brier <- train(formula, data=bene1_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN15b_brier$results
b1_model_DBN15b_brier$resample
b1_pred_DBN15b_brier <- predict(b1_model_DBN15b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_DBN15b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_DBN15b_brier$X1)
b1_DBN15b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN16a_roc <- train(formula, data=bene1_woe_train6, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN16a_roc <- predict(b1_model_DBN16a_roc,bene1_woe_test6,type="prob")
b1_DBN16a.ROC <- roc(predictor=b1predb_DBN16a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_DBN16a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_DBN16a_gini <- train(formula, data=bene1_woe_train6,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN16a_gini$results
b1_model_DBN16a_gini$resample
b1_pred_DBN16a_gini<- predict(b1_model_DBN16a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_DBN16a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_DBN16a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN16a_gini$X1[b1_pred_DBN16a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_DBN16a_gini$X1<=0.4]))
b1_DBN16a.ngini <- normalizedGini(a, p)
b1_DBN16a.ngini
b1_DBN16a.gini <-Gini(a, p)
b1_DBN16a.gini

#Brier score
set.seed(123); b1_model_DBN16a_brier <- train(formula, data=bene1_woe_train6,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN16a_brier$results
b1_model_DBN16a_brier$resample
b1_pred_DBN16a_brier <- predict(b1_model_DBN16a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_DBN16a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_DBN16a_brier$X1)
b1_DBN16a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_DBN16b_roc <- train(formula, data=bene1_woe_test6,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN16b_roc <- predict(b1_model_DBN16b_roc, bene1_woe_train6,type="prob")
b1_DBN16b.ROC <- roc(predictor=b1predb_DBN16b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_DBN16b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_DBN16b_gini <- train(formula, data=bene1_woe_test6,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN16b_gini$results
b1_model_DBN16b_gini$resample
b1_pred_DBN16b_gini<- predict(b1_model_DBN16b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_DBN16b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_DBN16b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN16b_gini$X1[b1_pred_DBN16b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_DBN16b_gini$X1<=0.4]))
b1_DBN16b.ngini <- normalizedGini(a, p)
b1_DBN16b.ngini
b1_DBN16b.gini <-Gini(a, p)
b1_DBN16b.gini

#Brier score
set.seed(123); b1_model_DBN16b_brier <- train(formula, data=bene1_woe_test6,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN16b_brier$results
b1_model_DBN16b_brier$resample
b1_pred_DBN16b_brier <- predict(b1_model_DBN16b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_DBN16b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_DBN16b_brier$X1)
b1_DBN16b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN17a_roc <- train(formula, data=bene1_woe_train7, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN17a_roc <- predict(b1_model_DBN17a_roc,bene1_woe_test7,type="prob")
b1_DBN17a.ROC <- roc(predictor=b1predb_DBN17a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_DBN17a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_DBN17a_gini <- train(formula, data=bene1_woe_train7,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN17a_gini$results
b1_model_DBN17a_gini$resample
b1_pred_DBN17a_gini<- predict(b1_model_DBN17a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_DBN17a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_DBN17a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN17a_gini$X1[b1_pred_DBN17a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_DBN17a_gini$X1<=0.4]))
b1_DBN17a.ngini <- normalizedGini(a, p)
b1_DBN17a.ngini
b1_DBN17a.gini <-Gini(a, p)
b1_DBN17a.gini

#Brier score
set.seed(123); b1_model_DBN17a_brier <- train(formula, data=bene1_woe_train7,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN17a_brier$results
b1_model_DBN17a_brier$resample
b1_pred_DBN17a_brier <- predict(b1_model_DBN17a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_DBN17a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_DBN17a_brier$X1)
b1_DBN17a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_DBN17b_roc <- train(formula, data=bene1_woe_test7,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN17b_roc <- predict(b1_model_DBN17b_roc, bene1_woe_train7,type="prob")
b1_DBN17b.ROC <- roc(predictor=b1predb_DBN17b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_DBN17b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_DBN17b_gini <- train(formula, data=bene1_woe_test7,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN17b_gini$results
b1_model_DBN17b_gini$resample
b1_pred_DBN17b_gini<- predict(b1_model_DBN17b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_DBN17b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_DBN17b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN17b_gini$X1[b1_pred_DBN17b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_DBN17b_gini$X1<=0.4]))
b1_DBN17b.ngini <- normalizedGini(a, p)
b1_DBN17b.ngini
b1_DBN17b.gini <-Gini(a, p)
b1_DBN17b.gini

#Brier score
set.seed(123); b1_model_DBN17b_brier <- train(formula, data=bene1_woe_test7,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN17b_brier$results
b1_model_DBN17b_brier$resample
b1_pred_DBN17b_brier <- predict(b1_model_DBN17b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_DBN17b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_DBN17b_brier$X1)
b1_DBN17b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN18a_roc <- train(formula, data=bene1_woe_train8, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN18a_roc <- predict(b1_model_DBN18a_roc,bene1_woe_test8,type="prob")
b1_DBN18a.ROC <- roc(predictor=b1predb_DBN18a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_DBN18a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_DBN18a_gini <- train(formula, data=bene1_woe_train8,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN18a_gini$results
b1_model_DBN18a_gini$resample
b1_pred_DBN18a_gini<- predict(b1_model_DBN18a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_DBN18a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_DBN18a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN18a_gini$X1[b1_pred_DBN18a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_DBN18a_gini$X1<=0.4]))
b1_DBN18a.ngini <- normalizedGini(a, p)
b1_DBN18a.ngini
b1_DBN18a.gini <-Gini(a, p)
b1_DBN18a.gini

#Brier score
set.seed(123); b1_model_DBN18a_brier <- train(formula, data=bene1_woe_train8,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN18a_brier$results
b1_model_DBN18a_brier$resample
b1_pred_DBN18a_brier <- predict(b1_model_DBN18a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_DBN18a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_DBN18a_brier$X1)
b1_DBN18a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_DBN18b_roc <- train(formula, data=bene1_woe_test8,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN18b_roc <- predict(b1_model_DBN18b_roc, bene1_woe_train8,type="prob")
b1_DBN18b.ROC <- roc(predictor=b1predb_DBN18b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_DBN18b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_DBN18b_gini <- train(formula, data=bene1_woe_test8,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN18b_gini$results
b1_model_DBN18b_gini$resample
b1_pred_DBN18b_gini<- predict(b1_model_DBN18b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_DBN18b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_DBN18b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN18b_gini$X1[b1_pred_DBN18b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_DBN18b_gini$X1<=0.4]))
b1_DBN18b.ngini <- normalizedGini(a, p)
b1_DBN18b.ngini
b1_DBN18b.gini <-Gini(a, p)
b1_DBN18b.gini

#Brier score
set.seed(123); b1_model_DBN18b_brier <- train(formula, data=bene1_woe_test8,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN18b_brier$results
b1_model_DBN18b_brier$resample
b1_pred_DBN18b_brier <- predict(b1_model_DBN18b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_DBN18b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_DBN18b_brier$X1)
b1_DBN18b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN19a_roc <- train(formula, data=bene1_woe_train9, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN19a_roc <- predict(b1_model_DBN19a_roc,bene1_woe_test9,type="prob")
b1_DBN19a.ROC <- roc(predictor=b1predb_DBN19a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_DBN19a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_DBN19a_gini <- train(formula, data=bene1_woe_train9,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN19a_gini$results
b1_model_DBN19a_gini$resample
b1_pred_DBN19a_gini<- predict(b1_model_DBN19a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_DBN19a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_DBN19a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN19a_gini$X1[b1_pred_DBN19a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_DBN19a_gini$X1<=0.4]))
b1_DBN19a.ngini <- normalizedGini(a, p)
b1_DBN19a.ngini
b1_DBN19a.gini <-Gini(a, p)
b1_DBN19a.gini

#Brier score
set.seed(123); b1_model_DBN19a_brier <- train(formula, data=bene1_woe_train9,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN19a_brier$results
b1_model_DBN19a_brier$resample
b1_pred_DBN19a_brier <- predict(b1_model_DBN19a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_DBN19a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_DBN19a_brier$X1)
b1_DBN19a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_DBN19b_roc <- train(formula, data=bene1_woe_test9,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN19b_roc <- predict(b1_model_DBN19b_roc, bene1_woe_train9,type="prob")
b1_DBN19b.ROC <- roc(predictor=b1predb_DBN19b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_DBN19b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_DBN19b_gini <- train(formula, data=bene1_woe_test9,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN19b_gini$results
b1_model_DBN19b_gini$resample
b1_pred_DBN19b_gini<- predict(b1_model_DBN19b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_DBN19b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_DBN19b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN19b_gini$X1[b1_pred_DBN19b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_DBN19b_gini$X1<=0.4]))
b1_DBN19b.ngini <- normalizedGini(a, p)
b1_DBN19b.ngini
b1_DBN19b.gini <-Gini(a, p)
b1_DBN19b.gini

#Brier score
set.seed(123); b1_model_DBN19b_brier <- train(formula, data=bene1_woe_test9,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN19b_brier$results
b1_model_DBN19b_brier$resample
b1_pred_DBN19b_brier <- predict(b1_model_DBN19b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_DBN19b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_DBN19b_brier$X1)
b1_DBN19b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN110a_roc <- train(formula, data=bene1_woe_train10, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN110a_roc <- predict(b1_model_DBN110a_roc,bene1_woe_test10,type="prob")
b1_DBN110a.ROC <- roc(predictor=b1predb_DBN110a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_DBN110a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_DBN110a_gini <- train(formula, data=bene1_woe_train10,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN110a_gini$results
b1_model_DBN110a_gini$resample
b1_pred_DBN110a_gini<- predict(b1_model_DBN110a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_DBN110a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_DBN110a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN110a_gini$X1[b1_pred_DBN110a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_DBN110a_gini$X1<=0.4]))
b1_DBN110a.ngini <- normalizedGini(a, p)
b1_DBN110a.ngini
b1_DBN110a.gini <-Gini(a, p)
b1_DBN110a.gini

#Brier score
set.seed(123); b1_model_DBN110a_brier <- train(formula, data=bene1_woe_train10,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN110a_brier$results
b1_model_DBN110a_brier$resample
b1_pred_DBN110a_brier <- predict(b1_model_DBN110a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_DBN110a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_DBN110a_brier$X1)
b1_DBN110a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_DBN110b_roc <- train(formula, data=bene1_woe_test10,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN110b_roc <- predict(b1_model_DBN110b_roc, bene1_woe_train10,type="prob")
b1_DBN110b.ROC <- roc(predictor=b1predb_DBN110b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_DBN110b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_DBN110b_gini <- train(formula, data=bene1_woe_test10,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN110b_gini$results
b1_model_DBN110b_gini$resample
b1_pred_DBN110b_gini<- predict(b1_model_DBN110b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_DBN110b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_DBN110b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN110b_gini$X1[b1_pred_DBN110b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_DBN110b_gini$X1<=0.4]))
b1_DBN110b.ngini <- normalizedGini(a, p)
b1_DBN110b.ngini
b1_DBN110b.gini <-Gini(a, p)
b1_DBN110b.gini

#Brier score
set.seed(123); b1_model_DBN110b_brier <- train(formula, data=bene1_woe_test10,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN110b_brier$results
b1_model_DBN110b_brier$resample
b1_pred_DBN110b_brier <- predict(b1_model_DBN110b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_DBN110b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_DBN110b_brier$X1)
b1_DBN110b.bs

##Restults RF!
b1_results_DBN1_AUC <- cbind(b1_DBN11a.ROC$auc,b1_DBN11b.ROC$auc,b1_DBN12a.ROC$auc,b1_DBN12b.ROC$auc,b1_DBN13a.ROC$auc,b1_DBN13b.ROC$auc,b1_DBN14a.ROC$auc,
                             b1_DBN14b.ROC$auc,b1_DBN15a.ROC$auc,b1_DBN15b.ROC$auc,b1_DBN16a.ROC$auc,b1_DBN16b.ROC$auc,b1_DBN17a.ROC$auc,b1_DBN17b.ROC$auc,
                             b1_DBN18a.ROC$auc,b1_DBN18b.ROC$auc,b1_DBN19a.ROC$auc,b1_DBN19b.ROC$auc,b1_DBN110a.ROC$auc,b1_DBN110b.ROC$auc)
b1_results_DBN1_bs <- cbind(b1_DBN11a.bs,b1_DBN11b.bs,b1_DBN12a.bs,b1_DBN12b.bs,b1_DBN13a.bs,b1_DBN13b.bs,b1_DBN14a.bs,b1_DBN14b.bs,b1_DBN15a.bs,b1_DBN15b.bs,
                            b1_DBN16a.bs,b1_DBN16b.bs,b1_DBN17a.bs,b1_DBN17b.bs,b1_DBN18a.bs,b1_DBN18b.bs,b1_DBN19a.bs,b1_DBN19b.bs,b1_DBN110a.bs,b1_DBN110b.bs)
b1_results_DBN1_ngini <- cbind(b1_DBN11a.ngini,b1_DBN11b.ngini,b1_DBN12a.ngini,b1_DBN12b.ngini,b1_DBN13a.ngini,b1_DBN13b.ngini,b1_DBN14a.ngini,b1_DBN14b.ngini,
                               b1_DBN15a.ngini,b1_DBN15b.ngini,b1_DBN16a.ngini,b1_DBN16b.ngini,b1_DBN17a.ngini,b1_DBN17b.ngini,b1_DBN18a.ngini,b1_DBN18b.ngini,
                               b1_DBN19a.ngini,b1_DBN19b.ngini,b1_DBN110a.ngini,b1_DBN110b.ngini)
b1_results_DBN1_gini <- cbind(b1_DBN11a.gini,b1_DBN11b.gini,b1_DBN12a.gini,b1_DBN12b.gini,b1_DBN13a.gini,b1_DBN13b.gini,b1_DBN14a.gini,b1_DBN14b.gini,
                              b1_DBN15a.gini,b1_DBN15b.gini,b1_DBN16a.gini,b1_DBN16b.gini,b1_DBN17a.gini,b1_DBN17b.gini,b1_DBN18a.gini,b1_DBN18b.gini,
                              b1_DBN19a.gini,b1_DBN19b.gini,b1_DBN110a.gini,b1_DBN110b.gini)
mean(b1_results_DBN1_AUC)
mean(b1_results_DBN1_bs)
mean(b1_results_DBN1_ngini)
mean(b1_results_DBN1_gini)

##############################
###########DBN3###############
##############################
DBN3grid <- expand.grid(.layer1=c(5,10,15,20), .layer2=c(5,10,15,20), .layer3=c(5,10,15,20), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0,0.5))
set.seed(123); b1_model_DBN31a_roc <- train(formula, data=bene1_woe_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN3grid <- expand.grid(.layer1=c(15), .layer2=c(5),.layer3=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0.25), .lr=c(1.5))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN31a_roc <- train(formula, data=bene1_woe_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN31a_roc <- predict(b1_model_DBN31a_roc,bene1_woe_test1,type="prob")
b1_DBN31a.ROC <- roc(predictor=b1predb_DBN31a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_DBN31a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_DBN31a_gini <- train(formula, data=bene1_woe_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN31a_gini$results
b1_model_DBN31a_gini$resample
b1_pred_DBN31a_gini<- predict(b1_model_DBN31a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_DBN31a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_DBN31a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN31a_gini$X1[b1_pred_DBN31a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_DBN31a_gini$X1<=0.4]))
b1_DBN31a.ngini <- normalizedGini(a, p)
b1_DBN31a.ngini
b1_DBN31a.gini <-Gini(a, p)
b1_DBN31a.gini

#Brier score
set.seed(123); b1_model_DBN31a_brier <- train(formula, data=bene1_woe_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN31a_brier$results
b1_model_DBN31a_brier$resample
b1_pred_DBN31a_brier <- predict(b1_model_DBN31a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_DBN31a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_DBN31a_brier$X1)
b1_DBN31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_DBN31b_roc <- train(formula, data=bene1_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN31b_roc <- predict(b1_model_DBN31b_roc, bene1_woe_train1,type="prob")
b1_DBN31b.ROC <- roc(predictor=b1predb_DBN31b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_DBN31b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_DBN31b_gini <- train(formula, data=bene1_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN31b_gini$results
b1_model_DBN31b_gini$resample
b1_pred_DBN31b_gini<- predict(b1_model_DBN31b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_DBN31b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_DBN31b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN31b_gini$X1[b1_pred_DBN31b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_DBN31b_gini$X1<=0.4]))
b1_DBN31b.ngini <- normalizedGini(a, p)
b1_DBN31b.ngini
b1_DBN31b.gini <-Gini(a, p)
b1_DBN31b.gini

#Brier score
set.seed(123); b1_model_DBN31b_brier <- train(formula, data=bene1_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN31b_brier$results
b1_model_DBN31b_brier$resample
b1_pred_DBN31b_brier <- predict(b1_model_DBN31b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_DBN31b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_DBN31b_brier$X1)
b1_DBN31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN32a_roc <- train(formula, data=bene1_woe_train2, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN32a_roc <- predict(b1_model_DBN32a_roc,bene1_woe_test2,type="prob")
b1_DBN32a.ROC <- roc(predictor=b1predb_DBN32a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_DBN32a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_DBN32a_gini <- train(formula, data=bene1_woe_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN32a_gini$results
b1_model_DBN32a_gini$resample
b1_pred_DBN32a_gini<- predict(b1_model_DBN32a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_DBN32a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_DBN32a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN32a_gini$X1[b1_pred_DBN32a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_DBN32a_gini$X1<=0.4]))
b1_DBN32a.ngini <- normalizedGini(a, p)
b1_DBN32a.ngini
b1_DBN32a.gini <-Gini(a, p)
b1_DBN32a.gini

#Brier score
set.seed(123); b1_model_DBN32a_brier <- train(formula, data=bene1_woe_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN32a_brier$results
b1_model_DBN32a_brier$resample
b1_pred_DBN32a_brier <- predict(b1_model_DBN32a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_DBN32a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_DBN32a_brier$X1)
b1_DBN32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_DBN32b_roc <- train(formula, data=bene1_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN32b_roc <- predict(b1_model_DBN32b_roc, bene1_woe_train2,type="prob")
b1_DBN32b.ROC <- roc(predictor=b1predb_DBN32b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_DBN32b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_DBN32b_gini <- train(formula, data=bene1_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN32b_gini$results
b1_model_DBN32b_gini$resample
b1_pred_DBN32b_gini<- predict(b1_model_DBN31b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_DBN32b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_DBN32b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN32b_gini$X1[b1_pred_DBN32b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_DBN32b_gini$X1<=0.4]))
b1_DBN32b.ngini <- normalizedGini(a, p)
b1_DBN32b.ngini
b1_DBN32b.gini <-Gini(a, p)
b1_DBN32b.gini

#Brier score
set.seed(123); b1_model_DBN32b_brier <- train(formula, data=bene1_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN32b_brier$results
b1_model_DBN32b_brier$resample
b1_pred_DBN32b_brier <- predict(b1_model_DBN32b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_DBN32b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_DBN32b_brier$X1)
b1_DBN32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN33a_roc <- train(formula, data=bene1_woe_train3, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN33a_roc <- predict(b1_model_DBN33a_roc,bene1_woe_test3,type="prob")
b1_DBN33a.ROC <- roc(predictor=b1predb_DBN33a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_DBN33a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_DBN33a_gini <- train(formula, data=bene1_woe_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN33a_gini$results
b1_model_DBN33a_gini$resample
b1_pred_DBN33a_gini<- predict(b1_model_DBN33a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_DBN33a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_DBN33a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN33a_gini$X1[b1_pred_DBN33a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_DBN33a_gini$X1<=0.4]))
b1_DBN33a.ngini <- normalizedGini(a, p)
b1_DBN33a.ngini
b1_DBN33a.gini <-Gini(a, p)
b1_DBN33a.gini

#Brier score
set.seed(123); b1_model_DBN33a_brier <- train(formula, data=bene1_woe_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN33a_brier$results
b1_model_DBN33a_brier$resample
b1_pred_DBN33a_brier <- predict(b1_model_DBN33a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_DBN33a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_DBN33a_brier$X1)
b1_DBN33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_DBN33b_roc <- train(formula, data=bene1_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN33b_roc <- predict(b1_model_DBN33b_roc, bene1_woe_train3,type="prob")
b1_DBN33b.ROC <- roc(predictor=b1predb_DBN33b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_DBN33b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_DBN33b_gini <- train(formula, data=bene1_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN33b_gini$results
b1_model_DBN33b_gini$resample
b1_pred_DBN33b_gini<- predict(b1_model_DBN33b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_DBN33b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_DBN33b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN33b_gini$X1[b1_pred_DBN33b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_DBN33b_gini$X1<=0.4]))
b1_DBN33b.ngini <- normalizedGini(a, p)
b1_DBN33b.ngini
b1_DBN33b.gini <-Gini(a, p)
b1_DBN33b.gini

#Brier score
set.seed(123); b1_model_DBN33b_brier <- train(formula, data=bene1_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN33b_brier$results
b1_model_DBN33b_brier$resample
b1_pred_DBN33b_brier <- predict(b1_model_DBN33b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_DBN33b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_DBN33b_brier$X1)
b1_DBN33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN34a_roc <- train(formula, data=bene1_woe_train4, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN34a_roc <- predict(b1_model_DBN34a_roc,bene1_woe_test4,type="prob")
b1_DBN34a.ROC <- roc(predictor=b1predb_DBN34a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_DBN34a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_DBN34a_gini <- train(formula, data=bene1_woe_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN34a_gini$results
b1_model_DBN34a_gini$resample
b1_pred_DBN34a_gini<- predict(b1_model_DBN34a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_DBN34a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_DBN34a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN34a_gini$X1[b1_pred_DBN34a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_DBN34a_gini$X1<=0.4]))
b1_DBN34a.ngini <- normalizedGini(a, p)
b1_DBN34a.ngini
b1_DBN34a.gini <-Gini(a, p)
b1_DBN34a.gini

#Brier score
set.seed(123); b1_model_DBN34a_brier <- train(formula, data=bene1_woe_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN34a_brier$results
b1_model_DBN34a_brier$resample
b1_pred_DBN34a_brier <- predict(b1_model_DBN34a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_DBN34a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_DBN34a_brier$X1)
b1_DBN34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_DBN34b_roc <- train(formula, data=bene1_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN34b_roc <- predict(b1_model_DBN34b_roc, bene1_woe_train4,type="prob")
b1_DBN34b.ROC <- roc(predictor=b1predb_DBN34b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_DBN34b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_DBN34b_gini <- train(formula, data=bene1_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN34b_gini$results
b1_model_DBN34b_gini$resample
b1_pred_DBN34b_gini<- predict(b1_model_DBN34b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_DBN34b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_DBN34b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN34b_gini$X1[b1_pred_DBN34b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_DBN34b_gini$X1<=0.4]))
b1_DBN34b.ngini <- normalizedGini(a, p)
b1_DBN34b.ngini
b1_DBN34b.gini <-Gini(a, p)
b1_DBN34b.gini

#Brier score
set.seed(123); b1_model_DBN34b_brier <- train(formula, data=bene1_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN34b_brier$results
b1_model_DBN34b_brier$resample
b1_pred_DBN34b_brier <- predict(b1_model_DBN34b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_DBN34b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_DBN34b_brier$X1)
b1_DBN34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN35a_roc <- train(formula, data=bene1_woe_train5, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN35a_roc <- predict(b1_model_DBN35a_roc,bene1_woe_test5,type="prob")
b1_DBN35a.ROC <- roc(predictor=b1predb_DBN35a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_DBN35a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_DBN35a_gini <- train(formula, data=bene1_woe_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN35a_gini$results
b1_model_DBN35a_gini$resample
b1_pred_DBN35a_gini<- predict(b1_model_DBN35a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_DBN35a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_DBN35a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN35a_gini$X1[b1_pred_DBN35a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_DBN35a_gini$X1<=0.4]))
b1_DBN35a.ngini <- normalizedGini(a, p)
b1_DBN35a.ngini
b1_DBN35a.gini <-Gini(a, p)
b1_DBN35a.gini

#Brier score
set.seed(123); b1_model_DBN35a_brier <- train(formula, data=bene1_woe_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN35a_brier$results
b1_model_DBN35a_brier$resample
b1_pred_DBN35a_brier <- predict(b1_model_DBN35a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_DBN35a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_DBN35a_brier$X1)
b1_DBN35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_DBN35b_roc <- train(formula, data=bene1_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN35b_roc <- predict(b1_model_DBN35b_roc, bene1_woe_train5,type="prob")
b1_DBN35b.ROC <- roc(predictor=b1predb_DBN35b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_DBN35b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_DBN35b_gini <- train(formula, data=bene1_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN35b_gini$results
b1_model_DBN35b_gini$resample
b1_pred_DBN35b_gini<- predict(b1_model_DBN35b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_DBN35b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_DBN35b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN35b_gini$X1[b1_pred_DBN35b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_DBN35b_gini$X1<=0.4]))
b1_DBN35b.ngini <- normalizedGini(a, p)
b1_DBN35b.ngini
b1_DBN35b.gini <-Gini(a, p)
b1_DBN35b.gini

#Brier score
set.seed(123); b1_model_DBN35b_brier <- train(formula, data=bene1_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN35b_brier$results
b1_model_DBN35b_brier$resample
b1_pred_DBN35b_brier <- predict(b1_model_DBN35b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_DBN35b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_DBN35b_brier$X1)
b1_DBN35b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN36a_roc <- train(formula, data=bene1_woe_train6, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN36a_roc <- predict(b1_model_DBN36a_roc,bene1_woe_test6,type="prob")
b1_DBN36a.ROC <- roc(predictor=b1predb_DBN36a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_DBN36a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_DBN36a_gini <- train(formula, data=bene1_woe_train6,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN36a_gini$results
b1_model_DBN36a_gini$resample
b1_pred_DBN36a_gini<- predict(b1_model_DBN36a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_DBN36a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_DBN36a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN36a_gini$X1[b1_pred_DBN36a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_DBN36a_gini$X1<=0.4]))
b1_DBN36a.ngini <- normalizedGini(a, p)
b1_DBN36a.ngini
b1_DBN36a.gini <-Gini(a, p)
b1_DBN36a.gini

#Brier score
set.seed(123); b1_model_DBN36a_brier <- train(formula, data=bene1_woe_train6,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN36a_brier$results
b1_model_DBN36a_brier$resample
b1_pred_DBN36a_brier <- predict(b1_model_DBN36a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_DBN36a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_DBN36a_brier$X1)
b1_DBN36a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_DBN36b_roc <- train(formula, data=bene1_woe_test6,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN36b_roc <- predict(b1_model_DBN36b_roc, bene1_woe_train6,type="prob")
b1_DBN36b.ROC <- roc(predictor=b1predb_DBN36b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_DBN36b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_DBN36b_gini <- train(formula, data=bene1_woe_test6,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN36b_gini$results
b1_model_DBN36b_gini$resample
b1_pred_DBN36b_gini<- predict(b1_model_DBN36b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_DBN36b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_DBN36b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN36b_gini$X1[b1_pred_DBN36b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_DBN36b_gini$X1<=0.4]))
b1_DBN36b.ngini <- normalizedGini(a, p)
b1_DBN36b.ngini
b1_DBN36b.gini <-Gini(a, p)
b1_DBN36b.gini

#Brier score
set.seed(123); b1_model_DBN36b_brier <- train(formula, data=bene1_woe_test6,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN36b_brier$results
b1_model_DBN36b_brier$resample
b1_pred_DBN36b_brier <- predict(b1_model_DBN36b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_DBN36b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_DBN36b_brier$X1)
b1_DBN36b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN37a_roc <- train(formula, data=bene1_woe_train7, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN37a_roc <- predict(b1_model_DBN37a_roc,bene1_woe_test7,type="prob")
b1_DBN37a.ROC <- roc(predictor=b1predb_DBN37a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_DBN37a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_DBN37a_gini <- train(formula, data=bene1_woe_train7,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN37a_gini$results
b1_model_DBN37a_gini$resample
b1_pred_DBN37a_gini<- predict(b1_model_DBN37a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_DBN37a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_DBN37a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN37a_gini$X1[b1_pred_DBN37a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_DBN37a_gini$X1<=0.4]))
b1_DBN37a.ngini <- normalizedGini(a, p)
b1_DBN37a.ngini
b1_DBN37a.gini <-Gini(a, p)
b1_DBN37a.gini

#Brier score
set.seed(123); b1_model_DBN37a_brier <- train(formula, data=bene1_woe_train7,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN37a_brier$results
b1_model_DBN37a_brier$resample
b1_pred_DBN37a_brier <- predict(b1_model_DBN37a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_DBN37a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_DBN37a_brier$X1)
b1_DBN37a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_DBN37b_roc <- train(formula, data=bene1_woe_test7,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN37b_roc <- predict(b1_model_DBN37b_roc, bene1_woe_train7,type="prob")
b1_DBN37b.ROC <- roc(predictor=b1predb_DBN37b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_DBN37b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_DBN37b_gini <- train(formula, data=bene1_woe_test7,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN37b_gini$results
b1_model_DBN37b_gini$resample
b1_pred_DBN37b_gini<- predict(b1_model_DBN37b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_DBN37b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_DBN37b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN37b_gini$X1[b1_pred_DBN37b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_DBN37b_gini$X1<=0.4]))
b1_DBN37b.ngini <- normalizedGini(a, p)
b1_DBN37b.ngini
b1_DBN37b.gini <-Gini(a, p)
b1_DBN37b.gini

#Brier score
set.seed(123); b1_model_DBN37b_brier <- train(formula, data=bene1_woe_test7,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN37b_brier$results
b1_model_DBN37b_brier$resample
b1_pred_DBN37b_brier <- predict(b1_model_DBN37b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_DBN37b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_DBN37b_brier$X1)
b1_DBN37b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN38a_roc <- train(formula, data=bene1_woe_train8, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN38a_roc <- predict(b1_model_DBN38a_roc,bene1_woe_test8,type="prob")
b1_DBN38a.ROC <- roc(predictor=b1predb_DBN38a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_DBN38a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_DBN38a_gini <- train(formula, data=bene1_woe_train8,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN38a_gini$results
b1_model_DBN38a_gini$resample
b1_pred_DBN38a_gini<- predict(b1_model_DBN38a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_DBN38a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_DBN38a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN38a_gini$X1[b1_pred_DBN38a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_DBN38a_gini$X1<=0.4]))
b1_DBN38a.ngini <- normalizedGini(a, p)
b1_DBN38a.ngini
b1_DBN38a.gini <-Gini(a, p)
b1_DBN38a.gini

#Brier score
set.seed(123); b1_model_DBN38a_brier <- train(formula, data=bene1_woe_train8,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN38a_brier$results
b1_model_DBN38a_brier$resample
b1_pred_DBN38a_brier <- predict(b1_model_DBN38a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_DBN38a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_DBN38a_brier$X1)
b1_DBN38a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_DBN38b_roc <- train(formula, data=bene1_woe_test8,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN38b_roc <- predict(b1_model_DBN38b_roc, bene1_woe_train8,type="prob")
b1_DBN38b.ROC <- roc(predictor=b1predb_DBN38b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_DBN38b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_DBN38b_gini <- train(formula, data=bene1_woe_test8,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN38b_gini$results
b1_model_DBN38b_gini$resample
b1_pred_DBN38b_gini<- predict(b1_model_DBN38b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_DBN38b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_DBN38b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN38b_gini$X1[b1_pred_DBN38b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_DBN38b_gini$X1<=0.4]))
b1_DBN38b.ngini <- normalizedGini(a, p)
b1_DBN38b.ngini
b1_DBN38b.gini <-Gini(a, p)
b1_DBN38b.gini

#Brier score
set.seed(123); b1_model_DBN38b_brier <- train(formula, data=bene1_woe_test8,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN38b_brier$results
b1_model_DBN38b_brier$resample
b1_pred_DBN38b_brier <- predict(b1_model_DBN38b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_DBN38b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_DBN38b_brier$X1)
b1_DBN38b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN39a_roc <- train(formula, data=bene1_woe_train9, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN39a_roc <- predict(b1_model_DBN39a_roc,bene1_woe_test9,type="prob")
b1_DBN39a.ROC <- roc(predictor=b1predb_DBN39a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_DBN39a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_DBN39a_gini <- train(formula, data=bene1_woe_train9,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN39a_gini$results
b1_model_DBN39a_gini$resample
b1_pred_DBN39a_gini<- predict(b1_model_DBN39a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_DBN39a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_DBN39a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN39a_gini$X1[b1_pred_DBN39a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_DBN39a_gini$X1<=0.4]))
b1_DBN39a.ngini <- normalizedGini(a, p)
b1_DBN39a.ngini
b1_DBN39a.gini <-Gini(a, p)
b1_DBN39a.gini

#Brier score
set.seed(123); b1_model_DBN39a_brier <- train(formula, data=bene1_woe_train9,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN39a_brier$results
b1_model_DBN39a_brier$resample
b1_pred_DBN39a_brier <- predict(b1_model_DBN39a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_DBN39a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_DBN39a_brier$X1)
b1_DBN39a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_DBN39b_roc <- train(formula, data=bene1_woe_test9,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN39b_roc <- predict(b1_model_DBN39b_roc, bene1_woe_train9,type="prob")
b1_DBN39b.ROC <- roc(predictor=b1predb_DBN39b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_DBN39b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_DBN39b_gini <- train(formula, data=bene1_woe_test9,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN39b_gini$results
b1_model_DBN39b_gini$resample
b1_pred_DBN39b_gini<- predict(b1_model_DBN39b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_DBN39b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_DBN39b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN39b_gini$X1[b1_pred_DBN39b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_DBN39b_gini$X1<=0.4]))
b1_DBN39b.ngini <- normalizedGini(a, p)
b1_DBN39b.ngini
b1_DBN39b.gini <-Gini(a, p)
b1_DBN39b.gini

#Brier score
set.seed(123); b1_model_DBN39b_brier <- train(formula, data=bene1_woe_test9,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN39b_brier$results
b1_model_DBN39b_brier$resample
b1_pred_DBN39b_brier <- predict(b1_model_DBN39b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_DBN39b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_DBN39b_brier$X1)
b1_DBN39b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN310a_roc <- train(formula, data=bene1_woe_train10, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN310a_roc <- predict(b1_model_DBN310a_roc,bene1_woe_test10,type="prob")
b1_DBN310a.ROC <- roc(predictor=b1predb_DBN310a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_DBN310a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_DBN310a_gini <- train(formula, data=bene1_woe_train10,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN310a_gini$results
b1_model_DBN310a_gini$resample
b1_pred_DBN310a_gini<- predict(b1_model_DBN310a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_DBN310a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_DBN310a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN310a_gini$X1[b1_pred_DBN310a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_DBN310a_gini$X1<=0.4]))
b1_DBN310a.ngini <- normalizedGini(a, p)
b1_DBN310a.ngini
b1_DBN310a.gini <-Gini(a, p)
b1_DBN310a.gini

#Brier score
set.seed(123); b1_model_DBN310a_brier <- train(formula, data=bene1_woe_train10,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN310a_brier$results
b1_model_DBN310a_brier$resample
b1_pred_DBN310a_brier <- predict(b1_model_DBN310a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_DBN310a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_DBN310a_brier$X1)
b1_DBN310a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_DBN310b_roc <- train(formula, data=bene1_woe_test10,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN310b_roc <- predict(b1_model_DBN310b_roc, bene1_woe_train10,type="prob")
b1_DBN310b.ROC <- roc(predictor=b1predb_DBN310b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_DBN310b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_DBN310b_gini <- train(formula, data=bene1_woe_test10,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN310b_gini$results
b1_model_DBN310b_gini$resample
b1_pred_DBN310b_gini<- predict(b1_model_DBN310b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_DBN310b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_DBN310b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN310b_gini$X1[b1_pred_DBN310b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_DBN310b_gini$X1<=0.4]))
b1_DBN310b.ngini <- normalizedGini(a, p)
b1_DBN310b.ngini
b1_DBN310b.gini <-Gini(a, p)
b1_DBN310b.gini

#Brier score
set.seed(123); b1_model_DBN310b_brier <- train(formula, data=bene1_woe_test10,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN310b_brier$results
b1_model_DBN310b_brier$resample
b1_pred_DBN310b_brier <- predict(b1_model_DBN310b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_DBN310b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_DBN310b_brier$X1)
b1_DBN310b.bs

##Restults RF!
b1_results_DBN3_AUC <- cbind(b1_DBN31a.ROC$auc,b1_DBN31b.ROC$auc,b1_DBN32a.ROC$auc,b1_DBN32b.ROC$auc,b1_DBN33a.ROC$auc,b1_DBN33b.ROC$auc,b1_DBN34a.ROC$auc,
                             b1_DBN34b.ROC$auc,b1_DBN35a.ROC$auc,b1_DBN35b.ROC$auc,b1_DBN36a.ROC$auc,b1_DBN36b.ROC$auc,b1_DBN37a.ROC$auc,b1_DBN37b.ROC$auc,
                             b1_DBN38a.ROC$auc,b1_DBN38b.ROC$auc,b1_DBN39a.ROC$auc,b1_DBN39b.ROC$auc,b1_DBN310a.ROC$auc,b1_DBN310b.ROC$auc)
b1_results_DBN3_bs <- cbind(b1_DBN31a.bs,b1_DBN31b.bs,b1_DBN32a.bs,b1_DBN32b.bs,b1_DBN33a.bs,b1_DBN33b.bs,b1_DBN34a.bs,b1_DBN34b.bs,b1_DBN35a.bs,b1_DBN35b.bs,
                            b1_DBN36a.bs,b1_DBN36b.bs,b1_DBN37a.bs,b1_DBN37b.bs,b1_DBN38a.bs,b1_DBN38b.bs,b1_DBN39a.bs,b1_DBN39b.bs,b1_DBN310a.bs,b1_DBN310b.bs)
b1_results_DBN3_gini <- cbind(b1_DBN31a.gini,b1_DBN31b.gini,b1_DBN32a.gini,b1_DBN32b.gini,b1_DBN33a.gini,b1_DBN33b.gini,b1_DBN34a.gini,b1_DBN34b.gini,
                              b1_DBN35a.gini,b1_DBN35b.gini,b1_DBN36a.gini,b1_DBN36b.gini,b1_DBN37a.gini,b1_DBN37b.gini,b1_DBN38a.gini,b1_DBN38b.gini,
                              b1_DBN39a.gini,b1_DBN39b.gini,b1_DBN310a.gini,b1_DBN310b.gini)
b1_results_DBN3_gini <- cbind(b1_DBN31b.gini,b1_DBN32b.gini,b1_DBN33a.gini,b1_DBN34b.gini,
                              b1_DBN35b.gini,b1_DBN36a.gini,b1_DBN37a.gini,b1_DBN37b.gini,b1_DBN38a.gini,b1_DBN38b.gini,
                              b1_DBN39b.gini,b1_DBN310a.gini,b1_DBN310b.gini) #many models dont have prediction p<0.4



mean(b1_results_DBN3_AUC)
mean(b1_results_DBN3_bs)
mean(b1_results_DBN3_gini)


##############################
###########DBN5###############
##############################
DBN5grid <- expand.grid(.layer1=c(5,10,15), .layer2=c(5,10,15), .layer3=c(5,10,15), .layer4=c(5,10,15), .layer5=c(5,10,15), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0,0.5))
set.seed(123); b1_model_DBN51a_roc <- train(formula, data=bene1_woe_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN5grid <- expand.grid(.layer1=c(10), .layer2=c(15),.layer3=c(15),.layer4=c(10),.layer5=c(15),.hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(2))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN51a_roc <- train(formula, data=bene1_woe_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN51a_roc <- predict(b1_model_DBN51a_roc,bene1_woe_test1,type="prob")
b1_DBN51a.ROC <- roc(predictor=b1predb_DBN51a_roc$X0,
                     response=bene1_woe_test1$TARGET,
                     levels=rev(levels(bene1_woe_test1$TARGET)))
b1_DBN51a.ROC

#normalizedGini
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
bene1_woe_test1$TARGETb <- as.numeric(levels(bene1_woe_test1$TARGETb))[bene1_woe_test1$TARGETb]
set.seed(123); b1_model_DBN51a_gini <- train(formula, data=bene1_woe_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN51a_gini$results
b1_model_DBN51a_gini$resample
b1_pred_DBN51a_gini<- predict(b1_model_DBN51a_gini, newdata = bene1_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test1$TARGETb, b1_pred_DBN51a_gini$X1)
Gini(bene1_woe_test1$TARGETb, b1_pred_DBN51a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN51a_gini$X1[b1_pred_DBN51a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test1$TARGETb[b1_pred_DBN51a_gini$X1<=0.4]))
b1_DBN51a.ngini <- normalizedGini(a, p)
b1_DBN51a.ngini
b1_DBN51a.gini <-Gini(a, p)
b1_DBN51a.gini

#Brier score
set.seed(123); b1_model_DBN51a_brier <- train(formula, data=bene1_woe_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN51a_brier$results
b1_model_DBN51a_brier$resample
b1_pred_DBN51a_brier <- predict(b1_model_DBN51a_brier, newdata = bene1_woe_test1, type='prob')
bene1_woe_test1$TARGETb <- bene1_woe_test1$TARGET
levels(bene1_woe_test1$TARGETb) <- c('0','1')
b1_DBN51a.bs <- Brier(as.numeric(as.character(bene1_woe_test1$TARGETb)), b1_pred_DBN51a_brier$X1)
b1_DBN51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b1_model_DBN51b_roc <- train(formula, data=bene1_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN51b_roc <- predict(b1_model_DBN51b_roc, bene1_woe_train1,type="prob")
b1_DBN51b.ROC <- roc(predictor=b1predb_DBN51b_roc$X0,
                     response=bene1_woe_train1$TARGET,
                     levels=rev(levels(bene1_woe_train1$TARGET)))
b1_DBN51b.ROC

#normalizedGini
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
bene1_woe_train1$TARGETb <- as.numeric(levels(bene1_woe_train1$TARGETb))[bene1_woe_train1$TARGETb]
set.seed(123); b1_model_DBN51b_gini <- train(formula, data=bene1_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN51b_gini$results
b1_model_DBN51b_gini$resample
b1_pred_DBN51b_gini<- predict(b1_model_DBN51b_gini, newdata = bene1_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train1$TARGETb, b1_pred_DBN51b_gini$X1)
Gini(bene1_woe_train1$TARGETb, b1_pred_DBN51b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN51b_gini$X1[b1_pred_DBN51b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train1$TARGETb[b1_pred_DBN51b_gini$X1<=0.4]))
b1_DBN51b.ngini <- normalizedGini(a, p)
b1_DBN51b.ngini
b1_DBN51b.gini <-Gini(a, p)
b1_DBN51b.gini

#Brier score
set.seed(123); b1_model_DBN51b_brier <- train(formula, data=bene1_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN51b_brier$results
b1_model_DBN51b_brier$resample
b1_pred_DBN51b_brier <- predict(b1_model_DBN51b_brier, newdata = bene1_woe_train1, type='prob')
bene1_woe_train1$TARGETb <- bene1_woe_train1$TARGET
levels(bene1_woe_train1$TARGETb) <- c('0','1')
b1_DBN51b.bs <- Brier(as.numeric(as.character(bene1_woe_train1$TARGETb)), b1_pred_DBN51b_brier$X1)
b1_DBN51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN52a_roc <- train(formula, data=bene1_woe_train2, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN52a_roc <- predict(b1_model_DBN52a_roc,bene1_woe_test2,type="prob")
b1_DBN52a.ROC <- roc(predictor=b1predb_DBN52a_roc$X0,
                     response=bene1_woe_test2$TARGET,
                     levels=rev(levels(bene1_woe_test2$TARGET)))
b1_DBN52a.ROC

#normalizedGini
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
bene1_woe_test2$TARGETb <- as.numeric(levels(bene1_woe_test2$TARGETb))[bene1_woe_test2$TARGETb]
set.seed(123); b1_model_DBN52a_gini <- train(formula, data=bene1_woe_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN52a_gini$results
b1_model_DBN52a_gini$resample
b1_pred_DBN52a_gini<- predict(b1_model_DBN52a_gini, newdata = bene1_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test2$TARGETb, b1_pred_DBN52a_gini$X1)
Gini(bene1_woe_test2$TARGETb, b1_pred_DBN52a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN52a_gini$X1[b1_pred_DBN52a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test2$TARGETb[b1_pred_DBN52a_gini$X1<=0.4]))
b1_DBN52a.ngini <- normalizedGini(a, p)
b1_DBN52a.ngini
b1_DBN52a.gini <-Gini(a, p)
b1_DBN52a.gini

#Brier score
set.seed(123); b1_model_DBN52a_brier <- train(formula, data=bene1_woe_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN52a_brier$results
b1_model_DBN52a_brier$resample
b1_pred_DBN52a_brier <- predict(b1_model_DBN52a_brier, newdata = bene1_woe_test2, type='prob')
bene1_woe_test2$TARGETb <- bene1_woe_test2$TARGET
levels(bene1_woe_test2$TARGETb) <- c('0','1')
b1_DBN52a.bs <- Brier(as.numeric(as.character(bene1_woe_test2$TARGETb)), b1_pred_DBN52a_brier$X1)
b1_DBN52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b1_model_DBN52b_roc <- train(formula, data=bene1_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN52b_roc <- predict(b1_model_DBN52b_roc, bene1_woe_train2,type="prob")
b1_DBN52b.ROC <- roc(predictor=b1predb_DBN52b_roc$X0,
                     response=bene1_woe_train2$TARGET,
                     levels=rev(levels(bene1_woe_train2$TARGET)))
b1_DBN52b.ROC

#normalizedGini
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
bene1_woe_train2$TARGETb <- as.numeric(levels(bene1_woe_train2$TARGETb))[bene1_woe_train2$TARGETb]
set.seed(123); b1_model_DBN52b_gini <- train(formula, data=bene1_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN52b_gini$results
b1_model_DBN52b_gini$resample
b1_pred_DBN52b_gini<- predict(b1_model_DBN52b_gini, newdata = bene1_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train2$TARGETb, b1_pred_DBN52b_gini$X1)
Gini(bene1_woe_train2$TARGETb, b1_pred_DBN52b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN52b_gini$X1[b1_pred_DBN52b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train2$TARGETb[b1_pred_DBN52b_gini$X1<=0.4]))
b1_DBN52b.ngini <- normalizedGini(a, p)
b1_DBN52b.ngini
b1_DBN52b.gini <-Gini(a, p)
b1_DBN52b.gini

#Brier score
set.seed(123); b1_model_DBN52b_brier <- train(formula, data=bene1_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN52b_brier$results
b1_model_DBN52b_brier$resample
b1_pred_DBN52b_brier <- predict(b1_model_DBN52b_brier, newdata = bene1_woe_train2, type='prob')
bene1_woe_train2$TARGETb <- bene1_woe_train2$TARGET
levels(bene1_woe_train2$TARGETb) <- c('0','1')
b1_DBN52b.bs <- Brier(as.numeric(as.character(bene1_woe_train2$TARGETb)), b1_pred_DBN52b_brier$X1)
b1_DBN52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN53a_roc <- train(formula, data=bene1_woe_train3, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN53a_roc <- predict(b1_model_DBN53a_roc,bene1_woe_test3,type="prob")
b1_DBN53a.ROC <- roc(predictor=b1predb_DBN53a_roc$X0,
                     response=bene1_woe_test3$TARGET,
                     levels=rev(levels(bene1_woe_test3$TARGET)))
b1_DBN53a.ROC

#normalizedGini
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
bene1_woe_test3$TARGETb <- as.numeric(levels(bene1_woe_test3$TARGETb))[bene1_woe_test3$TARGETb]
set.seed(123); b1_model_DBN53a_gini <- train(formula, data=bene1_woe_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN53a_gini$results
b1_model_DBN53a_gini$resample
b1_pred_DBN53a_gini<- predict(b1_model_DBN53a_gini, newdata = bene1_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test3$TARGETb, b1_pred_DBN53a_gini$X1)
Gini(bene1_woe_test3$TARGETb, b1_pred_DBN53a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN53a_gini$X1[b1_pred_DBN53a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test3$TARGETb[b1_pred_DBN53a_gini$X1<=0.4]))
b1_DBN53a.ngini <- normalizedGini(a, p)
b1_DBN53a.ngini
b1_DBN53a.gini <-Gini(a, p)
b1_DBN53a.gini

#Brier score
set.seed(123); b1_model_DBN53a_brier <- train(formula, data=bene1_woe_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN53a_brier$results
b1_model_DBN53a_brier$resample
b1_pred_DBN53a_brier <- predict(b1_model_DBN53a_brier, newdata = bene1_woe_test3, type='prob')
bene1_woe_test3$TARGETb <- bene1_woe_test3$TARGET
levels(bene1_woe_test3$TARGETb) <- c('0','1')
b1_DBN53a.bs <- Brier(as.numeric(as.character(bene1_woe_test3$TARGETb)), b1_pred_DBN53a_brier$X1)
b1_DBN53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b1_model_DBN53b_roc <- train(formula, data=bene1_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN53b_roc <- predict(b1_model_DBN53b_roc, bene1_woe_train3,type="prob")
b1_DBN53b.ROC <- roc(predictor=b1predb_DBN53b_roc$X0,
                     response=bene1_woe_train3$TARGET,
                     levels=rev(levels(bene1_woe_train3$TARGET)))
b1_DBN53b.ROC

#normalizedGini
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
bene1_woe_train3$TARGETb <- as.numeric(levels(bene1_woe_train3$TARGETb))[bene1_woe_train3$TARGETb]
set.seed(123); b1_model_DBN53b_gini <- train(formula, data=bene1_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN53b_gini$results
b1_model_DBN53b_gini$resample
b1_pred_DBN53b_gini<- predict(b1_model_DBN53b_gini, newdata = bene1_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train3$TARGETb, b1_pred_DBN53b_gini$X1)
Gini(bene1_woe_train3$TARGETb, b1_pred_DBN53b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN53b_gini$X1[b1_pred_DBN53b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train3$TARGETb[b1_pred_DBN53b_gini$X1<=0.4]))
b1_DBN53b.ngini <- normalizedGini(a, p)
b1_DBN53b.ngini
b1_DBN53b.gini <-Gini(a, p)
b1_DBN53b.gini

#Brier score
set.seed(123); b1_model_DBN53b_brier <- train(formula, data=bene1_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN53b_brier$results
b1_model_DBN53b_brier$resample
b1_pred_DBN53b_brier <- predict(b1_model_DBN53b_brier, newdata = bene1_woe_train3, type='prob')
bene1_woe_train3$TARGETb <- bene1_woe_train3$TARGET
levels(bene1_woe_train3$TARGETb) <- c('0','1')
b1_DBN53b.bs <- Brier(as.numeric(as.character(bene1_woe_train3$TARGETb)), b1_pred_DBN53b_brier$X1)
b1_DBN53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN54a_roc <- train(formula, data=bene1_woe_train4, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN54a_roc <- predict(b1_model_DBN54a_roc,bene1_woe_test4,type="prob")
b1_DBN54a.ROC <- roc(predictor=b1predb_DBN54a_roc$X0,
                     response=bene1_woe_test4$TARGET,
                     levels=rev(levels(bene1_woe_test4$TARGET)))
b1_DBN54a.ROC

#normalizedGini
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
bene1_woe_test4$TARGETb <- as.numeric(levels(bene1_woe_test4$TARGETb))[bene1_woe_test4$TARGETb]
set.seed(123); b1_model_DBN54a_gini <- train(formula, data=bene1_woe_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN54a_gini$results
b1_model_DBN54a_gini$resample
b1_pred_DBN54a_gini<- predict(b1_model_DBN54a_gini, newdata = bene1_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test4$TARGETb, b1_pred_DBN54a_gini$X1)
Gini(bene1_woe_test4$TARGETb, b1_pred_DBN54a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN54a_gini$X1[b1_pred_DBN54a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test4$TARGETb[b1_pred_DBN54a_gini$X1<=0.4]))
b1_DBN54a.ngini <- normalizedGini(a, p)
b1_DBN54a.ngini
b1_DBN54a.gini <-Gini(a, p)
b1_DBN54a.gini

#Brier score
set.seed(123); b1_model_DBN54a_brier <- train(formula, data=bene1_woe_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN54a_brier$results
b1_model_DBN54a_brier$resample
b1_pred_DBN54a_brier <- predict(b1_model_DBN54a_brier, newdata = bene1_woe_test4, type='prob')
bene1_woe_test4$TARGETb <- bene1_woe_test4$TARGET
levels(bene1_woe_test4$TARGETb) <- c('0','1')
b1_DBN54a.bs <- Brier(as.numeric(as.character(bene1_woe_test4$TARGETb)), b1_pred_DBN54a_brier$X1)
b1_DBN54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b1_model_DBN54b_roc <- train(formula, data=bene1_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN54b_roc <- predict(b1_model_DBN54b_roc, bene1_woe_train4,type="prob")
b1_DBN54b.ROC <- roc(predictor=b1predb_DBN54b_roc$X0,
                     response=bene1_woe_train4$TARGET,
                     levels=rev(levels(bene1_woe_train4$TARGET)))
b1_DBN54b.ROC

#normalizedGini
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
bene1_woe_train4$TARGETb <- as.numeric(levels(bene1_woe_train4$TARGETb))[bene1_woe_train4$TARGETb]
set.seed(123); b1_model_DBN54b_gini <- train(formula, data=bene1_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN54b_gini$results
b1_model_DBN54b_gini$resample
b1_pred_DBN54b_gini<- predict(b1_model_DBN54b_gini, newdata = bene1_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train4$TARGETb, b1_pred_DBN54b_gini$X1)
Gini(bene1_woe_train4$TARGETb, b1_pred_DBN54b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN54b_gini$X1[b1_pred_DBN54b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train4$TARGETb[b1_pred_DBN54b_gini$X1<=0.4]))
b1_DBN54b.ngini <- normalizedGini(a, p)
b1_DBN54b.ngini
b1_DBN54b.gini <-Gini(a, p)
b1_DBN54b.gini

#Brier score
set.seed(123); b1_model_DBN54b_brier <- train(formula, data=bene1_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN54b_brier$results
b1_model_DBN54b_brier$resample
b1_pred_DBN54b_brier <- predict(b1_model_DBN54b_brier, newdata = bene1_woe_train4, type='prob')
bene1_woe_train4$TARGETb <- bene1_woe_train4$TARGET
levels(bene1_woe_train4$TARGETb) <- c('0','1')
b1_DBN54b.bs <- Brier(as.numeric(as.character(bene1_woe_train4$TARGETb)), b1_pred_DBN54b_brier$X1)
b1_DBN54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN55a_roc <- train(formula, data=bene1_woe_train5, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN55a_roc <- predict(b1_model_DBN55a_roc,bene1_woe_test5,type="prob")
b1_DBN55a.ROC <- roc(predictor=b1predb_DBN55a_roc$X0,
                     response=bene1_woe_test5$TARGET,
                     levels=rev(levels(bene1_woe_test5$TARGET)))
b1_DBN55a.ROC

#normalizedGini
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
bene1_woe_test5$TARGETb <- as.numeric(levels(bene1_woe_test5$TARGETb))[bene1_woe_test5$TARGETb]
set.seed(123); b1_model_DBN55a_gini <- train(formula, data=bene1_woe_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN55a_gini$results
b1_model_DBN55a_gini$resample
b1_pred_DBN55a_gini<- predict(b1_model_DBN55a_gini, newdata = bene1_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test5$TARGETb, b1_pred_DBN55a_gini$X1)
Gini(bene1_woe_test5$TARGETb, b1_pred_DBN55a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN55a_gini$X1[b1_pred_DBN55a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test5$TARGETb[b1_pred_DBN55a_gini$X1<=0.4]))
b1_DBN55a.ngini <- normalizedGini(a, p)
b1_DBN55a.ngini
b1_DBN55a.gini <-Gini(a, p)
b1_DBN55a.gini

#Brier score
set.seed(123); b1_model_DBN55a_brier <- train(formula, data=bene1_woe_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN55a_brier$results
b1_model_DBN55a_brier$resample
b1_pred_DBN55a_brier <- predict(b1_model_DBN55a_brier, newdata = bene1_woe_test5, type='prob')
bene1_woe_test5$TARGETb <- bene1_woe_test5$TARGET
levels(bene1_woe_test5$TARGETb) <- c('0','1')
b1_DBN55a.bs <- Brier(as.numeric(as.character(bene1_woe_test5$TARGETb)), b1_pred_DBN55a_brier$X1)
b1_DBN55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b1_model_DBN55b_roc <- train(formula, data=bene1_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN55b_roc <- predict(b1_model_DBN55b_roc, bene1_woe_train5,type="prob")
b1_DBN55b.ROC <- roc(predictor=b1predb_DBN55b_roc$X0,
                     response=bene1_woe_train5$TARGET,
                     levels=rev(levels(bene1_woe_train5$TARGET)))
b1_DBN55b.ROC

#normalizedGini
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
bene1_woe_train5$TARGETb <- as.numeric(levels(bene1_woe_train5$TARGETb))[bene1_woe_train5$TARGETb]
set.seed(123); b1_model_DBN55b_gini <- train(formula, data=bene1_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN55b_gini$results
b1_model_DBN55b_gini$resample
b1_pred_DBN55b_gini<- predict(b1_model_DBN55b_gini, newdata = bene1_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train5$TARGETb, b1_pred_DBN55b_gini$X1)
Gini(bene1_woe_train5$TARGETb, b1_pred_DBN55b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN55b_gini$X1[b1_pred_DBN55b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train5$TARGETb[b1_pred_DBN55b_gini$X1<=0.4]))
b1_DBN55b.ngini <- normalizedGini(a, p)
b1_DBN55b.ngini
b1_DBN55b.gini <-Gini(a, p)
b1_DBN55b.gini

#Brier score
set.seed(123); b1_model_DBN55b_brier <- train(formula, data=bene1_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN55b_brier$results
b1_model_DBN55b_brier$resample
b1_pred_DBN55b_brier <- predict(b1_model_DBN55b_brier, newdata = bene1_woe_train5, type='prob')
bene1_woe_train5$TARGETb <- bene1_woe_train5$TARGET
levels(bene1_woe_train5$TARGETb) <- c('0','1')
b1_DBN55b.bs <- Brier(as.numeric(as.character(bene1_woe_train5$TARGETb)), b1_pred_DBN55b_brier$X1)
b1_DBN55b.bs

###data 6, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN56a_roc <- train(formula, data=bene1_woe_train6, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN56a_roc <- predict(b1_model_DBN56a_roc,bene1_woe_test6,type="prob")
b1_DBN56a.ROC <- roc(predictor=b1predb_DBN56a_roc$X0,
                     response=bene1_woe_test6$TARGET,
                     levels=rev(levels(bene1_woe_test6$TARGET)))
b1_DBN56a.ROC

#normalizedGini
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
bene1_woe_test6$TARGETb <- as.numeric(levels(bene1_woe_test6$TARGETb))[bene1_woe_test6$TARGETb]
set.seed(123); b1_model_DBN56a_gini <- train(formula, data=bene1_woe_train6,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN56a_gini$results
b1_model_DBN56a_gini$resample
b1_pred_DBN56a_gini<- predict(b1_model_DBN56a_gini, newdata = bene1_woe_test6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test6$TARGETb, b1_pred_DBN56a_gini$X1)
Gini(bene1_woe_test6$TARGETb, b1_pred_DBN56a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN56a_gini$X1[b1_pred_DBN56a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test6$TARGETb[b1_pred_DBN56a_gini$X1<=0.4]))
b1_DBN56a.ngini <- normalizedGini(a, p)
b1_DBN56a.ngini
b1_DBN56a.gini <-Gini(a, p)
b1_DBN56a.gini

#Brier score
set.seed(123); b1_model_DBN56a_brier <- train(formula, data=bene1_woe_train6,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN56a_brier$results
b1_model_DBN56a_brier$resample
b1_pred_DBN56a_brier <- predict(b1_model_DBN56a_brier, newdata = bene1_woe_test6, type='prob')
bene1_woe_test6$TARGETb <- bene1_woe_test6$TARGET
levels(bene1_woe_test6$TARGETb) <- c('0','1')
b1_DBN56a.bs <- Brier(as.numeric(as.character(bene1_woe_test6$TARGETb)), b1_pred_DBN56a_brier$X1)
b1_DBN56a.bs

###data 6 - test-train
#ROC curve 
set.seed(123); b1_model_DBN56b_roc <- train(formula, data=bene1_woe_test6,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN56b_roc <- predict(b1_model_DBN56b_roc, bene1_woe_train6,type="prob")
b1_DBN56b.ROC <- roc(predictor=b1predb_DBN56b_roc$X0,
                     response=bene1_woe_train6$TARGET,
                     levels=rev(levels(bene1_woe_train6$TARGET)))
b1_DBN56b.ROC

#normalizedGini
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
bene1_woe_train6$TARGETb <- as.numeric(levels(bene1_woe_train6$TARGETb))[bene1_woe_train6$TARGETb]
set.seed(123); b1_model_DBN56b_gini <- train(formula, data=bene1_woe_test6,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN56b_gini$results
b1_model_DBN56b_gini$resample
b1_pred_DBN56b_gini<- predict(b1_model_DBN56b_gini, newdata = bene1_woe_train6, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train6$TARGETb, b1_pred_DBN56b_gini$X1)
Gini(bene1_woe_train6$TARGETb, b1_pred_DBN56b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN56b_gini$X1[b1_pred_DBN56b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train6$TARGETb[b1_pred_DBN56b_gini$X1<=0.4]))
b1_DBN56b.ngini <- normalizedGini(a, p)
b1_DBN56b.ngini
b1_DBN56b.gini <-Gini(a, p)
b1_DBN56b.gini

#Brier score
set.seed(123); b1_model_DBN56b_brier <- train(formula, data=bene1_woe_test6,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN56b_brier$results
b1_model_DBN56b_brier$resample
b1_pred_DBN56b_brier <- predict(b1_model_DBN56b_brier, newdata = bene1_woe_train6, type='prob')
bene1_woe_train6$TARGETb <- bene1_woe_train6$TARGET
levels(bene1_woe_train6$TARGETb) <- c('0','1')
b1_DBN56b.bs <- Brier(as.numeric(as.character(bene1_woe_train6$TARGETb)), b1_pred_DBN56b_brier$X1)
b1_DBN56b.bs

###data 7, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN57a_roc <- train(formula, data=bene1_woe_train7, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN57a_roc <- predict(b1_model_DBN57a_roc,bene1_woe_test7,type="prob")
b1_DBN57a.ROC <- roc(predictor=b1predb_DBN57a_roc$X0,
                     response=bene1_woe_test7$TARGET,
                     levels=rev(levels(bene1_woe_test7$TARGET)))
b1_DBN57a.ROC

#normalizedGini
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
bene1_woe_test7$TARGETb <- as.numeric(levels(bene1_woe_test7$TARGETb))[bene1_woe_test7$TARGETb]
set.seed(123); b1_model_DBN57a_gini <- train(formula, data=bene1_woe_train7,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN57a_gini$results
b1_model_DBN57a_gini$resample
b1_pred_DBN57a_gini<- predict(b1_model_DBN57a_gini, newdata = bene1_woe_test7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test7$TARGETb, b1_pred_DBN57a_gini$X1)
Gini(bene1_woe_test7$TARGETb, b1_pred_DBN57a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN57a_gini$X1[b1_pred_DBN57a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test7$TARGETb[b1_pred_DBN57a_gini$X1<=0.4]))
b1_DBN57a.ngini <- normalizedGini(a, p)
b1_DBN57a.ngini
b1_DBN57a.gini <-Gini(a, p)
b1_DBN57a.gini

#Brier score
set.seed(123); b1_model_DBN57a_brier <- train(formula, data=bene1_woe_train7,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN57a_brier$results
b1_model_DBN57a_brier$resample
b1_pred_DBN57a_brier <- predict(b1_model_DBN57a_brier, newdata = bene1_woe_test7, type='prob')
bene1_woe_test7$TARGETb <- bene1_woe_test7$TARGET
levels(bene1_woe_test7$TARGETb) <- c('0','1')
b1_DBN57a.bs <- Brier(as.numeric(as.character(bene1_woe_test7$TARGETb)), b1_pred_DBN57a_brier$X1)
b1_DBN57a.bs

###data 7 - test-train
#ROC curve 
set.seed(123); b1_model_DBN57b_roc <- train(formula, data=bene1_woe_test7,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN57b_roc <- predict(b1_model_DBN57b_roc, bene1_woe_train7,type="prob")
b1_DBN57b.ROC <- roc(predictor=b1predb_DBN57b_roc$X0,
                     response=bene1_woe_train7$TARGET,
                     levels=rev(levels(bene1_woe_train7$TARGET)))
b1_DBN57b.ROC

#normalizedGini
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
bene1_woe_train7$TARGETb <- as.numeric(levels(bene1_woe_train7$TARGETb))[bene1_woe_train7$TARGETb]
set.seed(123); b1_model_DBN57b_gini <- train(formula, data=bene1_woe_test7,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN57b_gini$results
b1_model_DBN57b_gini$resample
b1_pred_DBN57b_gini<- predict(b1_model_DBN57b_gini, newdata = bene1_woe_train7, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train7$TARGETb, b1_pred_DBN57b_gini$X1)
Gini(bene1_woe_train7$TARGETb, b1_pred_DBN57b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN57b_gini$X1[b1_pred_DBN57b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train7$TARGETb[b1_pred_DBN57b_gini$X1<=0.4]))
b1_DBN57b.ngini <- normalizedGini(a, p)
b1_DBN57b.ngini
b1_DBN57b.gini <-Gini(a, p)
b1_DBN57b.gini

#Brier score
set.seed(123); b1_model_DBN57b_brier <- train(formula, data=bene1_woe_test7,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN57b_brier$results
b1_model_DBN57b_brier$resample
b1_pred_DBN57b_brier <- predict(b1_model_DBN57b_brier, newdata = bene1_woe_train7, type='prob')
bene1_woe_train7$TARGETb <- bene1_woe_train7$TARGET
levels(bene1_woe_train7$TARGETb) <- c('0','1')
b1_DBN57b.bs <- Brier(as.numeric(as.character(bene1_woe_train7$TARGETb)), b1_pred_DBN57b_brier$X1)
b1_DBN57b.bs

###data 8, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN58a_roc <- train(formula, data=bene1_woe_train8, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN58a_roc <- predict(b1_model_DBN58a_roc,bene1_woe_test8,type="prob")
b1_DBN58a.ROC <- roc(predictor=b1predb_DBN58a_roc$X0,
                     response=bene1_woe_test8$TARGET,
                     levels=rev(levels(bene1_woe_test8$TARGET)))
b1_DBN58a.ROC

#normalizedGini
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
bene1_woe_test8$TARGETb <- as.numeric(levels(bene1_woe_test8$TARGETb))[bene1_woe_test8$TARGETb]
set.seed(123); b1_model_DBN58a_gini <- train(formula, data=bene1_woe_train8,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN58a_gini$results
b1_model_DBN58a_gini$resample
b1_pred_DBN58a_gini<- predict(b1_model_DBN58a_gini, newdata = bene1_woe_test8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test8$TARGETb, b1_pred_DBN58a_gini$X1)
Gini(bene1_woe_test8$TARGETb, b1_pred_DBN58a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN58a_gini$X1[b1_pred_DBN58a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test8$TARGETb[b1_pred_DBN58a_gini$X1<=0.4]))
b1_DBN58a.ngini <- normalizedGini(a, p)
b1_DBN58a.ngini
b1_DBN58a.gini <-Gini(a, p)
b1_DBN58a.gini

#Brier score
set.seed(123); b1_model_DBN58a_brier <- train(formula, data=bene1_woe_train8,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN58a_brier$results
b1_model_DBN58a_brier$resample
b1_pred_DBN58a_brier <- predict(b1_model_DBN58a_brier, newdata = bene1_woe_test8, type='prob')
bene1_woe_test8$TARGETb <- bene1_woe_test8$TARGET
levels(bene1_woe_test8$TARGETb) <- c('0','1')
b1_DBN58a.bs <- Brier(as.numeric(as.character(bene1_woe_test8$TARGETb)), b1_pred_DBN58a_brier$X1)
b1_DBN58a.bs

###data 8 - test-train
#ROC curve 
set.seed(123); b1_model_DBN58b_roc <- train(formula, data=bene1_woe_test8,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN58b_roc <- predict(b1_model_DBN58b_roc, bene1_woe_train8,type="prob")
b1_DBN58b.ROC <- roc(predictor=b1predb_DBN58b_roc$X0,
                     response=bene1_woe_train8$TARGET,
                     levels=rev(levels(bene1_woe_train8$TARGET)))
b1_DBN58b.ROC

#normalizedGini
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
bene1_woe_train8$TARGETb <- as.numeric(levels(bene1_woe_train8$TARGETb))[bene1_woe_train8$TARGETb]
set.seed(123); b1_model_DBN58b_gini <- train(formula, data=bene1_woe_test8,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN58b_gini$results
b1_model_DBN58b_gini$resample
b1_pred_DBN58b_gini<- predict(b1_model_DBN58b_gini, newdata = bene1_woe_train8, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train8$TARGETb, b1_pred_DBN58b_gini$X1)
Gini(bene1_woe_train8$TARGETb, b1_pred_DBN58b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN58b_gini$X1[b1_pred_DBN58b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train8$TARGETb[b1_pred_DBN58b_gini$X1<=0.4]))
b1_DBN58b.ngini <- normalizedGini(a, p)
b1_DBN58b.ngini
b1_DBN58b.gini <-Gini(a, p)
b1_DBN58b.gini

#Brier score
set.seed(123); b1_model_DBN58b_brier <- train(formula, data=bene1_woe_test8,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN58b_brier$results
b1_model_DBN58b_brier$resample
b1_pred_DBN58b_brier <- predict(b1_model_DBN58b_brier, newdata = bene1_woe_train8, type='prob')
bene1_woe_train8$TARGETb <- bene1_woe_train8$TARGET
levels(bene1_woe_train8$TARGETb) <- c('0','1')
b1_DBN58b.bs <- Brier(as.numeric(as.character(bene1_woe_train8$TARGETb)), b1_pred_DBN58b_brier$X1)
b1_DBN58b.bs


###data 9, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN59a_roc <- train(formula, data=bene1_woe_train9, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN59a_roc <- predict(b1_model_DBN59a_roc,bene1_woe_test9,type="prob")
b1_DBN59a.ROC <- roc(predictor=b1predb_DBN59a_roc$X0,
                     response=bene1_woe_test9$TARGET,
                     levels=rev(levels(bene1_woe_test9$TARGET)))
b1_DBN59a.ROC

#normalizedGini
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
bene1_woe_test9$TARGETb <- as.numeric(levels(bene1_woe_test9$TARGETb))[bene1_woe_test9$TARGETb]
set.seed(123); b1_model_DBN59a_gini <- train(formula, data=bene1_woe_train9,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN59a_gini$results
b1_model_DBN59a_gini$resample
b1_pred_DBN59a_gini<- predict(b1_model_DBN59a_gini, newdata = bene1_woe_test9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test9$TARGETb, b1_pred_DBN59a_gini$X1)
Gini(bene1_woe_test9$TARGETb, b1_pred_DBN59a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN59a_gini$X1[b1_pred_DBN59a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test9$TARGETb[b1_pred_DBN59a_gini$X1<=0.4]))
b1_DBN59a.ngini <- normalizedGini(a, p)
b1_DBN59a.ngini
b1_DBN59a.gini <-Gini(a, p)
b1_DBN59a.gini

#Brier score
set.seed(123); b1_model_DBN59a_brier <- train(formula, data=bene1_woe_train9,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN59a_brier$results
b1_model_DBN59a_brier$resample
b1_pred_DBN59a_brier <- predict(b1_model_DBN59a_brier, newdata = bene1_woe_test9, type='prob')
bene1_woe_test9$TARGETb <- bene1_woe_test9$TARGET
levels(bene1_woe_test9$TARGETb) <- c('0','1')
b1_DBN59a.bs <- Brier(as.numeric(as.character(bene1_woe_test9$TARGETb)), b1_pred_DBN59a_brier$X1)
b1_DBN59a.bs

###data 9 - test-train
#ROC curve 
set.seed(123); b1_model_DBN59b_roc <- train(formula, data=bene1_woe_test9,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN59b_roc <- predict(b1_model_DBN59b_roc, bene1_woe_train9,type="prob")
b1_DBN59b.ROC <- roc(predictor=b1predb_DBN59b_roc$X0,
                     response=bene1_woe_train9$TARGET,
                     levels=rev(levels(bene1_woe_train9$TARGET)))
b1_DBN59b.ROC

#normalizedGini
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
bene1_woe_train9$TARGETb <- as.numeric(levels(bene1_woe_train9$TARGETb))[bene1_woe_train9$TARGETb]
set.seed(123); b1_model_DBN59b_gini <- train(formula, data=bene1_woe_test9,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN59b_gini$results
b1_model_DBN59b_gini$resample
b1_pred_DBN59b_gini<- predict(b1_model_DBN59b_gini, newdata = bene1_woe_train9, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train9$TARGETb, b1_pred_DBN59b_gini$X1)
Gini(bene1_woe_train9$TARGETb, b1_pred_DBN59b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN59b_gini$X1[b1_pred_DBN59b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train9$TARGETb[b1_pred_DBN59b_gini$X1<=0.4]))
b1_DBN59b.ngini <- normalizedGini(a, p)
b1_DBN59b.ngini
b1_DBN59b.gini <-Gini(a, p)
b1_DBN59b.gini

#Brier score
set.seed(123); b1_model_DBN59b_brier <- train(formula, data=bene1_woe_test9,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN59b_brier$results
b1_model_DBN59b_brier$resample
b1_pred_DBN59b_brier <- predict(b1_model_DBN59b_brier, newdata = bene1_woe_train9, type='prob')
bene1_woe_train9$TARGETb <- bene1_woe_train9$TARGET
levels(bene1_woe_train9$TARGETb) <- c('0','1')
b1_DBN59b.bs <- Brier(as.numeric(as.character(bene1_woe_train9$TARGETb)), b1_pred_DBN59b_brier$X1)
b1_DBN59b.bs

###data 10, train-test
#ROC curve 
#test it
set.seed(123); b1_model_DBN510a_roc <- train(formula, data=bene1_woe_train10, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN510a_roc <- predict(b1_model_DBN510a_roc,bene1_woe_test10,type="prob")
b1_DBN510a.ROC <- roc(predictor=b1predb_DBN510a_roc$X0,
                      response=bene1_woe_test10$TARGET,
                      levels=rev(levels(bene1_woe_test10$TARGET)))
b1_DBN510a.ROC

#normalizedGini
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
bene1_woe_test10$TARGETb <- as.numeric(levels(bene1_woe_test10$TARGETb))[bene1_woe_test10$TARGETb]
set.seed(123); b1_model_DBN510a_gini <- train(formula, data=bene1_woe_train10,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN510a_gini$results
b1_model_DBN510a_gini$resample
b1_pred_DBN510a_gini<- predict(b1_model_DBN510a_gini, newdata = bene1_woe_test10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_test10$TARGETb, b1_pred_DBN510a_gini$X1)
Gini(bene1_woe_test10$TARGETb, b1_pred_DBN510a_gini$X1)
#b <= 0.4
p <- b1_pred_DBN510a_gini$X1[b1_pred_DBN510a_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_test10$TARGETb[b1_pred_DBN510a_gini$X1<=0.4]))
b1_DBN510a.ngini <- normalizedGini(a, p)
b1_DBN510a.ngini
b1_DBN510a.gini <-Gini(a, p)
b1_DBN510a.gini

#Brier score
set.seed(123); b1_model_DBN510a_brier <- train(formula, data=bene1_woe_train10,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN510a_brier$results
b1_model_DBN510a_brier$resample
b1_pred_DBN510a_brier <- predict(b1_model_DBN510a_brier, newdata = bene1_woe_test10, type='prob')
bene1_woe_test10$TARGETb <- bene1_woe_test10$TARGET
levels(bene1_woe_test10$TARGETb) <- c('0','1')
b1_DBN510a.bs <- Brier(as.numeric(as.character(bene1_woe_test10$TARGETb)), b1_pred_DBN510a_brier$X1)
b1_DBN510a.bs

###data 10 - test-train
#ROC curve 
set.seed(123); b1_model_DBN510b_roc <- train(formula, data=bene1_woe_test10,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b1predb_DBN510b_roc <- predict(b1_model_DBN510b_roc, bene1_woe_train10,type="prob")
b1_DBN510b.ROC <- roc(predictor=b1predb_DBN510b_roc$X0,
                      response=bene1_woe_train10$TARGET,
                      levels=rev(levels(bene1_woe_train10$TARGET)))
b1_DBN510b.ROC

#normalizedGini
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
bene1_woe_train10$TARGETb <- as.numeric(levels(bene1_woe_train10$TARGETb))[bene1_woe_train10$TARGETb]
set.seed(123); b1_model_DBN510b_gini <- train(formula, data=bene1_woe_test10,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b1_model_DBN510b_gini$results
b1_model_DBN510b_gini$resample
b1_pred_DBN510b_gini<- predict(b1_model_DBN510b_gini, newdata = bene1_woe_train10, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene1_woe_train10$TARGETb, b1_pred_DBN510b_gini$X1)
Gini(bene1_woe_train10$TARGETb, b1_pred_DBN510b_gini$X1)
#b <= 0.4
p <- b1_pred_DBN510b_gini$X1[b1_pred_DBN510b_gini$X1<=0.4]
a <- as.numeric(as.character(bene1_woe_train10$TARGETb[b1_pred_DBN510b_gini$X1<=0.4]))
b1_DBN510b.ngini <- normalizedGini(a, p)
b1_DBN510b.ngini
b1_DBN510b.gini <-Gini(a, p)
b1_DBN510b.gini

#Brier score
set.seed(123); b1_model_DBN510b_brier <- train(formula, data=bene1_woe_test10,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b1_model_DBN510b_brier$results
b1_model_DBN510b_brier$resample
b1_pred_DBN510b_brier <- predict(b1_model_DBN510b_brier, newdata = bene1_woe_train10, type='prob')
bene1_woe_train10$TARGETb <- bene1_woe_train10$TARGET
levels(bene1_woe_train10$TARGETb) <- c('0','1')
b1_DBN510b.bs <- Brier(as.numeric(as.character(bene1_woe_train10$TARGETb)), b1_pred_DBN510b_brier$X1)
b1_DBN510b.bs

##Restults RF!
b1_results_DBN5_AUC <- cbind(b1_DBN51a.ROC$auc,b1_DBN51b.ROC$auc,b1_DBN52a.ROC$auc,b1_DBN52b.ROC$auc,b1_DBN53a.ROC$auc,b1_DBN53b.ROC$auc,b1_DBN54a.ROC$auc,
                             b1_DBN54b.ROC$auc,b1_DBN55a.ROC$auc,b1_DBN55b.ROC$auc,b1_DBN56a.ROC$auc,b1_DBN56b.ROC$auc,b1_DBN57a.ROC$auc,b1_DBN57b.ROC$auc,
                             b1_DBN58a.ROC$auc,b1_DBN58b.ROC$auc,b1_DBN59a.ROC$auc,b1_DBN59b.ROC$auc,b1_DBN510a.ROC$auc,b1_DBN510b.ROC$auc)
b1_results_DBN5_bs <- cbind(b1_DBN51a.bs,b1_DBN51b.bs,b1_DBN52a.bs,b1_DBN52b.bs,b1_DBN53a.bs,b1_DBN53b.bs,b1_DBN54a.bs,b1_DBN54b.bs,b1_DBN55a.bs,b1_DBN55b.bs,
                            b1_DBN56a.bs,b1_DBN56b.bs,b1_DBN57a.bs,b1_DBN57b.bs,b1_DBN58a.bs,b1_DBN58b.bs,b1_DBN59a.bs,b1_DBN59b.bs,b1_DBN510a.bs,b1_DBN510b.bs)
b1_results_DBN5_ngini <- cbind(b1_DBN51a.ngini,b1_DBN51b.ngini,b1_DBN52a.ngini,b1_DBN52b.ngini,b1_DBN53a.ngini,b1_DBN53b.ngini,b1_DBN54a.ngini,b1_DBN54b.ngini,
                               b1_DBN55a.ngini,b1_DBN55b.ngini,b1_DBN56a.ngini,b1_DBN56b.ngini,b1_DBN57a.ngini,b1_DBN57b.ngini,b1_DBN58a.ngini,b1_DBN58b.ngini,
                               b1_DBN59a.ngini,b1_DBN59b.ngini,b1_DBN510a.ngini,b1_DBN510b.ngini)
b1_results_DBN5_gini <- cbind(b1_DBN51a.gini,b1_DBN51b.gini,b1_DBN52a.gini,b1_DBN52b.gini,b1_DBN53a.gini,b1_DBN53b.gini,b1_DBN54a.gini,b1_DBN54b.gini,
                              b1_DBN55a.gini,b1_DBN55b.gini,b1_DBN56a.gini,b1_DBN56b.gini,b1_DBN57a.gini,b1_DBN57b.gini,b1_DBN58a.gini,b1_DBN58b.gini,
                              b1_DBN59a.gini,b1_DBN59b.gini,b1_DBN510a.gini,b1_DBN510b.gini)
mean(b1_results_DBN5_AUC)
mean(b1_results_DBN5_bs)
mean(b1_results_DBN5_ngini)
mean(b1_results_DBN5_gini)

###############################
###########EMP#################
###############################

#bene1 - logistic
set.seed(123); b1_model_log1a_emp <- train(formula, data=bene1_woe_train1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log1a_emp <- predict(b1_model_log1a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_log1a <- empCreditScoring(b1predb_log1a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_log1b_emp <- train(formula, data=bene1_woe_test1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log1b_emp <- predict(b1_model_log1b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_log1b <- empCreditScoring(b1predb_log1b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_log2a_emp <- train(formula, data=bene1_woe_train2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log2a_emp <- predict(b1_model_log2a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_log2a <- empCreditScoring(b1predb_log2a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_log2b_emp <- train(formula, data=bene1_woe_test2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log2b_emp <- predict(b1_model_log2b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_log2b <- empCreditScoring(b1predb_log2b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_log3a_emp <- train(formula, data=bene1_woe_train3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log3a_emp <- predict(b1_model_log3a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_log3a <- empCreditScoring(b1predb_log3a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_log3b_emp <- train(formula, data=bene1_woe_test3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log3b_emp <- predict(b1_model_log3b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_log3b <- empCreditScoring(b1predb_log3b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_log4a_emp <- train(formula, data=bene1_woe_train4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log4a_emp <- predict(b1_model_log4a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_log4a <- empCreditScoring(b1predb_log4a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_log4b_emp <- train(formula, data=bene1_woe_test4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log4b_emp <- predict(b1_model_log4b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_log4b <- empCreditScoring(b1predb_log4b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_log5a_emp <- train(formula, data=bene1_woe_train5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log5a_emp <- predict(b1_model_log5a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_log5a <- empCreditScoring(b1predb_log5a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_log5b_emp <- train(formula, data=bene1_woe_test5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log5b_emp <- predict(b1_model_log5b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_log5b <- empCreditScoring(b1predb_log5b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_log6a_emp <- train(formula, data=bene1_woe_train6, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log6a_emp <- predict(b1_model_log6a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_log6a <- empCreditScoring(b1predb_log6a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_log6b_emp <- train(formula, data=bene1_woe_test6, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log6b_emp <- predict(b1_model_log6b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_log6b <- empCreditScoring(b1predb_log6b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_log7a_emp <- train(formula, data=bene1_woe_train7, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log7a_emp <- predict(b1_model_log7a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_log7a <- empCreditScoring(b1predb_log7a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_log7b_emp <- train(formula, data=bene1_woe_test7, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log7b_emp <- predict(b1_model_log7b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_log7b <- empCreditScoring(b1predb_log7b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_log8a_emp <- train(formula, data=bene1_woe_train8, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log8a_emp <- predict(b1_model_log8a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_log8a <- empCreditScoring(b1predb_log8a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_log8b_emp <- train(formula, data=bene1_woe_test8, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log8b_emp <- predict(b1_model_log8b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_log8b <- empCreditScoring(b1predb_log8b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_log9a_emp <- train(formula, data=bene1_woe_train9, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log9a_emp <- predict(b1_model_log9a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_log9a <- empCreditScoring(b1predb_log9a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_log9b_emp <- train(formula, data=bene1_woe_test9, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log9b_emp <- predict(b1_model_log9b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_log9b <- empCreditScoring(b1predb_log9b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_log10a_emp <- train(formula, data=bene1_woe_train10, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log10a_emp <- predict(b1_model_log10a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_log10a <- empCreditScoring(b1predb_log10a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_log10b_emp <- train(formula, data=bene1_woe_test10, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b1predb_log10b_emp <- predict(b1_model_log10b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_log10b <- empCreditScoring(b1predb_log10b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - dt
DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
set.seed(123); b1reg_model_DT1a_emp <- train(formula, data=bene1_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT1a_emp <- predict(b1reg_model_DT1a_emp, newdata = bene1_test1, type='prob')
b1emp_dt1a <- empCreditScoring(b1regpredb_DT1a_emp$X1,bene1_test1$TARGET)
set.seed(123); b1reg_model_DT1b_emp <- train(formula, data=bene1_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT1b_emp <- predict(b1reg_model_DT1b_emp, newdata = bene1_train1, type='prob')
b1emp_dt1b <- empCreditScoring(b1regpredb_DT1b_emp$X1,bene1_train1$TARGET)
set.seed(123); b1reg_model_DT2a_emp <- train(formula, data=bene1_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT2a_emp <- predict(b1reg_model_DT2a_emp, newdata = bene1_test2, type='prob')
b1emp_dt2a <- empCreditScoring(b1regpredb_DT2a_emp$X1,bene1_test2$TARGET)
set.seed(123); b1reg_model_DT2b_emp <- train(formula, data=bene1_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT2b_emp <- predict(b1reg_model_DT2b_emp, newdata = bene1_train2, type='prob')
b1emp_dt2b <- empCreditScoring(b1regpredb_DT2b_emp$X1,bene1_train2$TARGET)
set.seed(123); b1reg_model_DT3a_emp <- train(formula, data=bene1_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT3a_emp <- predict(b1reg_model_DT3a_emp, newdata = bene1_test3, type='prob')
b1emp_dt3a <- empCreditScoring(b1regpredb_DT3a_emp$X1,bene1_test3$TARGET)
set.seed(123); b1reg_model_DT3b_emp <- train(formula, data=bene1_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT3b_emp <- predict(b1reg_model_DT3b_emp, newdata = bene1_train3, type='prob')
b1emp_dt3b <- empCreditScoring(b1regpredb_DT3b_emp$X1,bene1_train3$TARGET)
set.seed(123); b1reg_model_DT4a_emp <- train(formula, data=bene1_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT4a_emp <- predict(b1reg_model_DT4a_emp, newdata = bene1_test4, type='prob')
b1emp_dt4a <- empCreditScoring(b1regpredb_DT4a_emp$X1,bene1_test4$TARGET)
set.seed(123); b1reg_model_DT4b_emp <- train(formula, data=bene1_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT4b_emp <- predict(b1reg_model_DT4b_emp, newdata = bene1_train4, type='prob')
b1emp_dt4b <- empCreditScoring(b1regpredb_DT4b_emp$X1,bene1_train4$TARGET)
set.seed(123); b1reg_model_DT5a_emp <- train(formula, data=bene1_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT5a_emp <- predict(b1reg_model_DT5a_emp, newdata = bene1_test5, type='prob')
b1emp_dt5a <- empCreditScoring(b1regpredb_DT5a_emp$X1,bene1_test5$TARGET)
set.seed(123); b1reg_model_DT5b_emp <- train(formula, data=bene1_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT5b_emp <- predict(b1reg_model_DT5b_emp, newdata = bene1_train5, type='prob')
b1emp_dt5b <- empCreditScoring(b1regpredb_DT5b_emp$X1,bene1_train5$TARGET)
set.seed(123); b1reg_model_DT6a_emp <- train(formula, data=bene1_train6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT6a_emp <- predict(b1reg_model_DT6a_emp, newdata = bene1_test6, type='prob')
b1emp_dt6a <- empCreditScoring(b1regpredb_DT6a_emp$X1,bene1_test6$TARGET)
set.seed(123); b1reg_model_DT6b_emp <- train(formula, data=bene1_test6,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT6b_emp <- predict(b1reg_model_DT6b_emp, newdata = bene1_train6, type='prob')
b1emp_dt6b <- empCreditScoring(b1regpredb_DT6b_emp$X1,bene1_train6$TARGET)
set.seed(123); b1reg_model_DT7a_emp <- train(formula, data=bene1_train7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT7a_emp <- predict(b1reg_model_DT7a_emp, newdata = bene1_test7, type='prob')
b1emp_dt7a <- empCreditScoring(b1regpredb_DT7a_emp$X1,bene1_test7$TARGET)
set.seed(123); b1reg_model_DT7b_emp <- train(formula, data=bene1_test7,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT7b_emp <- predict(b1reg_model_DT7b_emp, newdata = bene1_train7, type='prob')
b1emp_dt7b <- empCreditScoring(b1regpredb_DT7b_emp$X1,bene1_train7$TARGET)
set.seed(123); b1reg_model_DT8a_emp <- train(formula, data=bene1_train8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT8a_emp <- predict(b1reg_model_DT8a_emp, newdata = bene1_test8, type='prob')
b1emp_dt8a <- empCreditScoring(b1regpredb_DT8a_emp$X1,bene1_test8$TARGET)
set.seed(123); b1reg_model_DT8b_emp <- train(formula, data=bene1_test8,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT8b_emp <- predict(b1reg_model_DT8b_emp, newdata = bene1_train8, type='prob')
b1emp_dt8b <- empCreditScoring(b1regpredb_DT8b_emp$X1,bene1_train8$TARGET)
set.seed(123); b1reg_model_DT9a_emp <- train(formula, data=bene1_train9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT9a_emp <- predict(b1reg_model_DT9a_emp, newdata = bene1_test9, type='prob')
b1emp_dt9a <- empCreditScoring(b1regpredb_DT9a_emp$X1,bene1_test9$TARGET)
set.seed(123); b1reg_model_DT9b_emp <- train(formula, data=bene1_test9,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT9b_emp <- predict(b1reg_model_DT9b_emp, newdata = bene1_train9, type='prob')
b1emp_dt9b <- empCreditScoring(b1regpredb_DT9b_emp$X1,bene1_train9$TARGET)
set.seed(123); b1reg_model_DT10a_emp <- train(formula, data=bene1_train10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT10a_emp <- predict(b1reg_model_DT10a_emp, newdata = bene1_test10, type='prob')
b1emp_dt10a <- empCreditScoring(b1regpredb_DT10a_emp$X1,bene1_test10$TARGET)
set.seed(123); b1reg_model_DT10b_emp <- train(formula, data=bene1_test10,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b1regpredb_DT10b_emp <- predict(b1reg_model_DT10b_emp, newdata = bene1_train10, type='prob')
b1emp_dt10b <- empCreditScoring(b1regpredb_DT10b_emp$X1,bene1_train10$TARGET)

#bene1 - RF
m <- floor(log2(length(bene1_train1$TARGET)+1))
RFgrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))
set.seed(123); b1reg_model_RF1a_emp <- train(formula, data=bene1_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF1a_emp <- predict(b1reg_model_RF1a_emp, newdata = bene1_test1, type='prob')
b1emp_RF1a <- empCreditScoring(b1regpredb_RF1a_emp$X1,bene1_test1$TARGET)
set.seed(123); b1reg_model_RF1b_emp <- train(formula, data=bene1_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF1b_emp <- predict(b1reg_model_RF1b_emp, newdata = bene1_train1, type='prob')
b1emp_RF1b <- empCreditScoring(b1regpredb_RF1b_emp$X1,bene1_train1$TARGET)
set.seed(123); b1reg_model_RF2a_emp <- train(formula, data=bene1_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF2a_emp <- predict(b1reg_model_RF2a_emp, newdata = bene1_test2, type='prob')
b1emp_RF2a <- empCreditScoring(b1regpredb_RF2a_emp$X1,bene1_test2$TARGET)
set.seed(123); b1reg_model_RF2b_emp <- train(formula, data=bene1_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF2b_emp <- predict(b1reg_model_RF2b_emp, newdata = bene1_train2, type='prob')
b1emp_RF2b <- empCreditScoring(b1regpredb_RF2b_emp$X1,bene1_train2$TARGET)
set.seed(123); b1reg_model_RF3a_emp <- train(formula, data=bene1_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF3a_emp <- predict(b1reg_model_RF3a_emp, newdata = bene1_test3, type='prob')
b1emp_RF3a <- empCreditScoring(b1regpredb_RF3a_emp$X1,bene1_test3$TARGET)
set.seed(123); b1reg_model_RF3b_emp <- train(formula, data=bene1_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF3b_emp <- predict(b1reg_model_RF3b_emp, newdata = bene1_train3, type='prob')
b1emp_RF3b <- empCreditScoring(b1regpredb_RF3b_emp$X1,bene1_train3$TARGET)
set.seed(123); b1reg_model_RF4a_emp <- train(formula, data=bene1_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF4a_emp <- predict(b1reg_model_RF4a_emp, newdata = bene1_test4, type='prob')
b1emp_RF4a <- empCreditScoring(b1regpredb_RF4a_emp$X1,bene1_test4$TARGET)
set.seed(123); b1reg_model_RF4b_emp <- train(formula, data=bene1_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF4b_emp <- predict(b1reg_model_RF4b_emp, newdata = bene1_train4, type='prob')
b1emp_RF4b <- empCreditScoring(b1regpredb_RF4b_emp$X1,bene1_train4$TARGET)
set.seed(123); b1reg_model_RF5a_emp <- train(formula, data=bene1_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF5a_emp <- predict(b1reg_model_RF5a_emp, newdata = bene1_test5, type='prob')
b1emp_RF5a <- empCreditScoring(b1regpredb_RF5a_emp$X1,bene1_test5$TARGET)
set.seed(123); b1reg_model_RF5b_emp <- train(formula, data=bene1_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF5b_emp <- predict(b1reg_model_RF5b_emp, newdata = bene1_train5, type='prob')
b1emp_RF5b <- empCreditScoring(b1regpredb_RF5b_emp$X1,bene1_train5$TARGET)
set.seed(123); b1reg_model_RF6a_emp <- train(formula, data=bene1_train6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF6a_emp <- predict(b1reg_model_RF6a_emp, newdata = bene1_test6, type='prob')
b1emp_RF6a <- empCreditScoring(b1regpredb_RF6a_emp$X1,bene1_test6$TARGET)
set.seed(123); b1reg_model_RF6b_emp <- train(formula, data=bene1_test6,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF6b_emp <- predict(b1reg_model_RF6b_emp, newdata = bene1_train6, type='prob')
b1emp_RF6b <- empCreditScoring(b1regpredb_RF6b_emp$X1,bene1_train6$TARGET)
set.seed(123); b1reg_model_RF7a_emp <- train(formula, data=bene1_train7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF7a_emp <- predict(b1reg_model_RF7a_emp, newdata = bene1_test7, type='prob')
b1emp_RF7a <- empCreditScoring(b1regpredb_RF7a_emp$X1,bene1_test7$TARGET)
set.seed(123); b1reg_model_RF7b_emp <- train(formula, data=bene1_test7,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF7b_emp <- predict(b1reg_model_RF7b_emp, newdata = bene1_train7, type='prob')
b1emp_RF7b <- empCreditScoring(b1regpredb_RF7b_emp$X1,bene1_train7$TARGET)
set.seed(123); b1reg_model_RF8a_emp <- train(formula, data=bene1_train8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF8a_emp <- predict(b1reg_model_RF8a_emp, newdata = bene1_test8, type='prob')
b1emp_RF8a <- empCreditScoring(b1regpredb_RF8a_emp$X1,bene1_test8$TARGET)
set.seed(123); b1reg_model_RF8b_emp <- train(formula, data=bene1_test8,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF8b_emp <- predict(b1reg_model_RF8b_emp, newdata = bene1_train8, type='prob')
b1emp_RF8b <- empCreditScoring(b1regpredb_RF8b_emp$X1,bene1_train8$TARGET)
set.seed(123); b1reg_model_RF9a_emp <- train(formula, data=bene1_train9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF9a_emp <- predict(b1reg_model_RF9a_emp, newdata = bene1_test9, type='prob')
b1emp_RF9a <- empCreditScoring(b1regpredb_RF9a_emp$X1,bene1_test9$TARGET)
set.seed(123); b1reg_model_RF9b_emp <- train(formula, data=bene1_test9,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF9b_emp <- predict(b1reg_model_RF9b_emp, newdata = bene1_train9, type='prob')
b1emp_RF9b <- empCreditScoring(b1regpredb_RF9b_emp$X1,bene1_train9$TARGET)
set.seed(123); b1reg_model_RF10a_emp <- train(formula, data=bene1_train10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF10a_emp <- predict(b1reg_model_RF10a_emp, newdata = bene1_test10, type='prob')
b1emp_RF10a <- empCreditScoring(b1regpredb_RF10a_emp$X1,bene1_test10$TARGET)
set.seed(123); b1reg_model_RF10b_emp <- train(formula, data=bene1_test10,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b1regpredb_RF10b_emp <- predict(b1reg_model_RF10b_emp, newdata = bene1_train10, type='prob')
b1emp_RF10b <- empCreditScoring(b1regpredb_RF10b_emp$X1,bene1_train10$TARGET)

#bene1 - MLP1
MLP1grid <- expand.grid(.size=c(10), .dropout=c(0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP11a_emp <- train(formula, data=bene1_woe_train1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP11a_emp <- predict(b1_model_MLP11a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_MLP11a <- empCreditScoring(b1predb_MLP11a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_MLP11b_emp <- train(formula, data=bene1_woe_test1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP11b_emp <- predict(b1_model_MLP11b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_MLP11b <- empCreditScoring(b1predb_MLP11b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_MLP12a_emp <- train(formula, data=bene1_woe_train2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP12a_emp <- predict(b1_model_MLP12a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_MLP12a <- empCreditScoring(b1predb_MLP12a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_MLP12b_emp <- train(formula, data=bene1_woe_test2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP12b_emp <- predict(b1_model_MLP12b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_MLP12b <- empCreditScoring(b1predb_MLP12b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_MLP13a_emp <- train(formula, data=bene1_woe_train3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP13a_emp <- predict(b1_model_MLP13a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_MLP13a <- empCreditScoring(b1predb_MLP13a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_MLP13b_emp <- train(formula, data=bene1_woe_test3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP13b_emp <- predict(b1_model_MLP13b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_MLP13b <- empCreditScoring(b1predb_MLP13b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_MLP14a_emp <- train(formula, data=bene1_woe_train4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP14a_emp <- predict(b1_model_MLP14a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_MLP14a <- empCreditScoring(b1predb_MLP14a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_MLP14b_emp <- train(formula, data=bene1_woe_test4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP14b_emp <- predict(b1_model_MLP14b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_MLP14b <- empCreditScoring(b1predb_MLP14b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_MLP15a_emp <- train(formula, data=bene1_woe_train5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP15a_emp <- predict(b1_model_MLP15a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_MLP15a <- empCreditScoring(b1predb_MLP15a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_MLP15b_emp <- train(formula, data=bene1_woe_test5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP15b_emp <- predict(b1_model_MLP15b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_MLP15b <- empCreditScoring(b1predb_MLP15b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_MLP16a_emp <- train(formula, data=bene1_woe_train6, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP16a_emp <- predict(b1_model_MLP16a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_MLP16a <- empCreditScoring(b1predb_MLP16a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_MLP16b_emp <- train(formula, data=bene1_woe_test6, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP16b_emp <- predict(b1_model_MLP16b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_MLP16b <- empCreditScoring(b1predb_MLP16b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_MLP17a_emp <- train(formula, data=bene1_woe_train7, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP17a_emp <- predict(b1_model_MLP17a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_MLP17a <- empCreditScoring(b1predb_MLP17a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_MLP17b_emp <- train(formula, data=bene1_woe_test7, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP17b_emp <- predict(b1_model_MLP17b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_MLP17b <- empCreditScoring(b1predb_MLP17b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_MLP18a_emp <- train(formula, data=bene1_woe_train8, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP18a_emp <- predict(b1_model_MLP18a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_MLP18a <- empCreditScoring(b1predb_MLP18a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_MLP18b_emp <- train(formula, data=bene1_woe_test8, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP18b_emp <- predict(b1_model_MLP18b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_MLP18b <- empCreditScoring(b1predb_MLP18b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_MLP19a_emp <- train(formula, data=bene1_woe_train9, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP19a_emp <- predict(b1_model_MLP19a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_MLP19a <- empCreditScoring(b1predb_MLP19a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_MLP19b_emp <- train(formula, data=bene1_woe_test9, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP19b_emp <- predict(b1_model_MLP19b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_MLP19b <- empCreditScoring(b1predb_MLP19b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_MLP110a_emp <- train(formula, data=bene1_woe_train10, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP110a_emp <- predict(b1_model_MLP110a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_MLP110a <- empCreditScoring(b1predb_MLP110a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_MLP110b_emp <- train(formula, data=bene1_woe_test10, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP110b_emp <- predict(b1_model_MLP110b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_MLP110b <- empCreditScoring(b1predb_MLP110b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - MLP3
MLP3grid <- expand.grid(.size1=c(15), .size2=c(10), .size3=c(20), .dropout=c(0.25), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP31a_emp <- train(formula, data=bene1_woe_train1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP31a_emp <- predict(b1_model_MLP31a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_MLP31a <- empCreditScoring(b1predb_MLP31a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_MLP31b_emp <- train(formula, data=bene1_woe_test1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP31b_emp <- predict(b1_model_MLP31b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_MLP31b <- empCreditScoring(b1predb_MLP31b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_MLP32a_emp <- train(formula, data=bene1_woe_train2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP32a_emp <- predict(b1_model_MLP32a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_MLP32a <- empCreditScoring(b1predb_MLP32a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_MLP32b_emp <- train(formula, data=bene1_woe_test2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP32b_emp <- predict(b1_model_MLP32b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_MLP32b <- empCreditScoring(b1predb_MLP32b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_MLP33a_emp <- train(formula, data=bene1_woe_train3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP33a_emp <- predict(b1_model_MLP33a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_MLP33a <- empCreditScoring(b1predb_MLP33a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_MLP33b_emp <- train(formula, data=bene1_woe_test3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP33b_emp <- predict(b1_model_MLP33b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_MLP33b <- empCreditScoring(b1predb_MLP33b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_MLP34a_emp <- train(formula, data=bene1_woe_train4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP34a_emp <- predict(b1_model_MLP34a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_MLP34a <- empCreditScoring(b1predb_MLP34a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_MLP34b_emp <- train(formula, data=bene1_woe_test4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP34b_emp <- predict(b1_model_MLP34b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_MLP34b <- empCreditScoring(b1predb_MLP34b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_MLP35a_emp <- train(formula, data=bene1_woe_train5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP35a_emp <- predict(b1_model_MLP35a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_MLP35a <- empCreditScoring(b1predb_MLP35a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_MLP35b_emp <- train(formula, data=bene1_woe_test5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP35b_emp <- predict(b1_model_MLP35b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_MLP35b <- empCreditScoring(b1predb_MLP35b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_MLP36a_emp <- train(formula, data=bene1_woe_train6, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP36a_emp <- predict(b1_model_MLP36a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_MLP36a <- empCreditScoring(b1predb_MLP36a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_MLP36b_emp <- train(formula, data=bene1_woe_test6, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP36b_emp <- predict(b1_model_MLP36b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_MLP36b <- empCreditScoring(b1predb_MLP36b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_MLP37a_emp <- train(formula, data=bene1_woe_train7, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP37a_emp <- predict(b1_model_MLP37a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_MLP37a <- empCreditScoring(b1predb_MLP37a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_MLP37b_emp <- train(formula, data=bene1_woe_test7, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP37b_emp <- predict(b1_model_MLP37b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_MLP37b <- empCreditScoring(b1predb_MLP37b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_MLP38a_emp <- train(formula, data=bene1_woe_train8, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP38a_emp <- predict(b1_model_MLP38a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_MLP38a <- empCreditScoring(b1predb_MLP38a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_MLP38b_emp <- train(formula, data=bene1_woe_test8, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP38b_emp <- predict(b1_model_MLP38b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_MLP38b <- empCreditScoring(b1predb_MLP38b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_MLP39a_emp <- train(formula, data=bene1_woe_train9, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP39a_emp <- predict(b1_model_MLP39a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_MLP39a <- empCreditScoring(b1predb_MLP39a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_MLP39b_emp <- train(formula, data=bene1_woe_test9, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP39b_emp <- predict(b1_model_MLP39b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_MLP39b <- empCreditScoring(b1predb_MLP39b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_MLP310a_emp <- train(formula, data=bene1_woe_train10, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP310a_emp <- predict(b1_model_MLP310a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_MLP310a <- empCreditScoring(b1predb_MLP310a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_MLP310b_emp <- train(formula, data=bene1_woe_test10, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP310b_emp <- predict(b1_model_MLP310b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_MLP310b <- empCreditScoring(b1predb_MLP310b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - MLP5
MLP5grid <- expand.grid(.size1=c(10), .size2=c(15), .size3=c(10), .size4=c(10), .size5=c(15), .dropout=c(0), .batch_size=batch, .lr=c(0.05), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b1_model_MLP51a_emp <- train(formula, data=bene1_woe_train1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP51a_emp <- predict(b1_model_MLP51a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_MLP51a <- empCreditScoring(b1predb_MLP51a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_MLP51b_emp <- train(formula, data=bene1_woe_test1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP51b_emp <- predict(b1_model_MLP51b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_MLP51b <- empCreditScoring(b1predb_MLP51b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_MLP52a_emp <- train(formula, data=bene1_woe_train2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP52a_emp <- predict(b1_model_MLP52a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_MLP52a <- empCreditScoring(b1predb_MLP52a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_MLP52b_emp <- train(formula, data=bene1_woe_test2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP52b_emp <- predict(b1_model_MLP52b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_MLP52b <- empCreditScoring(b1predb_MLP52b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_MLP53a_emp <- train(formula, data=bene1_woe_train3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP53a_emp <- predict(b1_model_MLP53a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_MLP53a <- empCreditScoring(b1predb_MLP53a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_MLP53b_emp <- train(formula, data=bene1_woe_test3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP53b_emp <- predict(b1_model_MLP53b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_MLP53b <- empCreditScoring(b1predb_MLP53b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_MLP54a_emp <- train(formula, data=bene1_woe_train4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP54a_emp <- predict(b1_model_MLP54a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_MLP54a <- empCreditScoring(b1predb_MLP54a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_MLP54b_emp <- train(formula, data=bene1_woe_test4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP54b_emp <- predict(b1_model_MLP54b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_MLP54b <- empCreditScoring(b1predb_MLP54b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_MLP55a_emp <- train(formula, data=bene1_woe_train5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP55a_emp <- predict(b1_model_MLP55a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_MLP55a <- empCreditScoring(b1predb_MLP55a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_MLP55b_emp <- train(formula, data=bene1_woe_test5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP55b_emp <- predict(b1_model_MLP55b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_MLP55b <- empCreditScoring(b1predb_MLP55b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_MLP56a_emp <- train(formula, data=bene1_woe_train6, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP56a_emp <- predict(b1_model_MLP56a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_MLP56a <- empCreditScoring(b1predb_MLP56a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_MLP56b_emp <- train(formula, data=bene1_woe_test6, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP56b_emp <- predict(b1_model_MLP56b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_MLP56b <- empCreditScoring(b1predb_MLP56b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_MLP57a_emp <- train(formula, data=bene1_woe_train7, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP57a_emp <- predict(b1_model_MLP57a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_MLP57a <- empCreditScoring(b1predb_MLP57a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_MLP57b_emp <- train(formula, data=bene1_woe_test7, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP57b_emp <- predict(b1_model_MLP57b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_MLP57b <- empCreditScoring(b1predb_MLP57b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_MLP58a_emp <- train(formula, data=bene1_woe_train8, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP58a_emp <- predict(b1_model_MLP58a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_MLP58a <- empCreditScoring(b1predb_MLP58a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_MLP58b_emp <- train(formula, data=bene1_woe_test8, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP58b_emp <- predict(b1_model_MLP58b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_MLP58b <- empCreditScoring(b1predb_MLP58b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_MLP59a_emp <- train(formula, data=bene1_woe_train9, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP59a_emp <- predict(b1_model_MLP59a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_MLP59a <- empCreditScoring(b1predb_MLP59a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_MLP59b_emp <- train(formula, data=bene1_woe_test9, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP59b_emp <- predict(b1_model_MLP59b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_MLP59b <- empCreditScoring(b1predb_MLP59b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_MLP510a_emp <- train(formula, data=bene1_woe_train10, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP510a_emp <- predict(b1_model_MLP510a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_MLP510a <- empCreditScoring(b1predb_MLP510a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_MLP510b_emp <- train(formula, data=bene1_woe_test10, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_MLP510b_emp <- predict(b1_model_MLP510b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_MLP510b <- empCreditScoring(b1predb_MLP510b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - DBN1  
DBN1grid <- expand.grid(.layer1=c(20), .hidden_dropout=c(0), .visible_dropout=c(0.25), .lr=c(1.0))
set.seed(123); b1_model_DBN11a_emp <- train(formula, data=bene1_woe_train1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN11a_emp <- predict(b1_model_DBN11a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_DBN11a <- empCreditScoring(b1predb_DBN11a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_DBN11b_emp <- train(formula, data=bene1_woe_test1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN11b_emp <- predict(b1_model_DBN11b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_DBN11b <- empCreditScoring(b1predb_DBN11b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_DBN12a_emp <- train(formula, data=bene1_woe_train2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN12a_emp <- predict(b1_model_DBN12a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_DBN12a <- empCreditScoring(b1predb_DBN12a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_DBN12b_emp <- train(formula, data=bene1_woe_test2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN12b_emp <- predict(b1_model_DBN12b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_DBN12b <- empCreditScoring(b1predb_DBN12b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_DBN13a_emp <- train(formula, data=bene1_woe_train3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN13a_emp <- predict(b1_model_DBN13a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_DBN13a <- empCreditScoring(b1predb_DBN13a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_DBN13b_emp <- train(formula, data=bene1_woe_test3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN13b_emp <- predict(b1_model_DBN13b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_DBN13b <- empCreditScoring(b1predb_DBN13b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_DBN14a_emp <- train(formula, data=bene1_woe_train4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN14a_emp <- predict(b1_model_DBN14a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_DBN14a <- empCreditScoring(b1predb_DBN14a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_DBN14b_emp <- train(formula, data=bene1_woe_test4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN14b_emp <- predict(b1_model_DBN14b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_DBN14b <- empCreditScoring(b1predb_DBN14b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_DBN15a_emp <- train(formula, data=bene1_woe_train5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN15a_emp <- predict(b1_model_DBN15a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_DBN15a <- empCreditScoring(b1predb_DBN15a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_DBN15b_emp <- train(formula, data=bene1_woe_test5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN15b_emp <- predict(b1_model_DBN15b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_DBN15b <- empCreditScoring(b1predb_DBN15b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_DBN16a_emp <- train(formula, data=bene1_woe_train6, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN16a_emp <- predict(b1_model_DBN16a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_DBN16a <- empCreditScoring(b1predb_DBN16a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_DBN16b_emp <- train(formula, data=bene1_woe_test6, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN16b_emp <- predict(b1_model_DBN16b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_DBN16b <- empCreditScoring(b1predb_DBN16b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_DBN17a_emp <- train(formula, data=bene1_woe_train7, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN17a_emp <- predict(b1_model_DBN17a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_DBN17a <- empCreditScoring(b1predb_DBN17a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_DBN17b_emp <- train(formula, data=bene1_woe_test7, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN17b_emp <- predict(b1_model_DBN17b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_DBN17b <- empCreditScoring(b1predb_DBN17b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_DBN18a_emp <- train(formula, data=bene1_woe_train8, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN18a_emp <- predict(b1_model_DBN18a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_DBN18a <- empCreditScoring(b1predb_DBN18a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_DBN18b_emp <- train(formula, data=bene1_woe_test8, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN18b_emp <- predict(b1_model_DBN18b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_DBN18b <- empCreditScoring(b1predb_DBN18b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_DBN19a_emp <- train(formula, data=bene1_woe_train9, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN19a_emp <- predict(b1_model_DBN19a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_DBN19a <- empCreditScoring(b1predb_DBN19a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_DBN19b_emp <- train(formula, data=bene1_woe_test9, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN19b_emp <- predict(b1_model_DBN19b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_DBN19b <- empCreditScoring(b1predb_DBN19b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_DBN110a_emp <- train(formula, data=bene1_woe_train10, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN110a_emp <- predict(b1_model_DBN110a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_DBN110a <- empCreditScoring(b1predb_DBN110a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_DBN110b_emp <- train(formula, data=bene1_woe_test10, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN110b_emp <- predict(b1_model_DBN110b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_DBN110b <- empCreditScoring(b1predb_DBN110b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - DBN3 
DBN3grid <- expand.grid(.layer1=c(15), .layer2=c(5),.layer3=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0.25), .lr=c(1.5))
set.seed(123); b1_model_DBN31a_emp <- train(formula, data=bene1_woe_train1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN31a_emp <- predict(b1_model_DBN31a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_DBN31a <- empCreditScoring(b1predb_DBN31a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_DBN31b_emp <- train(formula, data=bene1_woe_test1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN31b_emp <- predict(b1_model_DBN31b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_DBN31b <- empCreditScoring(b1predb_DBN31b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_DBN32a_emp <- train(formula, data=bene1_woe_train2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN32a_emp <- predict(b1_model_DBN32a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_DBN32a <- empCreditScoring(b1predb_DBN32a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_DBN32b_emp <- train(formula, data=bene1_woe_test2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN32b_emp <- predict(b1_model_DBN32b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_DBN32b <- empCreditScoring(b1predb_DBN32b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_DBN33a_emp <- train(formula, data=bene1_woe_train3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN33a_emp <- predict(b1_model_DBN33a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_DBN33a <- empCreditScoring(b1predb_DBN33a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_DBN33b_emp <- train(formula, data=bene1_woe_test3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN33b_emp <- predict(b1_model_DBN33b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_DBN33b <- empCreditScoring(b1predb_DBN33b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_DBN34a_emp <- train(formula, data=bene1_woe_train4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN34a_emp <- predict(b1_model_DBN34a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_DBN34a <- empCreditScoring(b1predb_DBN34a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_DBN34b_emp <- train(formula, data=bene1_woe_test4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN34b_emp <- predict(b1_model_DBN34b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_DBN34b <- empCreditScoring(b1predb_DBN34b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_DBN35a_emp <- train(formula, data=bene1_woe_train5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN35a_emp <- predict(b1_model_DBN35a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_DBN35a <- empCreditScoring(b1predb_DBN35a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_DBN35b_emp <- train(formula, data=bene1_woe_test5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN35b_emp <- predict(b1_model_DBN35b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_DBN35b <- empCreditScoring(b1predb_DBN35b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_DBN36a_emp <- train(formula, data=bene1_woe_train6, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN36a_emp <- predict(b1_model_DBN36a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_DBN36a <- empCreditScoring(b1predb_DBN36a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_DBN36b_emp <- train(formula, data=bene1_woe_test6, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN36b_emp <- predict(b1_model_DBN36b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_DBN36b <- empCreditScoring(b1predb_DBN36b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_DBN37a_emp <- train(formula, data=bene1_woe_train7, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN37a_emp <- predict(b1_model_DBN37a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_DBN37a <- empCreditScoring(b1predb_DBN37a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_DBN37b_emp <- train(formula, data=bene1_woe_test7, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN37b_emp <- predict(b1_model_DBN37b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_DBN37b <- empCreditScoring(b1predb_DBN37b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_DBN38a_emp <- train(formula, data=bene1_woe_train8, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN38a_emp <- predict(b1_model_DBN38a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_DBN38a <- empCreditScoring(b1predb_DBN38a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_DBN38b_emp <- train(formula, data=bene1_woe_test8, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN38b_emp <- predict(b1_model_DBN38b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_DBN38b <- empCreditScoring(b1predb_DBN38b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_DBN39a_emp <- train(formula, data=bene1_woe_train9, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN39a_emp <- predict(b1_model_DBN39a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_DBN39a <- empCreditScoring(b1predb_DBN39a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_DBN39b_emp <- train(formula, data=bene1_woe_test9, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN39b_emp <- predict(b1_model_DBN39b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_DBN39b <- empCreditScoring(b1predb_DBN39b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_DBN310a_emp <- train(formula, data=bene1_woe_train10, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN310a_emp <- predict(b1_model_DBN310a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_DBN310a <- empCreditScoring(b1predb_DBN310a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_DBN310b_emp <- train(formula, data=bene1_woe_test10, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN310b_emp <- predict(b1_model_DBN310b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_DBN310b <- empCreditScoring(b1predb_DBN310b_emp$X1,bene1_woe_train10$TARGET)

#bene1 - DBN5   
DBN5grid <- expand.grid(.layer1=c(10), .layer2=c(15),.layer3=c(15),.layer4=c(10),.layer5=c(15),.hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(2))
set.seed(123); b1_model_DBN51a_emp <- train(formula, data=bene1_woe_train1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN51a_emp <- predict(b1_model_DBN51a_emp, newdata = bene1_woe_test1, type='prob')
b1emp_DBN51a <- empCreditScoring(b1predb_DBN51a_emp$X1,bene1_woe_test1$TARGET)
set.seed(123); b1_model_DBN51b_emp <- train(formula, data=bene1_woe_test1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN51b_emp <- predict(b1_model_DBN51b_emp, newdata = bene1_woe_train1, type='prob')
b1emp_DBN51b <- empCreditScoring(b1predb_DBN51b_emp$X1,bene1_woe_train1$TARGET)
set.seed(123); b1_model_DBN52a_emp <- train(formula, data=bene1_woe_train2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN52a_emp <- predict(b1_model_DBN52a_emp, newdata = bene1_woe_test2, type='prob')
b1emp_DBN52a <- empCreditScoring(b1predb_DBN52a_emp$X1,bene1_woe_test2$TARGET)
set.seed(123); b1_model_DBN52b_emp <- train(formula, data=bene1_woe_test2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN52b_emp <- predict(b1_model_DBN52b_emp, newdata = bene1_woe_train2, type='prob')
b1emp_DBN52b <- empCreditScoring(b1predb_DBN52b_emp$X1,bene1_woe_train2$TARGET)
set.seed(123); b1_model_DBN53a_emp <- train(formula, data=bene1_woe_train3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN53a_emp <- predict(b1_model_DBN53a_emp, newdata = bene1_woe_test3, type='prob')
b1emp_DBN53a <- empCreditScoring(b1predb_DBN53a_emp$X1,bene1_woe_test3$TARGET)
set.seed(123); b1_model_DBN53b_emp <- train(formula, data=bene1_woe_test3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN53b_emp <- predict(b1_model_DBN53b_emp, newdata = bene1_woe_train3, type='prob')
b1emp_DBN53b <- empCreditScoring(b1predb_DBN53b_emp$X1,bene1_woe_train3$TARGET)
set.seed(123); b1_model_DBN54a_emp <- train(formula, data=bene1_woe_train4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN54a_emp <- predict(b1_model_DBN54a_emp, newdata = bene1_woe_test4, type='prob')
b1emp_DBN54a <- empCreditScoring(b1predb_DBN54a_emp$X1,bene1_woe_test4$TARGET)
set.seed(123); b1_model_DBN54b_emp <- train(formula, data=bene1_woe_test4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN54b_emp <- predict(b1_model_DBN54b_emp, newdata = bene1_woe_train4, type='prob')
b1emp_DBN54b <- empCreditScoring(b1predb_DBN54b_emp$X1,bene1_woe_train4$TARGET)
set.seed(123); b1_model_DBN55a_emp <- train(formula, data=bene1_woe_train5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN55a_emp <- predict(b1_model_DBN55a_emp, newdata = bene1_woe_test5, type='prob')
b1emp_DBN55a <- empCreditScoring(b1predb_DBN55a_emp$X1,bene1_woe_test5$TARGET)
set.seed(123); b1_model_DBN55b_emp <- train(formula, data=bene1_woe_test5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN55b_emp <- predict(b1_model_DBN55b_emp, newdata = bene1_woe_train5, type='prob')
b1emp_DBN55b <- empCreditScoring(b1predb_DBN55b_emp$X1,bene1_woe_train5$TARGET)
set.seed(123); b1_model_DBN56a_emp <- train(formula, data=bene1_woe_train6, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN56a_emp <- predict(b1_model_DBN56a_emp, newdata = bene1_woe_test6, type='prob')
b1emp_DBN56a <- empCreditScoring(b1predb_DBN56a_emp$X1,bene1_woe_test6$TARGET)
set.seed(123); b1_model_DBN56b_emp <- train(formula, data=bene1_woe_test6, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN56b_emp <- predict(b1_model_DBN56b_emp, newdata = bene1_woe_train6, type='prob')
b1emp_DBN56b <- empCreditScoring(b1predb_DBN56b_emp$X1,bene1_woe_train6$TARGET)
set.seed(123); b1_model_DBN57a_emp <- train(formula, data=bene1_woe_train7, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN57a_emp <- predict(b1_model_DBN57a_emp, newdata = bene1_woe_test7, type='prob')
b1emp_DBN57a <- empCreditScoring(b1predb_DBN57a_emp$X1,bene1_woe_test7$TARGET)
set.seed(123); b1_model_DBN57b_emp <- train(formula, data=bene1_woe_test7, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN57b_emp <- predict(b1_model_DBN57b_emp, newdata = bene1_woe_train7, type='prob')
b1emp_DBN57b <- empCreditScoring(b1predb_DBN57b_emp$X1,bene1_woe_train7$TARGET)
set.seed(123); b1_model_DBN58a_emp <- train(formula, data=bene1_woe_train8, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN58a_emp <- predict(b1_model_DBN58a_emp, newdata = bene1_woe_test8, type='prob')
b1emp_DBN58a <- empCreditScoring(b1predb_DBN58a_emp$X1,bene1_woe_test8$TARGET)
set.seed(123); b1_model_DBN58b_emp <- train(formula, data=bene1_woe_test8, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN58b_emp <- predict(b1_model_DBN58b_emp, newdata = bene1_woe_train8, type='prob')
b1emp_DBN58b <- empCreditScoring(b1predb_DBN58b_emp$X1,bene1_woe_train8$TARGET)
set.seed(123); b1_model_DBN59a_emp <- train(formula, data=bene1_woe_train9, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN59a_emp <- predict(b1_model_DBN59a_emp, newdata = bene1_woe_test9, type='prob')
b1emp_DBN59a <- empCreditScoring(b1predb_DBN59a_emp$X1,bene1_woe_test9$TARGET)
set.seed(123); b1_model_DBN59b_emp <- train(formula, data=bene1_woe_test9, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN59b_emp <- predict(b1_model_DBN59b_emp, newdata = bene1_woe_train9, type='prob')
b1emp_DBN59b <- empCreditScoring(b1predb_DBN59b_emp$X1,bene1_woe_train9$TARGET)
set.seed(123); b1_model_DBN510a_emp <- train(formula, data=bene1_woe_train10, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN510a_emp <- predict(b1_model_DBN510a_emp, newdata = bene1_woe_test10, type='prob')
b1emp_DBN510a <- empCreditScoring(b1predb_DBN510a_emp$X1,bene1_woe_test10$TARGET)
set.seed(123); b1_model_DBN510b_emp <- train(formula, data=bene1_woe_test10, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b1predb_DBN510b_emp <- predict(b1_model_DBN510b_emp, newdata = bene1_woe_train10, type='prob')
b1emp_DBN510b <- empCreditScoring(b1predb_DBN510b_emp$X1,bene1_woe_train10$TARGET)

b1emp_log <- cbind(b1emp_log1a$EMPC,b1emp_log1b$EMPC,b1emp_log2a$EMPC,b1emp_log2b$EMPC,b1emp_log3a$EMPC,b1emp_log3b$EMPC,
                   b1emp_log4a$EMPC,b1emp_log4b$EMPC,b1emp_log5a$EMPC,b1emp_log5b$EMPC,b1emp_log6a$EMPC,b1emp_log6b$EMPC,
                   b1emp_log7a$EMPC,b1emp_log7b$EMPC,b1emp_log8a$EMPC,b1emp_log8b$EMPC,b1emp_log9a$EMPC,b1emp_log9b$EMPC,
                   b1emp_log10a$EMPC,b1emp_log10b$EMPC)
b1emp_dt <- cbind(b1emp_dt1a$EMPC,b1emp_dt1b$EMPC,b1emp_dt2a$EMPC,b1emp_dt2b$EMPC,b1emp_dt3a$EMPC,b1emp_dt3b$EMPC,
                  b1emp_dt4a$EMPC,b1emp_dt4b$EMPC,b1emp_dt5a$EMPC,b1emp_dt5b$EMPC,b1emp_dt6a$EMPC,b1emp_dt6b$EMPC,
                  b1emp_dt7a$EMPC,b1emp_dt7b$EMPC,b1emp_dt8a$EMPC,b1emp_dt8b$EMPC,b1emp_dt9a$EMPC,b1emp_dt9b$EMPC,
                  b1emp_dt10a$EMPC,b1emp_dt10b$EMPC)
b1emp_rf <- cbind(b1emp_rf1a$EMPC,b1emp_rf1b$EMPC,b1emp_rf2a$EMPC,b1emp_rf2b$EMPC,b1emp_rf3a$EMPC,b1emp_rf3b$EMPC,
                  b1emp_rf4a$EMPC,b1emp_rf4b$EMPC,b1emp_rf5a$EMPC,b1emp_rf5b$EMPC,b1emp_rf6a$EMPC,b1emp_rf6b$EMPC,
                  b1emp_rf7a$EMPC,b1emp_rf7b$EMPC,b1emp_rf8a$EMPC,b1emp_rf8b$EMPC,b1emp_rf9a$EMPC,b1emp_rf9b$EMPC,
                  b1emp_rf10a$EMPC,b1emp_rf10b$EMPC)
b1emp_MLP1 <- cbind(b1emp_MLP11a$EMPC,b1emp_MLP11b$EMPC,b1emp_MLP12a$EMPC,b1emp_MLP12b$EMPC,b1emp_MLP13a$EMPC,b1emp_MLP13b$EMPC,
                    b1emp_MLP14a$EMPC,b1emp_MLP14b$EMPC,b1emp_MLP15a$EMPC,b1emp_MLP15b$EMPC,b1emp_MLP16a$EMPC,b1emp_MLP16b$EMPC,
                    b1emp_MLP17a$EMPC,b1emp_MLP17b$EMPC,b1emp_MLP18a$EMPC,b1emp_MLP18b$EMPC,b1emp_MLP19a$EMPC,b1emp_MLP19b$EMPC,
                    b1emp_MLP110a$EMPC,b1emp_MLP110b$EMPC)
b1emp_MLP3 <- cbind(b1emp_MLP31a$EMPC,b1emp_MLP31b$EMPC,b1emp_MLP32a$EMPC,b1emp_MLP32b$EMPC,b1emp_MLP33a$EMPC,b1emp_MLP33b$EMPC,
                    b1emp_MLP34a$EMPC,b1emp_MLP34b$EMPC,b1emp_MLP35a$EMPC,b1emp_MLP35b$EMPC,b1emp_MLP36a$EMPC,b1emp_MLP36b$EMPC,
                    b1emp_MLP37a$EMPC,b1emp_MLP37b$EMPC,b1emp_MLP38a$EMPC,b1emp_MLP38b$EMPC,b1emp_MLP39a$EMPC,b1emp_MLP39b$EMPC,
                    b1emp_MLP310a$EMPC,b1emp_MLP310b$EMPC)
b1emp_MLP5 <- cbind(b1emp_MLP51a$EMPC,b1emp_MLP51b$EMPC,b1emp_MLP52a$EMPC,b1emp_MLP52b$EMPC,b1emp_MLP53a$EMPC,b1emp_MLP53b$EMPC,
                    b1emp_MLP54a$EMPC,b1emp_MLP54b$EMPC,b1emp_MLP55a$EMPC,b1emp_MLP55b$EMPC,b1emp_MLP56a$EMPC,b1emp_MLP56b$EMPC,
                    b1emp_MLP57a$EMPC,b1emp_MLP57b$EMPC,b1emp_MLP58a$EMPC,b1emp_MLP58b$EMPC,b1emp_MLP59a$EMPC,b1emp_MLP59b$EMPC,
                    b1emp_MLP510a$EMPC,b1emp_MLP510b$EMPC)
b1emp_DBN1 <- cbind(b1emp_DBN11a$EMPC,b1emp_DBN11b$EMPC,b1emp_DBN12a$EMPC,b1emp_DBN12b$EMPC,b1emp_DBN13a$EMPC,b1emp_DBN13b$EMPC,
                    b1emp_DBN14a$EMPC,b1emp_DBN14b$EMPC,b1emp_DBN15a$EMPC,b1emp_DBN15b$EMPC,b1emp_DBN16a$EMPC,b1emp_DBN16b$EMPC,
                    b1emp_DBN17a$EMPC,b1emp_DBN17b$EMPC,b1emp_DBN18a$EMPC,b1emp_DBN18b$EMPC,b1emp_DBN19a$EMPC,b1emp_DBN19b$EMPC,
                    b1emp_DBN110a$EMPC,b1emp_DBN110b$EMPC)
b1emp_DBN3 <- cbind(b1emp_DBN31a$EMPC,b1emp_DBN31b$EMPC,b1emp_DBN32a$EMPC,b1emp_DBN32b$EMPC,b1emp_DBN33a$EMPC,b1emp_DBN33b$EMPC,
                    b1emp_DBN34a$EMPC,b1emp_DBN34b$EMPC,b1emp_DBN35a$EMPC,b1emp_DBN35b$EMPC,b1emp_DBN36a$EMPC,b1emp_DBN36b$EMPC,
                    b1emp_DBN37a$EMPC,b1emp_DBN37b$EMPC,b1emp_DBN38a$EMPC,b1emp_DBN38b$EMPC,b1emp_DBN39a$EMPC,b1emp_DBN39b$EMPC,
                    b1emp_DBN310a$EMPC,b1emp_DBN310b$EMPC)
b1emp_DBN5 <- cbind(b1emp_DBN51a$EMPC,b1emp_DBN51b$EMPC,b1emp_DBN52a$EMPC,b1emp_DBN52b$EMPC,b1emp_DBN53a$EMPC,b1emp_DBN53b$EMPC,
                    b1emp_DBN54a$EMPC,b1emp_DBN54b$EMPC,b1emp_DBN55a$EMPC,b1emp_DBN55b$EMPC,b1emp_DBN56a$EMPC,b1emp_DBN56b$EMPC,
                    b1emp_DBN57a$EMPC,b1emp_DBN57b$EMPC,b1emp_DBN58a$EMPC,b1emp_DBN58b$EMPC,b1emp_DBN59a$EMPC,b1emp_DBN59b$EMPC,
                    b1emp_DBN510a$EMPC,b1emp_DBN510b$EMPC)
mean(b1emp_log)
mean(b1emp_dt)
mean(b1emp_rf)
mean(b1emp_MLP1)
mean(b1emp_MLP3)
mean(b1emp_MLP5)
mean(b1emp_DBN1)
mean(b1emp_DBN3)
mean(b1emp_DBN5)







