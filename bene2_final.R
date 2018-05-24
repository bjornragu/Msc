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
bene2 <- read.delim("D:/Users/r0648841/Desktop/thesis/Data/input_bene2.txt", header=FALSE)
colnames(bene2) <- c("voorsch","mens", "epccb", "efccb", "chyem", "leen", "proczr", "pchem", 
                     "signemp","mont", "newbuts", "duree", "arevem", "revem", "proem2", "etcem", 
                     "gesl", "aaaem", "naie", "inbelgie", "bijwerk", "telefem", "prop","domem", "buitenl",
                     "tend", "inkomen","beschik", "target")
#mis val
any(is.na(bene2)) #no mis val 
vif = function(df){diag(solve(cor(df)))}
vif(bene2)
bene2 <- subset(bene2, select = -c(beschik))
vif(bene2)
bene2 <- subset(bene2, select = -c(mont)) 
vif(bene2)

bene2[,"targetb"] <- ifelse(bene2[,"target"] == 1, 0, 1)
bene2$target <- bene2$targetb
bene2 <- subset(bene2, select = -c(targetb))

########################
########-WOE-###########
########################
b2_woe <- Information::create_infotables(data=bene2, y ="target", parallel = TRUE)
###replace nominal values with woe values
bene2_woedata <- bene2
#proczr (9)
b2_woe$Tables$proczr
table(bene2$proczr)
bene2_woedata[,"proczr"] <- ifelse(bene2[,"proczr"] == 1, as.matrix(b2_woe$Tables$proczr$WOE)[1,],  
                                   ifelse(bene2[,"proczr"] == 2, as.matrix(b2_woe$Tables$proczr$WOE)[1,],
                                          ifelse(bene2[,"proczr"] == 3, as.matrix(b2_woe$Tables$proczr$WOE)[1,],
                                                 ifelse(bene2[,"proczr"] == 4, as.matrix(b2_woe$Tables$proczr$WOE)[2,],
                                                        ifelse(bene2[,"proczr"] == 5, as.matrix(b2_woe$Tables$proczr$WOE)[3,],
                                                               as.matrix(b2_woe$Tables$proczr$WOE)[4,])))))
table(bene2$proczr, bene2_woedata$proczr)

#signemp (2)
b2_woe$Tables$signemp
table(bene2$signemp)
bene2_woedata[,"signemp"] <- ifelse(bene2[,"signemp"] == 0, as.matrix(b2_woe$Tables$signemp$WOE)[1,], as.matrix(b2_woe$Tables$signemp$WOE)[2,])
table(bene2$signemp, bene2_woedata$signemp)

#newbuts(6)
b2_woe$Tables$newbuts
table(bene2$newbuts)
bene2_woedata[,"newbuts"] <- ifelse(bene2[,"newbuts"] == 1, as.matrix(b2_woe$Tables$newbuts$WOE)[1,],  
                                    ifelse(bene2[,"newbuts"] == 2, as.matrix(b2_woe$Tables$newbuts$WOE)[2,],
                                           ifelse(bene2[,"newbuts"] == 3, as.matrix(b2_woe$Tables$newbuts$WOE)[3,],
                                                  as.matrix(b2_woe$Tables$newbuts$WOE)[4,])))
table(bene2$newbuts, bene2_woedata$newbuts)

#proem2(29)
b2_woe$Tables$proem2
table(bene2$proem2)
bene2_woedata[,"proem2"] <- ifelse(bene2[,"proem2"] == 1, as.matrix(b2_woe$Tables$proem2$WOE)[1,],  
                                   ifelse(bene2[,"proem2"] == 2, as.matrix(b2_woe$Tables$proem2$WOE)[2,],
                                          ifelse(bene2[,"proem2"] == 3, as.matrix(b2_woe$Tables$proem2$WOE)[3,],
                                                 ifelse(bene2[,"proem2"] == 4, as.matrix(b2_woe$Tables$proem2$WOE)[3,],
                                                        ifelse(bene2[,"proem2"] == 5, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                               ifelse(bene2[,"proem2"] == 6, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                      ifelse(bene2[,"proem2"] == 7, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                             ifelse(bene2[,"proem2"] == 8, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                    ifelse(bene2[,"proem2"] == 9, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                           ifelse(bene2[,"proem2"] == 10, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                  ifelse(bene2[,"proem2"] == 11, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                         ifelse(bene2[,"proem2"] == 12, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                                ifelse(bene2[,"proem2"] == 13, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                                       ifelse(bene2[,"proem2"] == 14, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                                              ifelse(bene2[,"proem2"] == 15, as.matrix(b2_woe$Tables$proem2$WOE)[4,],
                                                                                                                                     ifelse(bene2[,"proem2"] == 16, as.matrix(b2_woe$Tables$proem2$WOE)[5,],
                                                                                                                                            ifelse(bene2[,"proem2"] == 17, as.matrix(b2_woe$Tables$proem2$WOE)[5,],
                                                                                                                                                   ifelse(bene2[,"proem2"] == 18, as.matrix(b2_woe$Tables$proem2$WOE)[5,],
                                                                                                                                                          as.matrix(b2_woe$Tables$proem2$WOE)[6,]))))))))))))))))))
table(bene2$proem2, bene2_woedata$proem2)

#etcem (7)
b2_woe$Tables$etcem
table(bene2$etcem)
bene2_woedata[,"etcem"] <- ifelse(bene2[,"etcem"] == 1, as.matrix(b2_woe$Tables$etcem$WOE)[1,],  
                                  ifelse(bene2[,"etcem"] == 2, as.matrix(b2_woe$Tables$etcem$WOE)[2,],
                                         ifelse(bene2[,"etcem"] == 3, as.matrix(b2_woe$Tables$etcem$WOE)[2,],
                                                ifelse(bene2[,"etcem"] == 4, as.matrix(b2_woe$Tables$etcem$WOE)[2,],
                                                       ifelse(bene2[,"etcem"] == 5, as.matrix(b2_woe$Tables$etcem$WOE)[2,],
                                                              as.matrix(b2_woe$Tables$etcem$WOE)[3,])))))
table(bene2$etcem, bene2_woedata$etcem)

#gesl (4)
b2_woe$Tables$gesl
table(bene2$gesl)
bene2_woedata[,"gesl"] <- ifelse(bene2[,"gesl"] == 1, as.matrix(b2_woe$Tables$gesl$WOE)[1,],  
                                 ifelse(bene2[,"gesl"] == 2, as.matrix(b2_woe$Tables$gesl$WOE)[2,],
                                        as.matrix(b2_woe$Tables$gesl$WOE)[3,]))
table(bene2$gesl, bene2_woedata$gesl)

#telefem(2)
b2_woe$Tables$telefem
table(bene2$telefem)
bene2_woedata[,"telefem"] <- ifelse(bene2[,"telefem"] == 0, as.matrix(b2_woe$Tables$telefem$WOE)[1,],
                                    as.matrix(b2_woe$Tables$telefem$WOE)[2,])
table(bene2$telefem, bene2_woedata$telefem)

#Prop (5)
b2_woe$Tables$prop
table(bene2$prop)
bene2_woedata[,"prop"] <- ifelse(bene2[,"prop"] == 1, as.matrix(b2_woe$Tables$prop$WOE)[1,],  
                                 ifelse(bene2[,"prop"] == 2, as.matrix(b2_woe$Tables$prop$WOE)[2,],
                                        as.matrix(b2_woe$Tables$prop$WOE)[3,]))
table(bene2$prop, bene2_woedata$prop)

#Domem (2)
b2_woe$Tables$domem
table(bene2$domem)
bene2_woedata[,"domem"] <- ifelse(bene2[,"domem"] == 0, as.matrix(b2_woe$Tables$domem$WOE)[1,],  
                                  as.matrix(b2_woe$Tables$domem$WOE)[2,])
table(bene2$domem, bene2_woedata$domem)

#buitenl (2)
b2_woe$Tables$buitenl
table(bene2$buitenl)
bene2_woedata[,"buitenl"] <- ifelse(bene2[,"buitenl"] == 0, as.matrix(b2_woe$Tables$buitenl$WOE)[1,],  
                                    as.matrix(b2_woe$Tables$buitenl$WOE)[2,])
table(bene2$buitenl, bene2_woedata$buitenl)
head(bene2_woedata)
summary(bene2_woedata)

####creating factors#####
bene2_woedata$target <- as.factor(bene2_woedata$target)
bene2$target <- as.factor(bene2$target)
bene2$proczr <- as.factor(bene2$proczr)
bene2$signemp <- as.factor(bene2$signemp)
bene2$newbuts <- as.factor(bene2$newbuts)
bene2$proem2 <- as.factor(bene2$proem2)
bene2$etcem <- as.factor(bene2$etcem)
bene2$gesl <- as.factor(bene2$gesl)
bene2$telefem <- as.factor(bene2$telefem)
bene2$prop <- as.factor(bene2$prop)
bene2$domem <- as.factor(bene2$domem)
bene2$buitenl <- as.factor(bene2$buitenl)

levels(bene2_woedata$target) <- make.names(levels(factor(bene2_woedata$target)))
levels(bene2$target) <- make.names(levels(factor(bene2$target)))
levels(bene2$proczr) <- make.names(levels(factor(bene2$proczr)))
levels(bene2$signemp) <- make.names(levels(factor(bene2$signemp)))
levels(bene2$newbuts) <- make.names(levels(factor(bene2$newbuts)))
levels(bene2$proem2) <- make.names(levels(factor(bene2$proem2)))
levels(bene2$etcem) <- make.names(levels(factor(bene2$etcem)))
levels(bene2$gesl) <- make.names(levels(factor(bene2$gesl)))
levels(bene2$telefem) <- make.names(levels(factor(bene2$telefem)))
levels(bene2$prop) <- make.names(levels(factor(bene2$prop)))
levels(bene2$domem) <- make.names(levels(factor(bene2$domem)))
levels(bene2$buitenl) <- make.names(levels(factor(bene2$buitenl)))
#nom var: proczr (9), signemp (2), newbuts(6), proem2 (29), etcem (7), gesl (4), telefem(2), Prop (5), Domem (2), buitenl (2)
summary(bene2)
summary(bene2_woedata)

#################################################################
##DATA PARTITIONING.Use Nx2-foldcross-validation(Dietterich,1998)
##################################################################
##bene_data
k=5
bene2_a <- createDataPartition(bene2$target, times = k, p = 0.5, list = FALSE) 
bene2_woe_train1 <- bene2[bene2_a[,1],]
bene2_woe_test1 <- bene2[-bene2_a[,1],]
bene2_woe_train2 <- bene2[bene2_a[,2],]
bene2_woe_test2 <- bene2[-bene2_a[,2],]
bene2_woe_train3 <- bene2[bene2_a[,3],]
bene2_woe_test3 <- bene2[-bene2_a[,3],]
bene2_woe_train4 <- bene2[bene2_a[,4],]
bene2_woe_test4 <- bene2[-bene2_a[,4],]
bene2_woe_train5 <- bene2[bene2_a[,5],]
bene2_woe_test5 <- bene2[-bene2_a[,5],]

bene2_b <- createDataPartition(bene2_woedata$target, times = k, p = 0.5, list = FALSE)
bene2_woe_train1 <- bene2_woedata[bene2_b[,1],]
bene2_woe_test1 <- bene2_woedata[-bene2_b[,1],]
bene2_woe_train2 <- bene2_woedata[bene2_b[,2],]
bene2_woe_test2 <- bene2_woedata[-bene2_b[,2],]
bene2_woe_train3 <- bene2_woedata[bene2_b[,3],]
bene2_woe_test3 <- bene2_woedata[-bene2_b[,3],]
bene2_woe_train4 <- bene2_woedata[bene2_b[,4],]
bene2_woe_test4 <- bene2_woedata[-bene2_b[,4],]
bene2_woe_train5 <- bene2_woedata[bene2_b[,5],]
bene2_woe_test5 <- bene2_woedata[-bene2_b[,5],]

formula <- target~voorsch+mens+epccb+efccb+chyem+leen+proczr+pchem+signemp+newbuts+duree+arevem+revem+proem2+etcem+gesl+aaaem+naie+inbelgie+bijwerk+telefem+prop+domem+buitenl+tend+inkomen

####################################################################################
##custom summary functions, reference recomends AUC, the PG, and the BS#############
####################################################################################
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
set.seed(321); b2_model_log1a_roc <- train(formula, data=bene2_woe_train1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log1a_roc <- predict(b2_model_log1a_roc,bene2_woe_test1,type="prob")
b2_log1a.ROC <- roc(predictor=b2predb_log1a_roc$X0,
                    response=bene2_woe_test1$target,
                    levels=rev(levels(bene2_woe_test1$target)))
b2_log1a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(321); b2_model_log1a_gini <- train(formula, data=bene2_woe_train1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log1a_gini$results
b2_model_log1a_gini$resample
b2_pred_log1a_gini<- predict(b2_model_log1a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_log1a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_log1a_gini$X1)
#b <= 0.4
p <- b2_pred_log1a_gini$X1[b2_pred_log1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_log1a_gini$X1<=0.4]))
b2_log1a.ngini <- normalizedGini(a, p)
b2_log1a.ngini
b2_log1a.gini <-Gini(a, p)
b2_log1a.gini

#Brier score
set.seed(321); b2_model_log1a_brier <- train(formula, data=bene2_woe_train1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log1a_brier$results
b2_model_log1a_brier$resample
b2_pred_log1a_brier <- predict(b2_model_log1a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_log1a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_log1a_brier$X1)
b2_log1a.bs

###data 1 - test-train
#ROC curve 
set.seed(321); b2_model_log1b_roc <- train(formula, data=bene2_woe_test1, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log1b_roc <- predict(b2_model_log1b_roc, bene2_woe_train1,type="prob")
b2_log1b.ROC <- roc(predictor=b2predb_log1b_roc$X0,
                    response=bene2_woe_train1$target,
                    levels=rev(levels(bene2_woe_train1$target)))
b2_log1b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(321); b2_model_log1b_gini <- train(formula, data=bene2_woe_test1, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log1b_gini$results
b2_model_log1b_gini$resample
b2_pred_log1b_gini<- predict(b2_model_log1b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_log1b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_log1b_gini$X1)
#b <= 0.4
p <- b2_pred_log1b_gini$X1[b2_pred_log1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_log1b_gini$X1<=0.4]))
b2_log1b.ngini <- normalizedGini(a, p)
b2_log1b.ngini
b2_log1b.gini <-Gini(a, p)
b2_log1b.gini

#Brier score
set.seed(321); b2_model_log1b_brier <- train(formula, data=bene2_woe_test1, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log1b_brier$results
b2_model_log1b_brier$resample
b2_pred_log1b_brier <- predict(b2_model_log1b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_log1b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_log1b_brier$X1)
b2_log1b.bs

###data 2, train-test
#ROC curve 
set.seed(321); b2_model_log2a_roc <- train(formula, data=bene2_woe_train2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log2a_roc <- predict(b2_model_log2a_roc,bene2_woe_test2,type="prob")
b2_log2a.ROC <- roc(predictor=b2predb_log2a_roc$X0,
                    response=bene2_woe_test2$target,
                    levels=rev(levels(bene2_woe_test2$target)))
b2_log2a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(321); b2_model_log2a_gini <- train(formula, data=bene2_woe_train2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log2a_gini$results
b2_model_log2a_gini$resample
b2_pred_log2a_gini<- predict(b2_model_log2a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_log2a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_log2a_gini$X1)
#b <= 0.4
p <- b2_pred_log2a_gini$X1[b2_pred_log2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_log2a_gini$X1<=0.4]))
b2_log2a.ngini <- normalizedGini(a, p)
b2_log2a.ngini
b2_log2a.gini <-Gini(a, p)
b2_log2a.gini

#Brier score
set.seed(321); b2_model_log2a_brier <- train(formula, data=bene2_woe_train2, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log2a_brier$results
b2_model_log2a_brier$resample
b2_pred_log2a_brier <- predict(b2_model_log2a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_log2a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_log2a_brier$X1)
b2_log2a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2_model_log2b_roc <- train(formula, data=bene2_woe_test2, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log2b_roc <- predict(b2_model_log2b_roc, bene2_woe_train2,type="prob")
b2_log2b.ROC <- roc(predictor=b2predb_log2b_roc$X0,
                    response=bene2_woe_train2$target,
                    levels=rev(levels(bene2_woe_train2$target)))
b2_log2b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(321); b2_model_log2b_gini <- train(formula, data=bene2_woe_test2, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log2b_gini$results
b2_model_log2b_gini$resample
b2_pred_log2b_gini<- predict(b2_model_log2b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_log2b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_log2b_gini$X1)
#b <= 0.4
p <- b2_pred_log2b_gini$X1[b2_pred_log2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_log2b_gini$X1<=0.4]))
b2_log2b.ngini <- normalizedGini(a, p)
b2_log2b.ngini
b2_log2b.gini <-Gini(a, p)
b2_log2b.gini

#Brier score
set.seed(321); b2_model_log2b_brier <- train(formula, data=bene2_woe_test2, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log2b_brier$results
b2_model_log2b_brier$resample
b2_pred_log2b_brier <- predict(b2_model_log2b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_log2b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_log2b_brier$X1)
b2_log2b.bs

###data 3, train-test
#ROC curve 
set.seed(321); b2_model_log3a_roc <- train(formula, data=bene2_woe_train3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log3a_roc <- predict(b2_model_log3a_roc,bene2_woe_test3,type="prob")
b2_log3a.ROC <- roc(predictor=b2predb_log3a_roc$X0,
                    response=bene2_woe_test3$target,
                    levels=rev(levels(bene2_woe_test3$target)))
b2_log3a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(321); b2_model_log3a_gini <- train(formula, data=bene2_woe_train3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log3a_gini$results
b2_model_log3a_gini$resample
b2_pred_log3a_gini<- predict(b2_model_log3a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_log3a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_log3a_gini$X1)
#b <= 0.4
p <- b2_pred_log3a_gini$X1[b2_pred_log3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_log3a_gini$X1<=0.4]))
b2_log3a.ngini <- normalizedGini(a, p)
b2_log3a.ngini
b2_log3a.gini <-Gini(a, p)
b2_log3a.gini

#Brier score
set.seed(321); b2_model_log3a_brier <- train(formula, data=bene2_woe_train3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log3a_brier$results
b2_model_log3a_brier$resample
b2_pred_log3a_brier <- predict(b2_model_log3a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_log3a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_log3a_brier$X1)
b2_log3a.bs

###data 3 - test-train
#ROC curve 
set.seed(321); b2_model_log3b_roc <- train(formula, data=bene2_woe_test3, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log3b_roc <- predict(b2_model_log3b_roc, bene2_woe_train3,type="prob")
b2_log3b.ROC <- roc(predictor=b2predb_log3b_roc$X0,
                    response=bene2_woe_train3$target,
                    levels=rev(levels(bene2_woe_train3$target)))
b2_log3b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(321); b2_model_log3b_gini <- train(formula, data=bene2_woe_test3, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log3b_gini$results
b2_model_log3b_gini$resample
b2_pred_log3b_gini<- predict(b2_model_log3b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_log3b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_log3b_gini$X1)
#b <= 0.4
p <- b2_pred_log3b_gini$X1[b2_pred_log3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_log3b_gini$X1<=0.4]))
b2_log3b.ngini <- normalizedGini(a, p)
b2_log3b.ngini
b2_log3b.gini <-Gini(a, p)
b2_log3b.gini

#Brier score
set.seed(321); b2_model_log3b_brier <- train(formula, data=bene2_woe_test3, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log3b_brier$results
b2_model_log3b_brier$resample
b2_pred_log3b_brier <- predict(b2_model_log3b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_log3b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_log3b_brier$X1)
b2_log3b.bs

###data 4, train-test
#ROC curve 
set.seed(321); b2_model_log4a_roc <- train(formula, data=bene2_woe_train4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log4a_roc <- predict(b2_model_log4a_roc,bene2_woe_test4,type="prob")
b2_log4a.ROC <- roc(predictor=b2predb_log4a_roc$X0,
                    response=bene2_woe_test4$target,
                    levels=rev(levels(bene2_woe_test4$target)))
b2_log4a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(321); b2_model_log4a_gini <- train(formula, data=bene2_woe_train4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log4a_gini$results
b2_model_log4a_gini$resample
b2_pred_log4a_gini<- predict(b2_model_log4a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_log4a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_log4a_gini$X1)
#b <= 0.4
p <- b2_pred_log4a_gini$X1[b2_pred_log4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_log4a_gini$X1<=0.4]))
b2_log4a.ngini <- normalizedGini(a, p)
b2_log4a.ngini
b2_log4a.gini <-Gini(a, p)
b2_log4a.gini

#Brier score
set.seed(321); b2_model_log4a_brier <- train(formula, data=bene2_woe_train4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log4a_brier$results
b2_model_log4a_brier$resample
b2_pred_log4a_brier <- predict(b2_model_log4a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_log4a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_log4a_brier$X1)
b2_log4a.bs

###data 4 - test-train
#ROC curve 
set.seed(321); b2_model_log4b_roc <- train(formula, data=bene2_woe_test4, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log4b_roc <- predict(b2_model_log4b_roc, bene2_woe_train4,type="prob")
b2_log4b.ROC <- roc(predictor=b2predb_log4b_roc$X0,
                    response=bene2_woe_train4$target,
                    levels=rev(levels(bene2_woe_train4$target)))
b2_log4b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(321); b2_model_log4b_gini <- train(formula, data=bene2_woe_test4, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log4b_gini$results
b2_model_log4b_gini$resample
b2_pred_log4b_gini<- predict(b2_model_log4b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_log4b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_log4b_gini$X1)
#b <= 0.4
p <- b2_pred_log4b_gini$X1[b2_pred_log4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_log4b_gini$X1<=0.4]))
b2_log4b.ngini <- normalizedGini(a, p)
b2_log4b.ngini
b2_log4b.gini <-Gini(a, p)
b2_log4b.gini

#Brier score
set.seed(321); b2_model_log4b_brier <- train(formula, data=bene2_woe_test4, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log4b_brier$results
b2_model_log4b_brier$resample
b2_pred_log4b_brier <- predict(b2_model_log4b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_log4b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_log4b_brier$X1)
b2_log4b.bs

###data 5, train-test
#ROC curve 
set.seed(321); b2_model_log5a_roc <- train(formula, data=bene2_woe_train5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log5a_roc <- predict(b2_model_log5a_roc,bene2_woe_test5,type="prob")
b2_log5a.ROC <- roc(predictor=b2predb_log5a_roc$X0,
                    response=bene2_woe_test5$target,
                    levels=rev(levels(bene2_woe_test5$target)))
b2_log5a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(321); b2_model_log5a_gini <- train(formula, data=bene2_woe_train5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log5a_gini$results
b2_model_log5a_gini$resample
b2_pred_log5a_gini<- predict(b2_model_log5a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_log5a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_log5a_gini$X1)
#b <= 0.4
p <- b2_pred_log5a_gini$X1[b2_pred_log5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_log5a_gini$X1<=0.4]))
b2_log5a.ngini <- normalizedGini(a, p)
b2_log5a.ngini
b2_log5a.gini <-Gini(a, p)
b2_log5a.gini

#Brier score
set.seed(321); b2_model_log5a_brier <- train(formula, data=bene2_woe_train5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log5a_brier$results
b2_model_log5a_brier$resample
b2_pred_log5a_brier <- predict(b2_model_log5a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_log5a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_log5a_brier$X1)
b2_log5a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2_model_log5b_roc <- train(formula, data=bene2_woe_test5, method = "glm",family="binomial", trControl=train_control_roc, metric='ROC', preProc=c("center","scale"))
b2predb_log5b_roc <- predict(b2_model_log5b_roc, bene2_woe_train5,type="prob")
b2_log5b.ROC <- roc(predictor=b2predb_log5b_roc$X0,
                    response=bene2_woe_train5$target,
                    levels=rev(levels(bene2_woe_train5$target)))
b2_log5b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(321); b2_model_log5b_gini <- train(formula, data=bene2_woe_test5, method = "glm",family="binomial", trControl=train_control_gini, metric='Gini', preProc=c("center","scale"))
b2_model_log5b_gini$results
b2_model_log5b_gini$resample
b2_pred_log5b_gini<- predict(b2_model_log5b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_log5b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_log5b_gini$X1)
#b <= 0.4
p <- b2_pred_log5b_gini$X1[b2_pred_log5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_log5b_gini$X1<=0.4]))
b2_log5b.ngini <- normalizedGini(a, p)
b2_log5b.ngini
b2_log5b.gini <-Gini(a, p)
b2_log5b.gini

#Brier score
set.seed(321); b2_model_log5b_brier <- train(formula, data=bene2_woe_test5, method = "glm",family="binomial", trControl=train_control_brier, metric='BrierScore', maximize=FALSE, preProc=c("center","scale"))
b2_model_log5b_brier$results
b2_model_log5b_brier$resample
b2_pred_log5b_brier <- predict(b2_model_log5b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_log5b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_log5b_brier$X1)
b2_log5b.bs

##Restults logistic regression!
b2.results_log_AUC <- cbind(b2_log1a.ROC$auc,b2_log1b.ROC$auc,b2_log2a.ROC$auc,b2_log2b.ROC$auc,b2_log3a.ROC$auc,b2_log3b.ROC$auc,b2_log4a.ROC$auc,b2_log4b.ROC$auc,b2_log5a.ROC$auc,b2_log5b.ROC$auc)
b2.results_log_bs <- cbind(b2_log1a.bs,b2_log1b.bs,b2_log2a.bs,b2_log2b.bs,b2_log3a.bs,b2_log3b.bs,b2_log4a.bs,b2_log4b.bs,b2_log5a.bs,b2_log5b.bs)
b2.results_log_ngini <- cbind(b2_log1a.ngini,b2_log1b.ngini,b2_log2a.ngini,b2_log2b.ngini,b2_log3a.ngini,b2_log3b.ngini,b2_log4a.ngini,b2_log4b.ngini,b2_log5a.ngini,b2_log5b.ngini)
b2.results_log_gini <- cbind(b2_log1a.gini,b2_log1b.gini,b2_log2a.gini,b2_log2b.gini,b2_log3a.gini,b2_log3b.gini,b2_log4a.gini,b2_log4b.gini,b2_log5a.gini,b2_log5b.gini)
mean(b2.results_log_AUC)
mean(b2.results_log_bs)
mean(b2.results_log_ngini)
mean(b2.results_log_gini)
#########################################
#######J4.8 Decission tree###############
#########################################
DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
###data 1, train-test
#ROC curve 
set.seed(321); b2reg_model_DT1a_roc <- train(formula, data=bene2_train1, method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT1a_roc <- predict(b2reg_model_DT1a_roc,bene2_test1,type="prob")
b2reg_DT1a.ROC <- roc(predictor=b2regpredb_DT1a_roc$X0,
                      response=bene2_test1$target,
                      levels=rev(levels(bene2_test1$target)))
b2reg_DT1a.ROC

#normalizedGini
bene2_test1$targetb <- bene2_test1$target
levels(bene2_test1$targetb) <- c('0','1')
bene2_test1$targetb <- as.numeric(levels(bene2_test1$targetb))[bene2_test1$targetb]
set.seed(321); b2reg_model_DT1a_gini <- train(formula, data=bene2_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT1a_gini$results
b2reg_model_DT1a_gini$resample
b2reg_pred_DT1a_gini<- predict(b2reg_model_DT1a_gini, newdata = bene2_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test1$targetb, b2reg_pred_DT1a_gini$X1)
Gini(bene2_test1$targetb, b2reg_pred_DT1a_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT1a_gini$X1[b2reg_pred_DT1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test1$targetb[b2reg_pred_DT1a_gini$X1<=0.4]))
b2reg_DT1a.ngini <- normalizedGini(a, p)
b2reg_DT1a.ngini
b2reg_DT1a.gini <-Gini(a, p)
b2reg_DT1a.gini

#Brier score
set.seed(321); b2reg_model_DT1a_brier <- train(formula, data=bene2_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT1a_brier$results
b2reg_model_DT1a_brier$resample
b2reg_pred_DT1a_brier <- predict(b2reg_model_DT1a_brier, newdata = bene2_test1, type='prob')
bene2_test1$targetb <- bene2_test1$target
levels(bene2_test1$targetb) <- c('0','1')
b2reg_DT1a.bs <- Brier(as.numeric(as.character(bene2_test1$targetb)), b2reg_pred_DT1a_brier$X1)
b2reg_DT1a.bs

###data 1 - test-train
#ROC curve 
set.seed(321); b2reg_model_DT1b_roc <- train(formula, data=bene2_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT1b_roc <- predict(b2reg_model_DT1b_roc, bene2_train1,type="prob")
b2reg_DT1b.ROC <- roc(predictor=b2regpredb_DT1b_roc$X0,
                      response=bene2_train1$target,
                      levels=rev(levels(bene2_train1$target)))
b2reg_DT1b.ROC

#normalizedGini
bene2_train1$targetb <- bene2_train1$target
levels(bene2_train1$targetb) <- c('0','1')
bene2_train1$targetb <- as.numeric(levels(bene2_train1$targetb))[bene2_train1$targetb]
set.seed(321); b2reg_model_DT1b_gini <- train(formula, data=bene2_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT1b_gini$results
b2reg_model_DT1b_gini$resample
b2reg_pred_DT1b_gini<- predict(b2reg_model_DT1b_gini, newdata = bene2_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train1$targetb, b2reg_pred_DT1b_gini$X1)
Gini(bene2_train1$targetb, b2reg_pred_DT1b_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT1b_gini$X1[b2reg_pred_DT1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train1$targetb[b2reg_pred_DT1b_gini$X1<=0.4]))
b2reg_DT1b.ngini <- normalizedGini(a, p)
b2reg_DT1b.ngini
b2reg_DT1b.gini <-Gini(a, p)
b2reg_DT1b.gini

#Brier score
set.seed(321); b2reg_model_DT1b_brier <- train(formula, data=bene2_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT1b_brier$results
b2reg_model_DT1b_brier$resample
b2reg_pred_DT1b_brier <- predict(b2reg_model_DT1b_brier, newdata = bene2_train1, type='prob')
bene2_train1$targetb <- bene2_train1$target
levels(bene2_train1$targetb) <- c('0','1')
b2reg_DT1b.bs <- Brier(as.numeric(as.character(bene2_train1$targetb)), b2reg_pred_DT1b_brier$X1)
b2reg_DT1b.bs

###data 2, train-test
#ROC curve 
set.seed(321); b2reg_model_DT2a_roc <- train(formula, data=bene2_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT2a_roc <- predict(b2reg_model_DT2a_roc,bene2_test2,type="prob")
b2reg_DT2a.ROC <- roc(predictor=b2regpredb_DT2a_roc$X0,
                      response=bene2_test2$target,
                      levels=rev(levels(bene2_test2$target)))
b2reg_DT2a.ROC

#normalizedGini
bene2_test2$targetb <- bene2_test2$target
levels(bene2_test2$targetb) <- c('0','1')
bene2_test2$targetb <- as.numeric(levels(bene2_test2$targetb))[bene2_test2$targetb]
set.seed(321); b2reg_model_DT2a_gini <- train(formula, data=bene2_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT2a_gini$results
b2reg_model_DT2a_gini$resample
b2reg_pred_DT2a_gini<- predict(b2reg_model_DT2a_gini, newdata = bene2_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test2$targetb, b2reg_pred_DT2a_gini$X1)
Gini(bene2_test2$targetb, b2reg_pred_DT2a_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT2a_gini$X1[b2reg_pred_DT2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test2$targetb[b2reg_pred_DT2a_gini$X1<=0.4]))
b2reg_DT2a.ngini <- normalizedGini(a, p)
b2reg_DT2a.ngini
b2reg_DT2a.gini <-Gini(a, p)
b2reg_DT2a.gini

#Brier score
set.seed(321); b2reg_model_DT2a_brier <- train(formula, data=bene2_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT2a_brier$results
b2reg_model_DT2a_brier$resample
b2reg_pred_DT2a_brier <- predict(b2reg_model_DT2a_brier, newdata = bene2_test2, type='prob')
bene2_test2$targetb <- bene2_test2$target
levels(bene2_test2$targetb) <- c('0','1')
b2reg_DT2a.bs <- Brier(as.numeric(as.character(bene2_test2$targetb)), b2reg_pred_DT2a_brier$X1)
b2reg_DT2a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2reg_model_DT2b_roc <- train(formula, data=bene2_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT2b_roc <- predict(b2reg_model_DT2b_roc, bene2_train2,type="prob")
b2reg_DT2b.ROC <- roc(predictor=b2regpredb_DT2b_roc$X0,
                      response=bene2_train2$target,
                      levels=rev(levels(bene2_train2$target)))
b2reg_DT2b.ROC

#normalizedGini
bene2_train2$targetb <- bene2_train2$target
levels(bene2_train2$targetb) <- c('0','1')
bene2_train2$targetb <- as.numeric(levels(bene2_train2$targetb))[bene2_train2$targetb]
set.seed(321); b2reg_model_DT2b_gini <- train(formula, data=bene2_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT2b_gini$results
b2reg_model_DT2b_gini$resample
b2reg_pred_DT2b_gini<- predict(b2reg_model_DT2b_gini, newdata = bene2_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train2$targetb, b2reg_pred_DT2b_gini$X1)
Gini(bene2_train2$targetb, b2reg_pred_DT2b_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT2b_gini$X1[b2reg_pred_DT2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train2$targetb[b2reg_pred_DT2b_gini$X1<=0.4]))
b2reg_DT2b.ngini <- normalizedGini(a, p)
b2reg_DT2b.ngini
b2reg_DT2b.gini <-Gini(a, p)
b2reg_DT2b.gini

#Brier score
set.seed(321); b2reg_model_DT2b_brier <- train(formula, data=bene2_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT2b_brier$results
b2reg_model_DT2b_brier$resample
b2reg_pred_DT2b_brier <- predict(b2reg_model_DT2b_brier, newdata = bene2_train2, type='prob')
bene2_train2$targetb <- bene2_train2$target
levels(bene2_train2$targetb) <- c('0','1')
b2reg_DT2b.bs <- Brier(as.numeric(as.character(bene2_train2$targetb)), b2reg_pred_DT2b_brier$X1)
b2reg_DT2b.bs

###data 3, train-test
#ROC curve 
set.seed(321); b2reg_model_DT3a_roc <- train(formula, data=bene2_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT3a_roc <- predict(b2reg_model_DT3a_roc,bene2_test3,type="prob")
b2reg_DT3a.ROC <- roc(predictor=b2regpredb_DT3a_roc$X0,
                      response=bene2_test3$target,
                      levels=rev(levels(bene2_test3$target)))
b2reg_DT3a.ROC

#normalizedGini
bene2_test3$targetb <- bene2_test3$target
levels(bene2_test3$targetb) <- c('0','1')
bene2_test3$targetb <- as.numeric(levels(bene2_test3$targetb))[bene2_test3$targetb]
set.seed(321); b2reg_model_DT3a_gini <- train(formula, data=bene2_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT3a_gini$results
b2reg_model_DT3a_gini$resample
b2reg_pred_DT3a_gini<- predict(b2reg_model_DT3a_gini, newdata = bene2_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test3$targetb, b2reg_pred_DT3a_gini$X1)
Gini(bene2_test3$targetb, b2reg_pred_DT3a_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT3a_gini$X1[b2reg_pred_DT3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test3$targetb[b2reg_pred_DT3a_gini$X1<=0.4]))
b2reg_DT3a.ngini <- normalizedGini(a, p)
b2reg_DT3a.ngini
b2reg_DT3a.gini <-Gini(a, p)
b2reg_DT3a.gini

#Brier score
set.seed(321); b2reg_model_DT3a_brier <- train(formula, data=bene2_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT3a_brier$results
b2reg_model_DT3a_brier$resample
b2reg_pred_DT3a_brier <- predict(b2reg_model_DT3a_brier, newdata = bene2_test3, type='prob')
bene2_test3$targetb <- bene2_test3$target
levels(bene2_test3$targetb) <- c('0','1')
b2reg_DT3a.bs <- Brier(as.numeric(as.character(bene2_test3$targetb)), b2reg_pred_DT3a_brier$X1)
b2reg_DT3a.bs

###data 3 - test-train
#ROC curve 
set.seed(321); b2reg_model_DT3b_roc <- train(formula, data=bene2_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT3b_roc <- predict(b2reg_model_DT3b_roc, bene2_train3,type="prob")
b2reg_DT3b.ROC <- roc(predictor=b2regpredb_DT3b_roc$X0,
                      response=bene2_train3$target,
                      levels=rev(levels(bene2_train3$target)))
b2reg_DT3b.ROC

#normalizedGini
bene2_train3$targetb <- bene2_train3$target
levels(bene2_train3$targetb) <- c('0','1')
bene2_train3$targetb <- as.numeric(levels(bene2_train3$targetb))[bene2_train3$targetb]
set.seed(321); b2reg_model_DT3b_gini <- train(formula, data=bene2_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT3b_gini$results
b2reg_model_DT3b_gini$resample
b2reg_pred_DT3b_gini<- predict(b2reg_model_DT3b_gini, newdata = bene2_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train3$targetb, b2reg_pred_DT3b_gini$X1)
Gini(bene2_train3$targetb, b2reg_pred_DT3b_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT3b_gini$X1[b2reg_pred_DT3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train3$targetb[b2reg_pred_DT3b_gini$X1<=0.4]))
b2reg_DT3b.ngini <- normalizedGini(a, p)
b2reg_DT3b.ngini
b2reg_DT3b.gini <-Gini(a, p)
b2reg_DT3b.gini

#Brier score
set.seed(321); b2reg_model_DT3b_brier <- train(formula, data=bene2_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT3b_brier$results
b2reg_model_DT3b_brier$resample
b2reg_pred_DT3b_brier <- predict(b2reg_model_DT3b_brier, newdata = bene2_train3, type='prob')
bene2_train3$targetb <- bene2_train3$target
levels(bene2_train3$targetb) <- c('0','1')
b2reg_DT3b.bs <- Brier(as.numeric(as.character(bene2_train3$targetb)), b2reg_pred_DT3b_brier$X1)
b2reg_DT3b.bs

###data 4, train-test
#ROC curve 
set.seed(321); b2reg_model_DT4a_roc <- train(formula, data=bene2_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT4a_roc <- predict(b2reg_model_DT4a_roc,bene2_test4,type="prob")
b2reg_DT4a.ROC <- roc(predictor=b2regpredb_DT4a_roc$X0,
                      response=bene2_test4$target,
                      levels=rev(levels(bene2_test4$target)))
b2reg_DT4a.ROC

#normalizedGini
bene2_test4$targetb <- bene2_test4$target
levels(bene2_test4$targetb) <- c('0','1')
bene2_test4$targetb <- as.numeric(levels(bene2_test4$targetb))[bene2_test4$targetb]
set.seed(321); b2reg_model_DT4a_gini <- train(formula, data=bene2_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT4a_gini$results
b2reg_model_DT4a_gini$resample
b2reg_pred_DT4a_gini<- predict(b2reg_model_DT4a_gini, newdata = bene2_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test4$targetb, b2reg_pred_DT4a_gini$X1)
Gini(bene2_test4$targetb, b2reg_pred_DT4a_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT4a_gini$X1[b2reg_pred_DT4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test4$targetb[b2reg_pred_DT4a_gini$X1<=0.4]))
b2reg_DT4a.ngini <- normalizedGini(a, p)
b2reg_DT4a.ngini
b2reg_DT4a.gini <-Gini(a, p)
b2reg_DT4a.gini

#Brier score
set.seed(321); b2reg_model_DT4a_brier <- train(formula, data=bene2_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT4a_brier$results
b2reg_model_DT4a_brier$resample
b2reg_pred_DT4a_brier <- predict(b2reg_model_DT4a_brier, newdata = bene2_test4, type='prob')
bene2_test4$targetb <- bene2_test4$target
levels(bene2_test4$targetb) <- c('0','1')
b2reg_DT4a.bs <- Brier(as.numeric(as.character(bene2_test4$targetb)), b2reg_pred_DT4a_brier$X1)
b2reg_DT4a.bs

###data 4 - test-train
#ROC curve 
set.seed(321); b2reg_model_DT4b_roc <- train(formula, data=bene2_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT4b_roc <- predict(b2reg_model_DT4b_roc, bene2_train4,type="prob")
b2reg_DT4b.ROC <- roc(predictor=b2regpredb_DT4b_roc$X0,
                      response=bene2_train4$target,
                      levels=rev(levels(bene2_train4$target)))
b2reg_DT4b.ROC

#normalizedGini
bene2_train4$targetb <- bene2_train4$target
levels(bene2_train4$targetb) <- c('0','1')
bene2_train4$targetb <- as.numeric(levels(bene2_train4$targetb))[bene2_train4$targetb]
set.seed(321); b2reg_model_DT4b_gini <- train(formula, data=bene2_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT4b_gini$results
b2reg_model_DT4b_gini$resample
b2reg_pred_DT4b_gini<- predict(b2reg_model_DT4b_gini, newdata = bene2_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train4$targetb, b2reg_pred_DT4b_gini$X1)
Gini(bene2_train4$targetb, b2reg_pred_DT4b_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT4b_gini$X1[b2reg_pred_DT4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train4$targetb[b2reg_pred_DT4b_gini$X1<=0.4]))
b2reg_DT4b.ngini <- normalizedGini(a, p)
b2reg_DT4b.ngini
b2reg_DT4b.gini <-Gini(a, p)
b2reg_DT4b.gini

#Brier score
set.seed(321); b2reg_model_DT4b_brier <- train(formula, data=bene2_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT4b_brier$results
b2reg_model_DT4b_brier$resample
b2reg_pred_DT4b_brier <- predict(b2reg_model_DT4b_brier, newdata = bene2_train4, type='prob')
bene2_train4$targetb <- bene2_train4$target
levels(bene2_train4$targetb) <- c('0','1')
b2reg_DT4b.bs <- Brier(as.numeric(as.character(bene2_train4$targetb)), b2reg_pred_DT4b_brier$X1)
b2reg_DT4b.bs

###data 5, train-test
#ROC curve 
set.seed(321); b2reg_model_DT5a_roc <- train(formula, data=bene2_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT5a_roc <- predict(b2reg_model_DT5a_roc,bene2_test5,type="prob")
b2reg_DT5a.ROC <- roc(predictor=b2regpredb_DT5a_roc$X0,
                      response=bene2_test5$target,
                      levels=rev(levels(bene2_test5$target)))
b2reg_DT5a.ROC

#normalizedGini
bene2_test5$targetb <- bene2_test5$target
levels(bene2_test5$targetb) <- c('0','1')
bene2_test5$targetb <- as.numeric(levels(bene2_test5$targetb))[bene2_test5$targetb]
set.seed(321); b2reg_model_DT5a_gini <- train(formula, data=bene2_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT5a_gini$results
b2reg_model_DT5a_gini$resample

b2reg_pred_DT5a_gini<- predict(b2reg_model_DT5a_gini, newdata = bene2_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test5$targetb, b2reg_pred_DT5a_gini$X1)
Gini(bene2_test5$targetb, b2reg_pred_DT5a_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT5a_gini$X1[b2reg_pred_DT5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test5$targetb[b2reg_pred_DT5a_gini$X1<=0.4]))
b2reg_DT5a.ngini <- normalizedGini(a, p)
b2reg_DT5a.ngini
b2reg_DT5a.gini <-Gini(a, p)
b2reg_DT5a.gini

#Brier score
set.seed(321); b2reg_model_DT5a_brier <- train(formula, data=bene2_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT5a_brier$results
b2reg_model_DT5a_brier$resample
b2reg_pred_DT5a_brier <- predict(b2reg_model_DT5a_brier, newdata = bene2_test5, type='prob')
bene2_test5$targetb <- bene2_test5$target
levels(bene2_test5$targetb) <- c('0','1')
b2reg_DT5a.bs <- Brier(as.numeric(as.character(bene2_test5$targetb)), b2reg_pred_DT5a_brier$X1)
b2reg_DT5a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2reg_model_DT5b_roc <- train(formula, data=bene2_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_DT5b_roc <- predict(b2reg_model_DT5b_roc, bene2_train5,type="prob")
b2reg_DT5b.ROC <- roc(predictor=b2regpredb_DT5b_roc$X0,
                      response=bene2_train5$target,
                      levels=rev(levels(bene2_train5$target)))
b2reg_DT5b.ROC

#normalizedGini
bene2_train5$targetb <- bene2_train5$target
levels(bene2_train5$targetb) <- c('0','1')
bene2_train5$targetb <- as.numeric(levels(bene2_train5$targetb))[bene2_train5$targetb]
set.seed(321); b2reg_model_DT5b_gini <- train(formula, data=bene2_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_DT5b_gini$results
b2reg_model_DT5b_gini$resample
b2reg_pred_DT5b_gini<- predict(b2reg_model_DT5b_gini, newdata = bene2_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train5$targetb, b2reg_pred_DT5b_gini$X1)
Gini(bene2_train5$targetb, b2reg_pred_DT5b_gini$X1)
#b <= 0.4
p <- b2reg_pred_DT5b_gini$X1[b2reg_pred_DT5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train5$targetb[b2reg_pred_DT5b_gini$X1<=0.4]))
b2reg_DT5b.ngini <- normalizedGini(a, p)
b2reg_DT5b.ngini
b2reg_DT5b.gini <-Gini(a, p)
b2reg_DT5b.gini

#Brier score
set.seed(321); b2reg_model_DT5b_brier <- train(formula, data=bene2_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_DT5b_brier$results
b2reg_model_DT5b_brier$resample
b2reg_pred_DT5b_brier <- predict(b2reg_model_DT5b_brier, newdata = bene2_train5, type='prob')
bene2_train5$targetb <- bene2_train5$target
levels(bene2_train5$targetb) <- c('0','1')
b2reg_DT5b.bs <- Brier(as.numeric(as.character(bene2_train5$targetb)), b2reg_pred_DT5b_brier$X1)
b2reg_DT5b.bs

##Restults DT!
b2reg_results_DT_AUC <- cbind(b2reg_DT1a.ROC$auc, b2reg_DT1b.ROC$auc, b2reg_DT2a.ROC$auc, b2reg_DT2b.ROC$auc, b2reg_DT3a.ROC$auc, b2reg_DT3b.ROC$auc, b2reg_DT4a.ROC$auc, b2reg_DT4b.ROC$auc, b2reg_DT5a.ROC$auc, b2reg_DT5b.ROC$auc)
b2reg_results_DT_bs <- cbind(b2reg_DT1a.bs, b2reg_DT1b.bs, b2reg_DT2a.bs, b2reg_DT2b.bs, b2reg_DT3a.bs, b2reg_DT3b.bs, b2reg_DT4a.bs, b2reg_DT4b.bs, b2reg_DT5a.bs, b2reg_DT5b.bs)
b2reg_results_DT_ngini <- cbind(b2reg_DT1a.ngini,b2reg_DT1b.ngini,b2reg_DT2a.ngini,b2reg_DT2b.ngini,b2reg_DT3a.ngini,b2reg_DT3b.ngini,b2reg_DT4a.ngini,b2reg_DT4b.ngini,b2reg_DT5a.ngini,b2reg_DT5b.ngini)
b2reg_results_DT_gini <- cbind(b2reg_DT1a.gini,b2reg_DT1b.gini,b2reg_DT2a.gini,b2reg_DT2b.gini,b2reg_DT3a.gini,b2reg_DT3b.gini,b2reg_DT4a.gini,b2reg_DT4b.gini,b2reg_DT5a.gini,b2reg_DT5b.gini)
mean(b2reg_results_DT_AUC)
mean(b2reg_results_DT_bs)
mean(b2reg_results_DT_ngini)
mean(b2reg_results_DT_gini)

#########################################
#######Random forest##################### https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/ dæmi um function til að gera forloop til að akveða numb trees
#########################################
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

m <- floor(log2(length(bene2_train1$target)+1))
RFgrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))
###data 1, train-test
#ROC curve 
set.seed(321); b2reg_model_RF1a_roc <- train(formula, data=bene2_train1, method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF1a_roc <- predict(b2reg_model_RF1a_roc,bene2_test1,type="prob")
b2reg_RF1a.ROC <- roc(predictor=b2regpredb_RF1a_roc$X0,
                      response=bene2_test1$target,
                      levels=rev(levels(bene2_test1$target)))
b2reg_RF1a.ROC

#normalizedGini
bene2_test1$targetb <- bene2_test1$target
levels(bene2_test1$targetb) <- c('0','1')
bene2_test1$targetb <- as.numeric(levels(bene2_test1$targetb))[bene2_test1$targetb]
set.seed(321); b2reg_model_RF1a_gini <- train(formula, data=bene2_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF1a_gini$results
b2reg_model_RF1a_gini$resample
b2reg_pred_RF1a_gini<- predict(b2reg_model_RF1a_gini, newdata = bene2_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test1$targetb, b2reg_pred_RF1a_gini$X1)
Gini(bene2_test1$targetb, b2reg_pred_RF1a_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF1a_gini$X1[b2reg_pred_RF1a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test1$targetb[b2reg_pred_RF1a_gini$X1<=0.4]))
b2reg_RF1a.ngini <- normalizedGini(a, p)
b2reg_RF1a.ngini
b2reg_RF1a.gini <-Gini(a, p)
b2reg_RF1a.gini

#Brier score
set.seed(321); b2reg_model_RF1a_brier <- train(formula, data=bene2_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF1a_brier$results
b2reg_model_RF1a_brier$resample
b2reg_pred_RF1a_brier <- predict(b2reg_model_RF1a_brier, newdata = bene2_test1, type='prob')
bene2_test1$targetb <- bene2_test1$target
levels(bene2_test1$targetb) <- c('0','1')
b2reg_RF1a.bs <- Brier(as.numeric(as.character(bene2_test1$targetb)), b2reg_pred_RF1a_brier$X1)
b2reg_RF1a.bs

###data 1 - test-train
#ROC curve 
set.seed(321); b2reg_model_RF1b_roc <- train(formula, data=bene2_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF1b_roc <- predict(b2reg_model_RF1b_roc, bene2_train1,type="prob")
b2reg_RF1b.ROC <- roc(predictor=b2regpredb_RF1b_roc$X0,
                      response=bene2_train1$target,
                      levels=rev(levels(bene2_train1$target)))
b2reg_RF1b.ROC

#normalizedGini
bene2_train1$targetb <- bene2_train1$target
levels(bene2_train1$targetb) <- c('0','1')
bene2_train1$targetb <- as.numeric(levels(bene2_train1$targetb))[bene2_train1$targetb]
set.seed(321); b2reg_model_RF1b_gini <- train(formula, data=bene2_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF1b_gini$results
b2reg_model_RF1b_gini$resample
b2reg_pred_RF1b_gini<- predict(b2reg_model_RF1b_gini, newdata = bene2_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train1$targetb, b2reg_pred_RF1b_gini$X1)
Gini(bene2_train1$targetb, b2reg_pred_RF1b_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF1b_gini$X1[b2reg_pred_RF1b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train1$targetb[b2reg_pred_RF1b_gini$X1<=0.4]))
b2reg_RF1b.ngini <- normalizedGini(a, p)
b2reg_RF1b.ngini
b2reg_RF1b.gini <-Gini(a, p)
b2reg_RF1b.gini

#Brier score
set.seed(321); b2reg_model_RF1b_brier <- train(formula, data=bene2_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF1b_brier$results
b2reg_model_RF1b_brier$resample
b2reg_pred_RF1b_brier <- predict(b2reg_model_RF1b_brier, newdata = bene2_train1, type='prob')
bene2_train1$targetb <- bene2_train1$target
levels(bene2_train1$targetb) <- c('0','1')
b2reg_RF1b.bs <- Brier(as.numeric(as.character(bene2_train1$targetb)), b2reg_pred_RF1b_brier$X1)
b2reg_RF1b.bs

###data 2, train-test
#ROC curve 
set.seed(321); b2reg_model_RF2a_roc <- train(formula, data=bene2_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF2a_roc <- predict(b2reg_model_RF2a_roc,bene2_test2,type="prob")
b2reg_RF2a.ROC <- roc(predictor=b2regpredb_RF2a_roc$X0,
                      response=bene2_test2$target,
                      levels=rev(levels(bene2_test2$target)))
b2reg_RF2a.ROC

#normalizedGini
bene2_test2$targetb <- bene2_test2$target
levels(bene2_test2$targetb) <- c('0','1')
bene2_test2$targetb <- as.numeric(levels(bene2_test2$targetb))[bene2_test2$targetb]
set.seed(321); b2reg_model_RF2a_gini <- train(formula, data=bene2_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF2a_gini$results
b2reg_model_RF2a_gini$resample
b2reg_pred_RF2a_gini<- predict(b2reg_model_RF2a_gini, newdata = bene2_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test2$targetb, b2reg_pred_RF2a_gini$X1)
Gini(bene2_test2$targetb, b2reg_pred_RF2a_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF2a_gini$X1[b2reg_pred_RF2a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test2$targetb[b2reg_pred_RF2a_gini$X1<=0.4]))
b2reg_RF2a.ngini <- normalizedGini(a, p)
b2reg_RF2a.ngini
b2reg_RF2a.gini <-Gini(a, p)
b2reg_RF2a.gini

#Brier score
set.seed(321); b2reg_model_RF2a_brier <- train(formula, data=bene2_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF2a_brier$results
b2reg_model_RF2a_brier$resample
b2reg_pred_RF2a_brier <- predict(b2reg_model_RF2a_brier, newdata = bene2_test2, type='prob')
bene2_test2$targetb <- bene2_test2$target
levels(bene2_test2$targetb) <- c('0','1')
b2reg_RF2a.bs <- Brier(as.numeric(as.character(bene2_test2$targetb)), b2reg_pred_RF2a_brier$X1)
b2reg_RF2a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2reg_model_RF2b_roc <- train(formula, data=bene2_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF2b_roc <- predict(b2reg_model_RF2b_roc, bene2_train2,type="prob")
b2reg_RF2b.ROC <- roc(predictor=b2regpredb_RF2b_roc$X0,
                      response=bene2_train2$target,
                      levels=rev(levels(bene2_train2$target)))
b2reg_RF2b.ROC

#normalizedGini
bene2_train2$targetb <- bene2_train2$target
levels(bene2_train2$targetb) <- c('0','1')
bene2_train2$targetb <- as.numeric(levels(bene2_train2$targetb))[bene2_train2$targetb]
set.seed(321); b2reg_model_RF2b_gini <- train(formula, data=bene2_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF2b_gini$results
b2reg_model_RF2b_gini$resample
b2reg_pred_RF2b_gini<- predict(b2reg_model_RF2b_gini, newdata = bene2_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train2$targetb, b2reg_pred_RF2b_gini$X1)
Gini(bene2_train2$targetb, b2reg_pred_RF2b_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF2b_gini$X1[b2reg_pred_RF2b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train2$targetb[b2reg_pred_RF2b_gini$X1<=0.4]))
b2reg_RF2b.ngini <- normalizedGini(a, p)
b2reg_RF2b.ngini
b2reg_RF2b.gini <-Gini(a, p)
b2reg_RF2b.gini

#Brier score
set.seed(321); b2reg_model_RF2b_brier <- train(formula, data=bene2_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF2b_brier$results
b2reg_model_RF2b_brier$resample
b2reg_pred_RF2b_brier <- predict(b2reg_model_RF2b_brier, newdata = bene2_train2, type='prob')
bene2_train2$targetb <- bene2_train2$target
levels(bene2_train2$targetb) <- c('0','1')
b2reg_RF2b.bs <- Brier(as.numeric(as.character(bene2_train2$targetb)), b2reg_pred_RF2b_brier$X1)
b2reg_RF2b.bs

###data 3, train-test
#ROC curve 
set.seed(321); b2reg_model_RF3a_roc <- train(formula, data=bene2_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF3a_roc <- predict(b2reg_model_RF3a_roc,bene2_test3,type="prob")
b2reg_RF3a.ROC <- roc(predictor=b2regpredb_RF3a_roc$X0,
                      response=bene2_test3$target,
                      levels=rev(levels(bene2_test3$target)))
b2reg_RF3a.ROC

#normalizedGini
bene2_test3$targetb <- bene2_test3$target
levels(bene2_test3$targetb) <- c('0','1')
bene2_test3$targetb <- as.numeric(levels(bene2_test3$targetb))[bene2_test3$targetb]
set.seed(321); b2reg_model_RF3a_gini <- train(formula, data=bene2_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF3a_gini$results
b2reg_model_RF3a_gini$resample
b2reg_pred_RF3a_gini<- predict(b2reg_model_RF3a_gini, newdata = bene2_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test3$targetb, b2reg_pred_RF3a_gini$X1)
Gini(bene2_test3$targetb, b2reg_pred_RF3a_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF3a_gini$X1[b2reg_pred_RF3a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test3$targetb[b2reg_pred_RF3a_gini$X1<=0.4]))
b2reg_RF3a.ngini <- normalizedGini(a, p)
b2reg_RF3a.ngini
b2reg_RF3a.gini <-Gini(a, p)
b2reg_RF3a.gini

#Brier score
set.seed(321); b2reg_model_RF3a_brier <- train(formula, data=bene2_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF3a_brier$results
b2reg_model_RF3a_brier$resample
b2reg_pred_RF3a_brier <- predict(b2reg_model_RF3a_brier, newdata = bene2_test3, type='prob')
bene2_test3$targetb <- bene2_test3$target
levels(bene2_test3$targetb) <- c('0','1')
b2reg_RF3a.bs <- Brier(as.numeric(as.character(bene2_test3$targetb)), b2reg_pred_RF3a_brier$X1)
b2reg_RF3a.bs

###data 3 - test-train
#ROC curve 
set.seed(321); b2reg_model_RF3b_roc <- train(formula, data=bene2_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF3b_roc <- predict(b2reg_model_RF3b_roc, bene2_train3,type="prob")
b2reg_RF3b.ROC <- roc(predictor=b2regpredb_RF3b_roc$X0,
                      response=bene2_train3$target,
                      levels=rev(levels(bene2_train3$target)))
b2reg_RF3b.ROC

#normalizedGini
bene2_train3$targetb <- bene2_train3$target
levels(bene2_train3$targetb) <- c('0','1')
bene2_train3$targetb <- as.numeric(levels(bene2_train3$targetb))[bene2_train3$targetb]
set.seed(321); b2reg_model_RF3b_gini <- train(formula, data=bene2_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF3b_gini$results
b2reg_model_RF3b_gini$resample
b2reg_pred_RF3b_gini<- predict(b2reg_model_RF3b_gini, newdata = bene2_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train3$targetb, b2reg_pred_RF3b_gini$X1)
Gini(bene2_train3$targetb, b2reg_pred_RF3b_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF3b_gini$X1[b2reg_pred_RF3b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train3$targetb[b2reg_pred_RF3b_gini$X1<=0.4]))
b2reg_RF3b.ngini <- normalizedGini(a, p)
b2reg_RF3b.ngini
b2reg_RF3b.gini <-Gini(a, p)
b2reg_RF3b.gini

#Brier score
set.seed(321); b2reg_model_RF3b_brier <- train(formula, data=bene2_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF3b_brier$results
b2reg_model_RF3b_brier$resample
b2reg_pred_RF3b_brier <- predict(b2reg_model_RF3b_brier, newdata = bene2_train3, type='prob')
bene2_train3$targetb <- bene2_train3$target
levels(bene2_train3$targetb) <- c('0','1')
b2reg_RF3b.bs <- Brier(as.numeric(as.character(bene2_train3$targetb)), b2reg_pred_RF3b_brier$X1)
b2reg_RF3b.bs

###data 4, train-test
#ROC curve 
set.seed(321); b2reg_model_RF4a_roc <- train(formula, data=bene2_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF4a_roc <- predict(b2reg_model_RF4a_roc,bene2_test4,type="prob")
b2reg_RF4a.ROC <- roc(predictor=b2regpredb_RF4a_roc$X0,
                      response=bene2_test4$target,
                      levels=rev(levels(bene2_test4$target)))
b2reg_RF4a.ROC

#normalizedGini
bene2_test4$targetb <- bene2_test4$target
levels(bene2_test4$targetb) <- c('0','1')
bene2_test4$targetb <- as.numeric(levels(bene2_test4$targetb))[bene2_test4$targetb]
set.seed(321); b2reg_model_RF4a_gini <- train(formula, data=bene2_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF4a_gini$results
b2reg_model_RF4a_gini$resample
b2reg_pred_RF4a_gini<- predict(b2reg_model_RF4a_gini, newdata = bene2_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test4$targetb, b2reg_pred_RF4a_gini$X1)
Gini(bene2_test4$targetb, b2reg_pred_RF4a_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF4a_gini$X1[b2reg_pred_RF4a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test4$targetb[b2reg_pred_RF4a_gini$X1<=0.4]))
b2reg_RF4a.ngini <- normalizedGini(a, p)
b2reg_RF4a.ngini
b2reg_RF4a.gini <-Gini(a, p)
b2reg_RF4a.gini

#Brier score
set.seed(321); b2reg_model_RF4a_brier <- train(formula, data=bene2_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF4a_brier$results
b2reg_model_RF4a_brier$resample
b2reg_pred_RF4a_brier <- predict(b2reg_model_RF4a_brier, newdata = bene2_test4, type='prob')
bene2_test4$targetb <- bene2_test4$target
levels(bene2_test4$targetb) <- c('0','1')
b2reg_RF4a.bs <- Brier(as.numeric(as.character(bene2_test4$targetb)), b2reg_pred_RF4a_brier$X1)
b2reg_RF4a.bs

###data 4 - test-train
#ROC curve 
set.seed(321); b2reg_model_RF4b_roc <- train(formula, data=bene2_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF4b_roc <- predict(b2reg_model_RF4b_roc, bene2_train4,type="prob")
b2reg_RF4b.ROC <- roc(predictor=b2regpredb_RF4b_roc$X0,
                      response=bene2_train4$target,
                      levels=rev(levels(bene2_train4$target)))
b2reg_RF4b.ROC

#normalizedGini
bene2_train4$targetb <- bene2_train4$target
levels(bene2_train4$targetb) <- c('0','1')
bene2_train4$targetb <- as.numeric(levels(bene2_train4$targetb))[bene2_train4$targetb]
set.seed(321); b2reg_model_RF4b_gini <- train(formula, data=bene2_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF4b_gini$results
b2reg_model_RF4b_gini$resample
b2reg_pred_RF4b_gini<- predict(b2reg_model_RF4b_gini, newdata = bene2_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train4$targetb, b2reg_pred_RF4b_gini$X1)
Gini(bene2_train4$targetb, b2reg_pred_RF4b_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF4b_gini$X1[b2reg_pred_RF4b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train4$targetb[b2reg_pred_RF4b_gini$X1<=0.4]))
b2reg_RF4b.ngini <- normalizedGini(a, p)
b2reg_RF4b.ngini
b2reg_RF4b.gini <-Gini(a, p)
b2reg_RF4b.gini

#Brier score
set.seed(321); b2reg_model_RF4b_brier <- train(formula, data=bene2_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF4b_brier$results
b2reg_model_RF4b_brier$resample
b2reg_pred_RF4b_brier <- predict(b2reg_model_RF4b_brier, newdata = bene2_train4, type='prob')
bene2_train4$targetb <- bene2_train4$target
levels(bene2_train4$targetb) <- c('0','1')
b2reg_RF4b.bs <- Brier(as.numeric(as.character(bene2_train4$targetb)), b2reg_pred_RF4b_brier$X1)
b2reg_RF4b.bs

###data 5, train-test
#ROC curve 
set.seed(321); b2reg_model_RF5a_roc <- train(formula, data=bene2_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF5a_roc <- predict(b2reg_model_RF5a_roc,bene2_test5,type="prob")
b2reg_RF5a.ROC <- roc(predictor=b2regpredb_RF5a_roc$X0,
                      response=bene2_test5$target,
                      levels=rev(levels(bene2_test5$target)))
b2reg_RF5a.ROC

#normalizedGini
bene2_test5$targetb <- bene2_test5$target
levels(bene2_test5$targetb) <- c('0','1')
bene2_test5$targetb <- as.numeric(levels(bene2_test5$targetb))[bene2_test5$targetb]
set.seed(321); b2reg_model_RF5a_gini <- train(formula, data=bene2_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF5a_gini$results
b2reg_model_RF5a_gini$resample

b2reg_pred_RF5a_gini<- predict(b2reg_model_RF5a_gini, newdata = bene2_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_test5$targetb, b2reg_pred_RF5a_gini$X1)
Gini(bene2_test5$targetb, b2reg_pred_RF5a_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF5a_gini$X1[b2reg_pred_RF5a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_test5$targetb[b2reg_pred_RF5a_gini$X1<=0.4]))
b2reg_RF5a.ngini <- normalizedGini(a, p)
b2reg_RF5a.ngini
b2reg_RF5a.gini <-Gini(a, p)
b2reg_RF5a.gini

#Brier score
set.seed(321); b2reg_model_RF5a_brier <- train(formula, data=bene2_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF5a_brier$results
b2reg_model_RF5a_brier$resample
b2reg_pred_RF5a_brier <- predict(b2reg_model_RF5a_brier, newdata = bene2_test5, type='prob')
bene2_test5$targetb <- bene2_test5$target
levels(bene2_test5$targetb) <- c('0','1')
b2reg_RF5a.bs <- Brier(as.numeric(as.character(bene2_test5$targetb)), b2reg_pred_RF5a_brier$X1)
b2reg_RF5a.bs

###data 2 - test-train
#ROC curve 
set.seed(321); b2reg_model_RF5b_roc <- train(formula, data=bene2_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_roc, metric='ROC')
b2regpredb_RF5b_roc <- predict(b2reg_model_RF5b_roc, bene2_train5,type="prob")
b2reg_RF5b.ROC <- roc(predictor=b2regpredb_RF5b_roc$X0,
                      response=bene2_train5$target,
                      levels=rev(levels(bene2_train5$target)))
b2reg_RF5b.ROC

#normalizedGini
bene2_train5$targetb <- bene2_train5$target
levels(bene2_train5$targetb) <- c('0','1')
bene2_train5$targetb <- as.numeric(levels(bene2_train5$targetb))[bene2_train5$targetb]
set.seed(321); b2reg_model_RF5b_gini <- train(formula, data=bene2_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_gini, metric='Gini')
b2reg_model_RF5b_gini$results
b2reg_model_RF5b_gini$resample
b2reg_pred_RF5b_gini<- predict(b2reg_model_RF5b_gini, newdata = bene2_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_train5$targetb, b2reg_pred_RF5b_gini$X1)
Gini(bene2_train5$targetb, b2reg_pred_RF5b_gini$X1)
#b <= 0.4
p <- b2reg_pred_RF5b_gini$X1[b2reg_pred_RF5b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_train5$targetb[b2reg_pred_RF5b_gini$X1<=0.4]))
b2reg_RF5b.ngini <- normalizedGini(a, p)
b2reg_RF5b.ngini
b2reg_RF5b.gini <-Gini(a, p)
b2reg_RF5b.gini

#Brier score
set.seed(321); b2reg_model_RF5b_brier <- train(formula, data=bene2_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2reg_model_RF5b_brier$results
b2reg_model_RF5b_brier$resample
b2reg_pred_RF5b_brier <- predict(b2reg_model_RF5b_brier, newdata = bene2_train5, type='prob')
bene2_train5$targetb <- bene2_train5$target
levels(bene2_train5$targetb) <- c('0','1')
b2reg_RF5b.bs <- Brier(as.numeric(as.character(bene2_train5$targetb)), b2reg_pred_RF5b_brier$X1)
b2reg_RF5b.bs

##Restults RF!
b2reg_results_RF_AUC <- cbind(b2reg_RF1a.ROC$auc, b2reg_RF1b.ROC$auc, b2reg_RF2a.ROC$auc, b2reg_RF2b.ROC$auc, b2reg_RF3a.ROC$auc, b2reg_RF3b.ROC$auc, b2reg_RF4a.ROC$auc, b2reg_RF4b.ROC$auc, b2reg_RF5a.ROC$auc, b2reg_RF5b.ROC$auc)
b2reg_results_RF_bs <- cbind(b2reg_RF1a.bs, b2reg_RF1b.bs, b2reg_RF2a.bs, b2reg_RF2b.bs, b2reg_RF3a.bs, b2reg_RF3b.bs, b2reg_RF4a.bs, b2reg_RF4b.bs, b2reg_RF5a.bs, b2reg_RF5b.bs)
b2reg_results_RF_ngini <- cbind(b2reg_RF1a.ngini,b2reg_RF1b.ngini,b2reg_RF2a.ngini,b2reg_RF2b.ngini,b2reg_RF3a.ngini,b2reg_RF3b.ngini,b2reg_RF4a.ngini,b2reg_RF4b.ngini,b2reg_RF5a.ngini,b2reg_RF5b.ngini)
b2reg_results_RF_gini <- cbind(b2reg_RF1a.gini,b2reg_RF1b.gini,b2reg_RF2a.gini,b2reg_RF2b.gini,b2reg_RF3a.gini,b2reg_RF3b.gini,b2reg_RF4a.gini,b2reg_RF4b.gini,b2reg_RF5a.gini,b2reg_RF5b.gini)
mean(b2reg_results_RF_AUC)
mean(b2reg_results_RF_bs)
mean(b2reg_results_RF_ngini)
mean(b2reg_results_RF_gini)

####################################
########Deep learning MLP#########
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

batch <- floor(nrow(bene2_woe_train1)/3)
##############################
###########MLP1###############
##############################

MLP1grid <- expand.grid(.size=c(5,10,15,20), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01,0.001), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP11a_roc <- train(formula, data=bene2_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP1grid <- expand.grid(.size=c(15), .dropout=c(0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP11a_roc <- train(formula, data=bene2_woe_train1, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP11a_roc <- predict(b2_model_MLP11a_roc,bene2_woe_test1,type="prob")
b2_MLP11a.ROC <- roc(predictor=b2predb_MLP11a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_MLP11a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_MLP11a_gini <- train(formula, data=bene2_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP11a_gini$results
b2_model_MLP11a_gini$resample
b2_pred_MLP11a_gini<- predict(b2_model_MLP11a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_MLP11a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_MLP11a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP11a_gini$X1[b2_pred_MLP11a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_MLP11a_gini$X1<=0.4]))
b2_MLP11a.ngini <- normalizedGini(a, p)
b2_MLP11a.ngini
b2_MLP11a.gini <-Gini(a, p)
b2_MLP11a.gini

#Brier score
set.seed(123); b2_model_MLP11a_brier <- train(formula, data=bene2_woe_train1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP11a_brier$results
b2_model_MLP11a_brier$resample
b2_pred_MLP11a_brier <- predict(b2_model_MLP11a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_MLP11a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_MLP11a_brier$X1)
b2_MLP11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_MLP11b_roc <- train(formula, data=bene2_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP11b_roc <- predict(b2_model_MLP11b_roc, bene2_woe_train1,type="prob")
b2_MLP11b.ROC <- roc(predictor=b2predb_MLP11b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_MLP11b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_MLP11b_gini <- train(formula, data=bene2_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP11b_gini$results
b2_model_MLP11b_gini$resample
b2_pred_MLP11b_gini<- predict(b2_model_MLP11b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_MLP11b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_MLP11b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP11b_gini$X1[b2_pred_MLP11b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_MLP11b_gini$X1<=0.4]))
b2_MLP11b.ngini <- normalizedGini(a, p)
b2_MLP11b.ngini
b2_MLP11b.gini <-Gini(a, p)
b2_MLP11b.gini

#Brier score
set.seed(123); b2_model_MLP11b_brier <- train(formula, data=bene2_woe_test1,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP11b_brier$results
b2_model_MLP11b_brier$resample
b2_pred_MLP11b_brier <- predict(b2_model_MLP11b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_MLP11b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_MLP11b_brier$X1)
b2_MLP11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP12a_roc <- train(formula, data=bene2_woe_train2, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP12a_roc <- predict(b2_model_MLP12a_roc,bene2_woe_test2,type="prob")
b2_MLP12a.ROC <- roc(predictor=b2predb_MLP12a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_MLP12a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_MLP12a_gini <- train(formula, data=bene2_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP12a_gini$results
b2_model_MLP12a_gini$resample
b2_pred_MLP12a_gini<- predict(b2_model_MLP12a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_MLP12a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_MLP12a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP12a_gini$X1[b2_pred_MLP12a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_MLP12a_gini$X1<=0.4]))
b2_MLP12a.ngini <- normalizedGini(a, p)
b2_MLP12a.ngini
b2_MLP12a.gini <-Gini(a, p)
b2_MLP12a.gini

#Brier score
set.seed(123); b2_model_MLP12a_brier <- train(formula, data=bene2_woe_train2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP12a_brier$results
b2_model_MLP12a_brier$resample
b2_pred_MLP12a_brier <- predict(b2_model_MLP12a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_MLP12a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_MLP12a_brier$X1)
b2_MLP12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_MLP12b_roc <- train(formula, data=bene2_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP12b_roc <- predict(b2_model_MLP12b_roc, bene2_woe_train2,type="prob")
b2_MLP12b.ROC <- roc(predictor=b2predb_MLP12b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_MLP12b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_MLP12b_gini <- train(formula, data=bene2_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP12b_gini$results
b2_model_MLP12b_gini$resample
b2_pred_MLP12b_gini<- predict(b2_model_MLP12b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_MLP12b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_MLP12b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP12b_gini$X1[b2_pred_MLP12b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_MLP12b_gini$X1<=0.4]))
b2_MLP12b.ngini <- normalizedGini(a, p)
b2_MLP12b.ngini
b2_MLP12b.gini <-Gini(a, p)
b2_MLP12b.gini

#Brier score
set.seed(123); b2_model_MLP12b_brier <- train(formula, data=bene2_woe_test2,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP12b_brier$results
b2_model_MLP12b_brier$resample
b2_pred_MLP12b_brier <- predict(b2_model_MLP12b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_MLP12b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_MLP12b_brier$X1)
b2_MLP12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP13a_roc <- train(formula, data=bene2_woe_train3, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP13a_roc <- predict(b2_model_MLP13a_roc,bene2_woe_test3,type="prob")
b2_MLP13a.ROC <- roc(predictor=b2predb_MLP13a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_MLP13a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_MLP13a_gini <- train(formula, data=bene2_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP13a_gini$results
b2_model_MLP13a_gini$resample
b2_pred_MLP13a_gini<- predict(b2_model_MLP13a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_MLP13a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_MLP13a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP13a_gini$X1[b2_pred_MLP13a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_MLP13a_gini$X1<=0.4]))
b2_MLP13a.ngini <- normalizedGini(a, p)
b2_MLP13a.ngini
b2_MLP13a.gini <-Gini(a, p)
b2_MLP13a.gini

#Brier score
set.seed(123); b2_model_MLP13a_brier <- train(formula, data=bene2_woe_train3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP13a_brier$results
b2_model_MLP13a_brier$resample
b2_pred_MLP13a_brier <- predict(b2_model_MLP13a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_MLP13a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_MLP13a_brier$X1)
b2_MLP13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_MLP13b_roc <- train(formula, data=bene2_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP13b_roc <- predict(b2_model_MLP13b_roc, bene2_woe_train3,type="prob")
b2_MLP13b.ROC <- roc(predictor=b2predb_MLP13b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_MLP13b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_MLP13b_gini <- train(formula, data=bene2_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP13b_gini$results
b2_model_MLP13b_gini$resample
b2_pred_MLP13b_gini<- predict(b2_model_MLP13b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_MLP13b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_MLP13b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP13b_gini$X1[b2_pred_MLP13b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_MLP13b_gini$X1<=0.4]))
b2_MLP13b.ngini <- normalizedGini(a, p)
b2_MLP13b.ngini
b2_MLP13b.gini <-Gini(a, p)
b2_MLP13b.gini

#Brier score
set.seed(123); b2_model_MLP13b_brier <- train(formula, data=bene2_woe_test3,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP13b_brier$results
b2_model_MLP13b_brier$resample
b2_pred_MLP13b_brier <- predict(b2_model_MLP13b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_MLP13b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_MLP13b_brier$X1)
b2_MLP13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP14a_roc <- train(formula, data=bene2_woe_train4, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP14a_roc <- predict(b2_model_MLP14a_roc,bene2_woe_test4,type="prob")
b2_MLP14a.ROC <- roc(predictor=b2predb_MLP14a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_MLP14a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_MLP14a_gini <- train(formula, data=bene2_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP14a_gini$results
b2_model_MLP14a_gini$resample
b2_pred_MLP14a_gini<- predict(b2_model_MLP14a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_MLP14a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_MLP14a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP14a_gini$X1[b2_pred_MLP14a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_MLP14a_gini$X1<=0.4]))
b2_MLP14a.ngini <- normalizedGini(a, p)
b2_MLP14a.ngini
b2_MLP14a.gini <-Gini(a, p)
b2_MLP14a.gini

#Brier score
set.seed(123); b2_model_MLP14a_brier <- train(formula, data=bene2_woe_train4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP14a_brier$results
b2_model_MLP14a_brier$resample
b2_pred_MLP14a_brier <- predict(b2_model_MLP14a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_MLP14a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_MLP14a_brier$X1)
b2_MLP14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_MLP14b_roc <- train(formula, data=bene2_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP14b_roc <- predict(b2_model_MLP14b_roc, bene2_woe_train4,type="prob")
b2_MLP14b.ROC <- roc(predictor=b2predb_MLP14b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_MLP14b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_MLP14b_gini <- train(formula, data=bene2_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP14b_gini$results
b2_model_MLP14b_gini$resample
b2_pred_MLP14b_gini<- predict(b2_model_MLP14b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_MLP14b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_MLP14b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP14b_gini$X1[b2_pred_MLP14b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_MLP14b_gini$X1<=0.4]))
b2_MLP14b.ngini <- normalizedGini(a, p)
b2_MLP14b.ngini
b2_MLP14b.gini <-Gini(a, p)
b2_MLP14b.gini

#Brier score
set.seed(123); b2_model_MLP14b_brier <- train(formula, data=bene2_woe_test4,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP14b_brier$results
b2_model_MLP14b_brier$resample
b2_pred_MLP14b_brier <- predict(b2_model_MLP14b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_MLP14b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_MLP14b_brier$X1)
b2_MLP14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP15a_roc <- train(formula, data=bene2_woe_train5, method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP15a_roc <- predict(b2_model_MLP15a_roc,bene2_woe_test5,type="prob")
b2_MLP15a.ROC <- roc(predictor=b2predb_MLP15a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_MLP15a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_MLP15a_gini <- train(formula, data=bene2_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP15a_gini$results
b2_model_MLP15a_gini$resample
b2_pred_MLP15a_gini<- predict(b2_model_MLP15a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_MLP15a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_MLP15a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP15a_gini$X1[b2_pred_MLP15a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_MLP15a_gini$X1<=0.4]))
b2_MLP15a.ngini <- normalizedGini(a, p)
b2_MLP15a.ngini
b2_MLP15a.gini <-Gini(a, p)
b2_MLP15a.gini

#Brier score
set.seed(123); b2_model_MLP15a_brier <- train(formula, data=bene2_woe_train5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP15a_brier$results
b2_model_MLP15a_brier$resample
b2_pred_MLP15a_brier <- predict(b2_model_MLP15a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_MLP15a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_MLP15a_brier$X1)
b2_MLP15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_MLP15b_roc <- train(formula, data=bene2_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP15b_roc <- predict(b2_model_MLP15b_roc, bene2_woe_train5,type="prob")
b2_MLP15b.ROC <- roc(predictor=b2predb_MLP15b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_MLP15b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_MLP15b_gini <- train(formula, data=bene2_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP15b_gini$results
b2_model_MLP15b_gini$resample
b2_pred_MLP15b_gini<- predict(b2_model_MLP15b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_MLP15b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_MLP15b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP15b_gini$X1[b2_pred_MLP15b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_MLP15b_gini$X1<=0.4]))
b2_MLP15b.ngini <- normalizedGini(a, p)
b2_MLP15b.ngini
b2_MLP15b.gini <-Gini(a, p)
b2_MLP15b.gini

#Brier score
set.seed(123); b2_model_MLP15b_brier <- train(formula, data=bene2_woe_test5,  method = MLP1, tuneGrid=MLP1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP15b_brier$results
b2_model_MLP15b_brier$resample
b2_pred_MLP15b_brier <- predict(b2_model_MLP15b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_MLP15b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_MLP15b_brier$X1)
b2_MLP15b.bs


##Restults RF!
b2_results_MLP1_AUC <- cbind(b2_MLP11a.ROC$auc,b2_MLP11b.ROC$auc,b2_MLP12a.ROC$auc,b2_MLP12b.ROC$auc,b2_MLP13a.ROC$auc,b2_MLP13b.ROC$auc,b2_MLP14a.ROC$auc,
                             b2_MLP14b.ROC$auc,b2_MLP15a.ROC$auc,b2_MLP15b.ROC$auc)
b2_results_MLP1_bs <- cbind(b2_MLP11a.bs,b2_MLP11b.bs,b2_MLP12a.bs,b2_MLP12b.bs,b2_MLP13a.bs,b2_MLP13b.bs,b2_MLP14a.bs,b2_MLP14b.bs,b2_MLP15a.bs,b2_MLP15b.bs)
b2_results_MLP1_ngini <- cbind(b2_MLP11a.ngini,b2_MLP11b.ngini,b2_MLP12a.ngini,b2_MLP12b.ngini,b2_MLP13a.ngini,b2_MLP13b.ngini,b2_MLP14a.ngini,b2_MLP14b.ngini,
                               b2_MLP15a.ngini,b2_MLP15b.ngini)
b2_results_MLP1_gini <- cbind(b2_MLP11a.gini,b2_MLP11b.gini,b2_MLP12a.gini,b2_MLP12b.gini,b2_MLP13a.gini,b2_MLP13b.gini,b2_MLP14a.gini,b2_MLP14b.gini,
                              b2_MLP15a.gini,b2_MLP15b.gini)
mean(b2_results_MLP1_AUC)
mean(b2_results_MLP1_bs)
mean(b2_results_MLP1_ngini)
mean(b2_results_MLP1_gini)



##############################
###########MLP3###############
##############################
MLP3grid <- expand.grid(.size1=c(5,10,15,20), .size2=c(5,10,15,20), .size3=c(5,10,15,20), .dropout=c(0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP31a_roc <- train(formula, data=bene2_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP3grid <- expand.grid(.size1=c(20), .size2=c(20), .size3=c(15), .dropout=c(0.25), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
set.seed(123); b2_model_MLP31a_roc <- train(formula, data=bene2_woe_train1, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP31a_roc <- predict(b2_model_MLP31a_roc,bene2_woe_test1,type="prob")
b2_MLP31a.ROC <- roc(predictor=b2predb_MLP31a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_MLP31a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_MLP31a_gini <- train(formula, data=bene2_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP31a_gini$results
b2_model_MLP31a_gini$resample
b2_pred_MLP31a_gini<- predict(b2_model_MLP31a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_MLP31a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_MLP31a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP31a_gini$X1[b2_pred_MLP31a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_MLP31a_gini$X1<=0.4]))
b2_MLP31a.ngini <- normalizedGini(a, p)
b2_MLP31a.ngini
b2_MLP31a.gini <-Gini(a, p)
b2_MLP31a.gini

#Brier score
set.seed(123); b2_model_MLP31a_brier <- train(formula, data=bene2_woe_train1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP31a_brier$results
b2_model_MLP31a_brier$resample
b2_pred_MLP31a_brier <- predict(b2_model_MLP31a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_MLP31a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_MLP31a_brier$X1)
b2_MLP31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_MLP31b_roc <- train(formula, data=bene2_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP31b_roc <- predict(b2_model_MLP31b_roc, bene2_woe_train1,type="prob")
b2_MLP31b.ROC <- roc(predictor=b2predb_MLP31b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_MLP31b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_MLP31b_gini <- train(formula, data=bene2_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP31b_gini$results
b2_model_MLP31b_gini$resample
b2_pred_MLP31b_gini<- predict(b2_model_MLP31b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_MLP31b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_MLP31b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP31b_gini$X1[b2_pred_MLP31b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_MLP31b_gini$X1<=0.4]))
b2_MLP31b.ngini <- normalizedGini(a, p)
b2_MLP31b.ngini
b2_MLP31b.gini <-Gini(a, p)
b2_MLP31b.gini

#Brier score
set.seed(123); b2_model_MLP31b_brier <- train(formula, data=bene2_woe_test1,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP31b_brier$results
b2_model_MLP31b_brier$resample
b2_pred_MLP31b_brier <- predict(b2_model_MLP31b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_MLP31b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_MLP31b_brier$X1)
b2_MLP31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP32a_roc <- train(formula, data=bene2_woe_train2, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP32a_roc <- predict(b2_model_MLP32a_roc,bene2_woe_test2,type="prob")
b2_MLP32a.ROC <- roc(predictor=b2predb_MLP32a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_MLP32a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_MLP32a_gini <- train(formula, data=bene2_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP32a_gini$results
b2_model_MLP32a_gini$resample
b2_pred_MLP32a_gini<- predict(b2_model_MLP32a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_MLP32a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_MLP32a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP32a_gini$X1[b2_pred_MLP32a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_MLP32a_gini$X1<=0.4]))
b2_MLP32a.ngini <- normalizedGini(a, p)
b2_MLP32a.ngini
b2_MLP32a.gini <-Gini(a, p)
b2_MLP32a.gini

#Brier score
set.seed(123); b2_model_MLP32a_brier <- train(formula, data=bene2_woe_train2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP32a_brier$results
b2_model_MLP32a_brier$resample
b2_pred_MLP32a_brier <- predict(b2_model_MLP32a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_MLP32a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_MLP32a_brier$X1)
b2_MLP32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_MLP32b_roc <- train(formula, data=bene2_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP32b_roc <- predict(b2_model_MLP32b_roc, bene2_woe_train2,type="prob")
b2_MLP32b.ROC <- roc(predictor=b2predb_MLP32b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_MLP32b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_MLP32b_gini <- train(formula, data=bene2_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP32b_gini$results
b2_model_MLP32b_gini$resample
b2_pred_MLP32b_gini<- predict(b2_model_MLP32b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_MLP32b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_MLP32b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP32b_gini$X1[b2_pred_MLP32b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_MLP32b_gini$X1<=0.4]))
b2_MLP32b.ngini <- normalizedGini(a, p)
b2_MLP32b.ngini
b2_MLP32b.gini <-Gini(a, p)
b2_MLP32b.gini

#Brier score
set.seed(123); b2_model_MLP32b_brier <- train(formula, data=bene2_woe_test2,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP32b_brier$results
b2_model_MLP32b_brier$resample
b2_pred_MLP32b_brier <- predict(b2_model_MLP32b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_MLP32b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_MLP32b_brier$X1)
b2_MLP32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP33a_roc <- train(formula, data=bene2_woe_train3, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP33a_roc <- predict(b2_model_MLP33a_roc,bene2_woe_test3,type="prob")
b2_MLP33a.ROC <- roc(predictor=b2predb_MLP33a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_MLP33a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_MLP33a_gini <- train(formula, data=bene2_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP33a_gini$results
b2_model_MLP33a_gini$resample
b2_pred_MLP33a_gini<- predict(b2_model_MLP33a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_MLP33a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_MLP33a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP33a_gini$X1[b2_pred_MLP33a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_MLP33a_gini$X1<=0.4]))
b2_MLP33a.ngini <- normalizedGini(a, p)
b2_MLP33a.ngini
b2_MLP33a.gini <-Gini(a, p)
b2_MLP33a.gini

#Brier score
set.seed(123); b2_model_MLP33a_brier <- train(formula, data=bene2_woe_train3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP33a_brier$results
b2_model_MLP33a_brier$resample
b2_pred_MLP33a_brier <- predict(b2_model_MLP33a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_MLP33a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_MLP33a_brier$X1)
b2_MLP33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_MLP33b_roc <- train(formula, data=bene2_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP33b_roc <- predict(b2_model_MLP33b_roc, bene2_woe_train3,type="prob")
b2_MLP33b.ROC <- roc(predictor=b2predb_MLP33b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_MLP33b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_MLP33b_gini <- train(formula, data=bene2_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP33b_gini$results
b2_model_MLP33b_gini$resample
b2_pred_MLP33b_gini<- predict(b2_model_MLP33b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_MLP33b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_MLP33b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP33b_gini$X1[b2_pred_MLP33b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_MLP33b_gini$X1<=0.4]))
b2_MLP33b.ngini <- normalizedGini(a, p)
b2_MLP33b.ngini
b2_MLP33b.gini <-Gini(a, p)
b2_MLP33b.gini

#Brier score
set.seed(123); b2_model_MLP33b_brier <- train(formula, data=bene2_woe_test3,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP33b_brier$results
b2_model_MLP33b_brier$resample
b2_pred_MLP33b_brier <- predict(b2_model_MLP33b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_MLP33b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_MLP33b_brier$X1)
b2_MLP33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP34a_roc <- train(formula, data=bene2_woe_train4, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP34a_roc <- predict(b2_model_MLP34a_roc,bene2_woe_test4,type="prob")
b2_MLP34a.ROC <- roc(predictor=b2predb_MLP34a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_MLP34a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_MLP34a_gini <- train(formula, data=bene2_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP34a_gini$results
b2_model_MLP34a_gini$resample
b2_pred_MLP34a_gini<- predict(b2_model_MLP34a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_MLP34a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_MLP34a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP34a_gini$X1[b2_pred_MLP34a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_MLP34a_gini$X1<=0.4]))
b2_MLP34a.ngini <- normalizedGini(a, p)
b2_MLP34a.ngini
b2_MLP34a.gini <-Gini(a, p)
b2_MLP34a.gini

#Brier score
set.seed(123); b2_model_MLP34a_brier <- train(formula, data=bene2_woe_train4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP34a_brier$results
b2_model_MLP34a_brier$resample
b2_pred_MLP34a_brier <- predict(b2_model_MLP34a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_MLP34a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_MLP34a_brier$X1)
b2_MLP34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_MLP34b_roc <- train(formula, data=bene2_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP34b_roc <- predict(b2_model_MLP34b_roc, bene2_woe_train4,type="prob")
b2_MLP34b.ROC <- roc(predictor=b2predb_MLP34b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_MLP34b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_MLP34b_gini <- train(formula, data=bene2_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP34b_gini$results
b2_model_MLP34b_gini$resample
b2_pred_MLP34b_gini<- predict(b2_model_MLP34b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_MLP34b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_MLP34b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP34b_gini$X1[b2_pred_MLP34b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_MLP34b_gini$X1<=0.4]))
b2_MLP34b.ngini <- normalizedGini(a, p)
b2_MLP34b.ngini
b2_MLP34b.gini <-Gini(a, p)
b2_MLP34b.gini

#Brier score
set.seed(123); b2_model_MLP34b_brier <- train(formula, data=bene2_woe_test4,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP34b_brier$results
b2_model_MLP34b_brier$resample
b2_pred_MLP34b_brier <- predict(b2_model_MLP34b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_MLP34b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_MLP34b_brier$X1)
b2_MLP34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP35a_roc <- train(formula, data=bene2_woe_train5, method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP35a_roc <- predict(b2_model_MLP35a_roc,bene2_woe_test5,type="prob")
b2_MLP35a.ROC <- roc(predictor=b2predb_MLP35a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_MLP35a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_MLP35a_gini <- train(formula, data=bene2_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP35a_gini$results
b2_model_MLP35a_gini$resample
b2_pred_MLP35a_gini<- predict(b2_model_MLP35a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_MLP35a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_MLP35a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP35a_gini$X1[b2_pred_MLP35a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_MLP35a_gini$X1<=0.4]))
b2_MLP35a.ngini <- normalizedGini(a, p)
b2_MLP35a.ngini
b2_MLP35a.gini <-Gini(a, p)
b2_MLP35a.gini

#Brier score
set.seed(123); b2_model_MLP35a_brier <- train(formula, data=bene2_woe_train5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP35a_brier$results
b2_model_MLP35a_brier$resample
b2_pred_MLP35a_brier <- predict(b2_model_MLP35a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_MLP35a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_MLP35a_brier$X1)
b2_MLP35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_MLP35b_roc <- train(formula, data=bene2_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP35b_roc <- predict(b2_model_MLP35b_roc, bene2_woe_train5,type="prob")
b2_MLP35b.ROC <- roc(predictor=b2predb_MLP35b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_MLP35b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_MLP35b_gini <- train(formula, data=bene2_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP35b_gini$results
b2_model_MLP35b_gini$resample
b2_pred_MLP35b_gini<- predict(b2_model_MLP35b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_MLP35b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_MLP35b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP35b_gini$X1[b2_pred_MLP35b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_MLP35b_gini$X1<=0.4]))
b2_MLP35b.ngini <- normalizedGini(a, p)
b2_MLP35b.ngini
b2_MLP35b.gini <-Gini(a, p)
b2_MLP35b.gini

#Brier score
set.seed(123); b2_model_MLP35b_brier <- train(formula, data=bene2_woe_test5,  method = MLP3, tuneGrid=MLP3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP35b_brier$results
b2_model_MLP35b_brier$resample
b2_pred_MLP35b_brier <- predict(b2_model_MLP35b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_MLP35b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_MLP35b_brier$X1)
b2_MLP35b.bs


##Results mlp
b2_results_MLP3_AUC <- cbind(b2_MLP31a.ROC$auc,b2_MLP31b.ROC$auc,b2_MLP32a.ROC$auc,b2_MLP32b.ROC$auc,b2_MLP33a.ROC$auc,b2_MLP33b.ROC$auc,b2_MLP34a.ROC$auc,
                             b2_MLP34b.ROC$auc,b2_MLP35a.ROC$auc,b2_MLP35b.ROC$auc)
b2_results_MLP3_bs <- cbind(b2_MLP31a.bs,b2_MLP31b.bs,b2_MLP32a.bs,b2_MLP32b.bs,b2_MLP33a.bs,b2_MLP33b.bs,b2_MLP34a.bs,b2_MLP34b.bs,b2_MLP35a.bs,b2_MLP35b.bs)
b2_results_MLP3_ngini <- cbind(b2_MLP31a.ngini,b2_MLP31b.ngini,b2_MLP32a.ngini,b2_MLP32b.ngini,b2_MLP33a.ngini,b2_MLP33b.ngini,b2_MLP34a.ngini,b2_MLP34b.ngini,
                               b2_MLP35a.ngini,b2_MLP35b.ngini)
b2_results_MLP3_gini <- cbind(b2_MLP31a.gini,b2_MLP31b.gini,b2_MLP32a.gini,b2_MLP32b.gini,b2_MLP33a.gini,b2_MLP33b.gini,b2_MLP34a.gini,b2_MLP34b.gini,
                              b2_MLP35a.gini,b2_MLP35b.gini)
mean(b2_results_MLP3_AUC)
mean(b2_results_MLP3_bs)
mean(b2_results_MLP3_ngini)
mean(b2_results_MLP3_gini)

##############################
###########MLP5###############
##############################

MLP5grid <- expand.grid(.size1=c(5,10,15), .size2=c(5,10,15), .size3=c(5,10,15), .size4=c(5,10,15), .size5=c(5,10,15), .dropout=c(0.0,0.25,0.5), .batch_size=batch, .lr=c(0.05,0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP51a_roc <- train(formula, data=bene2_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
MLP5grid <- expand.grid(.size1=c(15), .size2=c(15), .size3=c(10), .size4=c(10), .size5=c(5), .dropout=c(0.0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
###data 1, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP51a_roc <- train(formula, data=bene2_woe_train1, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP51a_roc <- predict(b2_model_MLP51a_roc,bene2_woe_test1,type="prob")
b2_MLP51a.ROC <- roc(predictor=b2predb_MLP51a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_MLP51a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_MLP51a_gini <- train(formula, data=bene2_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP51a_gini$results
b2_model_MLP51a_gini$resample
b2_pred_MLP51a_gini<- predict(b2_model_MLP51a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_MLP51a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_MLP51a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP51a_gini$X1[b2_pred_MLP51a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_MLP51a_gini$X1<=0.4]))
b2_MLP51a.ngini <- normalizedGini(a, p)
b2_MLP51a.ngini
b2_MLP51a.gini <-Gini(a, p)
b2_MLP51a.gini

#Brier score
set.seed(123); b2_model_MLP51a_brier <- train(formula, data=bene2_woe_train1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP51a_brier$results
b2_model_MLP51a_brier$resample
b2_pred_MLP51a_brier <- predict(b2_model_MLP51a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_MLP51a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_MLP51a_brier$X1)
b2_MLP51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_MLP51b_roc <- train(formula, data=bene2_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP51b_roc <- predict(b2_model_MLP51b_roc, bene2_woe_train1,type="prob")
b2_MLP51b.ROC <- roc(predictor=b2predb_MLP51b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_MLP51b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_MLP51b_gini <- train(formula, data=bene2_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP51b_gini$results
b2_model_MLP51b_gini$resample
b2_pred_MLP51b_gini<- predict(b2_model_MLP51b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_MLP51b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_MLP51b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP51b_gini$X1[b2_pred_MLP51b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_MLP51b_gini$X1<=0.4]))
b2_MLP51b.ngini <- normalizedGini(a, p)
b2_MLP51b.ngini
b2_MLP51b.gini <-Gini(a, p)
b2_MLP51b.gini

#Brier score
set.seed(123); b2_model_MLP51b_brier <- train(formula, data=bene2_woe_test1,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP51b_brier$results
b2_model_MLP51b_brier$resample
b2_pred_MLP51b_brier <- predict(b2_model_MLP51b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_MLP51b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_MLP51b_brier$X1)
b2_MLP51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP52a_roc <- train(formula, data=bene2_woe_train2, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP52a_roc <- predict(b2_model_MLP52a_roc,bene2_woe_test2,type="prob")
b2_MLP52a.ROC <- roc(predictor=b2predb_MLP52a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_MLP52a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_MLP52a_gini <- train(formula, data=bene2_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP52a_gini$results
b2_model_MLP52a_gini$resample
b2_pred_MLP52a_gini<- predict(b2_model_MLP52a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_MLP52a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_MLP52a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP52a_gini$X1[b2_pred_MLP52a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_MLP52a_gini$X1<=0.4]))
b2_MLP52a.ngini <- normalizedGini(a, p)
b2_MLP52a.ngini
b2_MLP52a.gini <-Gini(a, p)
b2_MLP52a.gini

#Brier score
set.seed(123); b2_model_MLP52a_brier <- train(formula, data=bene2_woe_train2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP52a_brier$results
b2_model_MLP52a_brier$resample
b2_pred_MLP52a_brier <- predict(b2_model_MLP52a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_MLP52a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_MLP52a_brier$X1)
b2_MLP52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_MLP52b_roc <- train(formula, data=bene2_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP52b_roc <- predict(b2_model_MLP52b_roc, bene2_woe_train2,type="prob")
b2_MLP52b.ROC <- roc(predictor=b2predb_MLP52b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_MLP52b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_MLP52b_gini <- train(formula, data=bene2_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP52b_gini$results
b2_model_MLP52b_gini$resample
b2_pred_MLP52b_gini<- predict(b2_model_MLP52b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_MLP52b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_MLP52b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP52b_gini$X1[b2_pred_MLP52b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_MLP52b_gini$X1<=0.4]))
b2_MLP52b.ngini <- normalizedGini(a, p)
b2_MLP52b.ngini
b2_MLP52b.gini <-Gini(a, p)
b2_MLP52b.gini

#Brier score
set.seed(123); b2_model_MLP52b_brier <- train(formula, data=bene2_woe_test2,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP52b_brier$results
b2_model_MLP52b_brier$resample
b2_pred_MLP52b_brier <- predict(b2_model_MLP52b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_MLP52b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_MLP52b_brier$X1)
b2_MLP52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP53a_roc <- train(formula, data=bene2_woe_train3, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP53a_roc <- predict(b2_model_MLP53a_roc,bene2_woe_test3,type="prob")
b2_MLP53a.ROC <- roc(predictor=b2predb_MLP53a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_MLP53a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_MLP53a_gini <- train(formula, data=bene2_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP53a_gini$results
b2_model_MLP53a_gini$resample
b2_pred_MLP53a_gini<- predict(b2_model_MLP53a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_MLP53a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_MLP53a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP53a_gini$X1[b2_pred_MLP53a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_MLP53a_gini$X1<=0.4]))
b2_MLP53a.ngini <- normalizedGini(a, p)
b2_MLP53a.ngini
b2_MLP53a.gini <-Gini(a, p)
b2_MLP53a.gini

#Brier score
set.seed(123); b2_model_MLP53a_brier <- train(formula, data=bene2_woe_train3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP53a_brier$results
b2_model_MLP53a_brier$resample
b2_pred_MLP53a_brier <- predict(b2_model_MLP53a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_MLP53a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_MLP53a_brier$X1)
b2_MLP53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_MLP53b_roc <- train(formula, data=bene2_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP53b_roc <- predict(b2_model_MLP53b_roc, bene2_woe_train3,type="prob")
b2_MLP53b.ROC <- roc(predictor=b2predb_MLP53b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_MLP53b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_MLP53b_gini <- train(formula, data=bene2_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP53b_gini$results
b2_model_MLP53b_gini$resample
b2_pred_MLP53b_gini<- predict(b2_model_MLP53b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_MLP53b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_MLP53b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP53b_gini$X1[b2_pred_MLP53b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_MLP53b_gini$X1<=0.4]))
b2_MLP53b.ngini <- normalizedGini(a, p)
b2_MLP53b.ngini
b2_MLP53b.gini <-Gini(a, p)
b2_MLP53b.gini

#Brier score
set.seed(123); b2_model_MLP53b_brier <- train(formula, data=bene2_woe_test3,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP53b_brier$results
b2_model_MLP53b_brier$resample
b2_pred_MLP53b_brier <- predict(b2_model_MLP53b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_MLP53b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_MLP53b_brier$X1)
b2_MLP53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP54a_roc <- train(formula, data=bene2_woe_train4, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP54a_roc <- predict(b2_model_MLP54a_roc,bene2_woe_test4,type="prob")
b2_MLP54a.ROC <- roc(predictor=b2predb_MLP54a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_MLP54a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_MLP54a_gini <- train(formula, data=bene2_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP54a_gini$results
b2_model_MLP54a_gini$resample
b2_pred_MLP54a_gini<- predict(b2_model_MLP54a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_MLP54a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_MLP54a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP54a_gini$X1[b2_pred_MLP54a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_MLP54a_gini$X1<=0.4]))
b2_MLP54a.ngini <- normalizedGini(a, p)
b2_MLP54a.ngini
b2_MLP54a.gini <-Gini(a, p)
b2_MLP54a.gini

#Brier score
set.seed(123); b2_model_MLP54a_brier <- train(formula, data=bene2_woe_train4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP54a_brier$results
b2_model_MLP54a_brier$resample
b2_pred_MLP54a_brier <- predict(b2_model_MLP54a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_MLP54a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_MLP54a_brier$X1)
b2_MLP54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_MLP54b_roc <- train(formula, data=bene2_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP54b_roc <- predict(b2_model_MLP54b_roc, bene2_woe_train4,type="prob")
b2_MLP54b.ROC <- roc(predictor=b2predb_MLP54b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_MLP54b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_MLP54b_gini <- train(formula, data=bene2_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP54b_gini$results
b2_model_MLP54b_gini$resample
b2_pred_MLP54b_gini<- predict(b2_model_MLP54b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_MLP54b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_MLP54b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP54b_gini$X1[b2_pred_MLP54b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_MLP54b_gini$X1<=0.4]))
b2_MLP54b.ngini <- normalizedGini(a, p)
b2_MLP54b.ngini
b2_MLP54b.gini <-Gini(a, p)
b2_MLP54b.gini

#Brier score
set.seed(123); b2_model_MLP54b_brier <- train(formula, data=bene2_woe_test4,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP54b_brier$results
b2_model_MLP54b_brier$resample
b2_pred_MLP54b_brier <- predict(b2_model_MLP54b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_MLP54b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_MLP54b_brier$X1)
b2_MLP54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_MLP55a_roc <- train(formula, data=bene2_woe_train5, method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP55a_roc <- predict(b2_model_MLP55a_roc,bene2_woe_test5,type="prob")
b2_MLP55a.ROC <- roc(predictor=b2predb_MLP55a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_MLP55a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_MLP55a_gini <- train(formula, data=bene2_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP55a_gini$results
b2_model_MLP55a_gini$resample
b2_pred_MLP55a_gini<- predict(b2_model_MLP55a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_MLP55a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_MLP55a_gini$X1)
#b <= 0.4
p <- b2_pred_MLP55a_gini$X1[b2_pred_MLP55a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_MLP55a_gini$X1<=0.4]))
b2_MLP55a.ngini <- normalizedGini(a, p)
b2_MLP55a.ngini
b2_MLP55a.gini <-Gini(a, p)
b2_MLP55a.gini

#Brier score
set.seed(123); b2_model_MLP55a_brier <- train(formula, data=bene2_woe_train5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP55a_brier$results
b2_model_MLP55a_brier$resample
b2_pred_MLP55a_brier <- predict(b2_model_MLP55a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_MLP55a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_MLP55a_brier$X1)
b2_MLP55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_MLP55b_roc <- train(formula, data=bene2_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_MLP55b_roc <- predict(b2_model_MLP55b_roc, bene2_woe_train5,type="prob")
b2_MLP55b.ROC <- roc(predictor=b2predb_MLP55b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_MLP55b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_MLP55b_gini <- train(formula, data=bene2_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_MLP55b_gini$results
b2_model_MLP55b_gini$resample
b2_pred_MLP55b_gini<- predict(b2_model_MLP55b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_MLP55b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_MLP55b_gini$X1)
#b <= 0.4
p <- b2_pred_MLP55b_gini$X1[b2_pred_MLP55b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_MLP55b_gini$X1<=0.4]))
b2_MLP55b.ngini <- normalizedGini(a, p)
b2_MLP55b.ngini
b2_MLP55b.gini <-Gini(a, p)
b2_MLP55b.gini

#Brier score
set.seed(123); b2_model_MLP55b_brier <- train(formula, data=bene2_woe_test5,  method = MLP5, tuneGrid=MLP5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_MLP55b_brier$results
b2_model_MLP55b_brier$resample
b2_pred_MLP55b_brier <- predict(b2_model_MLP55b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_MLP55b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_MLP55b_brier$X1)
b2_MLP55b.bs


##Restults!
b2_results_MLP5_AUC <- cbind(b2_MLP51a.ROC$auc,b2_MLP51b.ROC$auc,b2_MLP52a.ROC$auc,b2_MLP52b.ROC$auc,b2_MLP53a.ROC$auc,b2_MLP53b.ROC$auc,b2_MLP54a.ROC$auc,
                             b2_MLP54b.ROC$auc,b2_MLP55a.ROC$auc,b2_MLP55b.ROC$auc)
b2_results_MLP5_bs <- cbind(b2_MLP51a.bs,b2_MLP51b.bs,b2_MLP52a.bs,b2_MLP52b.bs,b2_MLP53a.bs,b2_MLP53b.bs,b2_MLP54a.bs,b2_MLP54b.bs,b2_MLP55a.bs,b2_MLP55b.bs)
b2_results_MLP5_ngini <- cbind(b2_MLP51a.ngini,b2_MLP51b.ngini,b2_MLP52a.ngini,b2_MLP52b.ngini,b2_MLP53a.ngini,b2_MLP53b.ngini,b2_MLP54a.ngini,b2_MLP54b.ngini,
                               b2_MLP55a.ngini,b2_MLP55b.ngini)
b2_results_MLP5_gini <- cbind(b2_MLP51a.gini,b2_MLP51b.gini,b2_MLP52a.gini,b2_MLP52b.gini,b2_MLP53a.gini,b2_MLP53b.gini,b2_MLP54a.gini,b2_MLP54b.gini,
                              b2_MLP55a.gini,b2_MLP55b.gini)
mean(b2_results_MLP5_AUC)
mean(b2_results_MLP5_bs)
mean(b2_results_MLP5_ngini)
mean(b2_results_MLP5_gini)

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
set.seed(123); b2_model_DBN11a_roc <- train(formula, data=bene2_woe_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN1grid <- expand.grid(.layer1=c(5), .hidden_dropout=c(0), .visible_dropout=c(0.25), .lr=c(1.0))

###data 1, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN11a_roc <- train(formula, data=bene2_woe_train1, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN11a_roc <- predict(b2_model_DBN11a_roc,bene2_woe_test1,type="prob")
b2_DBN11a.ROC <- roc(predictor=b2predb_DBN11a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_DBN11a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_DBN11a_gini <- train(formula, data=bene2_woe_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN11a_gini$results
b2_model_DBN11a_gini$resample
b2_pred_DBN11a_gini<- predict(b2_model_DBN11a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_DBN11a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_DBN11a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN11a_gini$X1[b2_pred_DBN11a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_DBN11a_gini$X1<=0.4]))
b2_DBN11a.ngini <- normalizedGini(a, p)
b2_DBN11a.ngini
b2_DBN11a.gini <-Gini(a, p)
b2_DBN11a.gini

#Brier score
set.seed(123); b2_model_DBN11a_brier <- train(formula, data=bene2_woe_train1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN11a_brier$results
b2_model_DBN11a_brier$resample
b2_pred_DBN11a_brier <- predict(b2_model_DBN11a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_DBN11a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_DBN11a_brier$X1)
b2_DBN11a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_DBN11b_roc <- train(formula, data=bene2_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN11b_roc <- predict(b2_model_DBN11b_roc, bene2_woe_train1,type="prob")
b2_DBN11b.ROC <- roc(predictor=b2predb_DBN11b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_DBN11b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_DBN11b_gini <- train(formula, data=bene2_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN11b_gini$results
b2_model_DBN11b_gini$resample
b2_pred_DBN11b_gini<- predict(b2_model_DBN11b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_DBN11b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_DBN11b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN11b_gini$X1[b2_pred_DBN11b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_DBN11b_gini$X1<=0.4]))
b2_DBN11b.ngini <- normalizedGini(a, p)
b2_DBN11b.ngini
b2_DBN11b.gini <-Gini(a, p)
b2_DBN11b.gini

#Brier score
set.seed(123); b2_model_DBN11b_brier <- train(formula, data=bene2_woe_test1,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN11b_brier$results
b2_model_DBN11b_brier$resample
b2_pred_DBN11b_brier <- predict(b2_model_DBN11b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_DBN11b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_DBN11b_brier$X1)
b2_DBN11b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN12a_roc <- train(formula, data=bene2_woe_train2, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN12a_roc <- predict(b2_model_DBN12a_roc,bene2_woe_test2,type="prob")
b2_DBN12a.ROC <- roc(predictor=b2predb_DBN12a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_DBN12a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_DBN12a_gini <- train(formula, data=bene2_woe_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN12a_gini$results
b2_model_DBN12a_gini$resample
b2_pred_DBN12a_gini<- predict(b2_model_DBN12a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_DBN12a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_DBN12a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN12a_gini$X1[b2_pred_DBN12a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_DBN12a_gini$X1<=0.4]))
b2_DBN12a.ngini <- normalizedGini(a, p)
b2_DBN12a.ngini
b2_DBN12a.gini <-Gini(a, p)
b2_DBN12a.gini

#Brier score
set.seed(123); b2_model_DBN12a_brier <- train(formula, data=bene2_woe_train2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN12a_brier$results
b2_model_DBN12a_brier$resample
b2_pred_DBN12a_brier <- predict(b2_model_DBN12a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_DBN12a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_DBN12a_brier$X1)
b2_DBN12a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_DBN12b_roc <- train(formula, data=bene2_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN12b_roc <- predict(b2_model_DBN12b_roc, bene2_woe_train2,type="prob")
b2_DBN12b.ROC <- roc(predictor=b2predb_DBN12b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_DBN12b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_DBN12b_gini <- train(formula, data=bene2_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN12b_gini$results
b2_model_DBN12b_gini$resample
b2_pred_DBN12b_gini<- predict(b2_model_DBN12b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_DBN12b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_DBN12b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN12b_gini$X1[b2_pred_DBN12b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_DBN12b_gini$X1<=0.4]))
b2_DBN12b.ngini <- normalizedGini(a, p)
b2_DBN12b.ngini
b2_DBN12b.gini <-Gini(a, p)
b2_DBN12b.gini

#Brier score
set.seed(123); b2_model_DBN12b_brier <- train(formula, data=bene2_woe_test2,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN12b_brier$results
b2_model_DBN12b_brier$resample
b2_pred_DBN12b_brier <- predict(b2_model_DBN12b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_DBN12b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_DBN12b_brier$X1)
b2_DBN12b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN13a_roc <- train(formula, data=bene2_woe_train3, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN13a_roc <- predict(b2_model_DBN13a_roc,bene2_woe_test3,type="prob")
b2_DBN13a.ROC <- roc(predictor=b2predb_DBN13a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_DBN13a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_DBN13a_gini <- train(formula, data=bene2_woe_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN13a_gini$results
b2_model_DBN13a_gini$resample
b2_pred_DBN13a_gini<- predict(b2_model_DBN13a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_DBN13a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_DBN13a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN13a_gini$X1[b2_pred_DBN13a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_DBN13a_gini$X1<=0.4]))
b2_DBN13a.ngini <- normalizedGini(a, p)
b2_DBN13a.ngini
b2_DBN13a.gini <-Gini(a, p)
b2_DBN13a.gini

#Brier score
set.seed(123); b2_model_DBN13a_brier <- train(formula, data=bene2_woe_train3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN13a_brier$results
b2_model_DBN13a_brier$resample
b2_pred_DBN13a_brier <- predict(b2_model_DBN13a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_DBN13a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_DBN13a_brier$X1)
b2_DBN13a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_DBN13b_roc <- train(formula, data=bene2_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN13b_roc <- predict(b2_model_DBN13b_roc, bene2_woe_train3,type="prob")
b2_DBN13b.ROC <- roc(predictor=b2predb_DBN13b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_DBN13b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_DBN13b_gini <- train(formula, data=bene2_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN13b_gini$results
b2_model_DBN13b_gini$resample
b2_pred_DBN13b_gini<- predict(b2_model_DBN13b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_DBN13b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_DBN13b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN13b_gini$X1[b2_pred_DBN13b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_DBN13b_gini$X1<=0.4]))
b2_DBN13b.ngini <- normalizedGini(a, p)
b2_DBN13b.ngini
b2_DBN13b.gini <-Gini(a, p)
b2_DBN13b.gini

#Brier score
set.seed(123); b2_model_DBN13b_brier <- train(formula, data=bene2_woe_test3,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN13b_brier$results
b2_model_DBN13b_brier$resample
b2_pred_DBN13b_brier <- predict(b2_model_DBN13b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_DBN13b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_DBN13b_brier$X1)
b2_DBN13b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN14a_roc <- train(formula, data=bene2_woe_train4, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN14a_roc <- predict(b2_model_DBN14a_roc,bene2_woe_test4,type="prob")
b2_DBN14a.ROC <- roc(predictor=b2predb_DBN14a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_DBN14a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_DBN14a_gini <- train(formula, data=bene2_woe_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN14a_gini$results
b2_model_DBN14a_gini$resample
b2_pred_DBN14a_gini<- predict(b2_model_DBN14a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_DBN14a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_DBN14a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN14a_gini$X1[b2_pred_DBN14a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_DBN14a_gini$X1<=0.4]))
b2_DBN14a.ngini <- normalizedGini(a, p)
b2_DBN14a.ngini
b2_DBN14a.gini <-Gini(a, p)
b2_DBN14a.gini

#Brier score
set.seed(123); b2_model_DBN14a_brier <- train(formula, data=bene2_woe_train4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN14a_brier$results
b2_model_DBN14a_brier$resample
b2_pred_DBN14a_brier <- predict(b2_model_DBN14a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_DBN14a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_DBN14a_brier$X1)
b2_DBN14a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_DBN14b_roc <- train(formula, data=bene2_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN14b_roc <- predict(b2_model_DBN14b_roc, bene2_woe_train4,type="prob")
b2_DBN14b.ROC <- roc(predictor=b2predb_DBN14b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_DBN14b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_DBN14b_gini <- train(formula, data=bene2_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN14b_gini$results
b2_model_DBN14b_gini$resample
b2_pred_DBN14b_gini<- predict(b2_model_DBN14b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_DBN14b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_DBN14b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN14b_gini$X1[b2_pred_DBN14b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_DBN14b_gini$X1<=0.4]))
b2_DBN14b.ngini <- normalizedGini(a, p)
b2_DBN14b.ngini
b2_DBN14b.gini <-Gini(a, p)
b2_DBN14b.gini

#Brier score
set.seed(123); b2_model_DBN14b_brier <- train(formula, data=bene2_woe_test4,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN14b_brier$results
b2_model_DBN14b_brier$resample
b2_pred_DBN14b_brier <- predict(b2_model_DBN14b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_DBN14b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_DBN14b_brier$X1)
b2_DBN14b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN15a_roc <- train(formula, data=bene2_woe_train5, method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN15a_roc <- predict(b2_model_DBN15a_roc,bene2_woe_test5,type="prob")
b2_DBN15a.ROC <- roc(predictor=b2predb_DBN15a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_DBN15a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_DBN15a_gini <- train(formula, data=bene2_woe_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN15a_gini$results
b2_model_DBN15a_gini$resample
b2_pred_DBN15a_gini<- predict(b2_model_DBN15a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_DBN15a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_DBN15a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN15a_gini$X1[b2_pred_DBN15a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_DBN15a_gini$X1<=0.4]))
b2_DBN15a.ngini <- normalizedGini(a, p)
b2_DBN15a.ngini
b2_DBN15a.gini <-Gini(a, p)
b2_DBN15a.gini

#Brier score
set.seed(123); b2_model_DBN15a_brier <- train(formula, data=bene2_woe_train5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN15a_brier$results
b2_model_DBN15a_brier$resample
b2_pred_DBN15a_brier <- predict(b2_model_DBN15a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_DBN15a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_DBN15a_brier$X1)
b2_DBN15a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_DBN15b_roc <- train(formula, data=bene2_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN15b_roc <- predict(b2_model_DBN15b_roc, bene2_woe_train5,type="prob")
b2_DBN15b.ROC <- roc(predictor=b2predb_DBN15b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_DBN15b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_DBN15b_gini <- train(formula, data=bene2_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN15b_gini$results
b2_model_DBN15b_gini$resample
b2_pred_DBN15b_gini<- predict(b2_model_DBN15b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_DBN15b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_DBN15b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN15b_gini$X1[b2_pred_DBN15b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_DBN15b_gini$X1<=0.4]))
b2_DBN15b.ngini <- normalizedGini(a, p)
b2_DBN15b.ngini
b2_DBN15b.gini <-Gini(a, p)
b2_DBN15b.gini

#Brier score
set.seed(123); b2_model_DBN15b_brier <- train(formula, data=bene2_woe_test5,  method = DBN1, tuneGrid=DBN1grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN15b_brier$results
b2_model_DBN15b_brier$resample
b2_pred_DBN15b_brier <- predict(b2_model_DBN15b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_DBN15b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_DBN15b_brier$X1)
b2_DBN15b.bs

##Restults RF!
b2_results_DBN1_AUC <- cbind(b2_DBN11a.ROC$auc,b2_DBN11b.ROC$auc,b2_DBN12a.ROC$auc,b2_DBN12b.ROC$auc,b2_DBN13a.ROC$auc,b2_DBN13b.ROC$auc,b2_DBN14a.ROC$auc,
                             b2_DBN14b.ROC$auc,b2_DBN15a.ROC$auc,b2_DBN15b.ROC$auc)
b2_results_DBN1_bs <- cbind(b2_DBN11a.bs,b2_DBN11b.bs,b2_DBN12a.bs,b2_DBN12b.bs,b2_DBN13a.bs,b2_DBN13b.bs,b2_DBN14a.bs,b2_DBN14b.bs,b2_DBN15a.bs,b2_DBN15b.bs)
b2_results_DBN1_ngini <- cbind(b2_DBN11a.ngini,b2_DBN11b.ngini,b2_DBN12a.ngini,b2_DBN12b.ngini,b2_DBN13a.ngini,b2_DBN13b.ngini,b2_DBN14a.ngini,b2_DBN14b.ngini,
                               b2_DBN15a.ngini,b2_DBN15b.ngini)
b2_results_DBN1_gini <- cbind(b2_DBN11a.gini,b2_DBN11b.gini,b2_DBN12a.gini,b2_DBN12b.gini,b2_DBN13a.gini,b2_DBN13b.gini,b2_DBN14a.gini,b2_DBN14b.gini,
                              b2_DBN15a.gini,b2_DBN15b.gini)
mean(b2_results_DBN1_AUC)
mean(b2_results_DBN1_bs)
mean(b2_results_DBN1_ngini)
mean(b2_results_DBN1_gini)

##############################
###########DBN3###############
##############################
DBN3_grid <- expand.grid(.layer1=c(5,10,15,20), .layer2=c(5,10,15,20), .layer3=c(5,10,15,20), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0))
set.seed(123); b2_model_DBN31a_roc <- train(formula, data=bene2_woe_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN3grid <- expand.grid(.layer1=c(10), .layer2=c(5), .layer3=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0), .lr=c(2))
###data 1, train-test
#ROC curve 
set.seed(123); b2_model_DBN31a_roc <- train(formula, data=bene2_woe_train1, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN31a_roc <- predict(b2_model_DBN31a_roc,bene2_woe_test1,type="prob")
b2_DBN31a.ROC <- roc(predictor=b2predb_DBN31a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_DBN31a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_DBN31a_gini <- train(formula, data=bene2_woe_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN31a_gini$results
b2_model_DBN31a_gini$resample
b2_pred_DBN31a_gini<- predict(b2_model_DBN31a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_DBN31a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_DBN31a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN31a_gini$X1[b2_pred_DBN31a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_DBN31a_gini$X1<=0.4]))
b2_DBN31a.ngini <- normalizedGini(a, p)
b2_DBN31a.ngini
b2_DBN31a.gini <-Gini(a, p)
b2_DBN31a.gini

#Brier score
set.seed(123); b2_model_DBN31a_brier <- train(formula, data=bene2_woe_train1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN31a_brier$results
b2_model_DBN31a_brier$resample
b2_pred_DBN31a_brier <- predict(b2_model_DBN31a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_DBN31a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_DBN31a_brier$X1)
b2_DBN31a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_DBN31b_roc <- train(formula, data=bene2_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN31b_roc <- predict(b2_model_DBN31b_roc, bene2_woe_train1,type="prob")
b2_DBN31b.ROC <- roc(predictor=b2predb_DBN31b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_DBN31b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_DBN31b_gini <- train(formula, data=bene2_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN31b_gini$results
b2_model_DBN31b_gini$resample
b2_pred_DBN31b_gini<- predict(b2_model_DBN31b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_DBN31b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_DBN31b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN31b_gini$X1[b2_pred_DBN31b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_DBN31b_gini$X1<=0.4]))
b2_DBN31b.ngini <- normalizedGini(a, p)
b2_DBN31b.ngini
b2_DBN31b.gini <-Gini(a, p)
b2_DBN31b.gini

#Brier score
set.seed(123); b2_model_DBN31b_brier <- train(formula, data=bene2_woe_test1,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN31b_brier$results
b2_model_DBN31b_brier$resample
b2_pred_DBN31b_brier <- predict(b2_model_DBN31b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_DBN31b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_DBN31b_brier$X1)
b2_DBN31b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN32a_roc <- train(formula, data=bene2_woe_train2, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN32a_roc <- predict(b2_model_DBN32a_roc,bene2_woe_test2,type="prob")
b2_DBN32a.ROC <- roc(predictor=b2predb_DBN32a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_DBN32a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_DBN32a_gini <- train(formula, data=bene2_woe_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN32a_gini$results
b2_model_DBN32a_gini$resample
b2_pred_DBN32a_gini<- predict(b2_model_DBN32a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_DBN32a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_DBN32a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN32a_gini$X1[b2_pred_DBN32a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_DBN32a_gini$X1<=0.4]))
b2_DBN32a.ngini <- normalizedGini(a, p)
b2_DBN32a.ngini
b2_DBN32a.gini <-Gini(a, p)
b2_DBN32a.gini

#Brier score
set.seed(123); b2_model_DBN32a_brier <- train(formula, data=bene2_woe_train2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN32a_brier$results
b2_model_DBN32a_brier$resample
b2_pred_DBN32a_brier <- predict(b2_model_DBN32a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_DBN32a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_DBN32a_brier$X1)
b2_DBN32a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_DBN32b_roc <- train(formula, data=bene2_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN32b_roc <- predict(b2_model_DBN32b_roc, bene2_woe_train2,type="prob")
b2_DBN32b.ROC <- roc(predictor=b2predb_DBN32b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_DBN32b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_DBN32b_gini <- train(formula, data=bene2_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN32b_gini$results
b2_model_DBN32b_gini$resample
b2_pred_DBN32b_gini<- predict(b2_model_DBN32b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_DBN32b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_DBN32b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN32b_gini$X1[b2_pred_DBN32b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_DBN32b_gini$X1<=0.4]))
b2_DBN32b.ngini <- normalizedGini(a, p)
b2_DBN32b.ngini
b2_DBN32b.gini <-Gini(a, p)
b2_DBN32b.gini

#Brier score
set.seed(123); b2_model_DBN32b_brier <- train(formula, data=bene2_woe_test2,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN32b_brier$results
b2_model_DBN32b_brier$resample
b2_pred_DBN32b_brier <- predict(b2_model_DBN32b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_DBN32b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_DBN32b_brier$X1)
b2_DBN32b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN33a_roc <- train(formula, data=bene2_woe_train3, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN33a_roc <- predict(b2_model_DBN33a_roc,bene2_woe_test3,type="prob")
b2_DBN33a.ROC <- roc(predictor=b2predb_DBN33a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_DBN33a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_DBN33a_gini <- train(formula, data=bene2_woe_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN33a_gini$results
b2_model_DBN33a_gini$resample
b2_pred_DBN33a_gini<- predict(b2_model_DBN33a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_DBN33a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_DBN33a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN33a_gini$X1[b2_pred_DBN33a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_DBN33a_gini$X1<=0.4]))
b2_DBN33a.ngini <- normalizedGini(a, p)
b2_DBN33a.ngini
b2_DBN33a.gini <-Gini(a, p)
b2_DBN33a.gini

#Brier score
set.seed(123); b2_model_DBN33a_brier <- train(formula, data=bene2_woe_train3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN33a_brier$results
b2_model_DBN33a_brier$resample
b2_pred_DBN33a_brier <- predict(b2_model_DBN33a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_DBN33a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_DBN33a_brier$X1)
b2_DBN33a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_DBN33b_roc <- train(formula, data=bene2_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN33b_roc <- predict(b2_model_DBN33b_roc, bene2_woe_train3,type="prob")
b2_DBN33b.ROC <- roc(predictor=b2predb_DBN33b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_DBN33b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_DBN33b_gini <- train(formula, data=bene2_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN33b_gini$results
b2_model_DBN33b_gini$resample
b2_pred_DBN33b_gini<- predict(b2_model_DBN33b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_DBN33b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_DBN33b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN33b_gini$X1[b2_pred_DBN33b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_DBN33b_gini$X1<=0.4]))
b2_DBN33b.ngini <- normalizedGini(a, p)
b2_DBN33b.ngini
b2_DBN33b.gini <-Gini(a, p)
b2_DBN33b.gini

#Brier score
set.seed(123); b2_model_DBN33b_brier <- train(formula, data=bene2_woe_test3,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN33b_brier$results
b2_model_DBN33b_brier$resample
b2_pred_DBN33b_brier <- predict(b2_model_DBN33b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_DBN33b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_DBN33b_brier$X1)
b2_DBN33b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN34a_roc <- train(formula, data=bene2_woe_train4, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN34a_roc <- predict(b2_model_DBN34a_roc,bene2_woe_test4,type="prob")
b2_DBN34a.ROC <- roc(predictor=b2predb_DBN34a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_DBN34a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_DBN34a_gini <- train(formula, data=bene2_woe_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN34a_gini$results
b2_model_DBN34a_gini$resample
b2_pred_DBN34a_gini<- predict(b2_model_DBN34a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_DBN34a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_DBN34a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN34a_gini$X1[b2_pred_DBN34a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_DBN34a_gini$X1<=0.4]))
b2_DBN34a.ngini <- normalizedGini(a, p)
b2_DBN34a.ngini
b2_DBN34a.gini <-Gini(a, p)
b2_DBN34a.gini

#Brier score
set.seed(123); b2_model_DBN34a_brier <- train(formula, data=bene2_woe_train4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN34a_brier$results
b2_model_DBN34a_brier$resample
b2_pred_DBN34a_brier <- predict(b2_model_DBN34a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_DBN34a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_DBN34a_brier$X1)
b2_DBN34a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_DBN34b_roc <- train(formula, data=bene2_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN34b_roc <- predict(b2_model_DBN34b_roc, bene2_woe_train4,type="prob")
b2_DBN34b.ROC <- roc(predictor=b2predb_DBN34b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_DBN34b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_DBN34b_gini <- train(formula, data=bene2_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN34b_gini$results
b2_model_DBN34b_gini$resample
b2_pred_DBN34b_gini<- predict(b2_model_DBN34b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_DBN34b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_DBN34b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN34b_gini$X1[b2_pred_DBN34b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_DBN34b_gini$X1<=0.4]))
b2_DBN34b.ngini <- normalizedGini(a, p)
b2_DBN34b.ngini
b2_DBN34b.gini <-Gini(a, p)
b2_DBN34b.gini

#Brier score
set.seed(123); b2_model_DBN34b_brier <- train(formula, data=bene2_woe_test4,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN34b_brier$results
b2_model_DBN34b_brier$resample
b2_pred_DBN34b_brier <- predict(b2_model_DBN34b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_DBN34b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_DBN34b_brier$X1)
b2_DBN34b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN35a_roc <- train(formula, data=bene2_woe_train5, method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN35a_roc <- predict(b2_model_DBN35a_roc,bene2_woe_test5,type="prob")
b2_DBN35a.ROC <- roc(predictor=b2predb_DBN35a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_DBN35a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_DBN35a_gini <- train(formula, data=bene2_woe_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN35a_gini$results
b2_model_DBN35a_gini$resample
b2_pred_DBN35a_gini<- predict(b2_model_DBN35a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_DBN35a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_DBN35a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN35a_gini$X1[b2_pred_DBN35a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_DBN35a_gini$X1<=0.4]))
b2_DBN35a.ngini <- normalizedGini(a, p)
b2_DBN35a.ngini
b2_DBN35a.gini <-Gini(a, p)
b2_DBN35a.gini

#Brier score
set.seed(123); b2_model_DBN35a_brier <- train(formula, data=bene2_woe_train5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN35a_brier$results
b2_model_DBN35a_brier$resample
b2_pred_DBN35a_brier <- predict(b2_model_DBN35a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_DBN35a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_DBN35a_brier$X1)
b2_DBN35a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_DBN35b_roc <- train(formula, data=bene2_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN35b_roc <- predict(b2_model_DBN35b_roc, bene2_woe_train5,type="prob")
b2_DBN35b.ROC <- roc(predictor=b2predb_DBN35b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_DBN35b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_DBN35b_gini <- train(formula, data=bene2_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN35b_gini$results
b2_model_DBN35b_gini$resample
b2_pred_DBN35b_gini<- predict(b2_model_DBN35b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_DBN35b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_DBN35b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN35b_gini$X1[b2_pred_DBN35b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_DBN35b_gini$X1<=0.4]))
b2_DBN35b.ngini <- normalizedGini(a, p)
b2_DBN35b.ngini
b2_DBN35b.gini <-Gini(a, p)
b2_DBN35b.gini

#Brier score
set.seed(123); b2_model_DBN35b_brier <- train(formula, data=bene2_woe_test5,  method = DBN3, tuneGrid=DBN3grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN35b_brier$results
b2_model_DBN35b_brier$resample
b2_pred_DBN35b_brier <- predict(b2_model_DBN35b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_DBN35b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_DBN35b_brier$X1)
b2_DBN35b.bs

##Restults RF!
b2_results_DBN3_AUC <- cbind(b2_DBN31a.ROC$auc,b2_DBN31b.ROC$auc,b2_DBN32a.ROC$auc,b2_DBN32b.ROC$auc,b2_DBN33a.ROC$auc,b2_DBN33b.ROC$auc,b2_DBN34a.ROC$auc,
                             b2_DBN34b.ROC$auc,b2_DBN35a.ROC$auc,b2_DBN35b.ROC$auc)
b2_results_DBN3_bs <- cbind(b2_DBN31a.bs,b2_DBN31b.bs,b2_DBN32a.bs,b2_DBN32b.bs,b2_DBN33a.bs,b2_DBN33b.bs,b2_DBN34a.bs,b2_DBN34b.bs,b2_DBN35a.bs,b2_DBN35b.bs)
b2_results_DBN3_ngini <- cbind(b2_DBN31a.ngini,b2_DBN31b.ngini,b2_DBN32a.ngini,b2_DBN32b.ngini,b2_DBN33a.ngini,b2_DBN33b.ngini,b2_DBN34a.ngini,b2_DBN34b.ngini,
                               b2_DBN35a.ngini,b2_DBN35b.ngini)
b2_results_DBN3_gini <- cbind(b2_DBN31a.gini,b2_DBN31b.gini,b2_DBN32a.gini,b2_DBN32b.gini,b2_DBN33a.gini,b2_DBN33b.gini,b2_DBN34a.gini,b2_DBN34b.gini,
                              b2_DBN35a.gini,b2_DBN35b.gini)
mean(b2_results_DBN3_AUC)
mean(b2_results_DBN3_bs)
mean(b2_results_DBN3_ngini)
mean(b2_results_DBN3_gini)


##############################
###########DBN5###############
##############################
DBN5_grid <- expand.grid(.layer1=c(5,10,15), .layer2=c(5,10,15), .layer3=c(5,10,15),.layer4=c(5,10,15),.layer5=c(5,10,15), .hidden_dropout=c(0,0.25,0.5), .visible_dropout=c(0,0.25,0.5), .lr=c(2.0,1.5,1.0))
set.seed(123); b2_model_DBN51a_roc <- train(formula, data=bene2_woe_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
#best tune
DBN5grid <- expand.grid(.layer1=c(10),.layer2=c(10),.layer3=c(10),.layer4=c(10),.layer5=c(10),.hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(1.5))
###data 1, train-test
#ROC curve 
set.seed(123); b2_model_DBN51a_roc <- train(formula, data=bene2_woe_train1, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN51a_roc <- predict(b2_model_DBN51a_roc,bene2_woe_test1,type="prob")
b2_DBN51a.ROC <- roc(predictor=b2predb_DBN51a_roc$X0,
                     response=bene2_woe_test1$target,
                     levels=rev(levels(bene2_woe_test1$target)))
b2_DBN51a.ROC

#normalizedGini
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
bene2_woe_test1$targetb <- as.numeric(levels(bene2_woe_test1$targetb))[bene2_woe_test1$targetb]
set.seed(123); b2_model_DBN51a_gini <- train(formula, data=bene2_woe_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN51a_gini$results
b2_model_DBN51a_gini$resample
b2_pred_DBN51a_gini<- predict(b2_model_DBN51a_gini, newdata = bene2_woe_test1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test1$targetb, b2_pred_DBN51a_gini$X1)
Gini(bene2_woe_test1$targetb, b2_pred_DBN51a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN51a_gini$X1[b2_pred_DBN51a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test1$targetb[b2_pred_DBN51a_gini$X1<=0.4]))
b2_DBN51a.ngini <- normalizedGini(a, p)
b2_DBN51a.ngini
b2_DBN51a.gini <-Gini(a, p)
b2_DBN51a.gini

#Brier score
set.seed(123); b2_model_DBN51a_brier <- train(formula, data=bene2_woe_train1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN51a_brier$results
b2_model_DBN51a_brier$resample
b2_pred_DBN51a_brier <- predict(b2_model_DBN51a_brier, newdata = bene2_woe_test1, type='prob')
bene2_woe_test1$targetb <- bene2_woe_test1$target
levels(bene2_woe_test1$targetb) <- c('0','1')
b2_DBN51a.bs <- Brier(as.numeric(as.character(bene2_woe_test1$targetb)), b2_pred_DBN51a_brier$X1)
b2_DBN51a.bs

###data 1 - test-train
#ROC curve 
set.seed(123); b2_model_DBN51b_roc <- train(formula, data=bene2_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN51b_roc <- predict(b2_model_DBN51b_roc, bene2_woe_train1,type="prob")
b2_DBN51b.ROC <- roc(predictor=b2predb_DBN51b_roc$X0,
                     response=bene2_woe_train1$target,
                     levels=rev(levels(bene2_woe_train1$target)))
b2_DBN51b.ROC

#normalizedGini
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
bene2_woe_train1$targetb <- as.numeric(levels(bene2_woe_train1$targetb))[bene2_woe_train1$targetb]
set.seed(123); b2_model_DBN51b_gini <- train(formula, data=bene2_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN51b_gini$results
b2_model_DBN51b_gini$resample
b2_pred_DBN51b_gini<- predict(b2_model_DBN51b_gini, newdata = bene2_woe_train1, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train1$targetb, b2_pred_DBN51b_gini$X1)
Gini(bene2_woe_train1$targetb, b2_pred_DBN51b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN51b_gini$X1[b2_pred_DBN51b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train1$targetb[b2_pred_DBN51b_gini$X1<=0.4]))
b2_DBN51b.ngini <- normalizedGini(a, p)
b2_DBN51b.ngini
b2_DBN51b.gini <-Gini(a, p)
b2_DBN51b.gini

#Brier score
set.seed(123); b2_model_DBN51b_brier <- train(formula, data=bene2_woe_test1,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN51b_brier$results
b2_model_DBN51b_brier$resample
b2_pred_DBN51b_brier <- predict(b2_model_DBN51b_brier, newdata = bene2_woe_train1, type='prob')
bene2_woe_train1$targetb <- bene2_woe_train1$target
levels(bene2_woe_train1$targetb) <- c('0','1')
b2_DBN51b.bs <- Brier(as.numeric(as.character(bene2_woe_train1$targetb)), b2_pred_DBN51b_brier$X1)
b2_DBN51b.bs

###data 2, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN52a_roc <- train(formula, data=bene2_woe_train2, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN52a_roc <- predict(b2_model_DBN52a_roc,bene2_woe_test2,type="prob")
b2_DBN52a.ROC <- roc(predictor=b2predb_DBN52a_roc$X0,
                     response=bene2_woe_test2$target,
                     levels=rev(levels(bene2_woe_test2$target)))
b2_DBN52a.ROC

#normalizedGini
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
bene2_woe_test2$targetb <- as.numeric(levels(bene2_woe_test2$targetb))[bene2_woe_test2$targetb]
set.seed(123); b2_model_DBN52a_gini <- train(formula, data=bene2_woe_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN52a_gini$results
b2_model_DBN52a_gini$resample
b2_pred_DBN52a_gini<- predict(b2_model_DBN52a_gini, newdata = bene2_woe_test2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test2$targetb, b2_pred_DBN52a_gini$X1)
Gini(bene2_woe_test2$targetb, b2_pred_DBN52a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN52a_gini$X1[b2_pred_DBN52a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test2$targetb[b2_pred_DBN52a_gini$X1<=0.4]))
b2_DBN52a.ngini <- normalizedGini(a, p)
b2_DBN52a.ngini
b2_DBN52a.gini <-Gini(a, p)
b2_DBN52a.gini

#Brier score
set.seed(123); b2_model_DBN52a_brier <- train(formula, data=bene2_woe_train2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN52a_brier$results
b2_model_DBN52a_brier$resample
b2_pred_DBN52a_brier <- predict(b2_model_DBN52a_brier, newdata = bene2_woe_test2, type='prob')
bene2_woe_test2$targetb <- bene2_woe_test2$target
levels(bene2_woe_test2$targetb) <- c('0','1')
b2_DBN52a.bs <- Brier(as.numeric(as.character(bene2_woe_test2$targetb)), b2_pred_DBN52a_brier$X1)
b2_DBN52a.bs

###data 2 - test-train
#ROC curve 
set.seed(123); b2_model_DBN52b_roc <- train(formula, data=bene2_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN52b_roc <- predict(b2_model_DBN52b_roc, bene2_woe_train2,type="prob")
b2_DBN52b.ROC <- roc(predictor=b2predb_DBN52b_roc$X0,
                     response=bene2_woe_train2$target,
                     levels=rev(levels(bene2_woe_train2$target)))
b2_DBN52b.ROC

#normalizedGini
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
bene2_woe_train2$targetb <- as.numeric(levels(bene2_woe_train2$targetb))[bene2_woe_train2$targetb]
set.seed(123); b2_model_DBN52b_gini <- train(formula, data=bene2_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN52b_gini$results
b2_model_DBN52b_gini$resample
b2_pred_DBN52b_gini<- predict(b2_model_DBN52b_gini, newdata = bene2_woe_train2, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train2$targetb, b2_pred_DBN52b_gini$X1)
Gini(bene2_woe_train2$targetb, b2_pred_DBN52b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN52b_gini$X1[b2_pred_DBN52b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train2$targetb[b2_pred_DBN52b_gini$X1<=0.4]))
b2_DBN52b.ngini <- normalizedGini(a, p)
b2_DBN52b.ngini
b2_DBN52b.gini <-Gini(a, p)
b2_DBN52b.gini

#Brier score
set.seed(123); b2_model_DBN52b_brier <- train(formula, data=bene2_woe_test2,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN52b_brier$results
b2_model_DBN52b_brier$resample
b2_pred_DBN52b_brier <- predict(b2_model_DBN52b_brier, newdata = bene2_woe_train2, type='prob')
bene2_woe_train2$targetb <- bene2_woe_train2$target
levels(bene2_woe_train2$targetb) <- c('0','1')
b2_DBN52b.bs <- Brier(as.numeric(as.character(bene2_woe_train2$targetb)), b2_pred_DBN52b_brier$X1)
b2_DBN52b.bs

###data 3, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN53a_roc <- train(formula, data=bene2_woe_train3, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN53a_roc <- predict(b2_model_DBN53a_roc,bene2_woe_test3,type="prob")
b2_DBN53a.ROC <- roc(predictor=b2predb_DBN53a_roc$X0,
                     response=bene2_woe_test3$target,
                     levels=rev(levels(bene2_woe_test3$target)))
b2_DBN53a.ROC

#normalizedGini
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
bene2_woe_test3$targetb <- as.numeric(levels(bene2_woe_test3$targetb))[bene2_woe_test3$targetb]
set.seed(123); b2_model_DBN53a_gini <- train(formula, data=bene2_woe_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN53a_gini$results
b2_model_DBN53a_gini$resample
b2_pred_DBN53a_gini<- predict(b2_model_DBN53a_gini, newdata = bene2_woe_test3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test3$targetb, b2_pred_DBN53a_gini$X1)
Gini(bene2_woe_test3$targetb, b2_pred_DBN53a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN53a_gini$X1[b2_pred_DBN53a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test3$targetb[b2_pred_DBN53a_gini$X1<=0.4]))
b2_DBN53a.ngini <- normalizedGini(a, p)
b2_DBN53a.ngini
b2_DBN53a.gini <-Gini(a, p)
b2_DBN53a.gini

#Brier score
set.seed(123); b2_model_DBN53a_brier <- train(formula, data=bene2_woe_train3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN53a_brier$results
b2_model_DBN53a_brier$resample
b2_pred_DBN53a_brier <- predict(b2_model_DBN53a_brier, newdata = bene2_woe_test3, type='prob')
bene2_woe_test3$targetb <- bene2_woe_test3$target
levels(bene2_woe_test3$targetb) <- c('0','1')
b2_DBN53a.bs <- Brier(as.numeric(as.character(bene2_woe_test3$targetb)), b2_pred_DBN53a_brier$X1)
b2_DBN53a.bs

###data 3 - test-train
#ROC curve 
set.seed(123); b2_model_DBN53b_roc <- train(formula, data=bene2_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN53b_roc <- predict(b2_model_DBN53b_roc, bene2_woe_train3,type="prob")
b2_DBN53b.ROC <- roc(predictor=b2predb_DBN53b_roc$X0,
                     response=bene2_woe_train3$target,
                     levels=rev(levels(bene2_woe_train3$target)))
b2_DBN53b.ROC

#normalizedGini
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
bene2_woe_train3$targetb <- as.numeric(levels(bene2_woe_train3$targetb))[bene2_woe_train3$targetb]
set.seed(123); b2_model_DBN53b_gini <- train(formula, data=bene2_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN53b_gini$results
b2_model_DBN53b_gini$resample
b2_pred_DBN53b_gini<- predict(b2_model_DBN53b_gini, newdata = bene2_woe_train3, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train3$targetb, b2_pred_DBN53b_gini$X1)
Gini(bene2_woe_train3$targetb, b2_pred_DBN53b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN53b_gini$X1[b2_pred_DBN53b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train3$targetb[b2_pred_DBN53b_gini$X1<=0.4]))
b2_DBN53b.ngini <- normalizedGini(a, p)
b2_DBN53b.ngini
b2_DBN53b.gini <-Gini(a, p)
b2_DBN53b.gini

#Brier score
set.seed(123); b2_model_DBN53b_brier <- train(formula, data=bene2_woe_test3,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN53b_brier$results
b2_model_DBN53b_brier$resample
b2_pred_DBN53b_brier <- predict(b2_model_DBN53b_brier, newdata = bene2_woe_train3, type='prob')
bene2_woe_train3$targetb <- bene2_woe_train3$target
levels(bene2_woe_train3$targetb) <- c('0','1')
b2_DBN53b.bs <- Brier(as.numeric(as.character(bene2_woe_train3$targetb)), b2_pred_DBN53b_brier$X1)
b2_DBN53b.bs

###data 4, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN54a_roc <- train(formula, data=bene2_woe_train4, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN54a_roc <- predict(b2_model_DBN54a_roc,bene2_woe_test4,type="prob")
b2_DBN54a.ROC <- roc(predictor=b2predb_DBN54a_roc$X0,
                     response=bene2_woe_test4$target,
                     levels=rev(levels(bene2_woe_test4$target)))
b2_DBN54a.ROC

#normalizedGini
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
bene2_woe_test4$targetb <- as.numeric(levels(bene2_woe_test4$targetb))[bene2_woe_test4$targetb]
set.seed(123); b2_model_DBN54a_gini <- train(formula, data=bene2_woe_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN54a_gini$results
b2_model_DBN54a_gini$resample
b2_pred_DBN54a_gini<- predict(b2_model_DBN54a_gini, newdata = bene2_woe_test4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test4$targetb, b2_pred_DBN54a_gini$X1)
Gini(bene2_woe_test4$targetb, b2_pred_DBN54a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN54a_gini$X1[b2_pred_DBN54a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test4$targetb[b2_pred_DBN54a_gini$X1<=0.4]))
b2_DBN54a.ngini <- normalizedGini(a, p)
b2_DBN54a.ngini
b2_DBN54a.gini <-Gini(a, p)
b2_DBN54a.gini

#Brier score
set.seed(123); b2_model_DBN54a_brier <- train(formula, data=bene2_woe_train4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN54a_brier$results
b2_model_DBN54a_brier$resample
b2_pred_DBN54a_brier <- predict(b2_model_DBN54a_brier, newdata = bene2_woe_test4, type='prob')
bene2_woe_test4$targetb <- bene2_woe_test4$target
levels(bene2_woe_test4$targetb) <- c('0','1')
b2_DBN54a.bs <- Brier(as.numeric(as.character(bene2_woe_test4$targetb)), b2_pred_DBN54a_brier$X1)
b2_DBN54a.bs

###data 4 - test-train
#ROC curve 
set.seed(123); b2_model_DBN54b_roc <- train(formula, data=bene2_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN54b_roc <- predict(b2_model_DBN54b_roc, bene2_woe_train4,type="prob")
b2_DBN54b.ROC <- roc(predictor=b2predb_DBN54b_roc$X0,
                     response=bene2_woe_train4$target,
                     levels=rev(levels(bene2_woe_train4$target)))
b2_DBN54b.ROC

#normalizedGini
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
bene2_woe_train4$targetb <- as.numeric(levels(bene2_woe_train4$targetb))[bene2_woe_train4$targetb]
set.seed(123); b2_model_DBN54b_gini <- train(formula, data=bene2_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN54b_gini$results
b2_model_DBN54b_gini$resample
b2_pred_DBN54b_gini<- predict(b2_model_DBN54b_gini, newdata = bene2_woe_train4, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train4$targetb, b2_pred_DBN54b_gini$X1)
Gini(bene2_woe_train4$targetb, b2_pred_DBN54b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN54b_gini$X1[b2_pred_DBN54b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train4$targetb[b2_pred_DBN54b_gini$X1<=0.4]))
b2_DBN54b.ngini <- normalizedGini(a, p)
b2_DBN54b.ngini
b2_DBN54b.gini <-Gini(a, p)
b2_DBN54b.gini

#Brier score
set.seed(123); b2_model_DBN54b_brier <- train(formula, data=bene2_woe_test4,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN54b_brier$results
b2_model_DBN54b_brier$resample
b2_pred_DBN54b_brier <- predict(b2_model_DBN54b_brier, newdata = bene2_woe_train4, type='prob')
bene2_woe_train4$targetb <- bene2_woe_train4$target
levels(bene2_woe_train4$targetb) <- c('0','1')
b2_DBN54b.bs <- Brier(as.numeric(as.character(bene2_woe_train4$targetb)), b2_pred_DBN54b_brier$X1)
b2_DBN54b.bs

###data 5, train-test
#ROC curve 
#test it
set.seed(123); b2_model_DBN55a_roc <- train(formula, data=bene2_woe_train5, method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN55a_roc <- predict(b2_model_DBN55a_roc,bene2_woe_test5,type="prob")
b2_DBN55a.ROC <- roc(predictor=b2predb_DBN55a_roc$X0,
                     response=bene2_woe_test5$target,
                     levels=rev(levels(bene2_woe_test5$target)))
b2_DBN55a.ROC

#normalizedGini
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
bene2_woe_test5$targetb <- as.numeric(levels(bene2_woe_test5$targetb))[bene2_woe_test5$targetb]
set.seed(123); b2_model_DBN55a_gini <- train(formula, data=bene2_woe_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN55a_gini$results
b2_model_DBN55a_gini$resample
b2_pred_DBN55a_gini<- predict(b2_model_DBN55a_gini, newdata = bene2_woe_test5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_test5$targetb, b2_pred_DBN55a_gini$X1)
Gini(bene2_woe_test5$targetb, b2_pred_DBN55a_gini$X1)
#b <= 0.4
p <- b2_pred_DBN55a_gini$X1[b2_pred_DBN55a_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_test5$targetb[b2_pred_DBN55a_gini$X1<=0.4]))
b2_DBN55a.ngini <- normalizedGini(a, p)
b2_DBN55a.ngini
b2_DBN55a.gini <-Gini(a, p)
b2_DBN55a.gini

#Brier score
set.seed(123); b2_model_DBN55a_brier <- train(formula, data=bene2_woe_train5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN55a_brier$results
b2_model_DBN55a_brier$resample
b2_pred_DBN55a_brier <- predict(b2_model_DBN55a_brier, newdata = bene2_woe_test5, type='prob')
bene2_woe_test5$targetb <- bene2_woe_test5$target
levels(bene2_woe_test5$targetb) <- c('0','1')
b2_DBN55a.bs <- Brier(as.numeric(as.character(bene2_woe_test5$targetb)), b2_pred_DBN55a_brier$X1)
b2_DBN55a.bs

###data 5 - test-train
#ROC curve 
set.seed(123); b2_model_DBN55b_roc <- train(formula, data=bene2_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_roc, metric='ROC')
b2predb_DBN55b_roc <- predict(b2_model_DBN55b_roc, bene2_woe_train5,type="prob")
b2_DBN55b.ROC <- roc(predictor=b2predb_DBN55b_roc$X0,
                     response=bene2_woe_train5$target,
                     levels=rev(levels(bene2_woe_train5$target)))
b2_DBN55b.ROC

#normalizedGini
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
bene2_woe_train5$targetb <- as.numeric(levels(bene2_woe_train5$targetb))[bene2_woe_train5$targetb]
set.seed(123); b2_model_DBN55b_gini <- train(formula, data=bene2_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_gini, metric='Gini')
b2_model_DBN55b_gini$results
b2_model_DBN55b_gini$resample
b2_pred_DBN55b_gini<- predict(b2_model_DBN55b_gini, newdata = bene2_woe_train5, type='prob')
#Gini and normalized gini of prediction
normalizedGini(bene2_woe_train5$targetb, b2_pred_DBN55b_gini$X1)
Gini(bene2_woe_train5$targetb, b2_pred_DBN55b_gini$X1)
#b <= 0.4
p <- b2_pred_DBN55b_gini$X1[b2_pred_DBN55b_gini$X1<=0.4]
a <- as.numeric(as.character(bene2_woe_train5$targetb[b2_pred_DBN55b_gini$X1<=0.4]))
b2_DBN55b.ngini <- normalizedGini(a, p)
b2_DBN55b.ngini
b2_DBN55b.gini <-Gini(a, p)
b2_DBN55b.gini

#Brier score
set.seed(123); b2_model_DBN55b_brier <- train(formula, data=bene2_woe_test5,  method = DBN5, tuneGrid=DBN5grid, preProc=c("center","scale"), trControl=train_control_brier, metric='BrierScore', maximize=FALSE)
b2_model_DBN55b_brier$results
b2_model_DBN55b_brier$resample
b2_pred_DBN55b_brier <- predict(b2_model_DBN55b_brier, newdata = bene2_woe_train5, type='prob')
bene2_woe_train5$targetb <- bene2_woe_train5$target
levels(bene2_woe_train5$targetb) <- c('0','1')
b2_DBN55b.bs <- Brier(as.numeric(as.character(bene2_woe_train5$targetb)), b2_pred_DBN55b_brier$X1)
b2_DBN55b.bs

##Restults dbn
b2_results_DBN5_AUC <- cbind(b2_DBN51a.ROC$auc,b2_DBN51b.ROC$auc,b2_DBN52a.ROC$auc,b2_DBN52b.ROC$auc,b2_DBN53a.ROC$auc,b2_DBN53b.ROC$auc,b2_DBN54a.ROC$auc,
                             b2_DBN54b.ROC$auc,b2_DBN55a.ROC$auc,b2_DBN55b.ROC$auc)
b2_results_DBN5_bs <- cbind(b2_DBN51a.bs,b2_DBN51b.bs,b2_DBN52a.bs,b2_DBN52b.bs,b2_DBN53a.bs,b2_DBN53b.bs,b2_DBN54a.bs,b2_DBN54b.bs,b2_DBN55a.bs,b2_DBN55b.bs)
b2_results_DBN5_ngini <- cbind(b2_DBN51a.ngini,b2_DBN51b.ngini,b2_DBN52a.ngini,b2_DBN52b.ngini,b2_DBN53a.ngini,b2_DBN53b.ngini,b2_DBN54a.ngini,b2_DBN54b.ngini,
                               b2_DBN55a.ngini,b2_DBN55b.ngini)
b2_results_DBN5_gini <- cbind(b2_DBN51a.gini,b2_DBN51b.gini,b2_DBN52a.gini,b2_DBN52b.gini,b2_DBN53a.gini,b2_DBN53b.gini,b2_DBN54a.gini,b2_DBN54b.gini,
                              b2_DBN55a.gini,b2_DBN55b.gini)
mean(b2_results_DBN5_AUC)
mean(b2_results_DBN5_bs)
mean(b2_results_DBN5_ngini)
mean(b2_results_DBN5_gini)

############################
###########EMP##############
############################



#bene2 - logistic
formula <- target~voorsch+mens+epccb+efccb+chyem+leen+proczr+pchem+signemp+newbuts+duree+arevem+revem+proem2+etcem+gesl+aaaem+naie+inbelgie+bijwerk+telefem+prop+domem+buitenl+tend+inkomen
set.seed(123); b2_model_log1a_emp <- train(formula, data=bene2_woe_train1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log1a_emp <- predict(b2_model_log1a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_log1a <- empCreditScoring(b2predb_log1a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_log1b_emp <- train(formula, data=bene2_woe_test1, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log1b_emp <- predict(b2_model_log1b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_log1b <- empCreditScoring(b2predb_log1b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_log2a_emp <- train(formula, data=bene2_woe_train2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log2a_emp <- predict(b2_model_log2a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_log2a <- empCreditScoring(b2predb_log2a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_log2b_emp <- train(formula, data=bene2_woe_test2, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log2b_emp <- predict(b2_model_log2b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_log2b <- empCreditScoring(b2predb_log2b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_log3a_emp <- train(formula, data=bene2_woe_train3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log3a_emp <- predict(b2_model_log3a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_log3a <- empCreditScoring(b2predb_log3a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_log3b_emp <- train(formula, data=bene2_woe_test3, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log3b_emp <- predict(b2_model_log3b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_log3b <- empCreditScoring(b2predb_log3b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_log4a_emp <- train(formula, data=bene2_woe_train4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log4a_emp <- predict(b2_model_log4a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_log4a <- empCreditScoring(b2predb_log4a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_log4b_emp <- train(formula, data=bene2_woe_test4, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log4b_emp <- predict(b2_model_log4b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_log4b <- empCreditScoring(b2predb_log4b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_log5a_emp <- train(formula, data=bene2_woe_train5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log5a_emp <- predict(b2_model_log5a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_log5a <- empCreditScoring(b2predb_log5a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_log5b_emp <- train(formula, data=bene2_woe_test5, method = "glm",family="binomial", trControl=train_control_emp, metric='emp', preProc = c("center", "scale"))
b2predb_log5b_emp <- predict(b2_model_log5b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_log5b <- empCreditScoring(b2predb_log5b_emp$X1,bene2_woe_train5$target)

#bene2 - dt
DTgrid <- expand.grid(C=c(0.01,0.1,0.2,0.3,0.4,0.5), M=c(3,4,5,6,7,8))
set.seed(123); b2reg_model_DT1a_emp <- train(formula, data=bene2_train1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT1a_emp <- predict(b2reg_model_DT1a_emp, newdata = bene2_test1, type='prob')
b2emp_dt1a <- empCreditScoring(b2regpredb_DT1a_emp$X1,bene2_test1$target)
set.seed(123); b2reg_model_DT1b_emp <- train(formula, data=bene2_test1,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT1b_emp <- predict(b2reg_model_DT1b_emp, newdata = bene2_train1, type='prob')
b2emp_dt1b <- empCreditScoring(b2regpredb_DT1b_emp$X1,bene2_train1$target)
set.seed(123); b2reg_model_DT2a_emp <- train(formula, data=bene2_train2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT2a_emp <- predict(b2reg_model_DT2a_emp, newdata = bene2_test2, type='prob')
b2emp_dt2a <- empCreditScoring(b2regpredb_DT2a_emp$X1,bene2_test2$target)
set.seed(123); b2reg_model_DT2b_emp <- train(formula, data=bene2_test2,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT2b_emp <- predict(b2reg_model_DT2b_emp, newdata = bene2_train2, type='prob')
b2emp_dt2b <- empCreditScoring(b2regpredb_DT2b_emp$X1,bene2_train2$target)
set.seed(123); b2reg_model_DT3a_emp <- train(formula, data=bene2_train3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT3a_emp <- predict(b2reg_model_DT3a_emp, newdata = bene2_test3, type='prob')
b2emp_dt3a <- empCreditScoring(b2regpredb_DT3a_emp$X1,bene2_test3$target)
set.seed(123); b2reg_model_DT3b_emp <- train(formula, data=bene2_test3,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT3b_emp <- predict(b2reg_model_DT3b_emp, newdata = bene2_train3, type='prob')
b2emp_dt3b <- empCreditScoring(b2regpredb_DT3b_emp$X1,bene2_train3$target)
set.seed(123); b2reg_model_DT4a_emp <- train(formula, data=bene2_train4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT4a_emp <- predict(b2reg_model_DT4a_emp, newdata = bene2_test4, type='prob')
b2emp_dt4a <- empCreditScoring(b2regpredb_DT4a_emp$X1,bene2_test4$target)
set.seed(123); b2reg_model_DT4b_emp <- train(formula, data=bene2_test4,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT4b_emp <- predict(b2reg_model_DT4b_emp, newdata = bene2_train4, type='prob')
b2emp_dt4b <- empCreditScoring(b2regpredb_DT4b_emp$X1,bene2_train4$target)
set.seed(123); b2reg_model_DT5a_emp <- train(formula, data=bene2_train5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT5a_emp <- predict(b2reg_model_DT5a_emp, newdata = bene2_test5, type='prob')
b2emp_dt5a <- empCreditScoring(b2regpredb_DT5a_emp$X1,bene2_test5$target)
set.seed(123); b2reg_model_DT5b_emp <- train(formula, data=bene2_test5,  method = "J48", tuneGrid=DTgrid, trControl=train_control_emp, metric='empc')
b2regpredb_DT5b_emp <- predict(b2reg_model_DT5b_emp, newdata = bene2_train5, type='prob')
b2emp_dt5b <- empCreditScoring(b2regpredb_DT5b_emp$X1,bene2_train5$target)

#bene2 - RF
m <- floor(log2(length(bene2_train1$target)+1))
RFgrid <- expand.grid(.mtry=c(as.integer(sqrt(m*0.1)),as.integer(sqrt(m*0.25)),as.integer(sqrt(m*0.5)),as.integer(sqrt(m*1)), as.integer(sqrt(m*2)), as.integer(sqrt(m*4))), .ntree=c(100, 250, 500, 750, 1000))
set.seed(123); b2reg_model_RF1a_emp <- train(formula, data=bene2_train1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF1a_emp <- predict(b2reg_model_RF1a_emp, newdata = bene2_test1, type='prob')
b2emp_RF1a <- empCreditScoring(b2regpredb_RF1a_emp$X1,bene2_test1$target)
set.seed(123); b2reg_model_RF1b_emp <- train(formula, data=bene2_test1,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF1b_emp <- predict(b2reg_model_RF1b_emp, newdata = bene2_train1, type='prob')
b2emp_RF1b <- empCreditScoring(b2regpredb_RF1b_emp$X1,bene2_train1$target)
set.seed(123); b2reg_model_RF2a_emp <- train(formula, data=bene2_train2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF2a_emp <- predict(b2reg_model_RF2a_emp, newdata = bene2_test2, type='prob')
b2emp_RF2a <- empCreditScoring(b2regpredb_RF2a_emp$X1,bene2_test2$target)
set.seed(123); b2reg_model_RF2b_emp <- train(formula, data=bene2_test2,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF2b_emp <- predict(b2reg_model_RF2b_emp, newdata = bene2_train2, type='prob')
b2emp_RF2b <- empCreditScoring(b2regpredb_RF2b_emp$X1,bene2_train2$target)
set.seed(123); b2reg_model_RF3a_emp <- train(formula, data=bene2_train3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF3a_emp <- predict(b2reg_model_RF3a_emp, newdata = bene2_test3, type='prob')
b2emp_RF3a <- empCreditScoring(b2regpredb_RF3a_emp$X1,bene2_test3$target)
set.seed(123); b2reg_model_RF3b_emp <- train(formula, data=bene2_test3,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF3b_emp <- predict(b2reg_model_RF3b_emp, newdata = bene2_train3, type='prob')
b2emp_RF3b <- empCreditScoring(b2regpredb_RF3b_emp$X1,bene2_train3$target)
set.seed(123); b2reg_model_RF4a_emp <- train(formula, data=bene2_train4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF4a_emp <- predict(b2reg_model_RF4a_emp, newdata = bene2_test4, type='prob')
b2emp_RF4a <- empCreditScoring(b2regpredb_RF4a_emp$X1,bene2_test4$target)
set.seed(123); b2reg_model_RF4b_emp <- train(formula, data=bene2_test4,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF4b_emp <- predict(b2reg_model_RF4b_emp, newdata = bene2_train4, type='prob')
b2emp_RF4b <- empCreditScoring(b2regpredb_RF4b_emp$X1,bene2_train4$target)
set.seed(123); b2reg_model_RF5a_emp <- train(formula, data=bene2_train5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF5a_emp <- predict(b2reg_model_RF5a_emp, newdata = bene2_test5, type='prob')
b2emp_RF5a <- empCreditScoring(b2regpredb_RF5a_emp$X1,bene2_test5$target)
set.seed(123); b2reg_model_RF5b_emp <- train(formula, data=bene2_test5,  method = customRF, tuneGrid=RFgrid, trControl=train_control_emp, metric='empc')
b2regpredb_RF5b_emp <- predict(b2reg_model_RF5b_emp, newdata = bene2_train5, type='prob')
b2emp_RF5b <- empCreditScoring(b2regpredb_RF5b_emp$X1,bene2_train5$target)

#bene2 - MLP1
MLP1grid <- expand.grid(.size=c(15), .dropout=c(0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP11a_emp <- train(formula, data=bene2_woe_train1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP11a_emp <- predict(b2_model_MLP11a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_MLP11a <- empCreditScoring(b2predb_MLP11a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_MLP11b_emp <- train(formula, data=bene2_woe_test1, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP11b_emp <- predict(b2_model_MLP11b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_MLP11b <- empCreditScoring(b2predb_MLP11b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_MLP12a_emp <- train(formula, data=bene2_woe_train2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP12a_emp <- predict(b2_model_MLP12a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_MLP12a <- empCreditScoring(b2predb_MLP12a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_MLP12b_emp <- train(formula, data=bene2_woe_test2, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP12b_emp <- predict(b2_model_MLP12b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_MLP12b <- empCreditScoring(b2predb_MLP12b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_MLP13a_emp <- train(formula, data=bene2_woe_train3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP13a_emp <- predict(b2_model_MLP13a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_MLP13a <- empCreditScoring(b2predb_MLP13a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_MLP13b_emp <- train(formula, data=bene2_woe_test3, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP13b_emp <- predict(b2_model_MLP13b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_MLP13b <- empCreditScoring(b2predb_MLP13b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_MLP14a_emp <- train(formula, data=bene2_woe_train4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP14a_emp <- predict(b2_model_MLP14a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_MLP14a <- empCreditScoring(b2predb_MLP14a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_MLP14b_emp <- train(formula, data=bene2_woe_test4, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP14b_emp <- predict(b2_model_MLP14b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_MLP14b <- empCreditScoring(b2predb_MLP14b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_MLP15a_emp <- train(formula, data=bene2_woe_train5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP15a_emp <- predict(b2_model_MLP15a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_MLP15a <- empCreditScoring(b2predb_MLP15a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_MLP15b_emp <- train(formula, data=bene2_woe_test5, method = MLP1, tuneGrid=MLP1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP15b_emp <- predict(b2_model_MLP15b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_MLP15b <- empCreditScoring(b2predb_MLP15b_emp$X1,bene2_woe_train5$target)

#bene2 - MLP3
MLP3grid <- expand.grid(.size1=c(20), .size2=c(20), .size3=c(15), .dropout=c(0.25), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP31a_emp <- train(formula, data=bene2_woe_train1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP31a_emp <- predict(b2_model_MLP31a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_MLP31a <- empCreditScoring(b2predb_MLP31a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_MLP31b_emp <- train(formula, data=bene2_woe_test1, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP31b_emp <- predict(b2_model_MLP31b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_MLP31b <- empCreditScoring(b2predb_MLP31b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_MLP32a_emp <- train(formula, data=bene2_woe_train2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP32a_emp <- predict(b2_model_MLP32a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_MLP32a <- empCreditScoring(b2predb_MLP32a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_MLP32b_emp <- train(formula, data=bene2_woe_test2, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP32b_emp <- predict(b2_model_MLP32b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_MLP32b <- empCreditScoring(b2predb_MLP32b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_MLP33a_emp <- train(formula, data=bene2_woe_train3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP33a_emp <- predict(b2_model_MLP33a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_MLP33a <- empCreditScoring(b2predb_MLP33a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_MLP33b_emp <- train(formula, data=bene2_woe_test3, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP33b_emp <- predict(b2_model_MLP33b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_MLP33b <- empCreditScoring(b2predb_MLP33b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_MLP34a_emp <- train(formula, data=bene2_woe_train4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP34a_emp <- predict(b2_model_MLP34a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_MLP34a <- empCreditScoring(b2predb_MLP34a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_MLP34b_emp <- train(formula, data=bene2_woe_test4, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP34b_emp <- predict(b2_model_MLP34b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_MLP34b <- empCreditScoring(b2predb_MLP34b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_MLP35a_emp <- train(formula, data=bene2_woe_train5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP35a_emp <- predict(b2_model_MLP35a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_MLP35a <- empCreditScoring(b2predb_MLP35a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_MLP35b_emp <- train(formula, data=bene2_woe_test5, method = MLP3, tuneGrid=MLP3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP35b_emp <- predict(b2_model_MLP35b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_MLP35b <- empCreditScoring(b2predb_MLP35b_emp$X1,bene2_woe_train5$target)

#bene2 - MLP5
MLP5grid <- expand.grid(.size1=c(15), .size2=c(15), .size3=c(10), .size4=c(10), .size5=c(5), .dropout=c(0.0), .batch_size=batch, .lr=c(0.01), .rho=0.9, .decay=0, .activation='relu')
set.seed(123); b2_model_MLP51a_emp <- train(formula, data=bene2_woe_train1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP51a_emp <- predict(b2_model_MLP51a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_MLP51a <- empCreditScoring(b2predb_MLP51a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_MLP51b_emp <- train(formula, data=bene2_woe_test1, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP51b_emp <- predict(b2_model_MLP51b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_MLP51b <- empCreditScoring(b2predb_MLP51b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_MLP52a_emp <- train(formula, data=bene2_woe_train2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP52a_emp <- predict(b2_model_MLP52a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_MLP52a <- empCreditScoring(b2predb_MLP52a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_MLP52b_emp <- train(formula, data=bene2_woe_test2, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP52b_emp <- predict(b2_model_MLP52b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_MLP52b <- empCreditScoring(b2predb_MLP52b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_MLP53a_emp <- train(formula, data=bene2_woe_train3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP53a_emp <- predict(b2_model_MLP53a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_MLP53a <- empCreditScoring(b2predb_MLP53a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_MLP53b_emp <- train(formula, data=bene2_woe_test3, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP53b_emp <- predict(b2_model_MLP53b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_MLP53b <- empCreditScoring(b2predb_MLP53b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_MLP54a_emp <- train(formula, data=bene2_woe_train4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP54a_emp <- predict(b2_model_MLP54a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_MLP54a <- empCreditScoring(b2predb_MLP54a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_MLP54b_emp <- train(formula, data=bene2_woe_test4, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP54b_emp <- predict(b2_model_MLP54b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_MLP54b <- empCreditScoring(b2predb_MLP54b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_MLP55a_emp <- train(formula, data=bene2_woe_train5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP55a_emp <- predict(b2_model_MLP55a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_MLP55a <- empCreditScoring(b2predb_MLP55a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_MLP55b_emp <- train(formula, data=bene2_woe_test5, method = MLP5, tuneGrid=MLP5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_MLP55b_emp <- predict(b2_model_MLP55b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_MLP55b <- empCreditScoring(b2predb_MLP55b_emp$X1,bene2_woe_train5$target)

#bene2 - DBN1  
DBN1grid <- expand.grid(.layer1=c(5), .hidden_dropout=c(0), .visible_dropout=c(0.25), .lr=c(1.0))
set.seed(123); b2_model_DBN11a_emp <- train(formula, data=bene2_woe_train1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN11a_emp <- predict(b2_model_DBN11a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_DBN11a <- empCreditScoring(b2predb_DBN11a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_DBN11b_emp <- train(formula, data=bene2_woe_test1, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN11b_emp <- predict(b2_model_DBN11b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_DBN11b <- empCreditScoring(b2predb_DBN11b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_DBN12a_emp <- train(formula, data=bene2_woe_train2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN12a_emp <- predict(b2_model_DBN12a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_DBN12a <- empCreditScoring(b2predb_DBN12a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_DBN12b_emp <- train(formula, data=bene2_woe_test2, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN12b_emp <- predict(b2_model_DBN12b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_DBN12b <- empCreditScoring(b2predb_DBN12b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_DBN13a_emp <- train(formula, data=bene2_woe_train3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN13a_emp <- predict(b2_model_DBN13a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_DBN13a <- empCreditScoring(b2predb_DBN13a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_DBN13b_emp <- train(formula, data=bene2_woe_test3, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN13b_emp <- predict(b2_model_DBN13b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_DBN13b <- empCreditScoring(b2predb_DBN13b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_DBN14a_emp <- train(formula, data=bene2_woe_train4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN14a_emp <- predict(b2_model_DBN14a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_DBN14a <- empCreditScoring(b2predb_DBN14a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_DBN14b_emp <- train(formula, data=bene2_woe_test4, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN14b_emp <- predict(b2_model_DBN14b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_DBN14b <- empCreditScoring(b2predb_DBN14b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_DBN15a_emp <- train(formula, data=bene2_woe_train5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN15a_emp <- predict(b2_model_DBN15a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_DBN15a <- empCreditScoring(b2predb_DBN15a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_DBN15b_emp <- train(formula, data=bene2_woe_test5, method = DBN1, tuneGrid=DBN1grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN15b_emp <- predict(b2_model_DBN15b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_DBN15b <- empCreditScoring(b2predb_DBN15b_emp$X1,bene2_woe_train5$target)

#bene2 - DBN3 
DBN3grid <- expand.grid(.layer1=c(10), .layer2=c(5), .layer3=c(10),.hidden_dropout=c(0.5), .visible_dropout=c(0), .lr=c(2))
set.seed(123); b2_model_DBN31a_emp <- train(formula, data=bene2_woe_train1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN31a_emp <- predict(b2_model_DBN31a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_DBN31a <- empCreditScoring(b2predb_DBN31a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_DBN31b_emp <- train(formula, data=bene2_woe_test1, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN31b_emp <- predict(b2_model_DBN31b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_DBN31b <- empCreditScoring(b2predb_DBN31b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_DBN32a_emp <- train(formula, data=bene2_woe_train2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN32a_emp <- predict(b2_model_DBN32a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_DBN32a <- empCreditScoring(b2predb_DBN32a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_DBN32b_emp <- train(formula, data=bene2_woe_test2, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN32b_emp <- predict(b2_model_DBN32b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_DBN32b <- empCreditScoring(b2predb_DBN32b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_DBN33a_emp <- train(formula, data=bene2_woe_train3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN33a_emp <- predict(b2_model_DBN33a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_DBN33a <- empCreditScoring(b2predb_DBN33a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_DBN33b_emp <- train(formula, data=bene2_woe_test3, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN33b_emp <- predict(b2_model_DBN33b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_DBN33b <- empCreditScoring(b2predb_DBN33b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_DBN34a_emp <- train(formula, data=bene2_woe_train4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN34a_emp <- predict(b2_model_DBN34a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_DBN34a <- empCreditScoring(b2predb_DBN34a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_DBN34b_emp <- train(formula, data=bene2_woe_test4, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN34b_emp <- predict(b2_model_DBN34b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_DBN34b <- empCreditScoring(b2predb_DBN34b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_DBN35a_emp <- train(formula, data=bene2_woe_train5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN35a_emp <- predict(b2_model_DBN35a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_DBN35a <- empCreditScoring(b2predb_DBN35a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_DBN35b_emp <- train(formula, data=bene2_woe_test5, method = DBN3, tuneGrid=DBN3grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN35b_emp <- predict(b2_model_DBN35b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_DBN35b <- empCreditScoring(b2predb_DBN35b_emp$X1,bene2_woe_train5$target)

#bene2 - DBN5   
DBN5grid <- expand.grid(.layer1=c(10),.layer2=c(10),.layer3=c(10),.layer4=c(10),.layer5=c(10),.hidden_dropout=c(0), .visible_dropout=c(0.5), .lr=c(1.5))
set.seed(123); b2_model_DBN51a_emp <- train(formula, data=bene2_woe_train1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN51a_emp <- predict(b2_model_DBN51a_emp, newdata = bene2_woe_test1, type='prob')
b2emp_DBN51a <- empCreditScoring(b2predb_DBN51a_emp$X1,bene2_woe_test1$target)
set.seed(123); b2_model_DBN51b_emp <- train(formula, data=bene2_woe_test1, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN51b_emp <- predict(b2_model_DBN51b_emp, newdata = bene2_woe_train1, type='prob')
b2emp_DBN51b <- empCreditScoring(b2predb_DBN51b_emp$X1,bene2_woe_train1$target)
set.seed(123); b2_model_DBN52a_emp <- train(formula, data=bene2_woe_train2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN52a_emp <- predict(b2_model_DBN52a_emp, newdata = bene2_woe_test2, type='prob')
b2emp_DBN52a <- empCreditScoring(b2predb_DBN52a_emp$X1,bene2_woe_test2$target)
set.seed(123); b2_model_DBN52b_emp <- train(formula, data=bene2_woe_test2, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN52b_emp <- predict(b2_model_DBN52b_emp, newdata = bene2_woe_train2, type='prob')
b2emp_DBN52b <- empCreditScoring(b2predb_DBN52b_emp$X1,bene2_woe_train2$target)
set.seed(123); b2_model_DBN53a_emp <- train(formula, data=bene2_woe_train3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN53a_emp <- predict(b2_model_DBN53a_emp, newdata = bene2_woe_test3, type='prob')
b2emp_DBN53a <- empCreditScoring(b2predb_DBN53a_emp$X1,bene2_woe_test3$target)
set.seed(123); b2_model_DBN53b_emp <- train(formula, data=bene2_woe_test3, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN53b_emp <- predict(b2_model_DBN53b_emp, newdata = bene2_woe_train3, type='prob')
b2emp_DBN53b <- empCreditScoring(b2predb_DBN53b_emp$X1,bene2_woe_train3$target)
set.seed(123); b2_model_DBN54a_emp <- train(formula, data=bene2_woe_train4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN54a_emp <- predict(b2_model_DBN54a_emp, newdata = bene2_woe_test4, type='prob')
b2emp_DBN54a <- empCreditScoring(b2predb_DBN54a_emp$X1,bene2_woe_test4$target)
set.seed(123); b2_model_DBN54b_emp <- train(formula, data=bene2_woe_test4, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN54b_emp <- predict(b2_model_DBN54b_emp, newdata = bene2_woe_train4, type='prob')
b2emp_DBN54b <- empCreditScoring(b2predb_DBN54b_emp$X1,bene2_woe_train4$target)
set.seed(123); b2_model_DBN55a_emp <- train(formula, data=bene2_woe_train5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN55a_emp <- predict(b2_model_DBN55a_emp, newdata = bene2_woe_test5, type='prob')
b2emp_DBN55a <- empCreditScoring(b2predb_DBN55a_emp$X1,bene2_woe_test5$target)
set.seed(123); b2_model_DBN55b_emp <- train(formula, data=bene2_woe_test5, method = DBN5, tuneGrid=DBN5grid, trControl=train_control_emp, metric='empc', preProc = c("center", "scale"))
b2predb_DBN55b_emp <- predict(b2_model_DBN55b_emp, newdata = bene2_woe_train5, type='prob')
b2emp_DBN55b <- empCreditScoring(b2predb_DBN55b_emp$X1,bene2_woe_train5$target)

b2emp_log <- cbind(b2emp_log1a$EMPC,b2emp_log1b$EMPC,b2emp_log2a$EMPC,b2emp_log2b$EMPC,b2emp_log3a$EMPC,b2emp_log3b$EMPC,
                   b2emp_log4a$EMPC,b2emp_log4b$EMPC,b2emp_log5a$EMPC,b2emp_log5b$EMPC)
b2emp_dt <- cbind(b2emp_dt1a$EMPC,b2emp_dt1b$EMPC,b2emp_dt2a$EMPC,b2emp_dt2b$EMPC,b2emp_dt3a$EMPC,b2emp_dt3b$EMPC,
                  b2emp_dt4a$EMPC,b2emp_dt4b$EMPC,b2emp_dt5a$EMPC,b2emp_dt5b$EMPC)
b2emp_rf <- cbind(b2emp_RF1a$EMPC,b2emp_RF1b$EMPC,b2emp_RF2a$EMPC,b2emp_RF2b$EMPC,b2emp_RF3a$EMPC,b2emp_RF3b$EMPC,
                  b2emp_RF4a$EMPC,b2emp_RF4b$EMPC,b2emp_RF5a$EMPC,b2emp_RF5b$EMPC)
b2emp_MLP1 <- cbind(b2emp_MLP11a$EMPC,b2emp_MLP11b$EMPC,b2emp_MLP12a$EMPC,b2emp_MLP12b$EMPC,b2emp_MLP13a$EMPC,b2emp_MLP13b$EMPC,
                    b2emp_MLP14a$EMPC,b2emp_MLP14b$EMPC,b2emp_MLP15a$EMPC,b2emp_MLP15b$EMPC)
b2emp_MLP3 <- cbind(b2emp_MLP31a$EMPC,b2emp_MLP31b$EMPC,b2emp_MLP32a$EMPC,b2emp_MLP32b$EMPC,b2emp_MLP33a$EMPC,b2emp_MLP33b$EMPC,
                    b2emp_MLP34a$EMPC,b2emp_MLP34b$EMPC,b2emp_MLP35a$EMPC,b2emp_MLP35b$EMPC)
b2emp_MLP5 <- cbind(b2emp_MLP51a$EMPC,b2emp_MLP51b$EMPC,b2emp_MLP52a$EMPC,b2emp_MLP52b$EMPC,b2emp_MLP53a$EMPC,b2emp_MLP53b$EMPC,
                    b2emp_MLP54a$EMPC,b2emp_MLP54b$EMPC,b2emp_MLP55a$EMPC,b2emp_MLP55b$EMPC)
b2emp_DBN1 <- cbind(b2emp_DBN11a$EMPC,b2emp_DBN11b$EMPC,b2emp_DBN12a$EMPC,b2emp_DBN12b$EMPC,b2emp_DBN13a$EMPC,b2emp_DBN13b$EMPC,
                    b2emp_DBN14a$EMPC,b2emp_DBN14b$EMPC,b2emp_DBN15a$EMPC,b2emp_DBN15b$EMPC)
b2emp_DBN3 <- cbind(b2emp_DBN31a$EMPC,b2emp_DBN31b$EMPC,b2emp_DBN32a$EMPC,b2emp_DBN32b$EMPC,b2emp_DBN33a$EMPC,b2emp_DBN33b$EMPC,
                    b2emp_DBN34a$EMPC,b2emp_DBN34b$EMPC,b2emp_DBN35a$EMPC,b2emp_DBN35b$EMPC)
b2emp_DBN5 <- cbind(b2emp_DBN51a$EMPC,b2emp_DBN51b$EMPC,b2emp_DBN52a$EMPC,b2emp_DBN52b$EMPC,b2emp_DBN53a$EMPC,b2emp_DBN53b$EMPC,
                    b2emp_DBN54a$EMPC,b2emp_DBN54b$EMPC,b2emp_DBN55a$EMPC,b2emp_DBN55b$EMPCÃ)
mean(b2emp_log)
mean(b2emp_dt)
mean(b2emp_rf)
mean(b2emp_MLP1)
mean(b2emp_MLP3)
mean(b2emp_MLP5)
mean(b2emp_DBN1)
mean(b2emp_DBN3)
mean(b2emp_DBN5)

