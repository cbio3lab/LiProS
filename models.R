library(caret)
library(dplyr)

#load datasets
dataset <- read.csv('/content/datasets/raw_data.csv')
dataset$cond <- as.factor(dataset$cond)
descs <- read.csv('/content/datasets/descriptors_smiles.csv')

#include experimental descriptors
descs$pKa <- dataset$pKa
descs$delta <- dataset$delta


#split datasets into acids/bases
acids <- dataset[which(dataset$type=='Acid'),]
descsA <- descs[which(dataset$type=='Acid'),]

bases <- dataset[which(dataset$type=='Base'),]
descsB <- descs[which(dataset$type=='Base'),]



#reduction step#
descsA <- descsA[, !apply(descsA, 2, function(x) any(is.na(x)) )] #remove NAs
descsA <- descsA[, !apply( descsA, 2, function(x) length(unique(x)) == 1 )] #remove constant columns

descsB <- descsB[, !apply(descsB, 2, function(x) any(is.na(x)) )] #remove NAs
descsB <- descsB[, !apply( descsB, 2, function(x) length(unique(x)) == 1 )] #remove constant columns



#remove correlated descriptors
r2 <- which(cor(descsA)^2 > .6, arr.ind=TRUE)
r2 <- r2[ r2[,1] > r2[,2] , ]
descsA <- descsA[, -unique(r2[,2])]


r2 <- which(cor(descsB)^2 > .6, arr.ind=TRUE)
r2 <- r2[ r2[,1] > r2[,2] , ]
descsB <- descsB[, -unique(r2[,2])]





#Divide dfs by their respective output value (cond = 0 or cond = 1)
descsA0 <- descsA %>% filter(cond==0) %>% select(-cond)
descsA1 <- descsA %>% filter(cond==1) %>% select(-cond)

descsB0 <- descsB %>% filter(cond==0) %>% select(-cond)
descsB1 <- descsB %>% filter(cond==1) %>% select(-cond)



#Calculate means and sds for each descriptor
meanA <- data.frame(
  'descs' = c(colnames(descsA0))
)
for (i in 1:length(meanA[,1])){
  meanA$mean0[i] = mean(descsA0[,i])
  meanA$mean1[i] = mean(descsA1[,i])
  meanA$diff_m[i] = abs(meanA$mean0[i]-meanA$mean1[i])
  meanA$rsd0[i] = sd(descsA0[,i])/sqrt(length(descsA0[,i]))
  meanA$rsd1[i] = sd(descsA1[,i])/sqrt(length(descsA1[,i]))
  meanA$diff_rsd[i] = sqrt(meanA$rsd0[i]^2+meanA$rsd1[i]^2)
}


meanB <- data.frame(
  'descs' = c(colnames(descsB0))
)
for (i in 1:length(meanB[,1])){
  meanB$mean0[i] = mean(descsB0[,i])
  meanB$mean1[i] = mean(descsB1[,i])
  meanB$diff_m[i] = abs(meanB$mean0[i]-meanB$mean1[i])
  meanB$rsd0[i] = sd(descsB0[,i])/sqrt(length(descsB0[,i]))
  meanB$rsd1[i] = sd(descsB1[,i])/sqrt(length(descsB1[,i]))
  meanB$diff_rsd[i] = sqrt(meanB$rsd0[i]^2+meanB$rsd1[i]^2)
}




#remove descriptors that have uncertainties bigger than the difference between means
meanA <- meanA %>% filter(diff_rsd<diff_m)
meanB <- meanB %>% filter(diff_rsd<diff_m)






#new dataframe with filtered descriptors
acids_d <- data.frame(matrix(NA,
                             nrow = nrow(descsA),
                             ncol = ncol(descsA)))

for (i in 1:length(descsA)){
  for (n in 1:nrow(meanA)){
    if (colnames(descsA)[i] == meanA$descs[n]) {
      acids_d[,i] <- descsA[,i]
    }
  }
}
acids_d <- acids_d %>% select_if(~ !any(is.na(.)))
colnames(acids_d) <- meanA$descs
acids_d$cond <- acids$cond





bases_d <- data.frame(matrix(NA,
                             nrow = nrow(descsB),
                             ncol = ncol(descsB)))

for (i in 1:length(descsB)){
  for (n in 1:nrow(meanB)){
    if (colnames(descsB)[i] == meanB$descs[n]) {
      bases_d[,i] <- descsB[,i]
    }
  }
}
bases_d <- bases_d %>% select_if(~ !any(is.na(.)))
colnames(bases_d) <- meanB$descs
bases_d$cond <- bases$cond


#_______________________________WELCH'S T-TEST_________________________________
#acids
pvalue_a <- c()
for (i in 1:(ncol(acids_d)-1)){
  pvalue_a <- c(pvalue_a,t.test(ifelse(acids_d$cond==1,acids_d[,i],NA),ifelse(acids_d$cond==0,acids_d[,i],NA), alternative = 'two.sided')[[3]])
}
meanA$welchs_p <- pvalue_a
meanA <- meanA %>% filter(welchs_p<0.05)


acids_d <- data.frame(matrix(NA,
                             nrow = nrow(descsA),
                             ncol = ncol(descsA)))

for (i in 1:length(descsA)){
  for (n in 1:nrow(meanA)){
    if (colnames(descsA)[i] == meanA$descs[n]) {
      acids_d[,i] <- descsA[,i]
    }
  }
}
acids_d <- acids_d %>% select_if(~ !any(is.na(.)))
colnames(acids_d) <- meanA$descs
acids_d$cond <- acids$cond




#bases
pvalue_b <- c()
for (i in 1:(ncol(bases_d)-1)){
  pvalue_b <- c(pvalue_b,t.test(ifelse(bases_d[,ncol(bases_d)]==1,bases_d[,i],NA),ifelse(bases_d[,ncol(bases_d)]==0,bases_d[,i],NA))[[3]])
}
meanB$welchs_p <- pvalue_b
meanB <- meanB %>% filter(welchs_p<0.05)

bases_d <- data.frame(matrix(NA,
                             nrow = nrow(descsB),
                             ncol = ncol(descsB)))

for (i in 1:length(descsB)){
  for (n in 1:nrow(meanB)){
    if (colnames(descsB)[i] == meanB$descs[n]) {
      bases_d[,i] <- descsB[,i]
    }
  }
}
bases_d <- bases_d %>% select_if(~ !any(is.na(.)))
colnames(bases_d) <- meanB$descs
bases_d$cond <- bases$cond

#_____________END WELCHS T-TEST______________________________





#------------------------------------------------------------


##DATA SAMPLONG
library(Metrics)


#training and test set
set.seed(1234)
sample.index <- sample(1:nrow(acids_d),nrow(acids_d)*0.8,replace=FALSE) #sample indexes
training.set_A <- acids_d[sample.index,]
test.set_A <- acids_d[-sample.index,]

sample.index <- sample(1:nrow(bases_d),nrow(bases_d)*0.8,replace=FALSE)
training.set_B <- bases_d[sample.index,]
test.set_B <- bases_d[-sample.index,]



#--------------LOGISTIC REGRESSION-----------------------------------

model_LR_A <- train(cond~.,data=training.set_A,method='glm',family='binomial')
saveRDS(model_LR_A,'model_LR_A.rds')

model_LR_B <- train(cond~.,data=training.set_B,method='glm',family='binomial')
saveRDS(model_LR_B,'model_LR_B.rds')
#--------RANDOM FOREST----------------------
library(randomForest)

#tune hyperparameters
mtry <- tuneRF(training.set_A[-ncol(training.set_A)],training.set_A$cond,
               stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
model_RF_A <- randomForest(cond~.,data=training.set_A,mtry=best.m,importance=TRUE)
saveRDS(model_RF_A,'model_RF_A.rds')





mtry <- tuneRF(training.set_B[-ncol(training.set_B)],training.set_B$cond,
               stepFactor=1.2,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
model_RF_B <- randomForest(cond~.,data=training.set_B,mtry=best.m,importance=TRUE)
saveRDS(model_RF_B,'model_RF_B.rds')

#--------SVM----------
library(e1071)

#-------linear kernel----------------------
###MODELOS PARA LOS ACIDOS

model_SVML_A <- svm(cond~.,data=training.set_A, type='C-classification',kernel="linear")
saveRDS(model_SVML_A,'model_SVML_A.rds')
                          
model_SVML_B <- svm(cond~.,data=training.set_B, type='C-classification',kernel="linear")
saveRDS(model_SVML_B,'model_SVML_B.rds')







#evaluation of models
test.set_A$LR <- predict(model_LR_A,test.set_A)
test.set_A$RF <- predict(model_RF_A,test.set_A)
test.set_A$SVML <- predict(model_SVML_A,test.set_A)


test.set_B$LR <- predict(model_LR_B,test.set_B)
test.set_B$RF <- predict(model_RF_B,test.set_B)
test.set_B$SVML <- predict(model_SVML_B,test.set_B)
