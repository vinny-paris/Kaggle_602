library(dplyr)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(stringr)

rm(list=ls())

traindat <- read.csv("../data/train_cl.csv")
testdat <- read.csv("../data/test_cl.csv")



traindat <- traindat %>% 
 mutate(BUILDING.CLASS.CATEGORY = as.factor(BUILDING.CLASS.CATEGORY),
         BUILDING.CLASS.AT.PRESENT = as.factor(BUILDING.CLASS.AT.PRESENT),
         BUILDING.CLASS.AT.TIME.OF.SALE = as.factor(BUILDING.CLASS.AT.TIME.OF.SALE), 
         TAX.CLASS.AT.PRESENT = as.factor(TAX.CLASS.AT.PRESENT), 
         TAX.CLASS.AT.TIME.OF.SALE = as.factor(TAX.CLASS.AT.TIME.OF.SALE), 
         LAND.SQUARE.FEET = as.numeric(as.factor(LAND.SQUARE.FEET)), 
         GROSS.SQUARE.FEET = as.numeric(as.factor(GROSS.SQUARE.FEET)),
         BUILDING.CLASS.AT.TIME.OF.SALE = stringr::str_trim(BUILDING.CLASS.AT.TIME.OF.SALE)
        ) 




testdat <- testdat %>% 
 mutate(BUILDING.CLASS.CATEGORY = as.factor(str_trim(BUILDING.CLASS.CATEGORY)),
         BUILDING.CLASS.AT.PRESENT = as.factor(BUILDING.CLASS.AT.PRESENT),
         BUILDING.CLASS.AT.TIME.OF.SALE = as.factor(str_trim(BUILDING.CLASS.AT.TIME.OF.SALE)), 
         TAX.CLASS.AT.PRESENT = as.factor(TAX.CLASS.AT.PRESENT), 
         TAX.CLASS.AT.TIME.OF.SALE = as.factor(TAX.CLASS.AT.TIME.OF.SALE), 
         LAND.SQUARE.FEET = as.numeric(as.factor(LAND.SQUARE.FEET)), 
         GROSS.SQUARE.FEET = as.numeric(as.factor(GROSS.SQUARE.FEET))
        ) 





str(traindat)
str(testdat)

id <- testdat$INDEX

rm.var <- c()
## NBD, BUILDING.CLASS.CATEGORY has a new level in test data

df.train <- traindat %>% select(-all_of(rm.var) )
df.test <- testdat %>% select(-all_of(rm.var))

colSums(is.na(df.train))

# df.train.no.NA <- df.train %>% select(-c("TAX.CLASS.AT.PRESENT",
#                                    "BUILDING.CLASS.AT.PRESENT",
#                                    "YEAR.BUILT",
#                                    "year_cutoff"))

df.train.no.NA <- df.train %>% select(-c("YEAR.BUILT",
                                         "year_cutoff"))








ctrl <- trainControl(method="LOOCV", savePredictions='final')
ctrl_2 <- trainControl(method="cv", number=3, savePredictions='final')
ctrl_3 <- trainControl(method="repeatedcv", number=6, repeats = 5, 
                       savePredictions='final')

ctrl_3 <- ctrl_2						## COMMENT IT

library(doParallel)
cl <- makeForkCluster(15)
registerDoParallel(cl)






## See these models also
# gaussprRadial, gam, gamLoess, gamSpline -- for base
# gamboost, glmboost, BstLm  -- for ensemble

models_used <- list(gbm = caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees=(1:8)*100, interaction.depth=1:5, shrinkage=0.075, n.minobsinnode=10)),
                   svmRadial  = caretModelSpec(method="svmRadial"),
                   ranger = caretModelSpec(method="ranger"),
                   earth = caretModelSpec(method="earth"),
                   rpart = caretModelSpec(method="rpart", tuneGrid=expand.grid(cp=c(3e-5, 1e-4, 3e-4, 1e-3, 0.0034, 0.0103, 0.03082, 0.1099, 0.2125))),
                   knn = caretModelSpec(method="knn", tuneGrid=expand.grid(k=(1:10)*3)),
                   nnet = caretModelSpec(method="nnet")
)


#models_used <- list(gaussprradial = caretModelSpec(method="gaussprRadial"),
#                    gam = caretModelSpec(method="gam"),
#                    gamLoess = caretModelSpec(method="gamLoess"),
#                    gamSpline = caretModelSpec(method="gamSpline"),
#                    nnet = caretModelSpec(method="nnet")
#)






models <- caretList(SALE.PRICE ~ ., data = df.train.no.NA, trControl = ctrl_3, 
                    preProc=c("center", "scale"), tuneList = models_used,
                    na.action='na.pass')



stack.glm <- caretStack(models, method="glm", metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=traincontrol_used )
stack.rf <- caretStack(models, method="rf", metric="ROC",
                       preProc=c("center", "scale"),
                       trControl=traincontrol_used )






models

stack.glm
stack.rf





setwd("..")

pred_all<- predict(models, df.test)
pred_all <- exp(pred)-1
cor(models)



pred<- predict(stack.glm, df.test)
pred<- exp(pred)-1
solution <- data.frame("INDEX"=id,"PRICE"= pred)
write.csv(solution, 
          paste0("./submission/solution", 
                 format(Sys.time(), "%d-%b-%Y %H.%M"), "_stack_glm.csv"), 
          row.names = FALSE)


pred<- predict(stack.rf, df.test)
pred<- exp(pred)-1
solution <- data.frame("INDEX"=id,"PRICE"= pred)
write.csv(solution,
          paste0("./submission/solution",
                 format(Sys.time(), "%d-%b-%Y %H.%M"), "_stack_glm.csv"),
          row.names = FALSE)
