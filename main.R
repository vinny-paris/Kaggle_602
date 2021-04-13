

train<- read.csv('./data/Queens Training Set.csv') %>% select(- X)
test <- read.csv('./data/Queens Test Set.csv') %>% select(- X)
id<-test[ , 1]


# creating partitions
set.seed(1)
ind<-createDataPartition(data$SALE.PRICE,times = 1,p = 0.9,list = FALSE)
df.train <- data[ind, ]
df.validation <- data[-ind, ]
write.csv(df.train, "../data/df.train.csv")
write.csv(df.validation, "../data/df.validation.csv")



##########################################################################################################
##########################################################################################################

# REMEMBER TO READ THE TRAIN AND TEST SET FIRST AND
# REMEMBER TO APPLY THE MANIPULATION ON TRAIN, DF.TRAIN, DF.VALIDATION 
# AND TEST SET TO HAVE THE SAME TYPE OF DATA


##########################################################################################################
##########################################################################################################






#Evaluate on 10% as the validation set
y_true=df.validation$SALE.PRICE #df.validation is the manipulated df of validation set
y_pred<-predict(lmtune, df.validation)
sqrt(mean((log(y_true+1)-log(exp(y_pred)+1))^2)) #or rmsle(y_true,exp(y_pred))




#Predict on test
pred<-predict(lmtune, df.test) #df.test is the manipulated df of test 
pred<-exp(pred)
solution<-data.frame("INDEX"=id,"PRICE"= pred)
write.csv(solution, 
          paste0("../submission/solution", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"), 
          row.names = FALSE)
