

train<- read.csv('./data/Queens Training Set.csv') %>% select(- X)
test <- read.csv('./data/Queens Test Set.csv') %>% select(- X)
id<-test[ , 1]

##########################################################################################################
##########################################################################################################

# REMEMBER TO READ THE TRAIN AND TEST SET FIRST AND
# REMEMBER TO APPLY THE MANIPULATION ON TRAIN,  
# AND TEST SET TO HAVE THE SAME TYPE OF DATA


##########################################################################################################
##########################################################################################################

#Predict on test
pred<-predict(lmtune, df.test) #df.test is the manipulated df of test 
pred<-exp(pred)
solution<-data.frame("INDEX"=id,"PRICE"= pred)
write.csv(solution, 
          paste0("../submission/solution", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"), 
          row.names = FALSE)
