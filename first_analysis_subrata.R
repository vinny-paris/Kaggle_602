library(tidyverse)

## Suggestion 1: traindat or train_dat because 'train' is already a function name

# setwd("~/R/Git/Kaggle_602/")
train_dat <- read.csv('./data/Queens Training Set.csv') %>% select(- X)

names(train_dat)
(n <- nrow(train_dat))
(p <- ncol(train_dat))

# BOROUGH has only 4 as input??? remove it:
# EASE.MENT all NA
train_dat$BOROUGH <- NULL
train_dat$EASE.MENT <- NULL

hist(train_dat$SALE.PRICE)  ## Too much skewed
hist(log(train_dat$SALE.PRICE+1))

train_dat$SALE.PRICE <- log(train_dat$SALE.PRICE+1)
train_dat$LAND.SQUARE.FEET <- log(train_dat$LAND.SQUARE.FEET+1)
train_dat$GROSS.SQUARE.FEET <- log(train_dat$GROSS.SQUARE.FEET+1)



## Pairplot:
(numeric_var <- unlist(lapply(train_dat, is.numeric)))
# plot(train_dat[,numeric_var], cex=0.7, pch=16)



## Blocks: We need to know more about how blocks and lots are distributed
## Block is kind of uniformly distributed(?) on it's range. 
## LOT is not, log(LOT+1) is more Gaussian looking.
hist(train_dat$BLOCK)
hist(train_dat$LOT)
hist(log(train_dat$LOT+1))

## 0 Residential units possibly means Studio type apartments






## Randomly select a small potion of the data:
set.seed(100)
small_train_dat <- train_dat[sample(n, 1000),]

small_train_dat_cov <- small_train_dat
small_train_dat_cov$SALE.PRICE <- NULL




## Blind Multi-dimensional scaling:
d <- dist(small_train_dat_cov) # euclidean distances between the rows
fit2 <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
plot(fit2$eig[1:20])
fit2$GOF

fit3 <- cmdscale(d, eig=TRUE, k=3) # k is the number of dim
plot(fit3$eig[1:20])
fit3$GOF

# GOF: a numeric vector of length 2, equal to say (g.1,g.2), 
# where g.i = (sum{j=1..k} lambda[j]) / (sum{j=1..n} T.i(lambda[j])), where lambda[j] are the eigenvalues (sorted in decreasing order), T.1(v) = abs(v), and T.2(v) = max(v, 0)



# plot solution
x <- fit2$points[,1]
y <- fit2$points[,2]

plot(x, y, xlab="Coord 1", ylab="Coord 2", main="Metric MDS", 
     cex=0.2)
text(x, y)

## So, it seems that there are some outlier problems for some data. 

# 
# small_train_dat_cov[180,]
# small_train_dat_cov[181,]
# summary(as.matrix(d)[180,])
# summary(as.matrix(d)[181,])
# 
# 
# ### 
# 
# d_2 <- dist(small_train_dat_cov[180:185,])
# as.matrix(d_2)
# fit2 <- cmdscale(d_2, eig=TRUE, k=2)
# fit2$GOF
# 
# 
# # plot solution
# x <- fit2$points[,1]
# y <- fit2$points[,2]
# plot(x, y, xlab="Coord 1", ylab="Coord 2", main="Metric MDS", type="n")
# text(x, y)
# 
# small_train_dat[180:185,]




######### Refreshed after taking log of the sq.ft.

x <- fit2$points[,1]
y <- fit2$points[,2]
hist(small_train_dat$SALE.PRICE)
max(small_train_dat$SALE.PRICE)
plot(x, y, xlab="Coord 1", ylab="Coord 2", main="Metric MDS", 
     col=rgb(0.1,exp(small_train_dat$SALE.PRICE)/exp(16),0.2),
     pch=16)

library(umap)
abc <- umap(as.matrix(d))
# str(abc)
plot(abc$layout,
     col=rgb(0.1,exp(small_train_dat$SALE.PRICE)/exp(16),0.1),
     pch=16)

