charity <- read.csv("/Users/raymondcarl/Dropbox/STAT 897D Team Project/charity.csv")


# Helper functions

# Make predictions from regsusets object
# https://github.com/yufree/democode/blob/master/rml/predict.regsubsets.R
predict.regsubsets = function(object, newdata, id, ...) {
  form  <-  as.formula(~.)
  mat  <-  model.matrix(form, newdata)
  coefi  <-  coef(object, id)
  xvars  <-  names(coefi)
  mat[, xvars] %*% coefi
}

#Center the variables
#http://gastonsanchez.com/blog/how-to/2014/01/15/Center-data-in-R.html
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}




# predictor transformations

charity.t <- charity
charity.t$avhv_log <- log(charity.t$avhv)
charity.t$agif_log <- log(charity.t$agif)
charity.t$tgif_log <- log(charity.t$tgif)
charity.t$inca_log <- log(charity.t$inca)
charity.t$incm_log <- log(charity.t$incm)
charity.t$lgif_log <- log(charity.t$lgif)
charity.t$rgif_log <- log(charity.t$rgif)
#charity.t$MnthsTilNextDon <- charity.t$tlag-charity.t$tdon
#charity.t$freqdon <- (charity.t$tgif/charity.t$agif)/charity.t$npro
charity.t$hincsq <- (charity.t$hinc^2)
charity.t$agifsq <- (charity.t$agif^2)
charity.t$chldsq <- (charity.t$chld^2)
charity.t$wratsq <- (charity.t$wrat^2)
charity.t$wratcu <- (charity.t$wrat^3)
charity.t$plowsq <- (charity.t$plow^2)
charity.t$plowcu <- (charity.t$plow^3)

# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,c(2:21,25:38)]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,c(2:21,25:38)]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,c(2:21,25:38)]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)



