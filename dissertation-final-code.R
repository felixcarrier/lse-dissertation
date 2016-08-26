#---------------------------
# Load functions and data
#---------------------------

source("functions.R")
reset.df()


# plot of the data
plot(x,y, pch=16, col=tgrey(0.4), xlab="2016 day of year", ylab="% support for Trump"
     , ylim=c(min(y)-0.02, max(y)+0.02)
     ,cex=2)

summary(df)
#---------------------------
# Parameter selection
#---------------------------

# One iteration of parameter selection
set.seed(3)
llcval <- llcv(x,y,1:30, n.folds=10)

plot(llcval)
abline(h=llcval$cv$rmse[llcval$cv$best.bandwidth], col=2, lty=2)
text(25,0.0445
     , paste("Lowest RMSE \n is ", round(llcval$cv$rmse[llcval$cv$best.bandwidth], 5), sep="")
     , cex=0.8)


### Repeat the cross-validation 100 times
### and check best bandwidth (bw)
set.seed(2)
winner.bw <- replicate(100, llcv(x,y,1:30, n.folds=10)$cv$best.bandwidth)
table(winner.bw)

hist(winner.bw, breaks=1:20, xaxt = "n", labels = TRUE, col="gray", xlab="Bandwidth", main="", ylim=c(0,27))
axis(side = 1, at = (1:20)-0.5, labels=1:20)

#  3(24%) and 15(25%) win. Generate plot:
reset.df()
ll3 <- llreg(x,y,bandwidth = 3)
ll15 <- llreg(x,y,bandwidth = 15)

plot(x,y, pch=16, col=tgrey(0.2), xlab="2016 day of year", ylab="% support for Trump", cex=2
     , ylim=c(min(y)-0.07, max(y)+0.02))
lines(ll3, col=1, lwd=3 )#, lty=2)
lines(ll15, col=2, lwd=3 )#, lty=4)
legend("bottomright"
       ,legend=c( "bw 3","bw 15")
       ,lwd=3
       #,lty=c(2,4)
       ,col=1:2
       ,cex=0.7
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)


### Comparing density
set.seed(1)
times = 1000
RMSEs.bw3  <- replicate(times, llcv(x,y,3 , n.folds=10)$cv$rmse)
RMSEs.bw14 <- replicate(times, llcv(x,y,14, n.folds=10)$cv$rmse)
RMSEs.bw15 <- replicate(times, llcv(x,y,15, n.folds=10)$cv$rmse)
RMSEs.bw16 <- replicate(times, llcv(x,y,16, n.folds=10)$cv$rmse)

density.plot(list( bw3  = RMSEs.bw3
                  , bw15 = RMSEs.bw15
                  ,bw14 = RMSEs.bw14
                  ,bw16 = RMSEs.bw16 
                  )
            # , xlim=c(0.040,0.048)
             , xlab="RMSE"
             , ylim=c(0,1300)
             #, ylim=c(0,1000)
             , yaxt="n"
             , legend.cex = 0.7
             , main=""
             )
text(0.0442, 400, "bw3", cex=1.1)
text(0.0421, 1150, "bw15", cex=1.1, col=2)
text(0.0436, 1200, "bw14", cex=1.1, col=3)
text(0.0436, 1050, "bw16", cex=1.1, col=4)

abline(v=median(RMSEs.bw6))
abline(v=median(RMSEs.nonneg.sum1), col=2)


# comparing means and stdev of bw 3, 14, 15 and 16
list <- list(RMSEs.bw3, RMSEs.bw14, RMSEs.bw15, RMSEs.bw16)

for(i in list) print(mean(i))
for(i in list) print(sd(i))




# is mean of RMSEs.bw15 significantly smaller than mean of RMSEs.3 ?
# 2 sample t-test

# check equal variance

var.test(RMSEs.bw15, RMSEs.bw3)

# pvalue < 0.000, so CANNOT assume that variance are homogeneous

t.test(RMSEs.bw15, RMSEs.bw3, var.equal=FALSE, paired=FALSE)

# Welch 2 sample t-test
# difference is significant: 95% CI is [0.048%, 0.063%] higher for bw3
# means:   bw15 = 4.289 %     bw3 = 4.345 %





#---------------------------
# Comparison with non-regularized linear stacking
#---------------------------
set.seed(1)
test.glmcv <- glmcv(x,y)

test.glmcv$rmse



# clearly this is too big
# After investigation, some values are outside of the range of y

bool <- test.glmcv$data$y.fitted > max(y) | test.glmcv$data$y.fitted < min(y)
test.glmcv$data$y.fitted[bool]

# After further investigation, the coefficient are huge (+ or -)!
options(scipen = 999)
round(test.glmcv$fold.coefficients, 2)

# sum of coefficients
colSums(test.glmcv$fold.coefficients)

# #check fold of obs #1
# exdf[1,"fold.assign"]
# # fold 3, so leave that fold out and fit glm on folds 1-2U4-10
# tempreg <- glm(y~. - fold.assign, data=exdf[exdf$fold.assign != 3,])
# 
# options(scipen = 999)
# data.frame(coefficient = round(unlist(tempreg$coefficients), 2) )
# 
# # generate fitted value for obs in fold 3 (including obs#1)
# predict(tempreg, newdata= exdf[exdf$fold.assign == 3,])

# density
set.seed(1)
RMSEs.ols <- replicate(1000, glmcv(x,y)$rmse)

density.plot(list.of.arrays = list(OLS = RMSEs.ols))
hist(RMSEs.ols, breaks=30, col=tgrey(0.5), main=""
     #, ylim=c(0,250)
     )

quantile(RMSEs.ols, c(0.125, 0.875))  # mid 75%
quantile(RMSEs.ols, c(0.10, 0.90))  # mid 80%
range(y)




#---------------------------
# Comparison with (ridge and lasso) linear stacking
#---------------------------
# 
# library(MASS)
# tempreg <- lm.ridge(y~. - fold.assign, data=exdf[exdf$fold.assign != 3,], lambda = seq(0, .1, .001))
# lambda <- tempreg$GCV[which.min(tempreg$GCV)] 
# coef.lambda <- tempreg$coef[colnames(tempreg$coef) == names(lambda)]
# 
# options(scipen = 999)
# data.frame(bw = row.names(tempreg$coef), coefficient = round(unlist(coef.lambda), 4) )
# 
# # generate fitted value for obs in fold 3 (including obs#1)
# predict(tempreg, newdata= exdf[exdf$fold.assign == 3,])
# NO PREDICT method for lm.rigde

#****


library(glmnet)

set.seed(1)
reset.df()
lambda.min <- cv.glmnet(x= as.matrix(exdf[,1:29]), y=exdf$y)$lambda.min
tempreg.glmnet <- glmnet(  x = as.matrix(exdf[exdf$fold.assign != 3 ,1:29])
                          , y = exdf[exdf$fold.assign != 3, "y"]
                          , lambda=lambda.min
                          , alpha=0) #if =0 then ridge only. if =1 then lasso only


# generate fitted value for obs in fold 3 (including obs#1)
predict(tempreg.glmnet, newx= as.matrix(exdf[exdf$fold.assign == 3,1:29]) ) 

# coefficients
tempreg.glmnet$beta



### RIDGE
set.seed(1)
reset.df()
ridge.rep <- replicate(1000, elasticnetcv(x,y, n.folds = 10, alpha = 0))
RMSEs.ridge <- unlist(ridge.rep["rmse",])
ridge.lambda <- unlist(ridge.rep["lambda",])

#hist of lambdas
hist(ridge.lambda, xlim=c(0,0.0010), breaks=1000, col=tgrey(0.5))
abline(v=median(ridge.lambda), col=2)
text(x=median(ridge.lambda), y=60, "median", col=2)
abline(v=mean(ridge.lambda), col=4)
text(x=mean(ridge.lambda), y=70, "mean", col=4)


# show example of coefs
set.seed(11)
reset.df()
temp.ridge <- elasticnetcv(x,y, alpha = 0)

round(temp.ridge$fold.coefficients, 3)
temp.ridge$rmse
temp.ridge[c("rmse","lambda")]


# density plot
density.plot( list(bw3=RMSEs.bw3,bw15=RMSEs.bw15,  ridge=RMSEs.ridge)
              , xlim=c(0.0380, 0.048)
              , ylim=c(0,1300)
              , legend.cex = 0.8
              , main=""
              , xlab="RMSE"
              , yaxt="n")
abline(v=mean(RMSEs.bw15), col=2)
abline(v=mean(RMSEs.ridge), col=3)


mean(RMSEs.bw3)
sd(RMSEs.bw3)

mean(RMSEs.ridge)
sd(RMSEs.ridge)


# check equal variance
var.test(RMSEs.bw15, RMSEs.ridge)
# pvalue < 0.000, so CANNOT assume that variance are homogeneous
t.test(RMSEs.bw15, RMSEs.ridge, var.equal=FALSE, paired=FALSE)


### LASSO
set.seed(1)
reset.df()
lasso.rep <- replicate(1000, elasticnetcv(x,y , n.folds = 10, alpha = 1))
RMSEs.lasso <- unlist(lasso.rep["rmse",])
lasso.lambda <- unlist(lasso.rep["lambda",])


# example of coef
set.seed(2)
temp.lasso <- elasticnetcv(x,y, alpha = 1)

round(temp.lasso$fold.coefficients, 3)
temp.lasso$rmse



# plot
list.RMSEs <- list(bw3=RMSEs.bw3,bw15=RMSEs.bw15,  ridge=RMSEs.ridge, lasso=RMSEs.lasso) 

density.plot( list.RMSEs
              , xlim=c(0.0380, 0.048)
             , ylim=c(0,1300)
             , legend.cex = 0.8
             , main=""
             , xlab="RMSE"
             , yaxt="n")

for (i in 1:length(list.RMSEs)) abline(v=mean(list.RMSEs[[i]]), col=i, lty=2, lwd=2)
# 
# abline(v=mean(RMSEs.bw3), col=1, lty=2, lwd=2)
# abline(v=mean(RMSEs.bw15), col=2, lty=2, lwd=2)
# abline(v=mean(RMSEs.ridge), col=3, lty=2, lwd=2)
# abline(v=mean(RMSEs.lasso), col=4, lty=2, lwd=2)
# abline(h=0)

mean(RMSEs.lasso)
sd(RMSEs.lasso)


## compare RIDGE and LASSO

# check equal variance
var.test(RMSEs.ridge, RMSEs.lasso)
# pvalue > 0.05, so CAN assume that variance are homogeneous
options(scipen = 999)
t.test(RMSEs.ridge, RMSEs.lasso, var.equal=TRUE, paired=FALSE)



RMSEs <- data.frame(bw3=RMSEs.bw3,bw15=RMSEs.bw15,  ridge=RMSEs.ridge, lasso=RMSEs.lasso)
round(colMeans(RMSEs), 5)



#---------------------------
# Comparison with regularized linear stacking
#---------------------------

######### Non-negative  AND  sum to one
set.seed(1)
RMSEs.nonneg.sum1 <- replicate(1000, lrm.cv(x,y, type=2)$rmse)

# density plot
plot(density(RMSEs.nonneg.sum1))

# showing examples of coefficient
set.seed(1)
temp.reg <- lrm.cv(x,y,type=2)

# some.coef <- temp.reg$fold.coefficients[,1]
# some.coef2 <- temp.reg$fold.coefficients[,2]
# some.coef3 <- temp.reg$fold.coefficients[,5]
# 
# 
# plot(x=2:30,y=round(some.coef, 4)
#      , type="h"
#      , lwd=7
#      , col=ifelse(some.coef>0, "green", 1)
#      #, lty=ifelse(test>0, 4, 1)
#      , ylab="Coefficient"
#      , xlab="Bandwidth")
# mtext("For each fold, sum of coefficients is 1", cex = 0.8)
# 
# points(x=2:30,y=round(some.coef2, 4)
#        , type="h"
#        , lwd=7
#        , col=ifelse(some.coef2>0, "blue", 1)
#        )
# 
# points(x=2:30,y=round(some.coef3, 4)
#        , type="h"
#        , lwd=7
#        , col=ifelse(some.coef3>0, "red", 1)
# )
# 
# legend("topleft"
#        , legend=c("Fold 1", "Fold 2", "Fold 5")
#        , fill =c("green", "blue", "red")
#        , horiz=TRUE
#        , cex=0.7
#        , bg=tgrey(0.2))

# show table in latex
round( temp.reg$fold.coefficients, 3)


# # density plot 
# list.RMSEs <- list(bw3=rep(-10, 1000)
#                     ,bw15=rep(-10, 1000)
#                     ,ridge = RMSEs.ridge
#                    , lasso = rep(-10, 1000)
#                    , non.neg.sum.1=RMSEs.nonneg.sum1
#                    ) 
# 
# density.plot( list.RMSEs
#               , xlim=c(0.0380, 0.048)
#               , ylim=c(0,700)
#               , legend.cex = 0.6
#               , main=""
#               , xlab="RMSE"
#               , yaxt="n")
# abline(v=mean(RMSEs.bw3), col=1, lty=2, lwd=2)
# abline(v=mean(RMSEs.bw15), col=2, lty=2, lwd=2)
# abline(v=mean(RMSEs.lasso), col=4, lty=2, lwd=2)
# 
# text(x=0.04375, y=700, "bw3", col=1)
# text(x=0.0432, y=650, "bw15", col=2)
# text(x=0.0425, y=700, "lasso", col=4)
# 
# abline(v=mean(RMSEs.bw15), col=2, lty=2, lwd=2)
# #abline(v=mean(RMSEs.ridge), col=3, lty=2, lwd=2)
# abline(v=mean(RMSEs.lasso), col=4, lty=2, lwd=2)



# density plot
plot(density(RMSEs.ridge), col=3, lwd=3
     , xlim=c(0.0380, 0.048)
     , ylim=c(0,1300)
     , main=""
     , xlab="RMSE"
     , yaxt="n")
lines(density(RMSEs.bw15), col=2, lwd=3)
lines(density(RMSEs.nonneg.sum1), col=5, lwd=3)


shadebands( x  = density(RMSEs.bw15)$x
            ,lo = rep(0, length(density(RMSEs.bw15)$x))
            ,hi = density(RMSEs.bw15)$y
            , col=col2t(2)
)
shadebands( x  = density(RMSEs.ridge)$x
            ,lo = rep(0, length(density(RMSEs.ridge)$x))
            ,hi = density(RMSEs.ridge)$y
            ,col = col2t(3)
)

shadebands( x  = density(RMSEs.nonneg.sum1)$x
            ,lo = rep(0, length(density(RMSEs.nonneg.sum1)$x))
            ,hi = density(RMSEs.nonneg.sum1)$y
            , col=col2t(5)
)

legend("topright"
       , legend = c("bw15", "ridge", "non.neg.sum.1")
       , fill = c(2,3,5)
       #, lwd=2
       #, lty=1:length(list.of.arrays)
       #, col=1:length(list.of.arrays)
       , cex=1
       , horiz = FALSE
       , ncol = 1
)




round(mean(RMSEs.nonneg.sum1), 5)
round(sd(RMSEs.nonneg.sum1), 5)



# for (i in 1:length(list.RMSEs)) abline(v=mean(list.RMSEs[[i]]), col=i, lty=2, lwd=2)
# 
# abline(v=mean(RMSEs.bw3), col=1, lty=2, lwd=2)
# abline(v=mean(RMSEs.bw15), col=2, lty=2, lwd=2)
# abline(v=mean(RMSEs.nonneg.sum1), col=3, lty=2, lwd=2)
# abline(h=0)


# check equal variance
var.test(RMSEs.bw15, RMSEs.nonneg.sum1)
# pvalue > 0.05, so CAN assume that variance are homogeneous
options(scipen = 999)
t.test(RMSEs.bw15, RMSEs.nonneg.sum1, var.equal=FALSE, paired=FALSE)






######### Non-negative  only
set.seed(1)
reset.df()
RMSEs.nonneg <- replicate(1000, lrm.cv(x,y, type=2, sum.to.one = FALSE)$rmse)

# density plot
plot(density(RMSEs.nonneg))

# showing examples of coefficient
set.seed(1)
temp.reg2 <- lrm.cv(x,y,type=2, sum.to.one = FALSE)

# show table in latex
round( temp.reg2$fold.coefficients, 3)
colSums( round( temp.reg2$fold.coefficients, 3) )

# trying to understand why is performs worst than nonneg.sum1
set.seed(2)
temp.reg2 <- lrm.cv(x,y,type=2, sum.to.one = FALSE)

any(temp.reg2$data$y.fitted > max(y) | temp.reg2$data$y.fitted < min(y))

plot(x,temp.reg2$data$y.fitted, pch=19, col=tgrey(0.4))
lines(ll15, col=2)


# # show sum of coefficients for 1 example
# set.seed(1)
# hist(colSums(  lrm.cv(x,y, type=2, sum.to.one = FALSE)$fold.coefficients  )
#      , main=""
#      , xlab="Sum of coefficients"
#      , breaks=10
#      , yaxp  = c(0, 3, 3)
#      , xaxp  = c(0.990, 1.015, 5)
#      , xlim=c(0.990, 1.015)
#      , col=tgrey(0.4))


# # density plot 
# list.RMSEs <- list(bw3=RMSEs.bw3,bw15=RMSEs.bw15,  non.neg.sum.1=RMSEs.nonneg.sum1, non.neg=RMSEs.nonneg) 
# 
# density.plot( list.RMSEs
#               , xlim=c(0.0410, 0.057)
#               , ylim=c(0,1300)
#               , legend.cex = 0.6
#               , main=""
#               , xlab="RMSE"
#               , yaxt="n"
#               , legend.ncol = 2)


# density plot
plot(density(RMSEs.ridge), col=col2t(3, 200), lwd=3
     , xlim=c(0.0380, 0.048)
     , ylim=c(0,1300)
     , main=""
     , xlab="RMSE"
     , yaxt="n")
lines(density(RMSEs.bw15), col=col2t(2,200), lwd=3)
lines(density(RMSEs.nonneg.sum1), col=col2t(5,200), lwd=3)
lines(density(RMSEs.nonneg), col=6, lwd=3)


shadebands( x  = density(RMSEs.bw15)$x
            ,lo = rep(0, length(density(RMSEs.bw15)$x))
            ,hi = density(RMSEs.bw15)$y
            , col=col2t(2,90)
)
shadebands( x  = density(RMSEs.ridge)$x
            ,lo = rep(0, length(density(RMSEs.ridge)$x))
            ,hi = density(RMSEs.ridge)$y
            ,col = col2t(3,90)
)

shadebands( x  = density(RMSEs.nonneg.sum1)$x
            ,lo = rep(0, length(density(RMSEs.nonneg.sum1)$x))
            ,hi = density(RMSEs.nonneg.sum1)$y
            , col=col2t(5,90)
)

shadebands( x  = density(RMSEs.nonneg)$x
            ,lo = rep(0, length(density(RMSEs.nonneg)$x))
            ,hi = density(RMSEs.nonneg)$y
            , col=col2t(6,150)
)

legend("topright"
       , legend = c("bw15", "ridge", "non.neg.sum.1", "non.neg")
       , fill = c(2,3,5,6)
       #, lwd=2
       #, lty=1:length(list.of.arrays)
       #, col=1:length(list.of.arrays)
       , cex=0.8
       , horiz = FALSE
       , ncol = 1
)




round(mean(RMSEs.nonneg), 5)
round(sd(RMSEs.nonneg), 5)



#### check nonneg.sum1 vs nonneg
# check equal variance
var.test(RMSEs.nonneg.sum1, RMSEs.nonneg)
# pvalue > 0.05, so CAN assume that variance are homogeneous
options(scipen = 999)
t.test(RMSEs.nonneg.sum1, RMSEs.nonneg, var.equal=TRUE, paired=FALSE)





####################
# Additive models
####################

reset.df()
set.seed(1)
#8 terms: takes about 1h
#1 term with interaction: takes about 7 min
#test.gam <- replicate(1000, gamcv(x,y)$rmse)   #s(2,15)
#test.gam2.21 <- replicate(1000, gamcv(x,y, bandwidths = c(1,2,21))$rmse)   #s(2,21)
#RMSEs.gam.3.15 <- replicate(1000, gamcv(x,y, bandwidths = c(1,3,15))$rmse)   #s(3,15)
#test.gam2.30 <- replicate(1000, gamcv(x,y, bandwidths = c(1,2,30))$rmse)   #s(2,30)
#RMSEs.gam.2p30 <- replicate(1000, gamcv(x,y, bandwidths = c(1,2,30))$rmse)   #s(2) + s(30)
RMSEs.gam.3by15 <- replicate(1000, gamcv(x,y, bandwidths = c(1,3,15))$rmse)   #s(3 by 15) 


### density plot
plot(density(RMSEs.ridge), col=col2t(3, 200), lwd=3
     #, xlim=c(0.030, 0.057)
     , xlim=c(0.034, 0.049)
     , ylim=c(0,1300)
     , main=""
     , xlab="RMSE"
     , yaxt="n"
     , type="n")
lines(density(RMSEs.bw15), col=col2t(2,200), lwd=3)
lines(density(RMSEs.ridge), col=col2t(3,200), lwd=3)
#lines(density(RMSEs.gam), col=rgb(255,215,0, maxColorValue = 255), lwd=3)
lines(density(RMSEs.gam.3.15), col=rgb(255,215,0, maxColorValue = 255), lwd=3)
#lines(density(RMSEs.gam.3p15), col=4, lwd=3)
lines(density(RMSEs.gam.3by15), col=4, lwd=3)


shadebands( x  = density(RMSEs.bw15)$x
            ,lo = rep(0, length(density(RMSEs.bw15)$x))
            ,hi = density(RMSEs.bw15)$y
            , col=col2t(2,90)
)
shadebands( x  = density(RMSEs.ridge)$x
            ,lo = rep(0, length(density(RMSEs.ridge)$x))
            ,hi = density(RMSEs.ridge)$y
            ,col = col2t(3,90)
)
# 
# shadebands( x  = density(RMSEs.gam)$x
#             ,lo = rep(0, length(density(RMSEs.gam)$x))
#             ,hi = density(RMSEs.gam)$y
#             , col=rgb(255,215,0, maxColorValue = 255, alpha=90)
# )


shadebands( x  = density(RMSEs.gam.3.15)$x
            ,lo = rep(0, length(density(RMSEs.gam.3.15)$x))
            ,hi = density(RMSEs.gam.3.15)$y
            , col=rgb(255,215,0, maxColorValue = 255, alpha=90)
)

legend("topright"
       , legend = c("bw15", "ridge", "am")
       , fill = c(2,3,rgb(255,215,0, maxColorValue = 255))
       #, lwd=2
       #, lty=1:length(list.of.arrays)
       #, col=1:length(list.of.arrays)
       , cex=1
       , horiz = FALSE
       , ncol = 1
)




## test difference between s(2,30) and s(3,15)
var.test(RMSEs.gam.3.15, test.gam2.30)
t.test(RMSEs.gam.3.15, test.gam2.30, paired = FALSE, var.equal = TRUE)
# ----> Difference is NOT significant
 






####################
# decision trees
####################

reset.df()
set.seed(1)
RMSEs.tree <- replicate(1000,tree.cv(x,y)$rmse)



### density plot
par(mar=c(5.1,4.1,4.1,2.1))
plot(density(RMSEs.ridge), col=col2t(3, 200), lwd=3
     #, xlim=c(0.030, 0.057)
     , xlim=c(0.037, 0.051)
     , ylim=c(0,1300)
     , main=""
     , xlab="RMSE"
     , yaxt="n"
     , type="n")
lines(density(RMSEs.bw15), col=col2t(2,200), lwd=3)
lines(density(RMSEs.ridge), col=col2t(3,200), lwd=3)
lines(density(RMSEs.tree), col=rgb(0,0,128, maxColorValue = 255), lwd=3)



shadebands( x  = density(RMSEs.bw15)$x
            ,lo = rep(0, length(density(RMSEs.bw15)$x))
            ,hi = density(RMSEs.bw15)$y
            , col=col2t(2,90)
)
shadebands( x  = density(RMSEs.ridge)$x
            ,lo = rep(0, length(density(RMSEs.ridge)$x))
            ,hi = density(RMSEs.ridge)$y
            ,col = col2t(3,90)
)

shadebands( x  = density(RMSEs.tree)$x
            ,lo = rep(0, length(density(RMSEs.tree)$x))
            ,hi = density(RMSEs.tree)$y
            , col=rgb(0,0,128, maxColorValue = 255, alpha=90)
)

legend("topright"
       , legend = c("bw15", "ridge", "tree")
       , fill = c(2,3,rgb(0,0,128, maxColorValue = 255))
       #, lwd=2
       #, lty=1:length(list.of.arrays)
       #, col=1:length(list.of.arrays)
       , cex=1
       , horiz = FALSE
       , ncol = 1
)



summary(RMSEs.tree)
sd(RMSEs.tree)

###
midrange(RMSEs.tree, 95)

var.test(RMSEs.bw15, RMSEs.tree)
t.test(RMSEs.bw15, RMSEs.tree, var.equal = FALSE, paired=FALSE)

var.test(RMSEs.ridge, RMSEs.tree)
t.test(RMSEs.ridge, RMSEs.tree, var.equal = FALSE, paired=FALSE)


sd(RMSEs.tree)
sd(RMSEs.ridge)


## draw 10 lines with tree (predict whole rage of data)
reset.df()
matr <- matrix(NA, nrow = length(2:92), ncol=1+length(2:30))
colnames(matr) <- c("intercept", paste0("bw", 2:30))
rownames(matr) <- 2:92
for(i in 2:30)  matr[,(i)] <- llreg(x,y,bandwidth = i)$y
matr[,1] <- 1  # for intercept, will use later

to.predict <- data.frame(id=1, fold.assign=0, matr[,2:30])

colnames(to.predict) <- c('id', 'fold.assign',  paste0("fitted.",colnames(matr[,2:30])) )

temp.fitted.tree <- matrix(NA, nrow=length(2:92), ncol=10)

par(mfrow=c(1,2))
set.seed(12345)
for(k in 7:8){
  tree <- rpart(y~. - id - y - fold.assign
                , data=data[data$fold.assign != k,]
                , method="anova"
                #,control=
  ) 
  #tempreg <- tree
  tempreg <- prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  
  #temp.fitted.tree[,k] <- predict(tempreg, newdata=to.predict, type="matrix")
  
  #heat.tree(tempreg, main=paste('Fold', k), Margin = 0.1, digits = 4, cex=0.8, type=4)
  rpart.plot(tempreg, main=paste('Fold', k), type=4, extra=0, branch.lty=3
             , box.palette=NA, digits = 4
             , cex=0.7
             , fallen.leaves = FALSE
             )
}



par(mfrow=c(1,1))
plot(x,y,pch=16, col=tgrey(0.2), xlab="2016 day of year", ylab="Support for Trump", cex=2)
lines(ll15, col=2, lwd=3)

for (k in 1:10) lines(2:92,temp.fitted.tree[,k], col=rgb(0,0,128,150, maxColorValue = 255))
legend("topleft"
       ,legend=c("bw15", "stacking tree")
       ,lwd=4
       #,lty=c(1,1,1)
       ,col=c(2,"darkblue")
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)



## close-up

par(mfrow=c(1,1))
plot(ll15, col=col2t(2,200), lwd=3, type="l", xlab="2016 day of year", ylab="Support for Trump"
     , ylim=c(0.34,0.47))

lines(ll3, lwd=3, lty=2, col=col2t(1,200))
for (k in 1:10) lines(2:92,temp.fitted.tree[,k], col=rgb(0,0,128,100, maxColorValue = 255), lwd=1)
legend("topleft"
       ,legend=c("bw3","bw15", "stacking tree")
       ,lwd=3
       , lty=c(2,1,1)
       #,lty=c(1,1,1)
       ,col=c(1,2,rgb(0,0,128,75, maxColorValue = 255))
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)



#######################
### density plot (ALL)

list.RMSEs <- list(bw3=RMSEs.bw3,bw15=RMSEs.bw15
                   , lasso=RMSEs.lasso
                   , ridge=RMSEs.ridge
                   ,  non.neg.sum.1=RMSEs.nonneg.sum1
                   , non.neg=RMSEs.nonneg
                   #, am=RMSEs.gam.3.15
                   , tree = RMSEs.tree
                   ) 

density.plot( list.RMSEs
              , xlim=c(0.037, 0.049)
              , ylim=c(0,1300)
              , legend.cex = 0.6
              , main=""
              , xlab="RMSE"
              , yaxt="n"
              , legend.ncol = 1
              , col=c(1,2,8,3,5,6,4))

# summary

summary( data.frame(bw3=RMSEs.bw3,bw15=RMSEs.bw15
                   ,  non.neg.sum.1=RMSEs.nonneg.sum1, non.neg=RMSEs.nonneg
                   , ridge=RMSEs.ridge
                   , lasso=RMSEs.lasso
                   #, am=RMSEs.gam.3.15
                   , tree=RMSEs.tree
                   )
         
)




# middle 95% of RMSEs

RMSEs <- list(bw3=RMSEs.bw3,bw15=RMSEs.bw15
              ,  ridge=RMSEs.ridge, lasso=RMSEs.lasso
              , non.neg.sum1=RMSEs.nonneg.sum1, non.neg=RMSEs.nonneg
              , tree = RMSEs.tree
              )
x <- 1:length(RMSEs)
M <- sapply(RMSEs,mean) 
L <- sapply(RMSEs,quantile, 0.025)
U <- sapply(RMSEs,quantile, 0.975)

require(plotrix)
par(mar = c(5.1, 2.1, 4.1, 4.1))
plotCI(x, M, ui=U, li=L, col=c(1:6, rgb(0,0,128, maxColorValue = 255)), ylab="RMSE"
       , yaxt="n", xaxt="n", xlab="", xlim=c(0.8,7.2), ylim=c(0.039, 0.051)
       , lwd=4
       )
#mtext("Middle 95% of RMSE", cex=0.8)
axis(4)
mtext("RMSE", 4, line = 2)

text(1,0.049, "bw3", col=1, srt=90)
text(2,0.049, "bw15", col=2, srt=90)
text(3,0.049, "ridge", col=3, srt=90)
text(4,0.049, "lasso", col=4, srt=90)
text(5,0.049, "nonneg.sum1", col=5, srt=90)
text(6,0.049, "nonneg", col=6, srt=90)
text(7,0.049, "tree", col="darkblue", srt=90)

abline(h=mean(RMSEs.bw15), col=2, lty=2)

#col=c(1,2,3,4,5,6,"darkblue")
#for (i in 1:length(RMSEs)) points(i, median(RMSEs[[i]]), pch="-", col=col[i], cex=3)


#reset default
par(mar=c(5.1,4.1,4.1,2.1))



### compare LL3, LL15 and stacking with ridge
reset.df()
matr <- matrix(NA, nrow = length(2:92), ncol=1+length(2:30))
colnames(matr) <- c("intercept", paste0("bw", 2:30))
rownames(matr) <- 2:92
for(i in 2:30)  matr[,(i)] <- llreg(x,y,bandwidth = i)$y
matr[,1] <- 1

stack.ridge.fitted <- matrix(NA,nrow=length(2:92), ncol = 10)
for(i in 1:10) stack.ridge.fitted[,i] <- matr %*% temp.ridge$fold.coefficients[,i]

#plot
plot(x,y, pch=16, col=tgrey(0.2), xlab="2016 day of year", ylab="% support for Trump", cex=2
     , ylim=c(min(y)-0.02, max(y)+0.07)
     #, type="n"
     )

lines(ll3, col=rgb(0,0,0, 0.7), lwd=2 )#, lty=2)
lines(ll15, col=tred(0.7), lwd=2 )#, lty=4)
for (i in 1:10) lines(2:92, stack.ridge.fitted[,i], col=rgb(0,200,0, alpha=75, maxColorValue = 255), lwd=2)
legend("topleft"
       ,legend=c( "bw 3","bw 15", "stacking ridge")
       ,lwd=3
       #,lty=c(2,4)
       ,col=c(1,2,rgb(0,200,0, maxColorValue = 255))
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)


# plot close-up
plot(ll3, lwd=2, type="n" #, col=rgb(0,0,0, 0.8)
     , xlab="2016 day of year"
     , ylab="Support for Trump"
     , ylim= c(0.34,0.47))
lines(ll3, col=rgb(0,0,0, 0.7), lwd=3 )#, lty=2)
lines(ll15, col=tred(0.7), lwd=3 , lty=1)
for (i in 1:10) lines(2:92, stack.ridge.fitted[,i], col=rgb(0,200,0, alpha=100, maxColorValue = 255), lwd=2)
#lines(2:92, stack.ridge.fitted[,1], col=rgb(0,200,0, alpha=200, maxColorValue = 255), lwd=4)
legend("topleft"
       ,legend=c( "bw 3","bw 15", "stacking ridge")
       ,lwd=4
       #,lty=c(1,1,1)
       ,col=c(1,2,rgb(0,200,0, maxColorValue = 255))
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)



### compare  stacking with ridge and lasso
stack.lasso.fitted <- matrix(NA,nrow=length(2:92), ncol = 10)
for(i in 1:10) stack.lasso.fitted[,i] <- matr %*% temp.lasso$fold.coefficients[,i]

# plot close-up
plot(ll3, lwd=2, type="n" #, col=rgb(0,0,0, 0.8)
     , xlab="2016 day of year"
     , ylab="Support for Trump"
     , ylim= c(0.34,0.47))
lines(ll3, col=rgb(0,0,0, 0.7), lwd=3 )#, lty=2)
lines(ll15, col=tred(0.7), lwd=3 , lty=1)
for (i in 1:10) lines(2:92, stack.ridge.fitted[,i], col=rgb(0,200,0, alpha=90, maxColorValue = 255), lwd=2)
for (i in 1:10) lines(2:92, stack.lasso.fitted[,i], col=rgb(0,0,200, alpha=90, maxColorValue = 255), lwd=2)

#lines(2:92, stack.ridge.fitted[,1], col=rgb(0,200,0, alpha=200, maxColorValue = 255), lwd=4)
legend("topleft"
       ,legend=c( "stacking ridge", "stacking lasso")
       ,lwd=4
       #,lty=c(1,1,1)
       ,col=c("green", "blue")
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)






### compare LL3, LL15 and stacking with AM

reset.df()

#fit model
set.seed(1)

llcv.fit <- llcv(x,y,bandwidths = 1:30, n.folds=10)

gam.fit <- gam(y~s(fitted.bw3,fitted.bw15), data=llcv.fit$data)

tofit <- data.frame(fitted.bw3=llreg(x,y,3)$y, fitted.bw15=llreg(x,y,15)$y)


#plot
plot(x,y, pch=16, col=tgrey(0.15), xlab="2016 day of year", ylab="% support for Trump", cex=2
     , ylim=c(min(y)-0.02, max(y)+0.07)
     #, type="n"
)

lines(ll3, col=rgb(0,0,0, 0.7), lwd=3 )#, lty=2)
lines(ll15, col=tred(0.7), lwd=3 )#, lty=4)
lines(2:92, predict(gam.fit, tofit), col=7, lwd=3)

legend("topleft"
       ,legend=c( "bw 3","bw 15", 'stacking am')
       ,lwd=3
       #,lty=c(2,4)
       ,col=c(1,2, 7)
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)


#close up

plot(ll3, col=rgb(0,0,0, 0.7), lwd=2, type="l" , ylim=c(0.34,0.47))#, lty=2)
lines(ll15, col=tred(0.7), lwd=2 )#, lty=4)
lines(2:92, predict(gam.fit, tofit), col=7, lwd=2, type="l")

legend("topleft"
       ,legend=c( "bw 3","bw 15", 'stacking am')
       ,lwd=3
       #,lty=c(2,4)
       ,col=c(1,2, 7)
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=2
)




### comparing bw3, bw15, ridge and trees


par(mfrow=c(1,1))
plot(ll15, col=col2t(2,200), lwd=3, type="l", xlab="2016 day of year", ylab="Support for Trump"
     , ylim=c(0.34,0.47))

lines(ll3, lwd=3, lty=2, col=col2t(1,200))
for (k in 10) lines(2:92,temp.fitted.tree[,k], col=rgb(0,0,128,200, maxColorValue = 255), lwd=3)
for (k in 1) lines(2:92,stack.ridge.fitted[,k], col=col2t(3,200), lwd=3)

legend("topleft"
       ,legend=c("bw3","bw15", "ridge", "stacking tree")
       ,lwd=2
       , lty=c(2,1,1,1)
       #,lty=c(1,1,1)
       ,col=c(1,2,rgb(0,0,128,maxColorValue = 255), 3)
       ,cex=0.65
       #,horiz=TRUE
       , bg=tgrey(0.1)
       , seg.len=3
)



