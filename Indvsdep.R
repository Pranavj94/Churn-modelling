library(ggplot2)
data('diamonds')
df=diamonds
#--------------------------------------------------------------------------------

set.seed(0)
rand2 = sample(1:nrow(df),500)
train = df[rand2,]


m1 <- lm(price ~ carat, train)
m2 <- lm(price ~ carat + I(carat^2), train)
m3 <- lm(price ~ carat + I(carat^2) + I(carat^3), train)
m4 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4), train)
m5 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5), train)
m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6), train)
m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
         + I(carat^6) + I(carat^7), train)
m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8), train)
m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8) + I(carat^9) , train)


plot(train$carat,train$price, pch=19, cex=0.5)
lines(sort(train$carat), fitted(m1)[order(train$carat)], col='orange', type='l',pch=19) 
lines(sort(train$carat), fitted(m2)[order(train$carat)], col='red', type='l',pch=19) 
lines(sort(train$carat), fitted(m3)[order(train$carat)], col='blue', type='l',pch=19) 
lines(sort(train$carat), fitted(m4)[order(train$carat)], col='green', type='l',pch=19) 
lines(sort(train$carat), fitted(m5)[order(train$carat)], col='yellow', type='l',pch=19) 
lines(sort(train$carat), fitted(m6)[order(train$carat)], col='black', type='l',pch=19) 
lines(sort(train$carat), fitted(m7)[order(train$carat)], col='purple', type='l',pch=19) 
lines(sort(train$carat), fitted(m8)[order(train$carat)], col='orange', type='l',pch=19) 
lines(sort(train$carat), fitted(m9)[order(train$carat)], col='violet', type='l',pch=19) 
