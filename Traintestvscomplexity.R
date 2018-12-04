library(ggplot2)
data('diamonds')
df=diamonds
#--------------------------------------------------------------------------------


testerror=list()
trainerror=list()
complexity=list(1,2,6,7,8,9,10,11,12)


set.seed(0)
rand2 = sample(1:nrow(df),100)
train = df[rand2,]
set.seed(100)
rand3 = sample(1:nrow(df),100)
test = df[rand3,]
#View(test)


m1 <- lm(price ~ carat, train)
pred = predict(m1, newdata=test)
x=sum((pred-test$price)^2)
testerror=append(testerror,x)
pred = predict(m1, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)
m2 <- lm(price ~ carat + I(carat^2), train)
pred = predict(m2, newdata=test)
x=sum((pred-test$price)^2)
testerror=append(testerror,x)
pred = predict(m2, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

#m3 <- lm(price ~ carat + I(carat^2) + I(carat^3), train)
#pred = predict(m3, newdata=test)
#x=sum((pred-test$carat)^2)
#testerror=append(testerror,x)
#m4 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4), train)
#pred = predict(m4, newdata=test)
#x=sum((pred-test$carat)^2)
#testerror=append(testerror,x)
#m5 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5), train)
#pred = predict(m5, newdata=test)
#x=sum((pred-test$carat)^2)
#testerror=append(testerror,x)
m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6), train)
pred = predict(m6, newdata=test)
x=sum((pred-test$price)^2)
testerror=append(testerror,x)
pred = predict(m6, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)
m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
         + I(carat^6) + I(carat^7), train)
pred = predict(m7, newdata=test)
x=sum((pred-test$price)^2)
testerror=append(testerror,x)
pred = predict(m7, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)
m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8), train)
pred = predict(m8, newdata=test)
x=sum((pred-test$price)^2)
testerror=append(testerror,x)
pred = predict(m8, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
I(carat^5) + I(carat^6) + I(carat^7) +
I(carat^8) + I(carat^9) , train)
pred = predict(m9, newdata=test)
x=sum((pred-test$carat)^2)
testerror=append(testerror,x)
pred = predict(m9, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

m10 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8) + I(carat^9) +I(carat^10), train)
pred = predict(m10, newdata=test)
x=sum((pred-test$carat)^2)
testerror=append(testerror,x)
pred = predict(m10, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

m11 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
            I(carat^5) + I(carat^6) + I(carat^7) +
            I(carat^8) + I(carat^9) +I(carat^10) +I(carat^11), train)
pred = predict(m11, newdata=test)
x=sum((pred-test$carat)^2)
testerror=append(testerror,x)
pred = predict(m11, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

m12 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
            I(carat^5) + I(carat^6) + I(carat^7) +
            I(carat^8) + I(carat^9) +I(carat^10) +I(carat^11) +I(carat^12), train)
pred = predict(m12, newdata=test)
x=sum((pred-test$carat)^2)
testerror=append(testerror,x)
pred = predict(m12, newdata=train)
x=sum((pred-train$price)^2)
trainerror=append(trainerror,x)

plot(complexity,trainerror, pch=19, cex=0.5)
lines(complexity,testerror, col='red', type='l',pch=20) 
lines(complexity,trainerror, col='blue', type='l',pch=20)

#trainerror
#testerror
