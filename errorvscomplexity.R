library(ggplot2)
data('diamonds')
df=diamonds
#--------------------------------------------------------------------------------

samplesize=list(10,20,70)
testerror=list()

complexity=list(1,2,6,7,8)
color=list('red','blue','green')



set.seed(0)
rand2 = sample(1:nrow(df),20)
train = df[rand2,]
set.seed(5)
rand3 = sample(1:nrow(df),20)
test = df[rand3,]
#View(test)


m1 <- lm(price ~ carat, train)
pred = predict(m1, newdata=test)
x=sum((pred-test$price)^2)
x=sqrt(x/20)
testerror=append(testerror,x)
m2 <- lm(price ~ carat + I(carat^2), train)
pred = predict(m2, newdata=test)
x=sum((pred-test$price)^2)
x=sqrt(x/20)
testerror=append(testerror,x)
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
x=sqrt(x/20)
testerror=append(testerror,x)
m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
         + I(carat^6) + I(carat^7), train)
pred = predict(m7, newdata=test)
x=sum((pred-test$price)^2)
x=sqrt(x/20)
testerror=append(testerror,x)
m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8), train)
pred = predict(m8, newdata=test)
x=sum((pred-test$price)^2)
x=sqrt(x/20)
testerror=append(testerror,x)
#m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           #I(carat^5) + I(carat^6) + I(carat^7) +
           #I(carat^8) + I(carat^9) , train)
#pred = predict(m9, newdata=test)
#x=sum((pred-test$carat)^2)
#testerror=append(testerror,x)
plot(complexity,testerror, pch=19, cex=0.5)
lines(complexity,testerror, col='brown', type='l',pch=20) 
   
c<-1
for (i in samplesize){
  testerror=list()
  set.seed(i)
  rand2 = sample(1:nrow(df),20)
  train = df[rand2,]
  set.seed(5)
  rand3 = sample(1:nrow(df),20)
  test = df[rand3,]
  #View(test)
  
  
  m1 <- lm(price ~ carat, train)
  pred = predict(m1, newdata=test)
  x=sum((pred-test$price)^2)
  x=sqrt(x/20)
  testerror=append(testerror,x)
  m2 <- lm(price ~ carat + I(carat^2), train)
  pred = predict(m2, newdata=test)
  x=sum((pred-test$price)^2)
  x=sqrt(x/20)
  testerror=append(testerror,x)
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
  x=sqrt(x/20)
  testerror=append(testerror,x)
  m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
           + I(carat^6) + I(carat^7), train)
  pred = predict(m7, newdata=test)
  x=sum((pred-test$price)^2)
  x=sqrt(x/20)
  testerror=append(testerror,x)
  m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
             I(carat^5) + I(carat^6) + I(carat^7) +
             I(carat^8), train)
  pred = predict(m8, newdata=test)
  x=sum((pred-test$price)^2)
  x=sqrt(x/20)
  testerror=append(testerror,x)
  #m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
             #I(carat^5) + I(carat^6) + I(carat^7) +
             #I(carat^8) + I(carat^9) , train)
  #pred = predict(m9, newdata=test)
  #x=sum((pred-test$carat)^2)
  #testerror=append(testerror,x)
  lines(complexity,testerror, col=as.character(color[c]), type='l',pch=20)
  c<-c+1
}
