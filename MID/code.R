library(dplyr)
setwd("/Users/username/Desktop/IMEN491R_midterm")
dia<-read.csv("diamonds.csv") 
library(ggplot2)

#1.1
mean(dia$price)

summary(dia)
str(dia)
is.factor(dia)
is.factor(dia$cut)
is.factor(dia$color)
is.factor(dia$clarity)

m1<-select(dia,startwith(dia$price))
set2<-select(car, -starts_with("mpg"))
set1<-select(dia, starts_with("price"))

#1.2
#hist(wt, breaks = 12,col = "orange", main="Histogram(Male)" , xlim=c(900,1700), ylim=c(0,25),xlab="brain weight")
par(mfrow=c(1,2))
hist(dia$price,col = "orange",xlab="Price", main="Histogram(price)" )
hist(dia$carat,col = "blue",xlab="carat", main="Histogram(carat)" )

boxplot(dia$price, col = "orange", boxex=0.5)
boxplot(dia$carat, col = "blue", boxex=0.5)

hist(wt, breaks = 12,col = "orange", main="Histogram(Male)" , xlim=c(900,1700), ylim=c(0,25),xlab="brain weight")
#hist(dia$price,col = "orange", main="Histogram(Price)" ,xlab="brain weight")
plot(wt, mpg,  col=as.integer(car$cyl), pch=19)

#1.3
boxplot(dia$price, dia$cut, )
ggplot(dia, aes(price, cut, color=c("orange","blue"))) + geom_boxplot()

#1.4
gg<-filter(dia, clarity=="I1" | clarity=="SI1" | clarity=="VS1")
subset1<-subset(gg)
rr<-lm(subset1$price~subset1$carat, data=subset1)
summary(rr)

gg2<-filter(dia,clarity=="SI1")
subset12<-subset(gg2)

  rr1<-lm(subset12$price~subset12$carat, data=subset12)
summary(rr1)


ggplot(subset12, aes(x=price, y=carat, color=clarity)) + 
  geom_point(size=3, alpha=0.6)

cor(subset12$price,subset12$carat)

#2.1
us<-read.csv("USecon.csv")
exdat<-arrange(us, ECAB)
set2<-subset(exdat)

#2.2
#WEST: Western state (1) or not (0)
eset<-filter(us, us$WEST==0)
wset<-filter(us, us$WEST==1)
t.test(eset$ECAB)
t.test(wset$ECAB)
#2.3.3
mean(eset$ECAB)
mean(wset$ECAB)


#t.test(eset)
#t.test(wset)
#table(us$ECAB)
#2.3
cor(us$GROW,us$ECAB)

par(mfrow = c(1, 1))
ggplot(us, aes(x=GROW, y=ECAB, color=WEST)) + 
  geom_point(size=3, alpha=0.6)
abline(lm(us$ECAB~us$GROW), col="red",lwd=2, lty=1)
abc<-lm(us$ECAB~us$GROW)
abc
summary(abc)
#2.3.2


#us$STATE
nv<-filter(us, !us$STATE=="NV")
#nv<-select(us, starts_with("NV"))
par(mfrow = c(1, 1))
ggplot(nv, aes(x=GROW, y=ECAB, color=WEST)) + 
  geom_point(size=3, alpha=0.6)

cor(nv$GROW,nv$ECAB)  

#2.4
rr<-lm(us$EX~us$YOUNG, data=us)
summary(rr)
