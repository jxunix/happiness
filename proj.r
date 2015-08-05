#
# Filename: proj.r
# Author: Jun Xu
# Email: junx@bu.edu
# Created Time: Tue 29 Apr 2014 03:25:47 PM EDT
#

# 1. load and save data
library(faraway)
str(happy)
happy.arch <- happy
happy.new <- happy[order(happy[,2],happy[,3],happy[,4],happy[,5],happy[,1]),]
write.table(happy.new,file='happy.csv')

# 2. data preprocessing
happy <- within(happy,{
  sex <- factor(sex)
  love <- factor(love)
  work <- factor(work)
})
str(happy)

# 3. simple relationship exploration
# scatterplot matrix
pdf(file='31.pdf')
plot(happy)
dev.off()
# boxplots
pdf(file='32.pdf')
boxplot(happy ~ sex,happy)
dev.off()
pdf(file='33.pdf')
boxplot(happy ~ love,happy)
dev.off()
pdf(file='34.pdf')
boxplot(happy ~ work,happy)
dev.off()

# 4. OLM
lm <- lm(happy ~ .,happy)
lm <- step(lm)
m1 <- lm
summary(m1)
# c.i.
confint(m1)
# residuals vs fitted
pdf(file='41.pdf')
plot(m1,1)
dev.off()

# 5. multinomial model
library(nnet)
m2 <- multinom(happy ~ money+love+work,happy)
summary(m2)
# c.i.
confint(m2)

library(MASS)
m3 <- polr(as.factor(happy) ~ money+love+work,happy)
summary(m3)
# c.i.
confint(m3)

# residuals vs fitted
happy.new <- read.csv('happy2.csv',sep='\t',comm='#',header=T)
happy.new <- within(happy.new,{
  f <- factor(f)
  sex <- factor(sex)
  love <- factor(love)
  work <- factor(work)
})
pm <- glm(cbind(y,n-y)~I(-money)+love+work+f-1,happy.new,family='binomial')
summary(pm)
confint(pm)
pdf(file='51.pdf')
plot(pm,1)
dev.off()

# 6. quasi-likelihood model
m4 <- glm(happy ~ money+love+work,happy,family=quasi(link='identity',variance='constant'))
summary(m4)
# c.i.
confint(m4)
# residuals vs fitted
pdf(file='61.pdf')
plot(m4,1)
dev.off()

# 7. joint mean & variance model
beta <- coef(lm)
repeat{
  d <- residuals(lm)^2
  gm <- glm(d ~ money+sex+love,happy,family=Gamma)
  lm <- lm(happy ~ money+love+work,happy,weights=1/fitted(gm))
  beta.new <- coef(lm)
  if(sqrt(sum((beta.new-beta)^2))<1e-6) break
  beta <- beta.new
}
m5 <- lm
summary(m5)
# c.i.
confint(m5)
# residuals vs fitted
pdf(file='71.pdf')
plot(m5,1)
dev.off()

# 8. prediction
# Adam
predict(m1,newdata=data.frame(money=40,sex='1',love='2',work='1'))
p1 <- predict(m3,newdata=data.frame(money=40,sex='1',love='2',work='1'),type='prob')
p1[1]*2+p1[2]*3+p1[3]*4+p1[4]*5+p1[5]*6+p1[6]*7+p1[7]*8+p1[8]*9+p1[9]*10
predict(m4,newdata=data.frame(money=40,sex='1',love='2',work='1'),type='response')
predict(m5,newdata=data.frame(money=40,sex='1',love='2',work='1'))

# Robert
predict(m1,newdata=data.frame(money=150,sex='0',love='1',work='5'))
p2 <- predict(m3,newdata=data.frame(money=150,sex='0',love='1',work='5'),type='prob')
p2[1]*2+p2[2]*3+p2[3]*4+p2[4]*5+p2[5]*6+p2[6]*7+p2[7]*8+p2[8]*9+p2[9]*10
predict(m4,newdata=data.frame(money=150,sex='0',love='1',work='5'),type='response')
predict(m5,newdata=data.frame(money=150,sex='0',love='1',work='5'))

# 9. interaction
pdf(file='91.pdf')
plot(happy ~ money,happy,col=sex,pch=c(1,19))
plot(happy ~ money,happy,col=love,pch=19)
plot(happy ~ money,happy,col=work,pch=19)
dev.off()

# money*love
m.noint <- glm(happy ~ money+love+work,happy,family=quasi(link='identity',variance='constant'))
m.int <- glm(happy ~ money+love+work+money*love,happy,family=quasi(link='identity',variance='constant'))
anova(m.noint,m.int,test='Chisq')

# money*work
m.int <- glm(happy ~ money+love+work+money*work,happy,family=quasi(link='identity',variance='constant'))
anova(m.noint,m.int,test='Chisq')

# love*work
m.int <- glm(happy ~ money+love+work+love*work,happy,family=quasi(link='identity',variance='constant'))
anova(m.noint,m.int,test='Chisq')

# check if money is significant in each stratification
summary(glm(happy ~ money+work,happy[happy$love==1,],family=quasi(link='identity',variance='constant')))
summary(glm(happy ~ money+work,happy[happy$love==2,],family=quasi(link='identity',variance='constant')))
summary(glm(happy ~ money+work,happy[happy$love==3,],family=quasi(link='identity',variance='constant')))
