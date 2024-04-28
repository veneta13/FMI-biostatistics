library(car)
library(aod)
library(faraway)
library(ellipse)
library(UsingR)

# Dataset from the lecture
data <- read.table(
  "data/tlc-data.txt",
  header=T, 
  row.names=1,
  colClasses=c("factor","integer","factor","integer","numeric")
)

head(data)

pairs(data)

cor(data[,c(1,3,4)])

# set reference level
data$sex <- relevel(data$sex, ref="1")

model <- lm(tlc ~ ., data)
summary(model)

# Type I SS
anova(model)

model1 <- lm(tlc ~ sex + age + height, data)
anova(model1)
# it is different from the Anova table above - sequential sum of squares

# Type III SS

Anova(model, type=3)

# Matrix calculation
x <- model.matrix(model)
(estimates <- solve(t(x)%*%x)%*%t(x)%*%data$tlc)
hat.matrix <- x%*%solve(t(x)%*%x)%*%t(x)
(pred <- x%*%estimates)
# the same as: (hat.matrix%*%data$tlc)
# and predict(model)
diag(1,3) # the I matrix
matrix(rep(1,3*3), ncol=3) # the J matrix

wald.test(b=coef(model1), Sigma=vcov(model1), Terms=3, H0=c(0))
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=4, H0=.1)
wald.test(b=coef(model1), Sigma=vcov(model1), Terms=3:4, H0=rep(0,2))

model_d_reduced <- lm(tlc ~ sex + height, data)
# Likelihood ratio test
2*(logLik(model)-logLik(model_d_reduced))
pchisq(1.26716, 1, lower.tail=F)


### Testing for regression coefficients
data(savings)
savings

pairs(savings)
hist(savings$sr)

g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g)
anova(g)

# rename the sum of squares
(tss <- sum((savings$sr-mean(savings$sr))^2))
(rss <- deviance(g)) ## rss=sum((savings$sr-g$fitted)^2)s

df.residual(g)

(fstat <- ((tss-rss)/4)/(rss/df.residual(g)))
1 - pf(fstat, 4, df.residual(g))

g2 <- lm(sr ~ pop75 + dpi + ddpi, savings)
(rss2 <- deviance(g2))
(fstat <- (deviance(g2) - deviance(g)) / (deviance(g)/df.residual(g)))
1 - pf(fstat, 1, df.residual(g))
sqrt(fstat)
(tstat <- summary(g)$coef[2,3])
2*(1 - pt(sqrt(fstat), 45))

anova(g2,g) # 147.01/(rss/df.residual(g))

qt(1 - 0.01 / 2, 11)

g3 <- lm(sr ~ pop15 + dpi , savings)
anova(g3, g) # 93.411/2/(rss/df.residual(g)) 


# Testing for a fixed value for a regression coefficient
gr <- lm(sr ~ pop15 + pop75 + dpi + offset(0.5*ddpi), savings)
summary(gr)
anova(gr, g)
(3.0635/1)/(650.71/45)
(tstat <- (0.409695-0.5)/0.196197)
2*pt(tstat,45)
tstat^2

par(mfrow=c(1,3))
plot(ellipse(g, c(2,3)), type="l")
points(coef(g)[2], coef(g)[3], lwd=2)
plot(ellipse(g, c(3,4)), type="l")
points(coef(g)[3],coef(g)[4],lwd=2)
plot(ellipse(g,c(2,4)), type="l")
points(coef(g)[2], coef(g)[4], lwd=2)
par(mfrow=c(1,1))

par(mfrow=c(1,1))
plot(ellipse(g, c(2,3)), type="l")
points(coef(g)[2], coef(g)[3], lwd=2)
points(ellipse(g, c(2,3), level = 0.9), type="l", col="red")
points(coef(g)[2], coef(g)[3], lwd=2)

g5 <- lm(sr ~ offset(-.5*pop15) + offset(-2*pop75) +
           dpi + ddpi, savings)
anova(g5, g)


# Adding higher order of the independent variable as a predictor
model <- lm(sr ~ ddpi, savings)
summary(model)
plot(fitted(model), residuals(model))
plot(savings$ddpi, residuals(model))

model1 <- lm(sr ~ ddpi + I(ddpi^2), savings)
summary(model1)
summary(lm(sr ~ ddpi+I(ddpi^2)+I(ddpi^3),savings))

plot(savings$sr, savings$ddpi)
plot(savings$sr, fitted(model1))
cor(savings$sr, fitted(model1))^2
plot(savings$sr, residuals(model1))
cor(savings$sr, residuals(model1))

## If your residuals are correlated with your dependent variable, 
## then there is a significantly large amount of unexplained 
## variance that you are not accounting for.
plot(savings$pop15, residuals(model1))
plot(savings$pop75, residuals(model1))
plot(savings$dpi, residuals(model1))
plot(savings$ddpi^2, residuals(model1))

plot(fitted(model1), residuals(model1))
qqnorm(residuals(model1)); qqline(residuals(model1))
plot(residuals(model1))



#####################
### When the outcome variable is not normally distributed
data(exec.pay) # or read in from file
simple.eda(exec.pay)
log.exec.pay <- log10(exec.pay[exec.pay>0])
simple.eda(log.exec.pay)


###################### artificial linear model:
## linear model with continuous and binary predictors with interaction
set.seed(42)
gender <- rbinom(1000, 1, 0.5)
age <- runif(1000, 20, 80)
weight <- 12+3.5*gender+2*age-0.5*gender*age+rnorm(1000)
modelwithinteraction <- lm(weight ~ age*as.factor(gender))
summary(modelwithinteraction)
plot(weight, residuals(modelwithinteraction))
plot(residuals(modelwithinteraction))

gender <- factor(gender)
modelwithinteraction <- lm(weight ~ age*gender)
summary(modelwithinteraction)

## the variable gender is out of the model
modela <- lm(weight ~ age)
summary(modela)
plot(weight,residuals(modela))
plot(fitted(modela), residuals(modela))

## the variable age is out of the model
modelb <- lm(weight ~ gender)
summary(modelb)
plot(weight, residuals(modelb))
plot(fitted(modelb), residuals(modelb))
plot(age, residuals(modelb))


plot(age[gender==1], weight[gender==1],
     col="blue", xlab="Age",ylab="Weight")
points(age[gender==0], weight[gender==0],col="red")
abline(coef(modelwithinteraction)[1]+coef(modelwithinteraction)[3],
       coef(modelwithinteraction)[2]+coef(modelwithinteraction)[4], col="blue")
abline(coef(modelwithinteraction)[1],
       coef(modelwithinteraction)[2], col="red")
legend(locator(1), legend=c("males","females"),
       col=c("blue","red"),lty=c(1,1))

########################################
## Polynomial linear model of second order 
age <- runif(1000, 20, 60)
g <- rbinom(1000, 1, 0.5)
weight <- 12+2*age-0.5*age^2+g+rnorm(1000)

model1 <- lm(weight ~ age)
summary(model1)
plot(age, residuals(model1))
plot(fitted(model1), residuals(model1))
plot(g,residuals(model1))

model1a <- lm(weight ~ age + g)
summary(model1a)
plot(age,residuals(model1a))
plot(fitted(model1a), residuals(model1a))
plot(g, residuals(model1a))

model2 <- lm(weight ~ age + I(age^2) + g)
plot(fitted(model2), residuals(model2))
plot(age, residuals(model2))

#############################################
## Linear model with factor with three levels

x <- factor(sample(c("a","b","c"), 300, replace=T))
y <- 2 + 2*ifelse(x=="a",1,0)-2*ifelse(x=="c",1,0) + 
  rnorm(300)
model1 <- lm(y~x)
summary(model1)

## changing the reference level
x <- relevel(x, ref="b")
model2 <- lm(y ~ x)
summary(model2)

################################

#######Model selection
## backward elimination
data(state)
statedata <- data.frame(state.x77, row.names=state.abb)
g <- lm(Life.Exp ~ ., data=statedata)
summary(g)
g <- update(g, . ~ . - Area)
summary(g)
g <- update(g, . ~ . - Illiteracy)
summary(g)
g <- update(g, . ~ . - Income)
summary(g)
g <- update(g, . ~ . - Population)
summary(g)
### update also works with adding predictors via "+"
summary(lm(Life.Exp ~ Illiteracy+Murder+Frost, statedata))

qqnorm(residuals(g)); qqline(residuals(g))
plot(fitted(g), residuals(g))
abline(h=0)
plot(statedata$Life.Exp, fitted(g))
plot(statedata$Murder, residuals(g)); abline(h=0)
plot(statedata$HS.Grad, residuals(g)); abline(h=0)
plot(statedata$Frost, residuals(g)); abline(h=0)

# height effect
g_all <- lm(tlc ~ sex + age + height, data)
g_height <- lm(tlc ~ sex + age, data)
(rss2 <- deviance(g_height))
(fstat <- (deviance(g_height) - deviance(g_all)) / (deviance(g_all)/df.residual(g_all)))
1 - pf(fstat, 1, df.residual(g_all))
sqrt(fstat)
(tstat <- summary(g_all)$coef[2,3])
2*(1 - pt(sqrt(fstat), 45))

anova(g_height,g_all)