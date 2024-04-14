data <- read.table("data/weight-rmr-simple-linear-regression.txt")
colnames(data) <- c("bw", "rmr") 
res <- lm(rmr ~ bw, data)
summary(res)
anova(res)

cor(data$bw, data$rmr)
plot(
  data$bw, 
  data$rmr, 
  xlab = "Body weight", 
  ylab = "Rest metabolic rate"
)
abline(coefficients(res))

pred <- predict(res, interval = "confidence")[,2:3]
lines(data$bw,pred[,1], lty=2)
lines(data$bw,pred[,2], lty=2)

pred_ind <- predict(res, interval = "prediction")[,2:3]
sigma <- summary(res)$sigma
upper.bound <- sigma * sqrt(1 + 1/nrow(data) + (data$bw - mean(data$bw))^2/
                              sum((data$bw - mean(data$bw))^2))
fitted(res) + qt(0.975, 42) * upper.bound
# the same as pred.slr.ind[,2]
lines(data$bw,pred_ind[,1], lty=3, col = "red")
lines(data$bw,pred_ind[,2], lty=3, col = "red")

head(pred_ind)

plot(residuals(res))

plot(data$bw, residuals(res), xlab = "Body weight")
abline(1,0)

plot(predict(res), residuals(res))
abline(1,0)

qqnorm(residuals(res))
qqline(residuals(res))

X <- model.matrix(res)
(estimates <- solve(t(X)%*%X)%*%t(X)%*%data$rmr)
sigmahat2 <- 24934
vcov(res)
sigmahat2*solve(t(X)%*%X)
sqrt(diag(vcov(res)))


# manual
X <- model.matrix(res)
Y <- data$rmr

I <- diag(44)
J <- matrix(1, 44, 44) 

H <- X %*% solve(t(X) %*% X) %*%t (X)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y 

Y_hat <- H %*% Y

MSE <- t(Y) %*% (I - H) %*% Y / 42 
s_2 <- c(MSE) * solve(t(X) %*% X)
sqrt(diag(s_2))

var_beta <- s_2 %*% solve(t(X) %*% X)

## Student Sleep

xs <- c(2, 4, 4.75, 4.25, 7.5, 8, 2.5, 0, 5, 5.45, 6, 8.5, 9)
ys <- c(12, 27, 31, 30, 52, 53, 13, 7, 32, 42, 45, 57, 62)

students <- data.frame(hours = xs, focus = ys)

model <- lm(focus ~ hours, students)
summary(model)
anova(model)
