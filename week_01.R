# R practice

set.seed(9)

data <- rnorm(100, 150, 10)

mu_obs <- mean(data)

sd_obs <- sd(data)

test <- t.test(data, mu=140, alternative="greater", conf.level=0.95)

test.stat <- (mu_obs - 140) / (sd_obs / sqrt(100))

qnorm(0.95)

test.stat > qnorm(0.95)
# reject the null hypothesis

# p-value = p(Z > |z_obs|) = P(Z > 9.87)
pnorm(9.87, lower.tail = FALSE)


############################################
# generating random numbers from bivariate normal

library(MASS)
set.seed(42)

round(
  mvrnorm(
    n = 10, 
    mu=c(150,140), 
    Sigma=matrix(
      c(100,50,50,100),
      ncol=2)
    )
  )


treatmentA <- round(
  mvrnorm(
    n = 10, 
    mu=c(150,140), 
    Sigma=matrix(
      c(100,50,50,100),
      ncol=2)
    )
  )


treatmentB <- round(
  mvrnorm(
    n = 10, 
    mu=c(150,130), 
    Sigma=matrix(
      c(100,50,50,100),
      ncol=2)
    )
  )


blood.pressure <- rbind(
  treatmentA, 
  treatmentB
)


trt <- as.factor(c(rep("A",10), rep("B",10)))
data <- data.frame(
  treatment=trt, 
  Pre=blood.pressure[,1],
  Post = blood.pressure[,2], 
  Diff=blood.pressure[,2]-blood.pressure[,1]
)
data


data.t.test <- read.table("data/t-test-lectures.txt", row.names=1)
names(data.t.test) <- c("medicine","pre","post","diff")


# Task 1 A - Manual

A_pre <- data.t.test$pre[data.t.test$medicine=="A"]

A_pre_mean <- mean(A_pre)
A_pre_sd <- sd(A_pre)
A_n <- sum(data.t.test$medicine=="A")

A_marg <- A_pre_sd / sqrt(A_n) * qt(0.975, 9)
c(A_pre_mean - A_marg, A_pre_mean + A_marg)

# Task 1 A - t.test

t.test(A_pre)


# Task 1 B - Manual

B_pre <- data.t.test$pre[data.t.test$medicine=="B"]

B_pre_mean <- mean(B_pre)
B_pre_sd <- sd(B_pre)
B_n <- sum(data.t.test$medicine=="B")

B_marg <- B_pre_sd / sqrt(B_n) * qt(0.975, 9)
c(B_pre_mean - B_marg, B_pre_mean + B_marg)

# Task 1 B - t.test

t.test(B_pre)


# Task 2 - Manual

S2.pre <- (var(A_pre) + var(B_pre))/2

t_stat <- (A_pre_mean - B_pre_mean) / (sqrt(S2.pre) * sqrt(0.2))

2*pt(t_stat, 18, lower.tail = F)

# Task 2 - t.test

t.test(A_pre, B_pre, var.equal=T)


# Task 3 - Manual

A_diff <- data.t.test$diff[data.t.test$medicine=="A"]
B_diff <- data.t.test$diff[data.t.test$medicine=="B"]

A_diff_mean <- mean(A_diff)
B_diff_mean <- mean(B_diff)

S2.diff <- (var(A_diff) + var(B_diff))/2

t_stat <- (A_diff_mean - B_diff_mean) / (sqrt(S2.diff) * sqrt(0.2))

pt(t_stat, 18, lower.tail=T)

# Task 3 - t.test

t.test(
  data.t.test[data.t.test$medicine=="A","diff"],
  data.t.test[data.t.test$medicine=="B","diff"],
  var.equal=T, 
  alt="less"
)


# Task 4 A - Manual

A_post <- data.t.test$post[data.t.test$medicine=="A"]

A_diff_sd <- sd(A_diff)

t_stat <- A_diff_mean / (A_diff_sd / sqrt(A_n))

pt(t_stat, A_n - 1, lower.tail=F)

# Task 4 A - t.test

t.test(
  A_pre,
  A_post, 
  var.equal=T, 
  alt="greater",
  paired=T
)


# Task 4 B - Manual

B_post <- data.t.test$post[data.t.test$medicine=="B"]

B_diff_sd <- sd(B_diff)

t_stat <- B_diff_mean / (B_diff_sd / sqrt(B_n))

pt(t_stat, B_n - 1, lower.tail=F)

# Task 4 B - t.test

t.test(
  B_pre,
  B_post, 
  var.equal=T, 
  alt="greater",
  paired=T
)


# Task 5 A - Manual

A_marg <- A_diff_sd / sqrt(A_n) * qt(0.95, 9)

c(A_diff_mean - A_marg, A_diff_mean + A_marg)

# Task 5 A - t.test

t.test(A_diff, conf.level=0.9)

# Task 5 B - Manual

B_marg <- B_diff_sd / sqrt(B_n) * qt(0.95, 9)

c(B_diff_mean - B_marg, B_diff_mean + B_marg)

# Task 5 B - t.test

t.test(B_diff, conf.level=0.9)


# Task 6 - Manual

S2.diff <- sqrt((var(A_diff) + var(B_diff)) * 0.5)

mean_diff <- A_diff_mean - B_diff_mean
diff_marg <- qt(0.995, (A_n + B_n - 2)) * S2.diff * sqrt(0.2)

c(mean_diff - diff_marg, mean_diff + diff_marg)

# Task 6 - t.test

t.test(
  A_diff,
  B_diff,
  conf.level=0.99,
  var.equal=T
)
