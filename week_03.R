library(UsingR)
library(gplots)
library(faraway)
library(dplyr)

data <- read.table("data/ralitsasdataanova.txt", row.names=1, header=T)
attach(data)


# Task 1 - anova

anova(lm(Post_trt~Group, data))

# Task 1 - manual

mean.gr <- rep(
  by(data$Post_trt, data$Group, mean), 
  each=10
)
mean.total <- mean(data$Post_trt)

sst <- sum((mean.gr - mean.total)^2)
sse <- sum((data$Post_trt - mean.gr)^2)

mst <- sst / 2
mse <- sse / 27

f_stat <- mst / mse
p_value <- pf(f_stat, 2, 27, lower.tail=F)
p_value


# Task 2 - anova

anova(lm((Pre_trt - Post_trt)~Group, data))

# Drug effect test

par(mfrow=c(1,2))
plotmeans(
  Pre_trt~Group, 
  data,
  ylim=c(120, 170),
  xlab="Group",
  ylab="Pre_trt", 
  main="Mean Plot\nwith 95% CI"
)
plotmeans(
  Post_trt~Group, 
  data,
  ylim=c(120, 170),
  xlab="Group",
  ylab="Post_trt", 
  main="Mean Plot\nwith 95% CI"
)

data_A <- subset(data[data$Group == "A", ], select=c(Pre_trt , Post_trt))
data_A <- stack(data_A)

data_B <- subset(data[data$Group == "B", ], select=c(Pre_trt , Post_trt))
data_B <- stack(data_B)

data_C <- subset(data[data$Group == "C", ], select=c(Pre_trt , Post_trt))
data_C <- stack(data_C)

# Task 2 A - anova
anova(lm(values~ind, data_A))

# Task 2 A - manual
mean.gr <- rep(
  by(data_A$values, data_A$ind, mean), 
  each=10
)
mean.total <- mean(data_A$values)
sst <- sum((mean.gr - mean.total)^2)
sse <- sum((data_A$values - mean.gr)^2)
mst <- sst
mse <- sse / 18
f_stat <- mst / mse
p_value <- pf(f_stat, 1, 18, lower.tail=F)
p_value

# Task 2 A - t.test
t.test(
  data_A$values[data_A$ind == 'Pre_trt'],
  data_A$values[data_A$ind == 'Post_trt'], 
  var.equal=T, 
  alt="greater",
  paired=T
)

# Task 2 B - anova
anova(lm(values~ind, data_B))

# Task 2 B - manual
mean.gr <- rep(
  by(data_B$values, data_B$ind, mean), 
  each=10
)
mean.total <- mean(data_B$values)
sst <- sum((mean.gr - mean.total)^2)
sse <- sum((data_B$values - mean.gr)^2)
mst <- sst
mse <- sse / 18
f_stat <- mst / mse
p_value <- pf(f_stat, 1, 18, lower.tail=F)
p_value

# Task 2 B - t.test
t.test(
  data_B$values[data_B$ind == 'Pre_trt'],
  data_B$values[data_B$ind == 'Post_trt'], 
  var.equal=T, 
  alt="greater",
  paired=T
)

# Task 2 C - anova
anova(lm(values~ind, data_C))

# Task 2 C - manual
mean.gr <- rep(
  by(data_C$values, data_C$ind, mean), 
  each=10
)
mean.total <- mean(data_C$values)
sst <- sum((mean.gr - mean.total)^2)
sse <- sum((data_C$values - mean.gr)^2)
mst <- sst
mse <- sse / 18
f_stat <- mst / mse
p_value <- pf(f_stat, 1, 18, lower.tail=F)
p_value

# Task 2 C - t.test
t.test(
  data_C$values[data_C$ind == 'Pre_trt'],
  data_C$values[data_C$ind == 'Post_trt'], 
  var.equal=T, 
  alt="greater",
  paired=T
)

# Task 3 A - manual
mean_A <- mean(data_A$values[data_A$ind == 'Post_trt'])
sd_A <- sd(data_A$values[data_A$ind == 'Post_trt'])
margin <- sd_A / sqrt(10) * qt(0.975, 9)
c(mean_A - margin, mean_A + margin)

# Task 3 A - t.test
t.test(data_A$values[data_A$ind == 'Post_trt'], conf.level=0.95)$conf.int

# Task 3 B - manual
mean_B <- mean(data_B$values[data_B$ind == 'Post_trt'])
sd_B <- sd(data_B$values[data_B$ind == 'Post_trt'])
margin <- sd_B / sqrt(10) * qt(0.975, 9)
c(mean_B - margin, mean_B + margin)

# Task 3 B - t.test
t.test(data_B$values[data_B$ind == 'Post_trt'], conf.level=0.95)$conf.int

# Task 3 C - manual
mean_C <- mean(data_C$values[data_C$ind == 'Post_trt'])
sd_C <- sd(data_C$values[data_C$ind == 'Post_trt'])
margin <- sd_C / sqrt(10) * qt(0.975, 9)
c(mean_C - margin, mean_C + margin)

# Task 3 C - t.test
t.test(data_C$values[data_C$ind == 'Post_trt'], conf.level=0.95)$conf.int

# Task 4

g0  <- c(5.2, 4.8, 5.1, 4.6, 4.1)
g10 <- c(5.7, 5.8, 6.2, 5.3, 6.0)
g20 <- c(6.2, 6.0, 5.8, 6.7, 6.2)
weights <- data.frame(
  c(g0, g10, g20), 
  c(rep('Plac', 5), rep('10g', 5), rep('20g', 5))
)
names(weights) <- c('g', 'group')
attach(weights)

# Task 4 B - anova

anova(lm(g ~ group, weights))

# Task 4 B - manual

mean_groups <- rep(
  by(weights$g, weights$group, mean), 
  each=5
)
mean_total <- mean(weights$g)
sst <- sum((mean_groups - mean_total)^2)
sse <- sum((weights$values - mean_groups)^2)
mst <- sst / 2
mse <- sse / 12
f_stat <- mst / mse
p_value <- pf(f_stat, 2, 12, lower.tail=F)
p_value

# Task 4 C - manual Tukey

data_10_20 <- filter(weights, group == '20g' | group == '10g')

fit <- aov(
  g ~ group, 
  data = data_10_20
)
summary(fit)

mean_20g <- mean(weights$g[weights$group == '20g'])
mean_10g <- mean(weights$g[weights$group == '10g'])

margin <- qtukey(0.95, 2, 8) * sqrt(0.1135) * sqrt(1/5 + 1/5)/sqrt(2)
means <- mean_20g - mean_10g

c(means - margin, means + margin)

# Task 4 C - TukeyHSD

TukeyHSD(fit, conf.level=0.95)

# Task 4 C - t.test

t.test(
  data_10_20$g[data_10_20$group == '20g'],
  data_10_20$g[data_10_20$group == '10g']
)

# Task 4 D - Manual
anova(lm(g ~ group, weights))

sigma_hat <- sqrt(0.14/12)

# Placebo - 10g
mean_diff <- mean(weights$g[group=="Plac"]) - mean(weights$g[group=="10g"])
margin <- qt(1-0.06/6, 12) * sigma_hat * sqrt(1/5 + 1/5)
c(mean_diff - margin, mean_diff + margin)

# Placebo - 20g
mean_diff <- mean(weights$g[group=="Plac"]) - mean(weights$g[group=="20g"])
margin <- qt(1-0.06/6, 12) * sigma_hat * sqrt(1/5 + 1/5)
c(mean_diff - margin, mean_diff + margin)

# 20g - 10g
mean_diff <- mean(weights$g[group=="20g"]) - mean(weights$g[group=="10g"])
margin <- qt(1-0.06/6, 12) * sigma_hat * sqrt(1/5 + 1/5)
c(mean_diff - margin, mean_diff + margin)
