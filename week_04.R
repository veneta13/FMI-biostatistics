library(gplots)

dataanovablocks <- read.table("data/ralitsasdataanovablocks.txt", header=T)

attach(dataanovablocks)
stripchart(Difference~Group,col=2:4)

plotmeans(
  Difference~Group,
  xlab="Group",
  ylab="Difference", 
  main="Mean Plot\nwith 95% CI"
)

anova(lm(Difference ~ Group + factor(Subject), dataanovablocks))

groupmeans <- by(dataanovablocks[,3], dataanovablocks$Group, mean)

fit <- aov(Difference ~ Group + factor(Subject), dataanovablocks)

## 94 CI
# Tukey
TukeyHSD(fit, conf.level = 0.94)

groupmeans[2]-groupmeans[1]- qtukey(0.94,3,18)*sqrt(74.87*2/10)/sqrt(2)  
groupmeans[2]-groupmeans[1]+ qtukey(0.94,3,18)*sqrt(74.87*2/10)/sqrt(2)  

## Bonferonni
groupmeans[2]-groupmeans[1]- qt(0.99,18)*sqrt(74.87*2/10) 
groupmeans[2]-groupmeans[1]+ qt(0.99,18)*sqrt(74.87*2/10)


# ANOVA - manual

mean_bck <- rep(
  by(dataanovablocks[,3], dataanovablocks[,1], mean), 
  times=3
)

  mean_trt <- rep(
  by(dataanovablocks[,3], dataanovablocks[,2], mean), 
  each=10
)

mean_total <- mean(dataanovablocks[,3])

sst <- sum((mean_trt - mean_total)^2)
ssb <- sum((mean_bck - mean_total)^2)
sse <- sum((dataanovablocks[,3] - mean_bck - mean_trt + mean_total)^2)

mst <- sst / 2
msb <- ssb / 9
mse <- sse / 18

f_stat_trt <- mst / mse
f_stat_bck <- msb / mse

p_value_trt <- pf(f_stat_trt, 2, 18, lower.tail=F)
p_value_grp <- pf(f_stat_bck, 3, 18, lower.tail=F)


# https://rpubs.com/andikaputri/RCBD

insec_1 <- c(56, 48, 66, 62)
insec_2 <- c(83, 78, 94, 93)
insec_3 <- c(80, 72, 83, 85)

data <- data.frame(insec_1, insec_2, insec_3)
data <- stack(data)
data$plot <- rep(c(1:4), times=3)
names(data) <- c('value', 'treatment', 'block')
data$treatment <- as.factor(data$treatment)
data$block <- as.factor(data$block)

plotmeans(value ~ treatment, data=data)
stripchart(value ~ treatment, data=data)

mean_bck <- rep(
  by(data[,1], data[,3], mean), 
  times=3
)

mean_trt <- rep(
  by(data[,1], data[,2], mean), 
  each=4
)

mean_total <- mean(data[,1])

sst <- sum((mean_trt - mean_total)^2)
ssb <- sum((mean_bck - mean_total)^2)
sse <- sum((data[,1] - mean_bck - mean_trt + mean_total)^2)

mst <- sst / 2
msb <- ssb / 3
mse <- sse / 6

f_stat_trt <- mst / mse
f_stat_bck <- msb / mse

p_value_trt <- pf(f_stat_trt, 2, 6, lower.tail=F)
p_value_grp <- pf(f_stat_bck, 3, 6, lower.tail=F)

anova(lm(value ~ treatment + block, data))

# Tukey

TukeyHSD(
  aov(value ~ treatment + block, data),
  conf.level = 0.94
)

# Bonferonni

mean_1 <- mean(data$value[data$block==1])
mean_2 <- mean(data$value[data$block==2])
mean_3 <- mean(data$value[data$block==3])
mean_4 <- mean(data$value[data$block==4])

# 2 - 1
mean_diff <- mean_2 - mean_1
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)

# 3 - 1
mean_diff <- mean_3 - mean_1
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)

# 4 - 1
mean_diff <- mean_4 - mean_1
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)

# 3 - 2
mean_diff <- mean_3 - mean_2
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)

# 4 - 2
mean_diff <- mean_4 - mean_2
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)

# 4 - 3
mean_diff <- mean_3 - mean_1
margin <- qt(0.99, 6) * sqrt(4.33 * 2/8) 
c(mean_diff - margin, mean_diff + margin)
