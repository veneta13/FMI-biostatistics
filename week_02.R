# Task 7 - Manual

n <- 1506
p <- 0.73

se <- sqrt(p * (1 - p) / n) 
margin <- qnorm(0.975) * se

ci <- c(p - margin, p + margin)

# Task 7 - prop.test

prop.test(p * n, n)


# Task 8 CI - Manual

n1 <- 800
u1 <- 337
p1 <- u1 / n1

n2 <- 640
u2 <- 374
p2 <- u2 / n2

p_diff <- p1 - p2
margin <- qnorm(0.95) * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
v_margin <- 3 * sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))

vi <- c(p_diff - v_margin, p_diff + v_margin)

ci <- c(p_diff - margin, p_diff + margin)

# Task 8 CI - prop.test

prop.test(c(u1, u2), c(n1, n2), conf.level=0.9)

# Task 8 HT - Manual

p <- (u1 + u2) / (n1 + n2)

z_star <- p_diff / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))

p_value <- 2 * (1 - pnorm(abs(z_star)))

# Task 8 HT - prop.test

prop.test(
  c(u1, u2),
  c(n1, n2), 
  alt="two.sided", 
  conf.level=0.9
)


############################################
# ANOVA

library(UsingR)
library(gplots)
library(faraway)

data <- read.table("data/ralitsasdataanova.txt", row.names=1, header=T)
attach(data)

# Task 1

anova(lm(Post_trt~Group, data))

# Task 2

data$Diff <- Pre_trt - Post_trt

anova(lm(Diff~Group, data))

# Task 3

t.test(data$Post_trt[data$Group=="A"], conf.level=0.95)$conf.int
t.test(data$Post_trt[data$Group=="B"], conf.level=0.95)$conf.int
t.test(data$Post_trt[data$Group=="C"], conf.level=0.95)$conf.int

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

# Task 4 B

anova(lm(g ~ group, weights))

# Task 4 C

fit <- aov(
  g ~ group, 
  data = subset(weights, weights$group == '10g' | weights$group == '20g')
)

TukeyHSD(fit, conf.level=0.95)

# Task 4 D


