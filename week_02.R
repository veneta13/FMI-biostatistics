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
