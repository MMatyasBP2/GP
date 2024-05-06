x="klnspg";
z=charToRaw(iconv(x, "latin1", "UTF-8"))
for (i in 1:6) v=paste("0x",z,sep="")
e=strtoi(v)
ax=e[1];ay=e[2];az=e[3];av=e[4];ss=sum(strtoi(v))+24
cat("ax=",ax,"\n")
cat("ay=",ay,"\n")
cat("az=",az,"\n")
cat("av=",av,"\n")
cat("ss=",ss,"\n")
ar=c( "FB","AAPL","AMZN","GOOG","NFLX","TSLA")
ai=ss-6*floor(ss/6)
ev=2022-(ss-10*floor(ss/10))
cat("ev=",ev,"\n")
cat("reszveny=",ar[ai+1],"\n")

#1. feladat

set.seed(ss)
nx=700
v=matrix(c(ax,abs(ax-ay),abs(ax-ay),ay),2)
w=chol(v)
z1=sqrt(-2*log(runif(nx)))*sin(runif(nx)*2*pi)
z2=sqrt(-2*log(runif(nx)))*cos(runif(nx)*2*pi)
zm=matrix(c(z1,z2),ncol=2)
zn=5*zm%*%w

mean_zn1 <- mean(zn[,1])
mean_zn2 <- mean(zn[,2])
sd_zn1 <- sd(zn[,1])
sd_zn2 <- sd(zn[,2])

cat("Az első változó várható értéke:", mean_zn1, "\n")
cat("Az első változó szórása:", sd_zn1, "\n")
cat("A második változó várható értéke:", mean_zn2, "\n")
cat("A második változó szórása:", sd_zn2, "\n")

ks_test_1 <- ks.test(zn[,1], "pnorm", mean = mean_zn1, sd = sd_zn1)
print(ks_test_1)

ks_test_2 <- ks.test(zn[,2], "pnorm", mean = mean_zn2, sd = sd_zn2)
print(ks_test_2)

correlation <- cor(zn[,1], zn[,2])
print(correlation)

#2. feladat

target_correlation <- -0.7
cov_matrix <- matrix(c(sd_zn1^2, target_correlation * sd_zn1 * sd_zn2,
                       target_correlation * sd_zn1 * sd_zn2, sd_zn2^2), nrow = 2)

exponential_sample <- MASS::mvrnorm(n = nx, mu = c(mean_zn1, mean_zn2), Sigma = cov_matrix)

plot(exponential_sample, xlab = "X", ylab = "Y", main = "Exponenciális eloszlású mintára realizáció")

# 3. feladat

set.seed(ss + 37)
mu <- ax
sigma <- (ax + az) / (ax + ay + az)
time_interval <- 500
time_steps <- 1:time_interval
brownian_motion <- cumsum(rnorm(time_interval, mean = mu, sd = sigma))

plot(time_steps, brownian_motion, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "Geometriai Brown-folyamat")

cat("Átlag:", mean(brownian_motion), "\n")
cat("Szórás:", sd(brownian_motion), "\n")
cat("Maximum:", max(brownian_motion), "\n")
cat("Minimum:", min(brownian_motion), "\n")

# 4. feladat

set.seed(ss + 17)
lambda <- 2
time_interval <- 1000
time_steps <- 1:time_interval
poisson_process <- rpois(n = time_interval, lambda = lambda)
plot(time_steps, poisson_process, type = "l", col = "green", xlab = "Time", ylab = "Number of occurrences", main = "Poisson-folyamat")
expected_occurrences <- mean(poisson_process)
cat("Bekövetkezések várható száma:", expected_occurrences, "\n")

# 5. feladat

aapl_data <- read.csv("C:/Users/Matyi/Desktop/AAPL.csv")
head(aapl_data)
log_returns <- diff(log(aapl_data$Close), lag = 1)
hist(log_returns, breaks = 30, freq = FALSE, main = "Log-hozamok eloszlása", xlab = "Log-return")
mean_log_return <- mean(log_returns)
next_year_expected_price <- aapl_data$Close[nrow(aapl_data)] * exp(mean_log_return)


