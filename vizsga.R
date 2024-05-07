ax = 117
ay = 51
az = 114
av = 111
ss = 634
ev = 2018

# Mintarealizáció generálása
set.seed(ss)

nx=700
v=matrix(c(ax,abs(ax-ay),abs(ax-ay),ay),2)
w=chol(v)
z1=sqrt(-2*log(runif(nx)))*sin(runif(nx)*2*pi)
z2=sqrt(-2*log(runif(nx)))*cos(runif(nx)*2*pi)
zm=matrix(c(z1,z2),ncol=2)
zn=5*zm%*%w


# Statisztikai elemzés, paraméterek becslése 
summary(zn)
# Min: A mintarealizáció legkisebb értéke
# 1st Qu. : Első kvartilis - A mintarealizáció 25%-a ez alá az érték alá esik
# Median: A mintarealizáció mediánja
# Mean: A mintarealizáció empirikus közepe
# 3rd Qu. : Harmadik kvartilis - A mintarealizáció 75%-a ez alá az érték alá esik
# Max. : A mintarealizáció legnagyobb értéke

library(moments)
# Ferdeség
skewness(zn)

# Lapultság
kurtosis(zn)

# Peremek függetlenségének vizsgálata
# Korrelációs mátrix:
cor(zn)
# Ha a korrelációs mátrix diagonális, vagyis a nem-diagonális elemek közelítőleg 0-k, akkor a peremek függetlenek lehetnek.
# "Az adatok erősen korreláltak." (Ha 0-tól távoli) 

#cor.test(zn[,1],zn[,2])

# Eloszlás vizsgálat

# mean_zn = colMeans(zn)
# cov_zn = cov(zn)

# marginal_1 = list(mean = mean_zn[1], sd = sqrt(cov_zn[1, 1]))
# marginal_2 = list(mean = mean_zn[2], sd = sqrt(cov_zn[2, 2]))


#Vizuális igazolás beépített függvényekkel.
#Sűrűségdiagrammal: A görbe bizonyítja a Poisson/Normális eloszlást.
library(ggpubr)
ggdensity(zn[,1], main = "Sűrűségfüggvény")
ggdensity(zn[,2], main = "Sűrűségfüggvény")
# Vizsgálat Kvantilis diagram alapján:
# Megrajzolja az összefüggést egy adott minta és a normális eloszlás között, 45 fokos referenciavonalon.
library(car)
qqPlot(zn)


# Ábrázolások

# Adatok betöltése
x <- zn[,1]
y <- zn[,2]

# Rács készítése
grid_size <- 20
x_range <- seq(min(x), max(x), length.out = grid_size)
y_range <- seq(min(y), max(y), length.out = grid_size)

# Kétváltozós sűrűség becslése
library(MASS)  # csomag a kde2d() függvényhez
fhat <- kde2d(x, y, n = grid_size)

# Plot-ok 2x2-es elhelyezése
par(mfrow = c(2, 2))

# Többdimenziós ábrázolás
plot(zn, main = "A zn mátrix")

# Szintvonalas ábrázolás
contour(
  x_range,
  y_range,
  fhat$z,
  xlab = "X",
  ylab = "Y",
  main = "Szintvonalak"
)

# Perspektívikus ábrázolás
persp(
  x_range,
  y_range,
  fhat$z,
  theta = 45,
  phi = 30,
  xlab = "X",
  ylab = "Y",
  zlab = "Z",
  main = "Perspektívikus ábrázolás"
)
  #------------------------------------------------------------
  # Exponenciális
  set.seed(ss)
  # Minta méret
  nx <- 700
  # Korrelációs együttható
  rho <- -0.7
  # Kovariancia mátrix
  cov_matrix <- matrix(c(1, rho, rho, 1), ncol = 2)

  # Eloszlás generálás
  library(MASS)
  data <- mvrnorm(n = nx, mu = c(0, 0), Sigma = cov_matrix)

  # Transzformálás exponenciálissá
  x <- rexp(nx, rate = exp(0.5 * data[, 1]))
  y <- rexp(nx, rate = exp(0.5 * data[, 2]))

  # Ábrázolás
  plot(x, y, xlab = "X", ylab = "Y", main = "Exponenciális eloszlás korrelációval")
  #------------------------------------------------------------
  #TITLE Brown szimulációs függvény definíciója
  brownian_motion <- function(T, dt, ss) {
    set.seed(ss) # Véletlenszám-generátor inicializálása
    t <- seq(0, T, dt) # Időlépések meghatározása
    W <- numeric(length(t)) # Wiener-folyamat tárolására szolgáló vektor
    W[1] <- 0 # Kezdeti érték (W(0) = 0)

    for (i in 2:length(t)) {
      dW <- rnorm(1, mean = 0, sd = sqrt(dt)) # Normális eloszlású lépések generálása
      W[i] <- W[i - 1] + dW # Wiener-folyamat értékének frissítése
    }

    plot(t, W, type = "l", main = "Sima Brown-folyamat (Wiener-folyamat)",
      xlab = "Idő", ylab = "Folyamat értéke", col = "blue")

    return(W) # A folyamat értékeinek visszaadása
  }
  # Függvény meghívása
  W <- brownian_motion(1, dt, ss) # T = 1, dt = 1/365
#------------------------------------------------------------
#TITLE Geometriai Brown szimulációs függvény definíciója
gbm <- function(mu, sigma, T, S0, dt, ss) {
  set.seed(ss + 27) # Véletlenszám-generátor inicializálása

  # Időlépések meghatározása
  t <- seq(0, T, dt) # T időpontig dt lépésközzel

  # Kezdeti értékek inicializálása
  S <- numeric(length(t)) # Sztochasztikus folyamat értékeinek tárolója
  S[1] <- S0 # Kezdeti érték beállítása

  # GBM szimuláció végrehajtása
  for (i in 2:length(t)) {
    dW <- rnorm(1, mean = 0, sd = sqrt(dt)) # Wiener-folyamat lépése (normális eloszlásból)
    S[i] <- S[i - 1] * exp((mu - 0.5 * sigma ^ 2) * dt + sigma * dW) # GBM képlet alkalmazása
  }

  # Az eredmény grafikus megjelenítése
  plot(t, S, type = "l", main = "Geometriai Brown folyamat",
       xlab = "Idő", ylab = "Ár", col = "blue")

  return(S) # Szimulációs eredmények visszaadása
}
# Függvény meghívása konkrét paraméterekkel
S <- gbm(mu = ax, sigma = (ax + ay) / (ax + ay + az), T = 100 / 365, S0 = 100, dt = 1 / 365, ss)
#------------------------------------------------------------
#TITLE Log-hozamok számítása és Khi-négyzet teszt végrehajtása a META.csv fájlra
# import the csv
details <- read.csv("META.csv")
logreturn = c() # logreturn létrehozása
zaro <- details$Close # A záró értékek kinyerése
for (i in 1:length(zaro) - 1) {
  logreturn[i] = log(zaro[i + 1] / zaro[i]) # logreturn értékek begyűjtése
}
chisq.test(logreturn) # Khi-négyzet teszt
hist(logreturn, main = "Záró árak változása") # Hisztogram
#------------------------------------------------------------
#TITLE Egy évre becsülés
mu <- mean(logreturn) # logreturn átlaga
sig <- sd(logreturn) # logreturn szórása
price <- rep(NA, 365) # egy év
price[1] <- zaro[length(zaro)] # utolsó ismert érték
# Árak szimulálása
for (i in 2:365) {
  price[i] <- price[i - 1] * exp(rnorm(1, mu, sig))
}
random_data <- cbind(price, 1:365)
colnames(random_data) <- c("Price", "Day")
random_data <- as.data.frame(random_data)
random_data %>% ggplot(aes(Day, Price)) + geom_line() + labs(title = "Meta árfolyam szimuláció 1 évre") + theme_bw()
#------------------------------------------------------------
#TITLE Poisson folyamat
set.seed(ss + 17)
# Várható érték (lambda)
lambda <- 2

# Generálás
# Időintervallum
T <- 1000
n <- rpois(T, lambda)

# Kumulatív összeggé alakítás
n_cumsum <- cumsum(n)

# Ábrázolás
plot(0:T, c(0, n_cumsum), type = "s", xlab = "Time", ylab = "Number of Events", main = "Poisson Process")
#------------------------------------------------------------
