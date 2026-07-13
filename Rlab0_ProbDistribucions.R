### ==================================================
### Estadística Grau Matemàtiques
### Rlab0: Funcions de distribució
### ==================================================

quartz(width = 20, height = 12)
par(mfrow = c(2,4))

# 1. Distribució Normal
?dnorm
x <- seq(-4, 4, length=100)
y <- dnorm(x, mean=0, sd=1)
plot(x, y, type="l", col="blue", lwd=2, main="PDF Normal",
     xlab="x", ylab="Densitat")
legend("topright", legend="Mitjana=0, DE=1", col="blue", lwd=2)

y_cdf <- pnorm(x, mean=0, sd=1)
plot(x, y_cdf, type="l", col="blue", lwd=2, main="CDF Normal",
     xlab="x", ylab="Probabilitat Acumulada")
legend("bottomright", legend="Mitjana=0, DE=1", col="blue", lwd=2)

# 2. Distribució Binomial
?dbinom
x <- 0:10
y <- dbinom(x, size=10, prob=0.5)
plot(x, y, type="h", col="red", lwd=2, main="PMF Binomial",
     xlab="Nombre d'Èxits", ylab="Probabilitat")
points(x, y, pch=16, col="red")
legend("topright", legend="n=10, p=0.5", col="red", lwd=2)

y_cdf <- pbinom(x, size=10, prob=0.5)
plot(x, y_cdf, type="s", col="red", lwd=2, main="CDF Binomial",
     xlab="Nombre d'Èxits", ylab="Probabilitat Acumulada")
legend("bottomright", legend="n=10, p=0.5", col="red", lwd=2)

# 3. Distribució de Poisson
?dpois
x <- 0:15
y <- dpois(x, lambda=4)
plot(x, y, type="h", col="green", lwd=2, main="PMF Poisson",
     xlab="Ocurrències", ylab="Probabilitat")
points(x, y, pch=16, col="green")
legend("topright", legend="Lambda=4", col="green", lwd=2)

y_cdf <- ppois(x, lambda=4)
plot(x, y_cdf, type="s", col="green", lwd=2, main="CDF Poisson",
     xlab="Ocurrències", ylab="Probabilitat Acumulada")
legend("bottomright", legend="Lambda=4", col="green", lwd=2)

# 4. Distribució Exponencial
?dexp
x <- seq(0, 5, length=100)
y <- dexp(x, rate=1)
plot(x, y, type="l", col="purple", lwd=2, main="PDF Exponencial",
     xlab="x", ylab="Densitat")
legend("topright", legend="Taxa=1", col="purple", lwd=2)

y_cdf <- pexp(x, rate=1)
plot(x, y_cdf, type="l", col="purple", lwd=2, main="CDF Exponencial",
     xlab="x", ylab="Probabilitat Acumulada")
legend("bottomright", legend="Taxa=1", col="purple", lwd=2)

# ============================================================================
# EXERCICIS
# ============================================================================

# EXERCICI 1: Distribució Normal en R
# Suposem que les altures de les persones adultes en una certa població segueixen 
# una distribució Normal amb:
# Mitjana = 175 cm
# Desviació estàndard = 8 cm
# 
# Preguntes:
#   
# 1) Càlcul de Probabilitats (CDF)
# Quina és la probabilitat que una persona seleccionada aleatòriament 
# sigui més baixa que 170 cm?
# Quina és la probabilitat que una persona seleccionada aleatòriament 
# sigui més alta que 185 cm?
# Quina és la probabilitat que una persona seleccionada aleatòriament 
# tingui una alçada entre 165 cm i 180 cm?
#   
# 2) Quantils (CDF Inversa)
# Troba l'alçada corresponent al percentil 90 (és a dir, el 90% dels 
# subjectes són més baixos que aquesta alçada).
# Troba l'alçada que separa el 5% més alt de la resta.
# 
# 3) Generació de Mostres Aleatòries
# Genera una mostra aleatòria de 10 alçades de persones d'aquesta 
# distribució i mostra-les.
# Calcula la mitjana i la desviació estàndard de la mostra. 
# Coincideix amb els paràmetres de la població?

# PISTES:
?pnorm
?qnorm
?rnorm

# EXERCICI 2: Distribució Binomial en R
# Una empresa de telecomunicacions sap que el 30% dels clients que contracten 
# un servei d'internet el cancel.len durant el primer any.
# L'empresa ha contractat 15 nous clients aquest mes.
# 
# Preguntes:
#   
# 1) Càlcul de Probabilitats (PMF i CDF)
# Quina és la probabilitat que exactament 5 clients cancel.lin el servei?
# Quina és la probabilitat que 3 o menys clients cancel.lin el servei?
# Quina és la probabilitat que almenys 8 clients cancel.lin el servei?
# Quina és la probabilitat que entre 4 i 7 clients (ambdós inclosos) 
# cancel.lin el servei?
#   
# 2) Valor Esperat i Variància
# Quin és el nombre esperat de cancel.lacions?
# Quina és la variància del nombre de cancel.lacions?
# 
# 3) Simulació
# Simula 1000 experiments on cada experiment representa els 15 nous clients.
# Per a cada experiment, compta quants clients cancel.len.
# Crea un histograma dels resultats i compara'l amb la distribució 
# teòrica binomial.
# Calcula la mitjana de les cancel.lacions en les 1000 simulacions. 
# S'assembla al valor esperat teòric?

# PISTES:
?dbinom  # Funció de massa de probabilitat
?pbinom  # Funció de distribució acumulada
?rbinom  # Generació de nombres aleatoris
# Valor esperat: n * p
# Variància: n * p * (1-p)

# ============================================================================
# SOLUCIONS EXERCICI 1
# ============================================================================

# 1)
pnorm(170, mean=175, sd = 8)
1-pnorm(185, mean=175, sd = 8)
pnorm(180, mean=175, sd = 8)-pnorm(165, mean=175, sd = 8)

# 2)
qnorm(0.90, mean=175, sd = 8)
qnorm(0.95, mean=175, sd = 8)

# 3)
set.seed(123)  # Per reproducibilitat
mu <- 175  # Mitjana
sigma <- 8  # Desviació estandard
sample_heights <- rnorm(10, mean=mu, sd=sigma)  # Genera 10 alçades  
sample_mean <- mean(sample_heights)  # Calcula la mitjana
sample_sd <- sd(sample_heights)  # Calcula la desviació estàndard

# Visualització per comparar distribució teorica i la mostra generada
quartz()
par(mfrow=c(1,3))

# Gràfic 1: PDF amb àrees de probabilitat
x_vals <- seq(145, 205, length=200)
y_vals <- dnorm(x_vals, mean=175, sd=8)
plot(x_vals, y_vals, type="l", col="blue", lwd=2,
     main="Distribució Normal d'Alçades",
     xlab="Alçada (cm)", ylab="Densitat")
abline(v=175, col="red", lty=2, lwd=2)
abline(v=c(170, 185), col="orange", lty=2)
legend("topright", 
       legend=c("Mitjana=175", "170 cm", "185 cm"),
       col=c("red", "orange", "orange"),
       lty=c(2,2,2), lwd=2)

# Gràfic 2: Mostra aleatòria n=10
hist(sample_heights, breaks=5, col="lightblue", border="black",
     main="Mostra Aleatòria (n=10)",
     xlab="Alçada (cm)", ylab="Freqüència")
abline(v=sample_mean, col="red", lwd=2, lty=2)
abline(v=175, col="blue", lwd=2, lty=1)
legend("topright",
       legend=c("Mitjana mostra", "Mitjana poblacional"),
       col=c("red", "blue"), lty=c(2,1), lwd=2)

# Gràfic 2: Mostra aleatòria n=100
sample100_heights <- rnorm(100, mean=mu, sd=sigma)
sample100_mean <- mean(sample100_heights)
hist(sample100_heights, breaks=5, col="lightblue", border="black",
     main="Mostra Aleatòria (n=10)",
     xlab="Alçada (cm)", ylab="Freqüència")
abline(v=sample100_mean, col="red", lwd=2, lty=2)
abline(v=175, col="blue", lwd=2, lty=1)
legend("topright",
       legend=c("Mitjana mostra", "Mitjana poblacional"),
       col=c("red", "blue"), lty=c(2,1), lwd=2)


# ============================================================================
# SOLUCIONS EXERCICI 2
# ============================================================================

# 1) Càlcul de Probabilitats
dbinom(5, size=15, prob=0.30) # a) P(X = 5)
pbinom(3, size=15, prob=0.30) # b) P(X ≤ 3)
1 - pbinom(7, size=15, prob=0.30) # c) P(X ≥ 8)
# O alternativament: pbinom(7, size=15, prob=0.30, lower.tail=FALSE)
pbinom(7, size=15, prob=0.30) - pbinom(3, size=15, prob=0.30) # d) P(4 ≤ X ≤ 7)

# 2) Valor Esperat i Variància
n <- 15
p <- 0.30
valor_esperat <- n * p
variancia <- n * p * (1 - p)
desviacio_estandard <- sqrt(variancia)

# 3) Simulació 
set.seed(456)  # Per reproducibilitat
simulacions <- rbinom(1000, size=15, prob=0.30)

mitjana_simulacions <- mean(simulacions)
de_simulacions <- sd(simulacions) 

# Visualització
quartz(width = 20, height = 15)
par(mfrow=c(1,2))

# Gràfic 1: PMF i CDF teòriques
x_bin <- 0:15
y_pmf <- dbinom(x_bin, size=15, prob=0.30)

plot(x_bin, y_pmf, type="h", col="red", lwd=3,
     main="Distribució Binomial (n=15, p=0.30)",
     xlab="Nombre de Cancel·lacions", ylab="Probabilitat")
points(x_bin, y_pmf, pch=16, col="red", cex=1.2)
abline(v=valor_esperat, col="blue", lwd=2, lty=2)
legend("topright", 
       legend=c("PMF", paste("E(X) =", round(valor_esperat, 2))),
       col=c("red", "blue"), lty=c(1,2), lwd=2)

# Gràfic 2: Comparació Simulació vs Teòrica
hist(simulacions, breaks=seq(-0.5, 15.5, 1), 
     col=rgb(0, 0, 1, 0.3), border="blue",
     main="Simulació vs Distribució Teòrica",
     xlab="Nombre de Cancel·lacions", 
     ylab="Freqüència",
     freq=FALSE)

# Superposar la distribució teòrica
points(x_bin, y_pmf, type="h", col="red", lwd=3)
points(x_bin, y_pmf, pch=16, col="red", cex=1.2)

abline(v=mitjana_simulacions, col="blue", lwd=2, lty=2)
abline(v=valor_esperat, col="red", lwd=2, lty=1)

legend("topright",
       legend=c("Simulació", "Teòrica", 
                "Mitjana simulació", "E(X) teòric"),
       col=c("blue", "red", "blue", "red"),
       lty=c(1, 1, 2, 1), lwd=2)
