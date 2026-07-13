### ==================================================
### Estadística Grau Matemàtiques
### marta.bofill.roig@upc.edu // mireia.besalu@upc.edu
### Rlab6: Test de Neyman–Pearson, error tipus 1
### i potència
### ==================================================

# --- Paràmetres del problema --------------------------------------------------

alpha <- 0.05   # nivell de significació
sigma <- 1      # desviació típica coneguda
mu0   <- 0      # hipòtesi nul·la
mu1   <- 1      # hipòtesi alternativa


# --- Mostra de l'enunciat -----------------------------------------------------
# Executa aquest bloc tal com està: genera la mostra reproduïble de l'enunciat.

set.seed(42)
mostra <- round(rnorm(10, mean = 1, sd = 1), 3)
mostra


# =============================================================================
# APARTAT 1. Fonament teòric
# =============================================================================
# No cal codi. Responeu per escrit (en comentaris o en un document apart):
#
# a) Escriviu la versemblança conjunta L(mu | x1,...,xn) per a X_i ~ N(mu, sigma^2).
#
# b) Escriviu la raó de versemblances Lambda(x) = L(mu1|x) / L(mu0|x).
#
# c) Calculeu la regió de rebuig és de la forma.


# =============================================================================
# APARTAT 2. Valor crític i nivell real (n = 10)
# =============================================================================

n <- 10

# Calculeu el valor crític c
# Pista: Sota H0, X̄ ~ N(mu0, sigma^2 / n) 
?qnorm
c_10 <- qnorm(0.95,mean=0,sd=1)/sqrt(n)
c_10

cat("Valor crític c =", round(c_10, 4), "\n")

# Pista: feu servir pnorm() 

alpha_real <- ...

cat("Nivell del test =", round(alpha_real, 6), "\n")

# Pregunta: coincideix exactament amb alpha? Per quin motiu?

# =============================================================================
# APARTAT 3. Decisió sobre la mostra de l'enunciat
# =============================================================================

# Calculeu la mitjana mostral
xbar <- mean(mostra)
cat("Mitjana mostral x̄ =", round(xbar, 4), "\n")

# Calculeu l'estadístic Z estandarditzat: Z = (x̄ - mu0) / (sigma / sqrt(n))
z_obs <- sqrt(n)*xbar
cat("Estadístic Z =", round(z_obs, 4), "\n")

# Calculeu el p-valor 
# Pista: 1 - pnorm(z_obs)
pvalor <- 1-pnorm(z_obs)
cat("p-valor =", round(pvalor, 4), "\n")

# Preneu la decisió comparant x̄ amb c_10 (o equivalentment z_obs amb qnorm(1-alpha))
if (...) {
  cat("Decisió: REBUTJA H0\n")
} else {
  cat("Decisió: NO rebutja H0\n")
}


# =============================================================================
# APARTAT 4. Simulació de 1000 experiments (n = 100)
# =============================================================================
#
# Esquema general:
#   1. Fixeu B = 1000 i n = 100.
#   2. Calculeu el valor crític c_100 per a aquesta n.
#   3. Error tipus I: genereu B mostres de mida n sota H0 (mu = mu0).
#      Per a cada mostra, calculeu x̄ i comproveu si x̄ >= c_100.
#      alpha_hat = proporció de vegades que es rebutja.
#   4. Potència: repetiu amb mostres generades sota H1 (mu = mu1).
#      potencia_hat = proporció de vegades que es rebutja.
#   5. Compareu amb els valors teòrics.

B <- 1000
n <- 100

# Valor crític per a n = 100
c_100 <- ...
cat("Valor crític per n = 100: c =", round(c_100, 4), "\n")

# --- Error de tipus I ---------------------------------------------------------
# Pista: replicate(B, mean(rnorm(n, mean = mu0, sd = sigma))) genera B mitjanes
#        de mostres de mida n sota H0.

set.seed(123)
xbars_H0  <- ...
alpha_hat <- ...
cat("Probabilitat estimada d'error tipus I =", alpha_hat, "\n")
cat("Nivell de significació =", alpha, "\n")

# --- Potència -----------------------------------------------------------------
set.seed(456)
xbars_H1     <- ...
potencia_hat <- ...

# Potència teòrica: P(X̄ >= c | mu1) = 1 - pnorm(c_100, mean = mu1, sd = sigma/sqrt(n))
potencia_teo <- ...

cat("Potència estimada  =", potencia_hat, "\n")
cat("Potència teòrica   =", round(potencia_teo, 4), "\n")


# =============================================================================
# APARTAT 5a. Gràfic: corba de potència per a n = 100
# =============================================================================

mu1_grid  <- seq(0.01, 3, by = 0.01)

# Calculeu la potència teòrica per a cada valor de mu1_grid
# Pista: 1 - pnorm(c_100, mean = mu1_grid, sd = sigma / sqrt(n))
potencies <- ...

plot(mu1_grid, potencies,
     type = "l", lwd = 2, col = "steelblue",
     xlab = expression(mu[1]),
     ylab = "Potència",
     main = "Corba de potència (n = 100)",
     ylim = c(0, 1), las = 1)

# Afegiu línies de referència: nivell alpha (horitzontal) i mu0, mu1 (verticals)
abline(...)
abline(...)

# =============================================================================
# APARTAT 5b. Gràfic: corbes de potència per a múltiples n
# =============================================================================
#
# Idea: per a cada n d'un vector ns = c(10, 25, 50, 100, 200),
#   1. Calculeu el valor crític c_n = mu0 + qnorm(1-alpha) * sigma / sqrt(n).
#   2. Calculeu la potència teòrica per a cada mu1 de mu1_grid.
#   3. Afegiu la corba al gràfic amb lines() usant un color diferent per a cada n.


