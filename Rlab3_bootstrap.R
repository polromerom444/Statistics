### ==================================================
### Estadística Grau Matemàtiques
### Rlab3: SIMULACIÓ I BOOTSTRAP
### ================================================== 

## ============================================================
## PART 1: SIMULACIÓ
## ============================================================

# 1.1 Simular dades d'una distribució coneguda 

set.seed(314) # reproductibilitat

n <- 200
dades <- rnorm(n, mean = 50, sd = 10)  # població X~normal
mean(dades) # Mitjana mostral
sd(dades) # Desv. típica mostral
var(dades) # Var mostral

# 1.2 Llei dels Grans Nombres 

mides <- c(10, 50, 100, 500, 1000, 5000)
mitjanes <- sapply(mides, function(n) mean(rnorm(n, mean = 50, sd = 10)))

# Llei dels Grans Nombres (mitjana teòrica = 50):
data.frame(n = mides, mitjana_mostral = round(mitjanes, 3), diferencies = round(mitjanes, 3)-50) 

# 1.3 Teorema Central del Límit 
# Distribució de la mitjana mostral den e mostres d'una exp(1) 

num_simulacions <- 5000
mida_mostra  <- 40

mitjanes_sim <- replicate(
 num_simulacions,
 mean(rexp(mida_mostra, rate = 1))   # exp(1): E=1, SD=1
)

# TCL – Distribució de la mitjana (exp, n=40)
round(mean(mitjanes_sim), 4) #Mitjana
round(sd(mitjanes_sim),  4) #sd estimada
round(1 / sqrt(mida_mostra), 4) #sd teorica

par(mfrow = c(1, 2))
hist(rexp(5000, rate = 1),
   main = "Distribució original (Exp)", xlab = "x",
   col = "#4E79A7", border = "white", breaks = 40)
hist(mitjanes_sim,
   main = paste0("Mitjanes mostrals (n=", mida_mostra, ")"),
   xlab = "Mitjana", col = "#F28E2B", border = "white", breaks = 40)
par(mfrow = c(1, 1))

# 1.4 Simulació de Monte Carlo ─
# Estimació de pi llançant punts aleatoris en un quadrat unitari

set.seed(1)
N <- 1000000
x <- runif(N, -1, 1)
y <- runif(N, -1, 1)
dins_cercle <- (x^2 + y^2) <= 1
pi_estimat <- 4 * mean(dins_cercle)
pi_estimat 

## ============================================================
## PART 2: BOOTSTRAP 
## ============================================================
#
# Idea: re-mostrar AMB REPOSICIÓ de la mostra original per
# obtenir la distribució mostral d'un estadístic qualsevol.

# 2.1 Bootstrap bàsic – interval de confiança per a la mitjana 

set.seed(99)
mostra <- rnorm(30, mean = 50, sd = 10)  # mostra original (n=30)
B   <- 10000              # nombre de rèpliques bootstrap

# Re-mostrar i calcular la mitjana en cada rèplica
boot_mitjanes <- numeric(B)
for (i in seq_len(B)) {
 mostra_boot  <- sample(mostra, size = length(mostra), replace = TRUE)
 boot_mitjanes[i] <- mean(mostra_boot)
}

# Quantiles bootstrap percentil alque f 95 %
IC_boot <- quantile(boot_mitjanes, c(0.025, 0.975))

# Mitjana i sd bootstrap
mean(mostra) #Mitjana mostral
sd(boot_mitjanes) #Error estàndard bootstrap 

hist(boot_mitjanes,
   main = "Distribució Bootstrap de la Mitjana",
   xlab = "Mitjana", col = "#59A14F", border = "white", breaks = 50)
abline(v = IC_boot,  col = "red", lty = 2, lwd = 2)
abline(v = mean(mostra), col = "black", lty = 1, lwd = 2)
legend("topright",
    legend = c("IC 95% bootstrap", "Mitjana mostral"),
    col = c("red", "black"), lty = c(2, 1), lwd = 2)

# 2.2 Bootstrap per a la mediana 
# L'IC clàssic de la mediana és difícil analíticament; bootstrap és ideal.

set.seed(7)
dades_asim <- rexp(50, rate = 0.1)    # distribució asimètrica

boot_medianes <- replicate(
 B,
 median(sample(dades_asim, length(dades_asim), replace = TRUE))
)

IC_mediana <- quantile(boot_medianes, c(0.025, 0.975))
median(dades_asim) #Mediana mostral 
sd(dades_asim)

# 2.3 Bootstrap per al coeficient de correlació 

set.seed(21)
n_cor <- 40
x_cor <- rnorm(n_cor)
y_cor <- 0.6 * x_cor + rnorm(n_cor, sd = 0.8)

boot_cors <- replicate(B, {
 idx <- sample(n_cor, replace = TRUE)
 cor(x_cor[idx], y_cor[idx])
})

IC_cor <- quantile(boot_cors, c(0.025, 0.975))

cat("\nBOOTSTRAP – IC 95% per a la CORRELACIÓ (n=40):\n")
cat(" r mostral:", round(cor(x_cor, y_cor), 3), "\n")
cat(" IC bootstrap: [",
  round(IC_cor[1], 3), ",", round(IC_cor[2], 3), "]\n")

# 2.4 Funció genèrica de bootstrap ─

bootstrap <- function(dades, estadistic_fn, B = 10000, alpha = 0.05) {
 # dades:     vector de dades originals
 # estadistic_fn: funció que pren un vector i retorna un escalar
 # B:       nombre de rèpliques
 # alpha:     nivell de significació (IC = 1-alpha)
 
 n   <- length(dades)
 boots <- replicate(B, estadistic_fn(sample(dades, n, replace = TRUE)))
 
 list(
  estimacio = estadistic_fn(dades),
  error_std = sd(boots),
  IC     = quantile(boots, c(alpha / 2, 1 - alpha / 2)),
  distribucio = boots
 )
}

# Exemple: IC del coeficient de variació (SD/mean)
cv <- function(x) sd(x) / mean(x)

set.seed(5)
mostra_cv <- rgamma(60, shape = 3, rate = 0.5)
resultat <- bootstrap(mostra_cv, cv)

# Bootstrap
round(resultat$estimacio, 4) #CV mostral
round(resultat$error_std, 4) #Error estàndard


## ------------------------------------------------------------
## EXERCICI -- Bootstrap: Error estàndard d'un parametre
## ------------------------------------------------------------
#
#  ENUNCIAT:
#  Tenim una mostra de 40 temps de resposta (ms) d'un servidor
#  web generats d'una Log-Normal(meanlog=5, sdlog=0.5).
#
#  a) Calcula la mitjana i la SD mostrals.
#  b) Usa bootstrap (B=10000) per estimar l'error estàndard (SE)
#     de la MITJANA i obtenir-ne un IC al 95%.
#  c) Repeteix b) pero per a la SD mostral.
#  d) Compara el SE bootstrap de la mitjana amb la formula
#     teorica: SE = sd(mostra) / sqrt(n).
#
#  Reflexió: per a quin dels dos estadístics (mitjana o SD)
#  el bootstrap aporta mes valor? Per que?

## -- SOLUCIÓ --------------------------------------------------

set.seed(77)

