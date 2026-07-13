### ==================================================
### Estadística Grau Matemàtiques
### marta.bofill.roig@upc.edu // mireia.besalu@upc.edu
### Rlab4: CI BOOTSTRAP
### ================================================== 

# 1. Revisar bootstrap CI
# 2. Implementar funcions bootstrap manualment
# 3. Comparar utilitzant exemples els diferents intervals
# 4. Exercicis penguins dataset

# ------------------------------------------------------------
# Warm-up
# ------------------------------------------------------------
#  La idea central és senzilla:
# - Per construir un IC necessitem la distribució mostral de l'estimador hattheta
# - Aquesta distribució depèn de la distribució poblacional, que és desconeguda.
# -La mostra observada és la millor aproximació que tenim d'aquesta
#      distribució -> la tractem com si fos la "població".
# -Generem B mostres de mida n amb reposició a partir de la mostra original
#      (mostres bootstrap o mostres*).
# -Per a cada mostra* calculem l'estimador d'interès hattheta*.
# -La col·lecció {hattheta*_1, ..., hattheta*_B} aproxima la distribució mostral de hattheta.
#
#  A partir d'aquesta distribució empírica podem construir intervals de
#  confiança sense cap supòsit sobre la forma de la distribució poblacional.

# ------------------------------------------------------------
# IMPLEMENTACIÓ MANUAL DELS TRES MÈTODES D'IC
# ------------------------------------------------------------ 

# Dades d'exemple (podeu substituir-les per qualsevol vector numèric)
x <- c(12.5, 14.0, 13.7, 15.1, 11.9, 16.3, 14.8, 13.2)

n <- length(x)
theta_hat <- mean(x) # estimació puntual sobre la mostra original

n
theta_hat 

# Pas comú: generar la distribució bootstrap
set.seed(123)
B <- 10000 # nombre de rèpliques bootstrap (més gran -> més estable)

theta_boot <- numeric(B) # contenidor per als B valors bootstrap

for (b in 1:B) {
  x_star   <- x[sample(1:n, size = n, replace = TRUE)]
  theta_boot[b]  <- mean(x_star)
}

# Diagnòstic visual
hist(theta_boot,
     breaks = 30,
     main = "Distribució bootstrap de la mitjana",
     xlab = "Mitjana bootstrap (θ̂*)",
     col  = "lightsteelblue",
     border = "white")
abline(v = theta_hat, col = "firebrick", lwd = 2, lty = 1)
legend("topright", legend = "θ̂ original", col = "firebrick",
       lwd = 2, bty = "n")

# Error estàndard bootstrap
SE_boot <- sd(theta_boot)
SE_boot

alpha <- 0.05 # nivell de significació; l'IC tindrà confiança 1 - alpha = 95 %

## ============================================================
# 2.1 Mètode Normal
## ============================================================
#  Supòsit: la distribució de hattheta és aproximadament normal.
#  El bootstrap s'utilitza únicament per estimar l'error estàndard se_B. 
#
#  Avantatge:  molt senzill de calcular.
#  Limitació:  força la simetria de l'interval; no funciona bé si la
#      distribució de hattheta  és asimètrica o té cues pesades.

z  <- qnorm(1 - alpha / 2)   # z_{0.025} ≈ 1.96
ic_nor <- c(inf = theta_hat - z * SE_boot,
            sup = theta_hat + z * SE_boot)

ic_nor # IC Normal (95%) 

## ============================================================
#  2.2 Mètode Percentil
## ============================================================
#  No requereix cap supòsit de normalitat.
#  L'interval s'obté directament com els quantils alpha/2 i 1-alpha/2 de la
#  distribució bootstrap 
#
#  Avantatge:  capta l'asimetria de la distribució bootstrap.
#  Limitació:  pot tenir cobertura inferior a la nominal en mostres petites.

ic_per <- quantile(theta_boot, probs = c(alpha / 2, 1 - alpha / 2))

ic_per # IC Percentil (95%) 


## ============================================================
# 2.3 Mètode Pivot (bàsic) 
## ============================================================
#  Es basa en la simetria del pivot. Reflexa la distribució bootstrap al voltant de hattheta, de manera que
#  corregeix el biaix respecte al mètode percentil,
#
#  Avantatge:  millora la cobertura quan hi ha biaix.
#  Limitació:  segueix assumint una certa simetria de la distribució pivot.

ic_piv <- 2 * theta_hat - quantile(theta_boot, probs = c(1 - alpha / 2, alpha / 2))

ic_piv # IC Pivot / Bàsic (95%)

## ============================================================
# 3.  IC CLÀSSIC (PARAMÈTRIC) PER A LA MITJANA
## ============================================================
#
#  Quan les dades segueixen una distribució normal (o n és gran),
#  l'interval exacte per a la mitjana s'obté amb la distribució t de Student.
#
#  Avantatge:  òptim sota normalitat.
#  Limitació:  no és adequat per a estadístics distints de la mitjana ni
#              per a distribucions molt allunyades de la normal.

classic_ci_mean <- function(x, alpha = 0.05) {
  n     <- length(x)
  m     <- mean(x)
  s     <- sd(x)
  t_val <- qt(1 - alpha / 2, df = n - 1)
  marge <- t_val * s / sqrt(n)
  c(inf = m - marge, sup = m + marge)
}

classic_ci_mean(x)


## ============================================================
# 4.  ÚS DEL PAQUET {boot}
## ============================================================
#
#  El paquet {boot} ofereix una implementació optimitzada i estandarditzada.
#  La funció estadística ha de tenir la signatura:  f(data, indices)
#  on `indices` és el vector d'índexs de la mostra bootstrap.

# install.packages("boot") 
# https://cran.r-project.org/web/packages/boot/index.html
library(boot)

# Definim l'estadístic d'interès  
boot_mean <- function(data, indices) {
  mean(data[indices])
}

set.seed(123)
boot_out <- boot(data = x, statistic = boot_mean, R = 10000)

cat("\nEstimació original (boot_out$t0):", boot_out$t0, "\n")
cat("Error estàndard bootstrap (boot):", round(sd(boot_out$t), 4), "\n")

# boot.ci() calcula els tres intervals principals en una sola crida
boot_ci <- boot.ci(boot.out = boot_out,
                   conf     = 1 - alpha,
                   type     = c("norm", "perc", "basic"))

boot_ci

## ============================================================
# 5.  FUNCIÓ GENÈRICA bootstrap_ci()
## ============================================================
#
#  Encapsula el mètode percentil per a qualsevol estadístic escalar.
#  Paràmetres:
#    x             – vector de dades
#    stat_function – funció R que retorna un escalar (mean, median, var, …)
#    B             – nombre de rèpliques bootstrap
#    alpha         – nivell de significació

bootstrap_ci <- function(x, stat_function, B = 1000, alpha = 0.05) {
  n <- length(x)
  stats <- numeric(B)
  
  for (i in 1:B) {
    idx <- sample(1:n, size = n, replace = TRUE)
    stats[i] <- stat_function(x[idx])
  }
  
  list(
    inf = quantile(stats, probs = alpha / 2),
    sup = quantile(stats, probs = 1 - alpha / 2),
    distribucio_bootstrap = stats
  )
}


## ============================================================
# 6.  EXEMPLES
## ============================================================
# 6.1  Dades simètriques: iris – longitud del sèpal (mitjana)  

x_iris <- iris$Sepal.Length

boot_mean <- function(data, indices) mean(data[indices])

set.seed(123)
boot_iris <- boot(data = x_iris, statistic = boot_mean, R = 2000)
ci_iris <- boot.ci(boot_iris, conf = 0.95, type = c("norm", "perc", "basic"))

(ic_classic = classic_ci_mean(x_iris))
ci_iris

# Diagnòstic visual: histograma + els quatre límits superposats
hist(boot_iris$t,
     breaks = 30,
     main   = "Distribució bootstrap – longitud del sèpal (mitjana)",
     xlab   = "Mitjana bootstrap (θ̂*)",
     col    = "lightsteelblue", border = "white")
abline(v = boot_iris$t0,          col = "black",     lwd = 2)
abline(v = ci_iris$normal[2:3],   col = "firebrick", lty = 2, lwd = 1.5)
abline(v = ci_iris$percent[4:5],  col = "darkgreen", lty = 3, lwd = 1.5)
abline(v = ci_iris$basic[4:5],    col = "darkorange",lty = 4, lwd = 1.5)
legend("topright", bty = "n",
       legend = c("θ̂", "Normal", "Percentil", "Pivot"),
       col    = c("black", "firebrick", "darkgreen", "darkorange"),
       lty    = c(1, 2, 3, 4), lwd = 1.5)


# 6.2  Dades asimètriques: distribució exponencial  

set.seed(42)
x_exp <- rexp(100, rate = 1)   # μ = 1, distribució clarament asimètrica

set.seed(42)
boot_exp <- boot(data = x_exp, statistic = boot_mean, R = 2000)
ci_exp <- boot.ci(boot_exp, conf = 0.95, type = c("norm", "perc", "basic"))

ci_exp
classic_ci_mean(x_exp)


hist(boot_exp$t,
     breaks = 30,
     main   = "Distribució bootstrap – Exp(1) (mitjana)",
     xlab   = "Mitjana bootstrap (θ̂*)",
     col    = "lightsteelblue", border = "white")
abline(v = boot_exp$t0,         col = "black",      lwd = 2)
abline(v = ci_exp$normal[2:3],  col = "firebrick",  lty = 2, lwd = 1.5)
abline(v = ci_exp$percent[4:5], col = "darkgreen",  lty = 3, lwd = 1.5)
abline(v = ci_exp$basic[4:5],   col = "darkorange", lty = 4, lwd = 1.5)
legend("topright", bty = "n",
       legend = c("θ̂", "Normal", "Percentil", "Pivot"),
       col    = c("black", "firebrick", "darkgreen", "darkorange"),
       lty    = c(1, 2, 3, 4), lwd = 1.5)

# 6.3  Mostra petita  

set.seed(99)
x_petit <- sample(x_iris, size = 10)

set.seed(99)
boot_petit <- boot(data = x_petit, statistic = boot_mean, R = 2000)
ci_petit <- boot.ci(boot_petit, conf = 0.95, type = c("norm", "perc", "basic"))

classic_ci_mean(x_petit)
ci_petit


## ============================================================
# 7.  EXERCICI: DATASET PALMER PENGUINS
## ============================================================
#
#  El dataset `penguins` conté mesures morfològiques de tres espècies de
#  pingüins de l'Antàrtida (Adèlia, Gentoo i Barbijo).
#
#  Variables d'interès:
# -bill_length_mm    – longitud del bec (mm)
# -flipper_length_mm – longitud de l'aleta (mm)
#
#  Tasques:
#    1. Carregar el paquet i eliminar valors absents (NA).
#    2. bill_length_mm:
#       a) Estimació puntual: mitjana.
#       b) IC 95 % clàssic (t-Student).
#       c) IC 95 % bootstrap (percentil, B = 2000).
#    3. flipper_length_mm:
#       a) Estimació puntual: mediana.
#       b) IC 95 % bootstrap (percentil, B = 2000). 
#    4. Comparar i interpretar les diferències entre mètodes.

# install.packages("palmerpenguins")   # descomenteu si cal
library(palmerpenguins)

# 1
bec    <- na.omit(penguins$bill_length_mm)
aleta  <- na.omit(penguins$flipper_length_mm)

# 2
mean(bec)   # estimació puntual

set.seed(1)
boot_bec <- boot(data = bec, statistic = boot_mean, R = 2000)
ci_bec   <- boot.ci(boot_bec, conf = 0.95, type = c("norm", "perc", "basic"))

# IC clàssic per comparar
classic_ci_mean(bec)

# Els tres IC bootstrap
ci_bec   # norm -> $normal[2:3]  |  perc -> $percent[4:5]  |  pivot -> $basic[4:5]

hist(boot_bec$t,
     breaks = 30,
     main   = "Distribució bootstrap – longitud del bec (mitjana)",
     xlab   = "Mitjana bootstrap (θ̂*)",
     col    = "lightsteelblue", border = "white")
abline(v = boot_bec$t0,          col = "black",      lwd = 2)
abline(v = ci_bec$normal[2:3],   col = "firebrick",  lty = 2, lwd = 1.5)
abline(v = ci_bec$percent[4:5],  col = "darkgreen",  lty = 3, lwd = 1.5)
abline(v = ci_bec$basic[4:5],    col = "darkorange", lty = 4, lwd = 1.5)
legend("topright", bty = "n",
       legend = c("θ̂", "Normal", "Percentil", "Pivot"),
       col    = c("black", "firebrick", "darkgreen", "darkorange"),
       lty    = c(1, 2, 3, 4), lwd = 1.5)

# 3
median(aleta)   # estimació puntual

boot_median <- function(data, indices) median(data[indices])

set.seed(1)
boot_aleta <- boot(data = aleta, statistic = boot_median, R = 2000)
ci_aleta   <- boot.ci(boot_aleta, conf = 0.95, type = c("norm", "perc", "basic"))

ci_aleta  

hist(boot_aleta$t,
     breaks = 20,
     main   = "Distribució bootstrap – longitud de l'aleta (mediana)",
     xlab   = "Mediana bootstrap (θ̂*)",
     col    = "lightsteelblue", border = "white")
abline(v = boot_aleta$t0,          col = "black",      lwd = 2)
abline(v = ci_aleta$normal[2:3],   col = "firebrick",  lty = 2, lwd = 1.5)
abline(v = ci_aleta$percent[4:5],  col = "darkgreen",  lty = 3, lwd = 1.5)
abline(v = ci_aleta$basic[4:5],    col = "darkorange", lty = 4, lwd = 1.5)
legend("topright", bty = "n",
       legend = c("θ̂", "Normal", "Percentil", "Pivot"),
       col    = c("black", "firebrick", "darkgreen", "darkorange"),
       lty    = c(1, 2, 3, 4), lwd = 1.5)


# Interpretació:
# - Longitud del bec (n gran, distribució ~normal):
#   L'IC clàssic i el bootstrap coincideixen molt bé. 
# 
# - Longitud de l'aleta (mediana):
# No disposem d'una fórmula tancada per a l'IC de la mediana.
# El bootstrap resol el problema de manera directa, sense
# cap supòsit sobre la distribució poblacional.
# 
# En general:
# - Si les dades són normals -> IC clàssic i bootstrap similars.
# - Si hi ha asimetria o estadístics complexos -> bootstrap
# proporciona solució a la búsqueda d'intervals.
# - Mostra petita: cal cautela amb tots dos mètodes.
