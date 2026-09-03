'''
install.packages(corrplot)

library(lmtest)
library(car)
library(readr)
library(corrplot)

farmac <- read.csv("farmac.csv", header = TRUE, dec= ',', sep = ";")

str(farmac)

summary(farmac)

farmaclm<- lm(TR  ~ LD + PM, data= farmac)

summary(farmaclm)


coef(farmaclm)


summary(farmaclm)$coefficients


vif(farmaclm)



cor_mat <-cor(farmac)
corrplot(cor_mat, method = "number", type = "upper")

#c)

par(mfrow = c(2,2))
plot(farmaclm)


bptest(farmaclm)

shapiro.test(farmaclm)

#d)

n <- nrow(farmac)
p <- length(coef(farmaclm))

rstud <- rstudent(farmaclm)
lev <- hatvalues(farmaclm)
cook <- cooks.distance(farmaclm)

lev_thr <- 2*p/n
cook_thr <- 4/n

diag_tbl <- data.frame(
  obs = 1:n,
  TR = farmac$TR,
  LD = farmac$LD,
  PM = farmac$PM,
  resid_student = rstud,
  leverage = lev, 
  cooksD = cook,
)

wh

farmaclm<- lm(log(TR)  ~ LD + PM, data= farmac)

summary(farmaclm)

par(mfrow = c(2,2))
plot(farmaclm)

x0 <-data.frame(LD = 2, PM = 61)
predict(farmaclm, newdata = x0, interval ="confidence", level = 0.95)
predict(farmaclm, newdata = x0, interval ="prediction", level = 0.95)
'''
#############################################################################################
library('lmtest')
library('car')
library(readr)
dades <- read_csv2("farmac.csv")
names(dades) <- c("ldose", "pression", "time")

# Apartat (a): Model de regressió lineal múltiple

model <- lm(time ~ ldose + pression, data=dades)
summary(model)

# Apartat (b): És significatiu globalment? I per separat?
# Globalment el test és: H0: b0 = b1 = b2 = 0 vs. H1: algun bj != 0
# Si mirem el summary, com el p-value del F-statistic és 0.0357< 0.05, podem dir que el model és globalment significatiu
# Per separat, també, ja que per cada predictor Pr(> |t1) < 0.05

# Apartat (c): Validació model

# 1. Validació gràfica (Residus vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage)
par(mfrow = c(2, 2))
plot(model)


# 2. Test d'Homocedasticitat (Breusch-Pagan test de la llibreria lmtest)
# H0: Els residus són homocedàstics (variància constant)
bptest(model)

# 3. Test de Normalitat dels residus (Shapiro-Wilk)
# H0: Els residus segueixen una distribució normal
shapiro.test(residuals(model))

# 4. Multicol·linearitat (VIF - Variance Inflation Factor de la llibreria car)
# Si algun VIF > 5 o 10, hi ha massa correlació entre LD i PM.
vif(model)

# 5. Dades extremes i influents
residus_estud <- rstandard(model)   # Outliers si superen |2| o |3|
leverage <- hatvalues(model)         # Punts de palanca (leverage)
cook_dist <- cooks.distance(model)   # Punts influents si Cook > 0.5 o 1

# Mostrem els valors màxims per detectar si hi ha algun cas crític
cat("Residu estudiantitzat màxim (en valor absolut):", max(abs(residus_estud)), "\n")
cat("Distància de Cook màxima:", max(cook_dist), "\n")

# Apartat (d): Predicció pacient LD = 2 i PM = 61
nou_pacient <- data.frame(ldose = 2, pression = 61)
prediccio_individual <- predict(model, newdata = nou_pacient, interval = "prediction", level = 0.95)

cat("\nPredicció del valor de TR (amb l'interval de predicció del 95%):\n")
print(prediccio_individual)




