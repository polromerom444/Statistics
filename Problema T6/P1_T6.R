

library('lmtest')
library('car')
library(readr)

dades <- read.csv("p1t5.csv", sep= ";", header = TRUE)


names(dades) <- c("taxa", "pes")

correlacio <- cor(dades$pes, dades$taxa)
cat("Coeficient de correlació (r):", correlacio, "\n\n")

model <- lm(taxa ~ pes, data=dades)
summary(model)

plot(dades$pes, dades$taxa,
     xlab = "Pes corporal",
     ylab = "Taxa metabòlica")
abline(model)

int_confianca <- confint(model, level = 0.95)
cat("Intervals de confiança (95%):\n")
print(int_confianca)

r_quadrat <- summary(model)$r.squared
cat("\nCoeficient de determinació (R^2):", r_quadrat, "\n")

par(mfrow = c(2, 2))
plot(model)

# Tornem la finestra gràfica a la normalitat (1 sol gràfic)
par(mfrow = c(1, 1))

# Calculem distàncies de Cook i residus per identificar valors extrems
distancia_cook <- cooks.distance(model)
residus_est <- rstandard(model)

cat("\nDistàncies de Cook:\n")
print(distancia_cook)
cat("\nResidus estandarditzats:\n")
print(residus_est)

nova_data <- data.frame(pes = 80)

# Calculem la predicció de la mitjana esperada amb el seu interval de confiança (95%)
prediccio_mitjana <- predict(model, newdata = nova_data, interval = "confidence", level = 0.95)

cat("\nPredicció de la taxa metabòlica mitjana per a 80 kg (amb IC 95%):\n")
print(prediccio_mitjana)