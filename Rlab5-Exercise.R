### ========================================
### Estadística Grau Matemàtiques
### marta.bofill.roig@upc.edu // mireia.besalu@upc.edu
### Rlab: Exercici
### ========================================

# Exercici: Anàlisi de dades de lliurament
# Exploreu el dataset Delivery Data mitjançant una anàlisi descriptiva,
# el càlcul d'estadístics resum i la construcció d'intervals de confiança
# per al temps de lliurament sota diferents condicions de trànsit.
#
# 1) Anàlisi descriptiva
# Utilitzarem diverses visualitzacions per entendre la distribució
# de les variables principals:
#
# - Histograma del temps de lliurament (Delivery Time): visualitza la distribució dels temps de lliurament per a tots els enviaments.
# - Boxplot del temps de lliurament per densitat de trànsit: compara els temps de lliurament entre els diferents nivells de trànsit (traffic density -> low, medium, jam)
# - Diagrama de barres de lliuraments múltiples: mostra el recompte de lliuraments realitzats per trajecte i repartidor.
# - Histograma de la valoració dels repartidors: visualitza la distribució de les puntuacions rebudes pels repartidors.
#
# 2) Estadístics resum del temps de lliurament
#
# - Calculeu la mitjana mostral i la desviació estàndard mostral del temps de lliurament per resumir-ne la tendència central i la variabilitat.
#
# 3) Interval de confiança per al temps de lliurament segons les condicions de trànsit
# Calculeu l'interval de confiança (IC) al 95 % per a la diferència de
# mitjanes del temps de lliurament entre:
# - Low traffic
# - Jam traffic
# Interpreteu els resultats.


load("DeliveryData.RData")
fooddelivery_data$traffic <- as.factor(fooddelivery_data$traffic)
fooddelivery_data$traffic <- factor(fooddelivery_data$traffic, levels = c("Low", "Medium", "High", "Jam"))

# 1)
quartz(width = 12, height = 8) 
par(mfrow = c(1, 4), lwd = 2, font.lab = 2, font.axis = 1, las = 1)

hist(fooddelivery_data$deliverytime, breaks = 10, 
     xlab= "Delivery Time (min)", main= "Distribution of Delivery Time",
     xlim = c(0,60),
     ylim = c(0, 8000),
     col = "lightgreen") 
axis(2, at = c(2000, 4000, 6000, 8000))
boxplot(fooddelivery_data$deliverytime ~ fooddelivery_data$traffic, frame.plot = FALSE,
        main = "Time versus versus Traffic",
        ylab="Delivery Time (min)",
        xlab="Road traffic density", ylim=c(10,60))
barplot(table(fooddelivery_data$multipledeliveries), 
        xlab="Multiple Deliveries", ylab="Counts", main="Distribution of Multiple Deliveries",
        ylim = c(0, 30000),
        col="red")
hist(fooddelivery_data$ratings,
     xlab="Ratings", main="Delivery Person Ratings",
     col="lightblue")

dev.off()

# 2)
mean(fooddelivery_data$deliverytime)
sd(fooddelivery_data$deliverytime)

# 3)
time_low <- subset(fooddelivery_data, fooddelivery_data$traffic=="Low")$deliverytime
time_jam <- subset(fooddelivery_data, fooddelivery_data$traffic=="Jam")$deliverytime
t.test(time_jam)
t.test(time_low)
t.test(time_jam,time_low)
