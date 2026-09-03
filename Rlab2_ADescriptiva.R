### ==================================================
### Estadística Grau Matemàtiques
### Rlab2: Anàlisi descriptiva
### ==================================================

# Install and load the package
# install.packages("palmerpenguins")
library(palmerpenguins)
data(penguins)

# Explore the dataset
head(penguins)
str(penguins)
dim(penguins)

# install.packages("Hmisc")
library(Hmisc) 
summary(penguins)

## 1. Numeric variables
## ====================
## See functions mean(), median(), sd(), range(), summary(), etc..

mean(penguins$body_mass_g)
mean(penguins$body_mass_g, na.rm = TRUE)
median(penguins$body_mass_g, na.rm = TRUE)
summary(penguins$body_mass_g)

# Another important function: quantile()
# --------------------------------------
quantile(penguins$body_mass_g, na.rm = TRUE)
# Quantiles 30% and 90%?
quantile(penguins$body_mass_g, c(0.3, 0.9), na.rm = TRUE)
quantile(penguins$body_mass_g, 1:4 / 5, na.rm = TRUE)


## 2. Categorical variables
## ========================
## A very simple frequency table
## -----------------------------
table(penguins$species)
summary(penguins$species)

table(penguins$island)
summary(penguins$island)

## Relative frequencies
## --------------------
prop.table(table(penguins$species))
prop.table(table(penguins$island))


## Some useful functions from contributed packages:
## ================================================
## (a) Function freq() ("descr")
## -----------------------------
# install.packages("descr")
library(descr)
freq(penguins$species)
freq(penguins$species, plot = FALSE) #no ens treu el gràfic

## (b) Function Freq() ("DescTools")
## ---------------------------------
# install.packages("DescTools")
library(DescTools)
Freq(penguins$species)
Freq(penguins$island)

## Each functions represents NAs in different ways:
## ------------------------------------------------
summary(penguins$species)
freq(penguins$species, plot = FALSE)
Freq(penguins$species)
Freq(penguins$species, useNA = "ifany")

Freq(penguins$sex)
Freq(penguins$sex, useNA = "ifany")


## ===========================================================
## 3. CONTINGENCY TABLES (Taules de contingència)
## ===========================================================

## Simple contingency table: species vs island
## -------------------------------------------
table(penguins$species, penguins$island)

# With better labels:
taula <- table(penguins$species, penguins$island)
taula

# Add marginal totals
addmargins(taula)

## Row proportions (percentatges per fila)
## ---------------------------------------
prop.table(taula, margin = 1)  # Each row sums to 1
round(prop.table(taula, margin = 1), 3)  # Rounded to 3 decimals
round(100 * prop.table(taula, margin = 1), 1)  # As percentages

## Column proportions (percentatges per columna)
## ---------------------------------------------
prop.table(taula, margin = 2)  # Each column sums to 1
round(prop.table(taula, margin = 2), 3)
round(100 * prop.table(taula, margin = 2), 1)

## Total proportions (percentatges sobre el total)
## -----------------------------------------------
prop.table(taula)  # All cells sum to 1
round(prop.table(taula), 3)
round(100 * prop.table(taula), 1)

## Using CrossTable() from "gmodels" package
## -----------------------------------------
# install.packages("gmodels")
library(gmodels)
CrossTable(penguins$species, penguins$island)

# With more options:
CrossTable(penguins$species, penguins$island, 
           prop.r = TRUE,    # Row proportions
           prop.c = TRUE,    # Column proportions
           prop.t = TRUE,    # Total proportions
           prop.chisq = FALSE)  # No chi-square contributions

## Using xtabs() function
## ----------------------
xtabs(~ species + island, data = penguins)
ftable(xtabs(~ species + island, data = penguins))


## Conversion of a numeric variable into an ordinal variable.
## ==========================================================
## Function cut2() of the "Hmisc" package
## --------------------------------------
library(Hmisc)
penguins$body_mass_cat <- cut2(penguins$body_mass_g, c(3500, 4500, 5500))
head(penguins)
summary(penguins)

## Slight change of the labels
levels(penguins$body_mass_cat)
levels(penguins$body_mass_cat)[c(1, 4)] <- c("< 3500", ">= 5500")
levels(penguins$body_mass_cat)
summary(penguins)


## PLOTS
## ===============
## 1. Scatterplots
## ===============
#plot(body ~ flipper, penguins, main = "A simple figure")
plot(body_mass_g ~ flipper_length_mm, penguins, main = "A simple figure")
# The same graph opened in a new graphical window:
# ------------------------------------------------
#windows()
quartz()
plot(body_mass_g ~ flipper_length_mm, penguins, main = "A simple figure", 
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)")

# Instead of argument "data = penguins", we can also use function with():
# ---------------------------------------------------------------------
with(penguins, plot(body_mass_g ~ flipper_length_mm, 
                    xlab = "Flipper Length (mm)", ylab = "Body Mass (g)"))
title("A simple scatterplot")

# We can close the active graphics window with function dev.off():
dev.off()


## Scatterplot with different colors by species
## =============================================
quartz()
par(font.lab = 2, font.axis = 4, las = 1)

# Simple version: automatic colors
plot(body_mass_g ~ flipper_length_mm, data=penguins, 
     col = species, pch = 16,
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)",
     main = "Body Mass vs Flipper Length by Species")
legend("topleft", legend = levels(penguins$species), 
       col = 1:3, pch = 16, title = "Species")

# Custom colors version
quartz()
par(font.lab = 2, font.axis = 4, las = 1)
colors_species <- c("darkorange", "purple", "cyan4")
plot(body_mass_g ~ flipper_length_mm, data = penguins, 
     col = colors_species[species], pch = 16, cex = 1.5,
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)",
     main = "Body Mass vs Flipper Length by Species")
legend("topleft", legend = levels(penguins$species), 
       col = colors_species, pch = 16, cex = 1.2, title = "Species")

# Alternative: using different symbols (pch) instead of colors
quartz()
par(font.lab = 2, font.axis = 4, las = 1)
plot(body_mass_g ~ flipper_length_mm, data = penguins, 
     col = as.numeric(species), pch = as.numeric(species) + 14, cex = 1.5,
     xlab = "Flipper Length (mm)", ylab = "Body Mass (g)",
     main = "Body Mass vs Flipper Length by Species")
legend("topleft", legend = levels(penguins$species), 
       col = 1:3, pch = 15:17, cex = 1.2, title = "Species") 
dev.off()

## 2. Histograms
## =============
quartz(width = 8)
par(font.lab = 2, font.axis = 4, las = 1)
hist(penguins$body_mass_g)

# We can improve the figure a little bit:
# ---------------------------------------
hist(penguins$body_mass_g, xlab = "Body Mass (g)", breaks = 15, col = 2,
     freq = FALSE, main = "Distribution of Penguin Body Mass")

# If we change the y-axis, it is possible to add a smoothed curve
# ---------------------------------------------------------------
hist(penguins$body_mass_g, xlab = "Body Mass (g)", breaks = 15, col = "steelblue",
     freq = FALSE, main = "Distribution of Penguin Body Mass")
lines(density(penguins$body_mass_g, na.rm = TRUE), lwd = 3)

# Would like some more colours? Choose among...
colours()


## Only plotting the estimated density function:
windows(width = 8)
par(font.lab = 2, font.axis = 4, las = 1)
plot(density(penguins$body_mass_g, na.rm = TRUE), lwd = 3)


## How to save a graph?
## --------------------
# (I) Use of function savePlot() [Under Mac, use: quartz.save()]:
# ---------------------------------------------------------------
quartz.save("BodyMassHisto", type = "pdf")
quartz.save("BodyMassHisto", type = "png")
dir("/Users/polromerom/Desktop")

# (II) Function pdf() can be used to create a pdf file
#      that contains one or more graphs.
# ----------------------------------------------------
pdf("Histograma_penguins.pdf", width=8)
par(font.lab = 2, font.axis = 4, las = 1)
hist(penguins$body_mass_g, xlab = "Body Mass (g)", breaks = 15, col = 2,
     main = "...")

hist(penguins$body_mass_g, xlab = "Body Mass (g)", breaks = 15, col = "steelblue",
     freq = FALSE, main = "Distribution of Penguin Body Mass")
lines(density(penguins$body_mass_g, na.rm = TRUE), lwd = 3)

hist(penguins$body_mass_g, xlab = "Body Mass (g)", breaks = 15, col = rainbow(20),
     main = "Distribution of Penguin Body Mass")
dev.off()

# PS: Similar functions exist for other formats: png(), tiff(), etc.

# Close all graph devices (if you want)
graphics.off()


## 3. Boxplots
## ===========
## Values to be passed to function par():
myPar <- list(font.lab = 2, font.axis = 4, las = 1)

quartz()
par(myPar)
boxplot(penguins$species)
boxplot(body_mass_g ~ species, data = penguins, xlab = "Species", 
        ylab = "Body Mass (g)", col = 3, pch = 16)
boxplot(body_mass_g ~ species, penguins, xlab = "Species", 
        ylab = "Body Mass (g)", col = 2:4, pch = 16, 
        main = "Body Mass by Species") 

## 4. Pie charts 
## ==================================== 
pie(table(penguins$species))
pie(table(penguins$species), col = 1:3, main = "Penguin Species")
pie(table(penguins$island), col = 1:3, main = "Islands")

## 5. Barplots: for some better examples, see help(barplot)
## ========================================================
barplot(table(penguins$species))
with(penguins, barplot(tapply(body_mass_g, species, median, na.rm = TRUE), 
                       col = 1:3, main = "Median Body Mass per Species"))
with(penguins, barplot(table(species, island), col = 1:3, 
                       ylab = "Number of penguins", legend = TRUE))
with(penguins, barplot(table(species, island), beside = TRUE, col = 1:3,
                       ylab = "Number of penguins", legend = TRUE))
dev.off()


## 6. Instead of a barplot, better use a mosaicplot
## ================================================
quartz(width = 8)
par(myPar)
mosaicplot(species ~ island, penguins, col = 1:3, xlab = "Species", 
           ylab = "Island", main = "Species per Island", cex.axis = 1)


## Closing all open graphics devices 
dev.off()







