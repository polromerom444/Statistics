### ==================================================
### Estadística Grau Matemàtiques
### marta.bofill.roig@upc.edu // mireia.besalu@upc.edu
### Rlab1: Manipulació de dades i descriptiva (1)
### ==================================================

# https://allisonhorst.github.io/palmerpenguins/
# Instal·lar i carregar el paquet palmerpenguins
# install.packages("palmerpenguins")
library(palmerpenguins)

## Vectors en R
## ============

# Exploració inicial de les dades
# -------------------------------
data(penguins)
?penguins
head(penguins)
str(penguins)

# Extreure vectors de la base de dades
# ------------------------------------
bill_length <- penguins$bill_length_mm
bill_depth <- penguins$bill_depth_mm
flipper_length <- penguins$flipper_length_mm
body_mass <- penguins$body_mass_g
species <- penguins$species
island <- penguins$island
sex <- penguins$sex
year <- penguins$year

# Modificacions lleugeres
# ----------------------
# Convertir els primers dos valors de "island" en valors perduts
island_mod <- island
island_mod[c(1, 2)] <- NA

# Reemplaçar totes les longituds de bec més grans que 50 mm per 50:
bill_length_mod <- bill_length
bill_length_mod[bill_length_mod > 50] <- 50

# Crear un data frame personalitzat amb una mostra
# ------------------------------------------------
set.seed(123)
mostra_ids <- sample(1:nrow(penguins), 20)
dfram <- data.frame(
  id = paste("Penguin", mostra_ids, sep = "-"),
  species = species[mostra_ids],
  island = island[mostra_ids],
  bill_length = bill_length[mostra_ids],
  bill_depth = bill_depth[mostra_ids],
  flipper_length = flipper_length[mostra_ids],
  body_mass = body_mass[mostra_ids],
  sex = sex[mostra_ids]
)
dfram

# Llista d'objectes continguts a l'espai de treball actual
# --------------------------------------------------------
ls()

# Quants objectes hi ha a l'espai de treball?
# -------------------------------------------
length(ls())

# Resum de diferents objectes d'R
# -------------------------------
summary(bill_length)         # Resum numèric
summary(dfram)               # Resum de totes les variables
summary(island)              # Resum de factor


## =============================================================
## Les funcions save() i save.image() es poden utilitzar per 
## desar objectes en espais de treball d'R.
## =============================================================
# Quin és el directori de treball actual?
getwd()
# Quins fitxers hi ha en aquest directori de treball?
dir()

# Desar
# ----------------------------------------------------------
save.image("Penguins_RLecture.RData")
save(dfram, bill_length, file = "Penguins_Selected.RData")

# Com podem eliminar objectes de l'espai de treball?
# --------------------------------------------------
rm(bill_length)
ls()

# Si ara m'adono que volia esborrar bill_depth (i no bill_length), què puc fer?
# -----------------------------------------------------------------------------
load("Penguins_Selected.RData")
ls()


## ===============================
## Factors versus vector de caràcters
## ===============================
# Vector "habitat" és un vector de caràcters
# ------------------------------------------
(habitat <- rep(c("Antarctica", "Subantarctic", "Coastal"), c(8, 6, 6)))
class(habitat)
table(habitat)
summary(habitat)

# La mateixa informació emmagatzemada com a factor
# ------------------------------------------------
# Les dues comandes d'R següents retornen el mateix objecte
(habitat_f <- factor(habitat))
(habitat_f <- factor(rep(c("Antarctica", "Subantarctic", "Coastal"), c(8, 6, 6))))
class(habitat_f)
table(habitat_f)
summary(habitat_f)

# Internament, els factors són objectes numèrics
# ----------------------------------------------
mode(habitat_f)
as.numeric(habitat_f)

mode(habitat)          # Character
as.numeric(habitat)    # NAs

# Nivells d'un factor
# ------------------
levels(habitat_f)
levels(habitat)        # NULL

# Els valors d'un factor es poden canviar fàcilment
# -------------------------------------------------
levels(habitat_f) <- c("Ant", "Coast", "SubAnt")
habitat_f

# L'ordre dels nivells d'un factor es pot canviar
# -----------------------------------------------
relevel(habitat_f, ref = "Coast")
(habitat_f <- factor(habitat_f, levels = c("Coast", "Ant", "SubAnt")))
table(habitat_f)

# Exemple amb les dades de penguins
# ---------------------------------
levels(species)
table(species)

# Canviar l'ordre dels nivells
(species_reord <- factor(species, levels = c("Gentoo", "Chinstrap", "Adelie")))
table(species_reord)


## ===============================================
## Alguns comentaris sobre les funcions round() i trunc()
## ===============================================
# Arrodonir la longitud del bec
bill_length_round <- round(bill_length, 1)
head(bill_length_round)

# Diferència entre truncar i arrodonir (sense decimals):
head(round(bill_length))
head(trunc(bill_length))

# Exemple amb la massa corporal
body_mass_kg <- body_mass / 1000  # Convertir a kg
round(body_mass_kg, 2)
round(body_mass_kg, 1)
trunc(body_mass_kg)


## ===============================================
## Funcions which(), which.min(), i which.max()
## ===============================================
## Quants pingüins tenen una longitud de bec superior a 45 mm?
sum(bill_length > 45, na.rm = TRUE)

# Quines són les seves posicions respectives?
which(bill_length > 45)

# De quines espècies són?
species[which(bill_length > 45)]
table(species[bill_length > 45])

# Quina és la longitud mínima de les aletes?
min(flipper_length, na.rm = TRUE)

# Quina és la posició de la longitud mínima de les aletes?
which.min(flipper_length)

# Quina és la posició de la longitud màxima de les aletes?
which.max(flipper_length)

# Observació: En el cas de valors màxims o mínims repetits, pot passar el següent:
max(flipper_length, na.rm = TRUE)
which.max(flipper_length)

# Però:
sum(flipper_length == max(flipper_length, na.rm = TRUE), na.rm = TRUE)

# Com podem obtenir totes les posicions on s'assoleix el màxim?
which(flipper_length == max(flipper_length, na.rm = TRUE))


## ==========================
## Detecció de dades repetides
## ==========================
# (a) Utilitzant la funció table()
# --------------------------------
table(flipper_length)
max(table(flipper_length))
max(table(flipper_length)) > 1                      # TRUE

# (b) Utilitzant les funcions unique() i length()
# -----------------------------------------------
head(unique(flipper_length), 10)
length(unique(flipper_length))
length(unique(flipper_length)) < length(na.omit(flipper_length))     # TRUE

# (c) Utilitzant les funcions duplicated() i any()
# ------------------------------------------------
head(duplicated(flipper_length), 20)
sum(duplicated(flipper_length))                     # Nombre de valors repetits
any(duplicated(flipper_length))                     # [Solució òptima]

# Exemple amb les espècies
duplicated(species)
sum(duplicated(species))
any(duplicated(species))


## ================
## Funció is.na()
## ================
# Hi ha algun valor perdut a bill_length o sex?
# --------------------------------------------
head(is.na(bill_length), 20)
any(is.na(bill_length))
any(is.na(sex))

# Quants valors de sex són perduts?
sum(is.na(sex))

# Quines són les dades perdudes de sex?
which(is.na(sex))

# Quines són les posicions amb dades perdudes a bill_length?
which(is.na(bill_length))

# La funció complete.cases() retorna l'oposat de is.na()
# ------------------------------------------------------
head(is.na(sex), 20)
head(complete.cases(sex), 20)
sum(complete.cases(sex))

# El mateix
sum(!is.na(sex))

# I en el data frame complet?
sum(is.na(penguins))

# El nombre de files amb informació completa
sum(complete.cases(penguins))

# Crear un subset sense valors perduts
penguins_complet <- penguins[complete.cases(penguins), ]
nrow(penguins)
nrow(penguins_complet)

# Quantes files hem perdut?
nrow(penguins) - nrow(penguins_complet)


## ========================================
## EXERCICIS AMB PALMERPENGUINS
## ========================================

# ============================================================================
# EXERCICI 1: Estadístiques Descriptives per Espècie
# ============================================================================
# Utilitzant els vectors extrets de la base de dades penguins:
# 
# Preguntes:
# 
# 1) Distribució d'Espècies
# a) Quantes espècies diferents de pingüins hi ha al dataset?
# b) Quants pingüins hi ha de cada espècie?
# c) Quina és l'espècie més abundant?
# 
# 2) Anàlisi de Massa Corporal
# a) Calcula la massa corporal mitjana per a cada espècie (en grams).
# b) Quina espècie té la massa mitjana més alta?
# c) Calcula la desviació estàndard de la massa corporal per espècie.
# 
# 3) Anàlisi de les Aletes
# a) Calcula la longitud mitjana de les aletes per a cada espècie.
# b) Quina espècie té les aletes més llargues en mitjana?
# c) Troba la longitud mínima i màxima de les aletes per espècie.
# 
# PISTES:
?table
?tapply
?aggregate
?levels

# ============================================================================
# EXERCICI 2: Anàlisi per Illes
# ============================================================================
# Continuant amb les dades de penguins:
# 
# Preguntes:
# 
# 1) Distribució Geogràfica
# a) Quantes illes diferents hi ha al dataset?
# b) Quants pingüins hi ha a cada illa?
# c) Crea una taula de contingència que mostri quantes observacions hi ha 
#    de cada espècie a cada illa.
# 
# 2) Característiques per Illa
# a) Calcula la massa corporal mitjana dels pingüins de cada illa.
# b) A quina illa es troben els pingüins més pesats en mitjana?
# c) Calcula la longitud mitjana del bec per cada illa.
# 
# 3) Diversitat d'Espècies
# a) Quantes espècies diferents hi ha a cada illa?
# b) Quina illa té la major diversitat d'espècies?
# c) Hi ha alguna espècie present a totes les illes?
# 
# PISTES:
?table
?tapply
?unique

# ============================================================================
# EXERCICI 3: Diferències entre Sexes
# ============================================================================
# Analitzant les diferències entre mascles i femelles:
# 
# Preguntes:
# 
# 1) Distribució per Sexe
# a) Quants pingüins mascles i femelles hi ha al dataset?
# b) Hi ha valors perduts a la variable sex? Quants?
# 
# 2) Dimorfisme Sexual - Massa Corporal
# a) Calcula la massa corporal mitjana per sexe (ignora NAs).
# b) Quin sexe és més pesat en mitjana?
# c) Quina és la diferència de massa mitjana entre sexes?
# 
# 3) Dimorfisme Sexual - Dimensions del Bec
# a) Calcula la longitud mitjana del bec per sexe.
# b) Calcula la profunditat mitjana del bec per sexe.
# c) En quina característica (longitud o profunditat del bec) és més 
#    pronunciada la diferència entre sexes?
# 
# 4) Anàlisi Combinada
# a) Calcula la massa corporal mitjana per espècie i sexe combinats.
# b) Quina combinació espècie-sexe té la massa mitjana més alta?
# 
# PISTES:
?tapply
?aggregate
?na.omit

# ============================================================================
# EXERCICI 4: Identificació de Valors Extrems
# ============================================================================
# Trobant els pingüins amb característiques extremes:
# 
# Preguntes:
# 
# 1) Pingüí més Pesat
# a) Quina és la massa del pingüí més pesat?
# b) En quina posició del vector està aquest pingüí?
# c) De quina espècie és?
# d) De quina illa prové?
# e) Quin és el seu sexe?
# 
# 2) Pingüí més Lleuger
# a) Quina és la massa del pingüí més lleuger?
# b) En quina posició del vector està aquest pingüí?
# c) De quina espècie és?
# 
# 3) Aletes més Llargues i més Curtes
# a) Quina és la longitud màxima de les aletes al dataset?
# b) Quants pingüins tenen aquesta longitud màxima?
# c) Quina és la longitud mínima de les aletes?
# d) De quina espècie són els pingüins amb les aletes més curtes?
# 
# 4) Bec més Llarg
# a) Quina és la longitud màxima del bec?
# b) De quina espècie és el pingüí amb el bec més llarg?
# c) Hi ha més d'un pingüí amb aquesta longitud màxima de bec?
# 
# PISTES:
?which.min
?which.max
?which

# ============================================================================
# EXERCICI 5: Filtres i Subsets
# ============================================================================
# Creant subconjunts de dades segons diferents criteris:
# 
# Preguntes:
# 
# 1) Filtres Simples
# a) Quants pingüins Adelie tenen una massa corporal superior a 4000g?
# b) Quines són les posicions d'aquests pingüins?
# c) Quina és la massa mitjana d'aquests pingüins pesats Adelie?
# 
# 2) Filtres Múltiples
# a) Quants pingüins de l'illa Biscoe tenen aletes més llargues que 200mm?
# b) Crea un vector amb les espècies d'aquests pingüins.
# c) Quina espècie predomina en aquest subset?
# 
# 3) Exclusió de Valors Perduts
# a) Crea un nou data frame que només contingui observacions sense cap 
#    valor perdut.
# b) Quantes files s'han eliminat?
# c) Quin percentatge de dades es va perdre?
# 
# PISTES:
?which
?subset
?complete.cases
?mean

# ============================================================================
# EXERCICI 6: Visualitzacions
# ============================================================================
# Creant gràfics per explorar les dades:
# 
# Objectius:
# 
# 1) Crea un histograma de la distribució de la massa corporal.
#    - Utilitza 20 intervals
#    - Afegeix un títol descriptiu
#    - Marca la mitjana amb una línia vertical
# 
# 2) Crea un boxplot de la massa corporal per espècie.
#    - Utilitza colors diferents per a cada espècie
#    - Afegeix títols als eixos
# 
# 3) Crea un gràfic de dispersió de longitud del bec vs profunditat del bec.
#    - Utilitza colors diferents per a cada espècie
#    - Afegeix una llegenda
#    - Afegeix títols descriptius
# 
# 4) Crea un gràfic de barres que mostri la distribució d'espècies per illa.
#    - Utilitza barres agrupades (beside = TRUE)
#    - Afegeix una llegenda
#    - Utilitza colors diferents per a cada illa
# 
# 5) Crea un gràfic de dispersió de massa corporal vs longitud de les aletes.
#    - Coloreja per espècie
#    - Afegeix una línia de tendència per a cada espècie (opcional)
# 
# 6) Crea un gràfic de barres de la distribució per sexe.
#    - Exclou els valors perduts
#    - Utilitza colors rosa i blau
# 
# PISTES:
?hist
?boxplot
?plot
?barplot
?legend
?abline


## ========================================
## SOLUCIONS DELS EXERCICIS
## ========================================

# ============================================================================
# SOLUCIONS EXERCICI 1: Estadístiques Descriptives per Espècie
# ============================================================================

# 1)
# a) Quantes espècies diferents?
num_especies <- length(levels(species))
num_especies
levels(species) #quines?

# b) Quants pingüins de cada espècie?
taula_especies <- table(species) 
taula_especies

# c) Espècie més abundant
names(which.max(taula_especies)) 

# 2)
# a) Massa mitjana per espècie
massa_mitjana_especie <- tapply(body_mass, species, mean, na.rm = TRUE)
round(massa_mitjana_especie, 2)

# b) Espècie amb massa mitjana més alta
especie_mes_pesada <- names(which.max(massa_mitjana_especie))
especie_mes_pesada

# c) Desviació estàndard per espècie
tapply(body_mass, species, sd, na.rm = TRUE)

# 3)
# a) Longitud mitjana de les aletes per espècie
aletes_mitjana_especie <- tapply(flipper_length, species, mean, na.rm = TRUE)
round(aletes_mitjana_especie, 2)

# b) Espècie amb aletes més llargues
especie_aletes_llargues <- names(which.max(aletes_mitjana_especie))
especie_aletes_llargues
round(max(aletes_mitjana_especie), 2)

# c) Longitud mínima i màxima per espècie 
rang_aletes <- tapply(flipper_length, species, function(x) {
  c(Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
})
for (esp in names(rang_aletes)) {
  cat("  ", esp, ": Min =", rang_aletes[[esp]]["Min"], 
      "mm, Max =", rang_aletes[[esp]]["Max"], "mm\n")
}

# ============================================================================
# SOLUCIONS EXERCICI 2: Anàlisi per Illes
# ============================================================================

# 1) Distribució Geogràfica
# a) Quantes illes diferents?
num_illes <- length(levels(island))
num_illes
levels(island)

# b) Quants pingüins per illa?
taula_illes <- table(island)
taula_illes

# c) Taula de contingència espècie x illa
table(island, species)

# 2) Característiques per Illa
# a) Massa mitjana per illa
massa_mitjana_illa <- tapply(body_mass, island, mean, na.rm = TRUE)
massa_mitjana_illa

# b) Illa amb pingüins més pesats
illa_mes_pesada <- names(which.max(massa_mitjana_illa))
illa_mes_pesada
round(max(massa_mitjana_illa), 2)

# c) Longitud mitjana del bec per illa
tapply(bill_length, island, mean, na.rm = TRUE)

# 3) Diversitat d'Espècies
# a) Quantes espècies per illa?
especies_per_illa <- tapply(as.character(species), island, function(x) {
  length(unique(x[!is.na(x)]))
})
especies_per_illa

# b) Illa amb major diversitat
illa_mes_diversa <- names(which.max(especies_per_illa))
illa_mes_diversa
max(especies_per_illa)

# c) Espècies presents a totes les illes 
for (illa in levels(island)) {
  especies_illa <- unique(species[island == illa & !is.na(species)])
  cat("  ", illa, ":", paste(especies_illa, collapse = ", "), "\n")
}

especies_totes_illes <- Reduce(intersect, lapply(levels(island), function(illa) {
  unique(as.character(species[island == illa & !is.na(species)]))
}))

if (length(especies_totes_illes) > 0) {
  cat("\nEspècies presents a totes les illes:", 
      paste(especies_totes_illes, collapse = ", "), "\n")
} else {
  cat("\nNo hi ha cap espècie present a totes les illes\n")
}

# ============================================================================
# SOLUCIONS EXERCICI 3: Diferències entre Sexes
# ============================================================================

# 1) Distribució per Sexe

# a) Quants mascles i femelles?
table(sex)

# b) Valors perduts
num_na_sex <- sum(is.na(sex)) # Nombre de valors perduts a la variable sex
round(num_na_sex / length(sex) * 100, 2) # Percentatge de NAs

# 2)
# a) Massa mitjana per sexe
massa_mitjana_sexe <- tapply(body_mass, sex, mean, na.rm = TRUE)
massa_mitjana_sexe

# b) Sexe més pesat
if (!all(is.na(massa_mitjana_sexe))) {
  sexe_mes_pesat <- names(which.max(massa_mitjana_sexe))
  cat("\nb) Sexe més pesat en mitjana:", sexe_mes_pesat,
      "amb", round(max(massa_mitjana_sexe, na.rm = TRUE), 2), "g\n")
  
  # c) Diferència entre sexes
  if (length(massa_mitjana_sexe) >= 2) {
    diferencia_massa <- abs(massa_mitjana_sexe["male"] - massa_mitjana_sexe["female"])
    cat("\nc) Diferència de massa mitjana entre sexes:", 
        round(diferencia_massa, 2), "g\n")
  }
}

# 3)
# a) Longitud mitjana del bec per sexe
bec_long_sexe <- tapply(bill_length, sex, mean, na.rm = TRUE)
cat("a) Longitud mitjana del bec per sexe (mm):\n")
print(round(bec_long_sexe, 2))

# b) Profunditat mitjana del bec per sexe
bec_prof_sexe <- tapply(bill_depth, sex, mean, na.rm = TRUE) 

# c) Característica amb més diferència
if (length(bec_long_sexe) >= 2 && length(bec_prof_sexe) >= 2) {
  dif_longitud <- abs(bec_long_sexe["male"] - bec_long_sexe["female"])
  dif_profunditat <- abs(bec_prof_sexe["male"] - bec_prof_sexe["female"])
  
  cat("\nc) Diferències entre sexes:\n")
  cat("   Longitud del bec:", round(dif_longitud, 2), "mm\n")
  cat("   Profunditat del bec:", round(dif_profunditat, 2), "mm\n")
  
  if (dif_longitud > dif_profunditat) {
    cat("   La diferència és més pronunciada en la longitud del bec\n")
  } else {
    cat("   La diferència és més pronunciada en la profunditat del bec\n")
  }
}

# 4) Anàlisi Combinada
# a) Massa mitjana per espècie i sexe
massa_especie_sexe <- aggregate(body_mass ~ species + sex, 
                                data = penguins, 
                                FUN = mean, 
                                na.rm = TRUE)
massa_especie_sexe

# b) Combinació amb massa més alta
max_row <- which.max(massa_especie_sexe$body_mass) 
# Espècie
as.character(massa_especie_sexe$species[max_row])
# Sexe
as.character(massa_especie_sexe$sex[max_row])
# Massa
round(massa_especie_sexe$body_mass[max_row], 2) 

# ============================================================================
# SOLUCIONS EXERCICI 4: Identificació de Valors Extrems
# ============================================================================

# 1) Pingüí més Pesat
# a-e) Informació completa del pingüí més pesat
pos_max_massa <- which.max(body_mass)
# a) Massa del pingüí més pesat
body_mass[pos_max_massa]
# b) Posició al vector
pos_max_massa
# c) Espècie
as.character(species[pos_max_massa])
# d) Illa
as.character(island[pos_max_massa])
# e) Sexe
as.character(sex[pos_max_massa])

# 2) Pingüí més Lleuger
# a-c) Informació del pingüí més lleuger
pos_min_massa <- which.min(body_mass)
body_mass[pos_min_massa] #a) Massa del pingüí més lleuger
pos_min_massa #b) Posició al vector
as.character(species[pos_min_massa]) #c) Espècie 

# 3) Aletes més Llargues i més Curtes
# a) Longitud màxima de les aletes
max_aletes <- max(flipper_length, na.rm = TRUE) 

# b) Quants pingüins tenen aquesta longitud?
pos_max_aletes <- which(flipper_length == max_aletes) 

# c) Longitud mínima de les aletes
min_aletes <- min(flipper_length, na.rm = TRUE) 

# d) Espècie amb aletes més curtes
pos_min_aletes <- which(flipper_length == min_aletes)
especies_min_aletes <- unique(species[pos_min_aletes]) 

# 4) Bec més Llarg
# a) Longitud màxima del bec
max_bec <- max(bill_length, na.rm = TRUE)
max_bec

# b) Espècie amb el bec més llarg
pos_max_bec <- which.max(bill_length)
species[pos_max_bec]

# c) Hi ha més d'un pingüí amb aquesta longitud?
num_max_bec <- sum(bill_length == max_bec, na.rm = TRUE)
num_max_bec

# ============================================================================
# SOLUCIONS EXERCICI 5: Filtres i Subsets
# ============================================================================ 

# 1) Filtres Simples

# a) Adelie amb massa > 4000g
adelie_pesats <- which(species == "Adelie" & body_mass > 4000)
cat("a) Nombre de pingüins Adelie amb massa > 4000g:", 
    length(adelie_pesats), "\n")

# b) Posicions
cat("b) Posicions (primeres 10):", 
    paste(head(adelie_pesats, 10), collapse = ", "), 
    ifelse(length(adelie_pesats) > 10, "...", ""), "\n")

# c) Massa mitjana
if (length(adelie_pesats) > 0) {
  massa_mitjana_adelie_pesats <- mean(body_mass[adelie_pesats], na.rm = TRUE)
  cat("c) Massa mitjana dels Adelie pesats:", 
      round(massa_mitjana_adelie_pesats, 2), "g\n")
}

# 2) Filtres Múltiples
# a) Biscoe amb aletes > 200mm
biscoe_aletes_llargues <- which(island == "Biscoe" & flipper_length > 200)
length(biscoe_aletes_llargues) #Nombre de pingüins de Biscoe amb aletes > 200mm 

# b) Vector d'espècies
especies_biscoe_aletes <- species[biscoe_aletes_llargues]
table(especies_biscoe_aletes)

# c) Espècie predominant
if (length(biscoe_aletes_llargues) > 0) {
  taula_esp_biscoe <- table(especies_biscoe_aletes)
  especie_predominant <- names(which.max(taula_esp_biscoe))
  cat("c) Espècie predominant:", especie_predominant, "\n")
}

# 3)
# a) Data frame sense NAs
penguins_complet <- penguins[complete.cases(penguins), ]

# b) Files eliminades
files_eliminades <- nrow(penguins) - nrow(penguins_complet)

# c) Percentatge perdut
percentatge_perdut <- (files_eliminades / nrow(penguins)) * 100

# ============================================================================
# SOLUCIONS EXERCICI 6: Visualitzacions
# ============================================================================

windows(width = 14, height = 10)
par(mfrow = c(2, 3))

# 1) Histograma de massa corporal
hist(body_mass, breaks = 20, col = "lightblue", border = "black",
     main = "Distribució de la Massa Corporal",
     xlab = "Massa Corporal (g)", ylab = "Freqüència")
abline(v = mean(body_mass, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Mitjana", col = "red", lty = 2, lwd = 2)

# 2) Boxplot de massa per espècie
boxplot(body_mass ~ species, 
        col = c("orange", "purple", "cyan"),
        main = "Massa Corporal per Espècie",
        xlab = "Espècie", ylab = "Massa Corporal (g)")

# 3) Dispersió longitud vs profunditat del bec
plot(bill_length, bill_depth, 
     col = as.numeric(species), pch = 16, cex = 0.8,
     main = "Longitud vs Profunditat del Bec",
     xlab = "Longitud del Bec (mm)", 
     ylab = "Profunditat del Bec (mm)")
legend("topright", legend = levels(species), 
       col = 1:3, pch = 16, cex = 0.8)

# 4) Barres d'espècies per illa
barplot(table(island, species), 
        beside = TRUE, 
        col = c("brown", "darkgreen", "lightblue"),
        main = "Espècies per Illa",
        xlab = "Espècie", ylab = "Nombre de Pingüins",
        legend = levels(island))

# 5) Dispersió massa vs longitud aletes
plot(body_mass, flipper_length,
     col = as.numeric(species), pch = 16, cex = 0.8,
     main = "Massa Corporal vs Longitud Aletes",
     xlab = "Massa Corporal (g)",
     ylab = "Longitud Aletes (mm)")
legend("bottomright", legend = levels(species),
       col = 1:3, pch = 16, cex = 0.8)

# 6) Barres per sexe (sense NAs)
sex_sense_na <- sex[!is.na(sex)]
barplot(table(sex_sense_na), 
        col = c("pink", "lightblue"),
        main = "Distribució per Sexe",
        xlab = "Sexe", ylab = "Nombre de Pingüins")




