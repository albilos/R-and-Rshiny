# Exercice 1:
getwd()
library(readxl)
setwd('C:/Users/BILLO/Desktop/Machine_learning')
#1 
ges = read_excel("EmissionsGES.xls")
View(ges)
#2
ges_sans_na <- na.omit(ges)
View(ges_sans_na)
#3 
dim(ges_sans_na)
colnames(ges_sans_na)
# 4
median_ges <- median(ges_sans_na$"Emissions 2003")
mean_ges <-  mean(ges_sans_na$"Emissions 2003")
sum(ges_sans_na$"Emissions 2003" > median_ges)
sum(ges_sans_na$"Emissions 2003" > mean_ges)
#5
# Pour calculer la médiane des émissions en 2003 sans les États-Unis, utilisez la fonction median() en appliquant un filtre sur la colonne correspondante et excluez les lignes où le pays est "États-Unis". 
median_emissions_sans_US <-median(ges_sans_na$"Emissions 2003"[ges$Pays != "USA"])
#on calcule la moyenne des émissions en 2003 sans les États-Unis, utilisez la fonction mean() avec le même filtre.
mean_emissions_sans_US <- mean(ges_sans_na$"Emissions 2003"[ges$Pays != "USA"])
# ensuite on compare ces valeurs avec les résultats de la question précédente en soustrayant le nombre d'émissions supérieures à la médiane ou à la moyenne avec les États-Unis
pays_sup_median_sans_US <- sum(ges_sans_na$"Emissions 2003" > median_emissions_sans_US)
pays_sup_moyenne_sans_US <- sum(ges_sans_na$"Emissions 2003" > mean_emissions_sans_US)
#Les variables diff_pays_sup_median et diff_pays_sup_moyenne contiendront la différence entre les résultats précédents (incluant les États-Unis) et les résultats actuels (sans les États-Unis) pour le nombre de pays ayant des émissions supérieures à la médiane et à la moyenne, respectivement.
diff_pays_sup_median <- pays_sup_median - pays_sup_median_sans_US
diff_pays_sup_moyenne <- pays_sup_moyenne - pays_sup_moyenne_sans_US
diff_pays_sup_median
diff_pays_sup_moyenne
#6
correlation <- cor(ges$"Emissions 2003", ges$Population, use = "complete.obs")
plot(ges$"Emissions 2003", ges$Population, xlab = "Emissions2003", ylab = "Population", main = "Nuage de points")
#7
modele <- lm(Population ~ ges$"Emissions 2003", data = ges)
summary_modele <- summary(modele)
equation <- paste("Population =", round(summary_modele$coefficients[1, 1], 2), "+", round(summary_modele$coefficients[2, 1], 2), "* Emissions2003")
coefficient_r2 <- summary_modele$r.squared
plot(ges$"Emissions 2003", ges$Population, xlab = "Emissions2003", ylab = "Population", main = "Nuage de points")
abline(modele, col = "red")
#8
correlation <- cor(ges_sans_na$"Emissions 2003", ges_sans_na$Population, use = "complete.obs")
plot(ges_sans_na$"Emissions 2003", ges_sans_na$Population, xlab = "Emissions2003", ylab = "Population", main = "Nuage de points")
modele <- lm(Population ~ ges_sans_na$"Emissions 2003", data = ges_sans_na)
summary_modele <- summary(modele)
equation <- paste("Population =", round(summary_modele$coefficients[1, 1], 2), "+", round(summary_modele$coefficients[2, 1], 2), "* Emissions2003")
coefficient_r2 <- summary_modele$r.squared
plot(ges_sans_na$"Emissions 2003", ges_sans_na$Population, xlab = "Emissions2003", ylab = "Population", main = "Nuage de points")
abline(modele, col = "red")
#9
population <- 50
prediction <- predict(modele, newdata = data.frame(ges_sans_na$"Emissions 2003" = NA, Population = population))

# Exercice 2
# 1
donnees <- read.table("NO2trafic.txt", header = TRUE)
View(donnees)
head(donnees)  # Affiche les premières lignes du jeu de données

# 2
donnees[, -2]
mesures_par_route <- apply(donnees[,-2], MARGIN = 1, FUN = sum)
print(mesures_par_route)
# 3
library(dplyr)

donnees <- donnees %>%
  mutate(fluidité2 = case_when(
    fluidité == 1 ~ "régime fluide",
    fluidité == 2 ~ "régime synchronisé",
    fluidité %in% c(3, 4) ~ "congestions majeures et mobiles"
  ))
# Exercice 3

# 1
chanson1 <- read.csv("chanson-française1.csv", header = TRUE)
chanson2 <- read.csv("chanson-française2.csv", header = TRUE)
View(chanson1)
# 2
chansons <- rbind(chanson1, chanson2)

# 3
nombre_chansons <- table(chansons$Chanteur)
print(nombre_chansons)


