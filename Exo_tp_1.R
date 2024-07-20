library(car) # package permettant de verifier la multicollinearité
library(ggplot2) # créer  des graphe
#setwd('C:/Users/BILLO/Desktop/Machine_learning')
laptops = read.table(file='laptops.csv',sep=';',header = TRUE)#Visualiser le contenu d'un tableau de données à l'aide de la fonction
setwd('C:/Users/BILLO/Desktop/Machine_learning')
file.choose(new=FALSE)


#4 Visualiser le contenu d'un tableau de données à l'aide de la fonction

View(laptops)

# 5 Afficher les 6 premières lignes de l'objet "laptops"
head(laptops)
laptops

# 6 Attribuer aux colonnes du dataset les noms suivants : Prix", "Taille"

names(laptops) = c("Prix","Taille","RAM","Disque","Ports","Marque","Poids")

# 7 Afficher les mesures statistiques de base pour le dataset à l’aide de summary
summary(laptops)

#8 Quel est le prix maximal et minimal d’un laptop en $

max(laptops$Prix)
# 9 Quel est le nombre minimal de ports qu’on peut trouver dans un laptop de l’échantillon étude
min(laptops$Ports)
# 10 Quelle est la taille moyenne d’un laptop
mean(laptops$Taille)
# 11 Quelle est la valeur médiane du poids en onces
median(laptops$Poids)
# 12 Quels sont les caractéristiques de la marque (longueur, classe, mode) ?
length(laptops$Marque)
class(laptops$Marque)
mode(laptops$Marque)
#13 On propose d’expliquer le Prix ($) en fonction de la taille du disque (Giga). Afficher le nuage de point à l’aide de ggplot2. Que remarquez-vous ?
nuage = ggplot(data=laptops,aes(Disque,Prix))

n1   = lm(laptops$Prix~laptops$Disque)
plot(laptops$Disque,laptops$Prix)
abline(n1,col="red")

model1 = lm(Prix~ Disque,data = laptops)
#14) Peut-on établir un modèle linéaire pour représenter la relation entre les variables Prix et Disque. Si oui, ajouter la droite de régression linéaire.
#15) Evaluer la qualité du modèle à partir du graphique.
sqrt(0.06903)
# 16 Déterminer la covariance entre les variables Prix et Disque. Interpréter.
cov(laptops$Prix,laptops$Disque)
# 17 Déterminer le coefficient de corrélation. Interpréter
cor(laptops$Prix,laptops$Disque)
#18 Déterminer le coefficient de détermination. Interpréter
summary(model1)$r.squared
#19 Afficher les mesures statistiques pour le modèle linéaire obtenu.
summary(model1)
#20 Analyser le modèle linéaire sur le plan inférentiel en interprétant la
#valeur de p-value

#22 model linéaire multiple
model2 = lm(Prix ~ Disque+Taille+Ports+ Poids + RAM, data = laptops)
summary(model2)
# 23 determiner la valeur de vif
vif(model2)
# 24 Déterminer la matrice de corrélation du dataset privé de la variable
#qualitative (marque3). Vérifier la corrélation entre Poids et taille
# 25 On propose de corriger le modèle en ignorant la variable taille.
#Etablir le nouveau modèle linéaire (model4). Est-ce le modèle a été
#amélioré
model4 = lm(Prix ~ Disque+Ports+ Poids+RAM,data= laptops)
summary(model4)
vif(model4)
confint(model4)
