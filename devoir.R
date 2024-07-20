
# 1.2Etudes des variables qualitatifs

setwd("C:/Users/BILLO/Desktop/Machine_learning")
Eva <- read.csv2("Data_devoir.csv", sep=";") 
View(Eva)

attach(Eva) # Chargement de la table

#Etude des paramètres de position et de dispersion des variables qualitatives

summary(Eva)
names(Eva)
x=Smoker
y=FavoritePlayer
x=as.factor(x)
y=as.factor(y)
class(x)
table(x) # les effectifs en x et y
table(y) 

prop.table(table(x))*100 #fr?quence smoker


#Pour tracer le diagramme en b?tons smoker
plot(x) 
plot(table(x),xlab="donnees x",ylab="Effectif")
plot(prop.table(table(x)),xlab="donnees x",ylab="Fr?quence")
pie(table(x))          #diagramme circulaire ou "camembert" 
plot(ecdf(table(x)))   #courbe des frequences cummul?es croissantes

#Pour tracer le diagramme en barres smoker
barplot(table(x),xlab="donnees x",ylab="Effectif")
barplot(prop.table(table(x)),xlab="donnees x",ylab="Effectif")




prop.table(table(y))*100 #fr?quence FavoritePlayer 


#Pour tracer le diagramme en b?tons FavoritePlayer
plot(y) 
plot(table(y),xlab="donnees x",ylab="Effectif")
plot(prop.table(table(y)),xlab="donnees x",ylab="Fr?quence")
pie(table(y))          #diagramme circulaire ou "camembert" 
plot(ecdf(table(y)))   #courbe des frequences cummul?es croissantes

#Pour tracer le diagramme en barres FavoritePlayer
barplot(table(y),xlab="donnees x",ylab="Effectif")
barplot(prop.table(table(y)),xlab="donnees x",ylab="Effectif")


# CARACTERE QUANTITATIF DISCRETS

z=Age
class(z)
summary(z)

var(z)          #variance corrig?e
sd(z)           #?cart-type corrig?
mean(z)         #Moyenne
quantile(z)     #Toutes les quartiles

median(z)       #Mediane
length(z)       #Longueur

#Pour tracer le diagramme en barres
barplot(table(z),xlab="donnees x",ylab="Effectif")
barplot(prop.table(table(z)),xlab="donnees x",ylab="Effectif")

#Pour tracer le diagramme en barres 
barplot(table(z),xlab="donnees x",ylab="Effectif")
barplot(prop.table(table(z)),xlab="donnees x",ylab="Effectif")
par(mfrow=c(1,3))
boxplot(z)             #diagramme en bo?te ? moustaches 
hist(z)                #histogramme 


#  CARACTERE QUANTITATIF CONTINU

c =WaitingTime
class(c)
summary(c)

var(c)          #variance corrig?e
sd(c)           #?cart-type corrig?
mean(c)         #Moyenne
quantile(c)     #Toutes les quartiles

median(c)       #Mediane
length(c)       #Longueur


#Pour obtenir les effectifs et les fr?quences,
table(c) #effectif
round(prop.table(table(c))*100,2) #frequence 


#Pour tracer le diagramme en b?tons 
plot(table(c),xlab="donnees x",ylab="Effectif")
plot(prop.table(table(c)),xlab="donnees x",ylab="Fr?quence")
boxplot(c)

#Pour tracer le diagramme en barres 
barplot(table(c),xlab="donnees x",ylab="Effectif")
barplot(prop.table(table(c)),xlab="donnees x",ylab="Effectif")

pie(table(c))          #diagramme circulaire ou "camembert" 
               #courbe des frequences cummul?es croissantes

boxplot(c)             #diagramme en bo?te ? moustaches 
hist(c)                #histogramme 
# Regression lineaire 
E = seq(1,100)
plot(Eva$Population,E)
# correlation
cor(Eva$Population,E)
#conclusion: cor=-0.9684 les caractères sont forte correlation avec une decroissance
modele=lm(E~Eva$Population)
modele
abline(modele$coefficients,lwd=6,col="RED")

#  Equation de la droite de regression : 
coeff=coefficients(modele)
coeff

paste( "la droite et regression est: y= ",coeff[2],"x+",coeff[1])
# add predict with 102
z=102
predict(modele, newdata =


 

