getwd()
install.packages("readxl")
library(readxl)
setwd('C:/Users/BILLO/Desktop/Machine_learning')
#3.1
nutri1 = read_excel("nutri1.xls")
nutri2 = read_excel("nutri2.xls")
View(nutri1)
View(nutri2)
colnames(nutri2) = toupper(colnames(nutri2))
colnames(nutri2)
result = rbind(nutri1,nutri2)
View(result)
#3.2
nutri3 = read_excel("nutri3.xls")
nutri4 = read_excel("nutri4.xls")
#3.3 
# fichier intima_media:
intima = read_excel("Intima_Media.xls")
View(intima)
intima$IMC = intima$poids/(intima$taille/100)^2
# 3.2 
nrow(intima[intima$IMC>30,])
#3.3
intima[intima$SPORT==1&intima$SEXE==2,]
#3.4
intima[intima$AGE>=50 & intima$IMC<=30,]
# Fichier imcenfant:
#3.1
imcenfant <- read_excel("imcenfant.xls")
imcenfant$IMC<-imcenfant$poids/((imcenfant$taille/100)^2)
IMC<-imcenfant$poids/((imcenfant$taille/100)^2)
imcenfant <- cbind(imcenfant,IMC)

#3.2
imcenfant[imcenfant$IMC<15&imcenfant$an<=3&imcenfant$mois<=6,]

#3.3
dim(imcenfant[imcenfant$IMC<15&imcenfant$an<=3&imcenfant$mois<=6,])[1]
#Fichier Poids_de_naissance
#3.1
poid.nais <- read_excel("Poids_naissance.xls")

PTL1 <- poid.nais$PTL
PTL1[poid.nais$PTL>=2] <- 2
poid.nais <- cbind(poid.nais,PTL1)

poid.nais$PTL1 <- poid.nais$PTL
PTL1[poid.nais$PTL>=2] <- 2

Poid_naissance$PLT1 <- ifelse(Poid_naissance$PTL <=1, Poid_naissance$PTL, 2)

poids$PTL1= (ifelse((poids$PTL==0),0,ifelse(poids$PTL==1,1,2)))

#3.2
FVT1 <- poid.nais$FVT
FVT1[poid.nais$FVT>=2] <- 2
poid.nais <- cbind(poid.nais,FVT1)

#3.3
poid.nais[order(poid.nais$BWT),]
poid.nais[order(poid.nais$BWT , decreasing = T),]
#3.4
poid.nais[poid.nais$RACE<=2 & poid.nais$SMOKE==1,]
poid.nais[(poid.nais$RACE==2 | poid.nais$RACE==1 )& poid.nais$SMOKE==1,]

Poid_naissance[(Poid_naissance$RACE== 1 & Poid_naissance$SMOKE==1) | (Poid_naissance$RACE==2 & Poid_naissance$SMOKE==1),]