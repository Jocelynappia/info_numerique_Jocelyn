rm(list = ls()) #commande pour supprimer l'espace de travail.

#******************************
#******MASTER 1 PROJET S2IN ***
#*APPIA ASSADOU JOCELYN *******
#*HABES AMINE *****************
#*EL BADAOUI Bilel ************
#******************************

#### 1. INTRODUCTION ####






#### 1.1 IMPORTER LES DONNEES ####

#definir l'espace de travail.

setwd("C:/Users/jocel/OneDrive - Université Savoie Mont Blanc/Bureau/MASTER S2IN IAE 2023-2024/LOGICIEL R") #attention à changer le slash

# importation des libraries et affectation des variables 

library("readxl")
install.packages("readxl")

my_data1 <- read_excel("FLUVACESSAISPP.xlsx")
my_data1$...1 <- NULL
#rownames(my_data1)<- my_data1$...1
my_data1 <- na.omit(my_data1) # supprimer les valeurs manquantes dans my_data1
rownames(my_data1) <- my_data1$Pays # definir la colonne pays en rowname

View(my_data1)




#***************************************
#### 2. ANALYSE DESCRIPTIVE ####

 ********
#***************************************


### 2.1 - ANALYSE UNIVARIEE ####

## REMARQUE : Pour une population de 39 pays, notre fichier a été reduit à 21 pays
# à cause de donnée maquantes issue des variables independentes.

attach(my_data1)

summary(txvacc)
summary(my_data1)
sd(txvacc)

install.packages("pastecs")

library(pastecs)

stat.desc(my_data1) 
stat.desc(my_data1)["txvacc"]

## indentification des quartiles autour de la boites

boxplot(txvacc)

## verification avec l'histogramme

hist(txvacc)
hist(txvacc, breaks = 7, col = "grey", main = "Histogramme", xlab = "taux de vaccination", ylab = "fréquence")

## avec la densite

d <- density(txvacc)
plot(d, main ='densite', xlab = 'tx de vaccination', ylab = 'densite')

# VERIFIONS L'ASYMETRIE ET L'APPLATISSEMENT.


install.packages("e1071")
library(e1071)


skewness(txvacc)

#interpretation du resultat : 
# skewness = -0.133 < 0 on a une distribution asymetrie négative vers le bas du aux valeurs extrêmes 

kurtosis(txvacc)

#kurtosis  = -1.756 applatissement < 0 = platykurtic




#### 2.2 - ANALYSE BIVARIEE ####




#### 2.2.1 ETUDE ENTRE DEUX VARIABLE QUANTITATIVE.####

# Nous choisirons deux variables pour tester la 

# DIAGRAMME DE CORRELATION 

attach(my_data1)
install.packages("corrplot")

cor_matrix <- cor(my_data1[c("txvacc", "depsantepropBIP", "popcouvertsante", "consulmedparhabit", "etatdesanteetlimitationauto", "depsanteparhab", "etendconversante", "incapac65", "maladiechronique65", "espdevie65", "persnelinfipour100")])

library(corrplot)

# Diagramme de correlation

corrplot(cor_matrix)

# correlation par le haut

corrplot(cor_matrix, type = "upper", diag = FALSE)


# exemple1 : txvacc et etendue de la converture sante

cor(txvacc, etendconversante) # resultat : cor =  0.4972666

#exemple2 : txvacc et esperance de vie

cor(txvacc, espdevie65) # resultat : cor  = 0.7898065

cor(txvacc, depsanteparhab)

#LA NOTION DE CAUSALITE

#débutons pas un nuage de point (entre deux variable continue par exemple)

plot(txvacc ~ etendconversante)

plot(txvacc ~ espdevie65, main = "NUAGE DE POINT", xlab = "esperance de vie", ylab = "taux de vaccination")

# droite de regression
droite_reg <- lm(txvacc ~ espdevie65)
summary(droite_reg)

#L’espérance de vie étant toujours une valeur positive, 
#lorsque l'espérance de vie est égale à 0, le taux de vaccination (-734%) n’a aucune signification,
#sauf une impossibilité de cas.
#Lorsque l'espérance de vie (espdevie65) augmente d'une unité, 
#on peut s'attendre à ce que le taux de vaccination (txvacc) augmente d'environ 9.104 unités,
#toutes choses égales par ailleurs. (c'est la relation causale)


# la droite de regression est  : taxvacc = -735 + 9,104* espdevie65
#representation de la droite de regression

abline(droite_reg, col= "red")




#*************************************
#### 2.3 ANALYSE MULTIVARIEE ####
#*************************************


#### 2.3.1 ANALYSE DE COMPOSANTES FONCTIONNELLES ####

## analyse en composantes principales

install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

#pca= principal component analysis

# Sont considéré dans cette analyse toutes les variables du fichier my_data1


datapca <- my_data1

#rownames(datapca)<- datapca$...1


attach(datapca)

datapca$Pays <- NULL

View(datapca)


res.pca <- PCA(datapca, graph = FALSE)

# eigenvalues = valeurs propres des axes factoriels

res.pca[["eig"]]

# On ne retient que les composantes qui ont une valeur propre (eigen value) supérieur à 1. On ne retient donc que les deux composantes.

fviz_eig(res.pca,addlabels = TRUE)


# eigenvectors = axes factoriels

res.pca[["var"]]

fviz_pca_var(res.pca, axes = c(1,2))

fviz_pca_ind(res.pca) #pour representer les individus dans le plan factoriel
fviz_pca(res.pca) # pour avoir les deux informatiosn sur le graphique



#### 2.3.2 CLASSIFICATION HIÉRARCHIQUE SUR COMPOSANTE PRINCIPALES ####

#hierarchical classification on proncipal components

res.hcpc <- HCPC(res.pca, graph = FALSE)

#Graphique : dendogramme ( pour montrer l'information utiliser)

fviz_dend(res.hcpc, cex = 0.3)

# R nous dit de considerer 3 classes ranger par couleur. 
# On souhaite avoir des groupes suffisemment distincts entre eux
# pour interpréter les classes
## 1 commençons par représenter les classes qui ont été créees.

fviz_cluster(res.hcpc)

#1-1 comment interpreter

#interessons à la position du centre par rapport aux axes.
# en rouge le centre est proche de l'axe des abscisses : ce sont des véhicules qui sont pas rapide
# et qui consomment peu


#individu les plus representatifs de chaque classe (parangons)
res.hcpc$desc.ind$para

#*****************************************************************************************




#*************************************
#### 3. ANALYSE ECONOMETRIQUE ####


#*****
#*************************************


#### 3.1. MODELE DE REGRESSION MULTIPLE ####

my_data1$Pays <- NULL
attach(my_data1)
model1 <- lm(txvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100)
summary(model1)

#R2 (r-square) = variance expliquee / variance observee

#r-square = 0.6656 soit 66,56% de la variance observee du taux de vaccination est expliquee par le model
#c'est a dire par des differences liee a l'esperance de vie
#par des diffreence de l'etendu de la politique de couverture sante, par les depenses de sante allouees à chaque habitant
#de la capacite en terme d'effectif du personnel infirmier pour 100 habitant et des depenses de sante en proportion du PIB.

# F-test
#H0 tous les coefficients sont nuls
#H1 au moin un coef est non nul

#p-value = 0,003118 < 0,05  => on prends 0 risque de se tromper en rejettant H0
#On accept H1 alors le modele est globalement explicatif. 

# effet de l'esperance de vie

#1- hypothese
# H0 : la variable 
# H1 : le modele est globalement significatif

#2- Significativite

#p-value = 0,009 < 0,05, On prends 0 risque à rejeter l'hyposthèse nul et on accept H1 alors l'esperance de vie
# explique le taux de vaccination

#3- sens de l'effet

#le sens de l'effet est positif

#4- ampleur de l'effet

# l'esperance de vie a un effet non nul sur le taux de vaccination. On dira que pour une annee de vie supplementaire
#on a une hause de taux de vaccination de 7,17% en moyenne et toutes choses egale par ailleurs.


#maladie chroniques  :

#En moyenne et toutes choses égales par ailleurs, les pays qui enregistrent 1 maladie chronique  ont un taux de vaccination qui diminue de 0,78%.



#### 3.2 LE MODELE EST IL BIEN SPECIFIEE  ####

#methode 1 (TEST DE RAMSEY)

#Hypothese 

#H0 : le modele est bien specifie
#H1 : mauvaise forme fonctionnele et/ou omission de x

#Procedure de test : 

my_data1$txvaccp <- model1$fitted.values

attach(my_data1)

model2 <- lm(txvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100 + I(txvaccp^2))

summary(model2)

#p-value (txvaccp)  = 0,276 > 0,05 
#On accepte H0

#Le modèle est bien spécifié.  



#Test complementaire

#test de forme fonctionnelle

#estimation du modele log-lineaire

#estimation du modele log-lineaire

model3 <- lm(txvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100)

summary(model3)

# diff = log(salaire predit avec le modele lineaire)-salaire predit avec le model log-lineaire

my_data1$diff <- log(model1$fitted.values) - model3$fitted.values

attach(my_data1)

model4 <- lm(txvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100 + diff)

summary(model4)




#### 3.3 MULTICOLINERAITE ####


## les variables explicatives sont-elles colineaies ? (TEST DE KLEIN)

## => problEme de muticolinearite

summary(model3)


my_datacor1 <- my_data1[c("espdevie65", "maladiechronique65","depsanteparhab", "depsantepropBIP", "persnelinfipour100")]

cor(my_datacor1)



# R2= 0,59

# le coeficient entre depsanteparhab et espdevie65 = 0,68 > R2 nous permets de soupçonner une colinearite entre ces deux variables.




#### 3.3.1 VERIFIONS LA COLINEARITE ####


my_data1$lntxvacc <- log(my_data1$txvacc, base = exp(1))

attach(my_data1)

model5 <- lm(lntxvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100 + I(depsanteparhab * espdevie65))

summary(model5)

#*******************************

#### 3.4 ENDOGENEITE ####

#******************************



#Testons l'endogénéité de la variable espérance de vie (espdevie65 )

model6 <- lm(espdevie65 ~ etatdesanteetlimitationauto)

summary(model6)

#p-value = 0,011 < 0,05 on rejette H0 le modèle est  significatife. 


my_data1$h <- espdevie65 - model6$fitted.values

attach(my_data1)

model7 <- lm(lntxvacc ~ espdevie65 + maladiechronique65 + depsanteparhab + depsantepropBIP + persnelinfipour100 + I(depsanteparhab * espdevie65) + h, data = my_data1)

summary(model7)


##### TEST DE SARGAN #####


sum(model5$residuals^2) #SCR_0 = 2.183802
sum(model6$residuals^2) #SCR_1 = 43.12745
(sum(model5$residuals^2) * sum(model6$residuals^2))/21 # statistique de test = 4.4848

# k - p = 10 - 1 = 9 et 0.05 -> on regarde la table chi2

# valeur thÃ©orique = 3.33

# S  >  valeur thÃ©orique = 3.33

#S > Khi² => on rejette HO, on conclure que l'instrument n'est pas valide. 


# On rejette H0 et l'instrument n'est pas valide
>>>>>>> 6a3a836 (modification HTML)
