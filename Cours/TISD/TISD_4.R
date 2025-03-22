setwd("/Users/adrien/R/Data")

# TP4 - Quelques test statistiques

#######################
## Test de Student   ##
#######################


# Student distributions
t <- seq(-10,10,0.1)
Col <- rainbow(5)
plot(t,dnorm(t),type='l',xlim=c(-3,3),ylim=c(0,0.5),ylab="",xlab="black = N(0,1), color = t_i with i=1:5")
for (i in 1:5){
  lines(t,dt(t,i),col=Col[i],type='l')
}

# Student test
help(t.test)
x <- rnorm(5,12,3)
t.test(x, mu=0)
t.test(x, mu=12)
t.test(x, mu=0, alternative = "greater")
t.test(x, mu=0, alternative = "less")

# EXO: Sepal iris data
help(iris)
names(iris)
iris
Sep <- iris$Sepal.Width ; Sep ; length(Sep)
t.test(Sep, mu=3, alternative = "less")


###############################
## II - Tests de Gaussianité ##
###############################


# On simule un échantillon de taille n (n=50 par exemple)
# de Y (avec X de loi exponentielle):
Y <- function(n){
  
  Y <- vector()
  for (i in 1:n){
  Y[i] <- sqrt(20)*(mean(rexp(20))-1)}
  
  return(Y)}

ech <- Y(50)
# Test de Kolmogorov-Smirnov :
ks.test(ech,"pnorm")

# Test de Lilliefors 
# après avoir installé la librarie : install.packages("nortest")
library(nortest) 
lillie.test(ech)

# Test de Shapiro-Wilk
shapiro.test(ech)

# On recommence dans le cas Bernoulli :
B <- function(n){
  B <- vector()
  for (i in 1:n){
    B[i] <- sqrt(20)*(mean(rbinom(20,1,0.01))-0.01)/sqrt(0.0099)}
 return(B)}

# Tests de normalité:
ks.test(B(50),"pnorm") 
lillie.test(B(50))
shapiro.test(B(50))

# Conclusion : On peut conclure que l'hypothèse de normalité est
# raisonnable dans le cas exp mais pas dans le cas Bernoulli. 
# Une explication théorique : l'inégalité de Berry-Esseen

# Les sépales (cf. EXO 1)
Sep
lillie.test(Sep)
shapiro.test(Sep) # On ne rejète pas H0

Sep2 <- iris$Sepal.Length
shapiro.test(Sep2) # On rejète H0


######################################
##  III - Test d'équilibre d'un dé  ##
######################################

Des <- read.table("des.txt")
Eff <- table(Des) ; Eff
sum(Eff)
E <- Eff/sum(Eff) ; E

# Statistique du Chi2 calculée à la main :
xi <- 100*sum((E-1/6)^2)/(1/6) ; xi
qchisq(0.95,df=5)
# Comme 17.24 > 11.071, on rejete H0 à 5% : le dé n'est pas équilibré

# Test d'adéquation du Chi^2
Eff
chisq.test(Eff)
# Comme p-value << 0.01, on a une forte évidence contre H0.

# ATTENTION : chisq.test calcule la statistique du Chi^2 quand
# on lui donne la table des effectifs Eff, et non pas la table
# des fréquences E !

# Estimtion de la moyenne m
Mn<-mean(Des[,1]) ; Mn
# L'inégalité de Hoeffding donne P(|Mn-m|>t)<2exp(-8t^2)
# donc P(m appartient à I)>1-alpha avec I=[Mn-C,Mn+C] où
# C=sqrt(log(2/alpha))/8. Pour alpha=0.05, C=0.6790508

# Conclusion : pas assez précis pour distiguer du dé équilibré !


#############################
## V - Syndicats aux U.S. ##
#############################

#1
Dat <- read.table("cps85.txt", header=TRUE)

#2
names(Dat)
F1<-table(Dat$NONWH)/sum(Dat$NONWH); F1; pie(F1)
F2<-table(Dat$HISP)/sum(Dat$HISP) ; F2; pie(F2)
F3<-table(Dat$FE)/sum(Dat$FE) ; F3; pie(F3)
F4<-table(Dat$UNION)/sum(Dat$UNION) ; F4; pie(F4)

par(mfrow=c(2,2)) ; pie(F1) ; pie(F2) ; pie(F3) ; pie(F4)

#3
Tab <- table(Dat$NONWH,Dat$UNION)
Tab

# Test d'indépendance du Chi^2 :
chisq.test(Tab)
qchisq(0.95,1)

# Conclusion : Comme p-value > alpha=0.05, on ne peux pas 
# rejeter H0 : Aucune preuve de corrélation entre NONWH 
# (le fait d'être blanc ou non) et UNION (le fait d'être syndiqué ou non). 


###############################
## VI - Passagers du Titanic ##
###############################

data(Titanic)
help("Titanic")
names(Titanic)
is.array(Titanic)
dim(Titanic)
n <- sum(Titanic)
n

# On lit que les variables sont : 
# Class=1, Sex=2, Age=3, Survived=4

# fréquences marginales à la main :
freq1 <- array(data=c(0),dim=c(4,1))
for(i in 1:4){
  freq1[i]=sum(Titanic[i,,,])/n
}

# fréquences marginales avec 'apply' : mieux !
freq1 <- apply(Titanic, 1, sum)/n
freq1

# Diagrammes en secteurs des variables :
pie(freq1, main="Class")
pie(apply(Titanic,2,sum), main="Sex")
pie(apply(Titanic,3,sum), main="Age")
pie(apply(Titanic,4,sum), main="Survived")

# Table de contingence des variables Survived et Class
Tab14 <- apply(Titanic,c(1,4),sum) ; Tab14

# Test d'indépendance du Chi^2 pour Survived et Class
chisq.test(Tab14)
qchisq(0.95,3)

# Conclusion : On peut clairement rejeter H0, dans la mesure 
# où la p-value est extrêmement petite 

# Test d'indépendance du Chi^2 pour Survived et Sex
Tab24 <- apply(Titanic,c(2,4),sum) ; Tab24
chisq.test(Tab24)
qchisq(0.95,1)

# Conclusion : La même que pour Class

# Test d'indépendance du Chi^2 pour Survived et Age
Tab34 <- apply(Titanic,c(3,4),sum) ; Tab34

names(chisq.test(Tab34))
qchisq(0.95,1)
# Conclusion : La même que pour Class

names(chisq.test(Tab34))

# Coefficients de Cramer : C'est une version normalisée de la statistique de test,
# moins dépendante de la taille n des données et des paramètres r et s des variables.
# Il permet une comparaison plus objective de la dépendance entre différentes variables.


Cramer14 <- sqrt(chisq.test(Tab14)$statistic /(n*min(4-1,2-1)))
Cramer24 <- sqrt(chisq.test(Tab24)$statistic/(n*min(2-1,2-1)))
Cramer34 <- sqrt(chisq.test(Tab34)$statistic/(n*min(2-1,2-1)))


# NB: min(4-1,2-1)=min(2-1,2-1)=1... mais en général c'est utile
  
Cramer14 ; Cramer24 ; Cramer34

# Comme le coefficient de Cramer le plus élévé est Cramer24, 
# la variable Sex semble être celle qui est la plus liée à
# la variable Survived : les passagers ont été plus galants 
# que prévenants vis-à-vis des classes supérieures et des enfants
