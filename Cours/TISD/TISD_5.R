setwd("/Users/adrien/R/Data")

# TP5 - Régression linéaire

################################
##  I - DONNEES IMMOBILIERES  ##
################################

Data <- read.table("donneesImmobilieres_euros.txt")
names(Data)

# Les variables :

X <- Data$surface
Y <- Data$prix

plot(X,Y,xlab='surface (m^2)',ylab="prix (euros par mois)")

# Calcul des estimateurs :

a <- cov(X,Y)/var(X) ; a
b <- mean(Y)-a*mean(X) ; b
abline(b,a,col='blue')

# Avec la commande R
lm(Y~X)

RLS <- lm(Y~X)
RLS
names(RLS)
plot(RLS)
b <- RLS$coefficients[[1]]
a <- RLS$coefficients[[2]]
a ; b

# Calcul du coefficient R^2 :

Y_est <- a*X+b
R2 <- sum((Y_est-mean(Y))^2)/sum((Y-mean(Y))^2)
R2

# Variante : Avec la commande R

summary.lm(RLS)
summary.lm(RLS)$r.squared

# Test de significativité de Fisher

Xi <- (length(X)-2)*R2/(1-R2)
Xi
qf(0.95,1,32)

# Variante :

summary.lm(RLS)$fstatistic
qf(0.95,1,32)

# Conclusion : Comme Xi >> qf(0.95,1,32), l'hypothèse H0={a=0}
# est clairement à rejeter. Alternativement, on voit dans 
# summary.lm(RLS) que la p-value associée à ce test est 
# 1.245e-14 << 0.05.


# Etude des résidus :

Er <- RLS$residuals

mean(Er) ; var(Er)
hist(Er,breaks=10,freq=FALSE)
plot(Er)
# Test si les résidus sont indépendants :
Box.test(Er,lag=1)
Box.test(Er,lag=2)
Box.test(Er,lag=3)
Box.test(Er,lag=4)
Box.test(Er,lag=5)
Box.test(Er,lag=6)
Box.test(Er,lag=7)
Box.test(Er,lag=8)
# -> On ne peut rejeter t'hypothèse de résidus indépendants pour lag=1 à lag=8
shapiro.test(Er)
# les résidus passent le test de Shapiro-Wilks, avec une p-valeur de 0.2 
# -> il n'est pas déraisonnable de supposer les résidus gaussiens 

qqnorm(Er)

# Estimateur de la variance :
n <- length(X)
sigma2 <- sum((Y-a*X-b)^2)/(n-2)

# Comme la loi de Student est symétrique, on prend comme quantiles quand alpha=0,05 :
q <- qt(0.975,n-2) 

# IC pour le coefficient a théorique, à 95% :
c <- 1/sum((X-mean(X))^2) 
a-q*sqrt(c*sigma2) ; a+q*sqrt(c*sigma2)

# IC pour le coefficient b théorique, à 95% :
c2  <- mean(X^2)/sum((X-mean(X))^2)
b-q*sqrt(c2*sigma2) ; b+q*sqrt(c2*sigma2)

# Prédiction du prix y d'un appartement et IC à 95%
x <- 140
y <- a*x+b ; y
c3 <- 1+1/n+(x-mean(X))^2/sum((X-mean(X))^2)
a*x+b-q*sqrt(c3*sigma2) ; a*x+b+q*sqrt(c3*sigma2)

# Bonne affaire ?
x <- 35
y <- a*x+b ; y
c3 <- 1+1/n+(x-mean(X))^2/sum((X-mean(X))^2)
a*x+b-q*sqrt(c3*sigma2) ; a*x+b+q*sqrt(c3*sigma2)
