# TP6 - Séries temporelles

######################
##  I - Simulation  ##
######################

n <- 500
t <- seq(1,n,1)
eps <- rnorm(n,0,1)

# 1.
y1 <- eps 
plot(y1,type='l',ylim=c(-6,6))
lines(t*0, col='blue', lwd=2)

# 2.
y2 <- 2*sin(0.1*t/pi)+eps
plot(y2,type='l',ylim=c(-6,6))
lines(2*sin(0.1*t/pi),col='blue',lwd=2)

# 3.
y3 <- sqrt(t/10)-4+sin(0.1*t/pi)+eps
plot(y3,type='l',ylim=c(-6,6))
lines(sqrt(t/10)-4+2*sin(0.1*t/pi), type='l', col='blue', lwd=2)

# 4.
y4 <- t*sin(0.1*t/pi)*eps/200
plot(y4,type='l',ylim=c(-6,6))
lines(t*sin(0.1*t/pi)/200, type='l', col='blue', lwd=2)

# La saisonnalité :
# Méthode 1 (bof bof)
season <- function(k){
  if (k%%60 %in% 1:20){s <- 0}  # notez qu'on utilise %in% et pas in comme dans les boucles for  
  if (k%%60 %in% 21:30){s <- 3}
  if (k%%60 %in% 31:50){s <- -4}
  if (k%%60 %in% 51:60){s <- 2}
  return(s)}  
s <- vector() ; for(k in 1:500){s[k]<-season(k)} # Si vous essayez, vous verrez que la commande Vectorize ne marche pas directement ici
plot(s,type='l')

# Méthode 2 (plus simple et plus propre)
s0 <- vector()
for (j in 1:20){s0[j]<- 0}
for (j in 21:30){s0[j]<- 3}
for (j in 31:50){s0[j]<- -4}
for (j in 51:60){s0[j]<- 2}
s0
s <- array(data=s0,dim=c(n,1))

# 5. 
y5 <- s+eps
plot(y5,type='l',ylim=c(-6,6))
lines(s,type='l', col='blue', lwd=2)

# 6.
y6 <- s*eps/2
plot(y6,type='l',ylim=c(-6,6))
lines(s/2,type='l', col='blue', lwd=2)

# processus stationnaires
ARMA11 <- function(phi,theta){
  Z <- rnorm(2*n,0,1)
  X <- vector(,2*n) 
  X[1] <- Z[1]
  for (t in 2:(2*n)){
    X[t] <- phi*X[t-1]+ Z[t] + theta*Z[t-1]
  }
  return(X[n:(2*n)]) # on ne garde que les X[t] pour t entre n et 2n 
}

plotARMA11 <- function(phi,theta){plot(ARMA11(phi,theta),type='l')}
acfARMA11 <- function(phi,theta){acf(ARMA11(phi,theta))}

# AR(1)
plotARMA11(0.01,0)  
acfARMA11(0.01,0) # presque indépendant

plotARMA11(0.4,0) 
acfARMA11(0.4,0) # faible corrélation positive

plotARMA11(0.9,0) 
acfARMA11(0.9,0) # forte corrélation positive

plotARMA11(-0.9,0) 
acfARMA11(-0.9,0) # forte corrélation négative (resp. positive) entre voinsins à distance impaire (resp. paire).

# MA(1)
plotARMA11(0,0.1) 
acfARMA11(0,0.1)# presque indépendant

plotARMA11(0,0.9) 
acfARMA11(0,0.9) # forte corrélation positive entre X_t et X_{t+1} puis indépendance

plotARMA11(0,-0.9) 
acfARMA11(0,-0.9) # forte corrélation négative entre X_t et X_{t+1} puis indépendance

# ARMA(1,1) : on observe que le profil des correlations entre voisins est plus variables que dans AR et MA

plotARMA11(0.9,0.7)
acfARMA11(0.9,0.7)

plotARMA11(0.9,-0.7)
acfARMA11(0.9,-0.7)

plotARMA11(-0.8,0.2)
acfARMA11(-0.8,0.2)

# Avec la commande arima.sim
plot(arima.sim(model=list(ar=0.5),n=500)) # AR(1) de paramètre 0.5
plot(arima.sim(model=list(ma=0.2),n=500)) # MA(1) de paramètre 0.2
plot(arima.sim(model=list(ar=0.5,ma=0.2),n=500)) # ARMA(1,1) de paramètres AR=0.5 et MA=0.2
plot(arima.sim(model=list(ar=c(-0.8,0,0.6),ma=c(0.2,0.6)),n=500)) # ARMA(3,2) de paramètres AR=-0.8,0,0.6 et MA=0.2,0.6

acf(arima.sim(model=list(ar=c(-0.8,0,0.6),ma=c(0.2,0.6)),n=500))


#################################
##  II - Concentration de CO2  ##
#################################

# On travaille sur la serie temporelle co2 de R
co2
data(co2)
help(co2)
is.ts(co2)
plot(co2)

Dec <- decompose(co2,type='additive')
plot(Dec)

# Tendance

Dec$trend
plot(co2)
lines(Dec$trend, col='blue', lwd=2)

length(co2)
Dec$trend
Dec$trend[6] ; Dec$trend[7]
Dec$trend[462] ; Dec$trend[463]
Y <- Dec$trend[7:462] # on enlève les valeurs NA
X <- 1:length(Y)
plot(X,Y,type='l')
RLS <- lm(Y~X)
a <- RLS$coefficient[2]
b <- RLS$coefficient[1]
a; b
plot(Y,type='l')
lines(a*X+b,col='blue')


# Alternative pour gérer les NA :
X = 1:length(Dec$trend)
lm(Dec$trend ~ X, na.action=na.omit)

# Saisonnalité

plot(Dec$seasonal)
Dec$seasonal[1:15] # on voit que T=12
sum(Dec$seasonal[1:12]) # la somme vaut (essentiellement) zéro

# Opérateurs de différence

plot(diff(co2, lag=1, differences=2)) # la tendance a disparu (pour s'en convaicre, comparer avec "difference=1")
plot(diff(co2, lag=12, differences=1)) # la saisonnalité a disparu (pour s'en convaicre, comparer avec "lag=11" ou 13)
plot(diff(co2, lag=12, differences=2)) # plus que du bruit

# Bruit résiduel

Bruit <- Dec$random
Bruit

mean(Bruit,na.rm=TRUE) # on supprime les NA de la moyenne 
sd(Bruit,na.rm=TRUE) # on supprime les NA de l'écart-type
plot(Bruit)
hist(Dec$random,freq=FALSE)
qqnorm(Bruit)
shapiro.test(Bruit)

# Ljung-Box test
Box.test(Bruit,50,type='Ljung-Box') # on choisit H=50

# la p-valeur étant très petite, on peut clairement rejeter
# l'hypothèse H0 que le bruit résiduel est un bruit blanc  
# (au moins du point de vue des 50 premières fonctions 
# d'auto-correlation)

# On pourrait aussi utiliser le test de Box-Pierce, et obtenir la même conclusion :
Box.test(Bruit,50)

# Fonction d'autocorrélation du bruit :
acf(Bruit[7:462],100) # on supprime manuellement les valeurs NA
acf(Bruit,100,na.action=na.pass) # alternative
acf(Bruit,1000,na.action=na.pass) 
