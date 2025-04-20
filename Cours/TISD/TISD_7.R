# TP 7 - Algorithme EM et applications

#############################
##  I - Données censurées  ##
#############################

# la fonction m0 :
m0 <- function(x,Time){
  n <- length(x) ; m <- 0
  for (j in 1:n){if (x[j]>=Time){m <- m+1}}
  return(m)}

# plus malin :
m0 <- function(x,Time){sum(x>=Time)}

# l'algorithme EM pour données censurées :
EMcensure <- function(x,k,Time,theta0){
  n <- length(x)
  theta <- 1:(k+1)
  theta[1] <- theta0 
  for (j in 1:(k+1)){
    theta[j+1] <- 1/(mean(x)+m0(x,Time)/(n*theta[j]))
  }
  return(theta)
}

# Test sur données artificielles :
rexp(200,0.1)
T1 <- 15

x <- pmin(rexp(200,0.1),T1)
theta0 <- rexp(1)
EM1 <- EMcensure(x,10,T1,theta0)
EM1
plot(EM1, ylim=c(0.05,0.15), pch=4)
abline(a=0.1,b=0,col="blue")

# En moyenne sur 100 jeux de données X aléatoires ?
theta0 <- rexp(1)
EMmoy <- EMcensure(pmin(rexp(200,0.1),T1),10,T1,theta0)
for (i in 1:100){EMmoy <- EMmoy + EMcensure(pmin(rexp(200,0.1),T1),10,T1,theta0)}
plot(EMmoy/100, ylim=c(0.05,0.15), pch=4)
abline(a=0.1,b=0,col="blue")


T2 <- 2
y <- pmin(rexp(200,0.1),T2)
EM2 <- EMcensure(y,30,T2,theta0)
EM2
plot(EM2, ylim=c(0.05,0.15), pch=4)
abline(a=0.1,b=0,col="blue")

# En moyenne sur 100 jeux de données X aléatoires ?
theta0 <- rexp(1)
EMmoy <- EMcensure(pmin(rexp(200,0.1),T2),50,T2,theta0)
for (i in 1:100){EMmoy <- EMmoy + EMcensure(pmin(rexp(200,0.1),T2),50,T2,theta0)}
plot(EMmoy/100, ylim=c(0.05,0.15), pch=4)
abline(a=0.1,b=0,col="blue")

# Influence de T :
Time <- seq(0.1,10,0.1)
Y <- vector()
for (j in 1:100){
  Y[j] <- EMcensure(pmin(rexp(200,0.1),Time[j]),15,Time[j],theta0)[15]
}
plot(Time,Y,type='l',ylim=c(0,0.2))
abline(a=0.1,b=0,col="blue")



###############################
##  II - Mélanges gaussiens  ##
###############################

set.seed(124)

MelangeGauss <- function(n,theta){
  x <- vector()
  for (i in 1:n){
    z <-rbinom(1,1,theta[1])+1
    if (z==1) {x[i] <- rnorm(1, mean=theta[2])} 
    else {x[i] <- rnorm(1, mean=theta[3])}
  }
  return(x)}

Mix17 <- MelangeGauss(1000,c(0.25,1,7))
hist(Mix17,breaks=40,freq=FALSE)
u <- seq(-10,10,0.1)
lines(u,0.25*dnorm(u,1,1)+0.75*dnorm(u,7,1),col='red')


Mix12 <- MelangeGauss(1000,c(0.25,1,2))
hist(Mix12,breaks=40,freq = FALSE)
lines(u,0.25*dnorm(u,1,1)+0.75*dnorm(u,2,1),col='red')


# Retrouver les paramètres avec l'algorithme EM

# on construit la matrice H_ij(theta) :

H <- function(x,theta){
  n <- length(x)
  H <- array(0,dim=c(n,2))
  
  for (i in 1:n){
    Somme = (theta[1]*dnorm(x[i],theta[2],1)+(1-theta[1])*dnorm(x[i],theta[3],1))
    
    H[i,1] <- theta[1]*dnorm(x[i],theta[2],1)/Somme
    
    H[i,2] <- (1-theta[1])*dnorm(x[i],theta[3],1)/Somme
  } 
  return(H)
}

# on construit la suite theta_k obtenue par l'algorithme EM :
EMgauss <- function(x,k,theta0){
  theta <- array(0,dim=c(k+1,3))
  theta[1,] <- theta0
  for (u in 2:(k+1)){
    theta[u,1] <- mean(H(x,theta[u-1,])[,1])
    theta[u,2] <- sum( x*(H(x,theta[u-1,])[,1]) )/sum( H(x,theta[u-1,])[,1] )
    theta[u,3] <- sum( x*(H(x,theta[u-1,])[,2]) )/sum( H(x,theta[u-1,])[,2] )
  }
  return(theta)
}

# Conditions initiales aléatoires :
theta0 <- c(runif(1),rnorm(2))
EMgauss(Mix17,15,theta0)[16,]

# Conditions initiales aléatoires :
init <- c(runif(1),rnorm(2))
EMgauss(Mix12,15,init)[16,]

# Retrouver qui étaient les z_i ?
theta_est <- EMgauss(Mix17,15,init)[16,]
theta_est
H_est <- H(Mix17,theta_est) 
z_est <- c() ; 
for (i in 1:1000){z_est[i] <- which.max(H_est[i,])} # commande which.max !
z_est



# BONUS : Il existe des commandes R [ça utilise le package "mixtools"]
library(mixtools)
EM_Mix17 <- normalmixEM(Mix17)
names(EM_Mix17)
EM_Mix17$lambda # donne les estimations de p et 1-p 
EM_Mix17$mu # donne les estimations mu1 et mu2

EM_Mix12 <- normalmixEM(Mix12)
EM_Mix12$lambda 
EM_Mix12$mu

