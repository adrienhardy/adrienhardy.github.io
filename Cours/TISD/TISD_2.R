#############
# TISD TP 2 #
#############

# EXERCICE 1

# Question 1

D <- function(n,p){ 
  
plot(1:n, cumsum(rbinom(n,1,p))/1:n, 
     type='l', col='blue', ylim=c(0,1))
  abline(p,0) #pour tracer une droite d'équation y=ax+b
}

D(1000,0.5)

# Question 2
N <- 1:1000
plot(N, cumsum(rcauchy(1000))/N, 
      type='l', col='blue', ylim=c(-20,20))

# loi de Cauchy ne satisfait pas les conditions
# de la loi des grands nombres : elle n'a pas d'espérance


# EXERCICE 2
?rexp

# Question 2
x <- seq(0,10,0.001)
plot(x, dexp(x,0.5), type='l', col="red")
lines(x,dexp(x,2), type='l', col="blue")

x <- seq(0,10,0.001)
plot(x,pexp(x,0.5), type='l', col="red", ylim=c(0,1))
lines(x,pexp(x,2), type='l', col="blue")

x <- seq(0,1,0.001)
plot(x,qexp(x,0.5), type='l', col="red", ylim=c(0,10))
lines(x,qexp(x,2), type='l', col="blue")

# Question 3

# Un fonction générale : 
Test <- function(n,s,t,lambda){
  x <- rexp(n, lambda)
  supS=0
  supT=0
  supST=0
  
  for(i in 1:n)
  {
    if(x[i]>s) supS <- supS+1
    if(x[i]>t) supT <- supT+1
    if(x[i]>s+t) supST <- supST+1
  }
  A <- array(,dim=c(1,2)) # on présente le résultat dans un tableau
  A[1,1] = supST/supS ; A[1,2] = supT/n
  return(A)
}

Test(n=100,s=0.4,t=1,lambda=2)  
Test(n=1000,s=0.4,t=1,lambda=2)
Test(n=10000,s=0.4,t=1,lambda=2)

# Pour faire un dessin, on isole les résultats et on vectorise: 
Test1 <- function(n,s,t,lambda){ Test(n,s,t,lambda)[1,1] } 
Test1 <- Vectorize(Test1)
Test2 <- function(n,s,t,lambda){ Test(n,s,t,lambda)[1,2] }
Test2 <- Vectorize(Test2)
# "Vectoriser" permet d'appliquer une fonction qu'on a créée à un vecteur composante par composante

X <- seq(10000,100000,1000)
plot(X,Test1(X,s=0.4,t=1,lambda=2)-Test2(X,s=0.4,t=1,lambda=2),
     type='l',ylim=c(-0.01,0.01), ylab ='Erreur')


# EXERCICE 3

# Question 1
x <- sort(rnorm(1000))
n <- length(x)
plot(x, 1:n/n, type="s", xlim=c(-2,2), ylim=c(0,1))
lines(x,pnorm(x,0,1), col="blue")

# Question 3
I <- seq(from=-4, to=4, by=0.01)

D <- function(n){
  Fn <- ecdf(rnorm(n))
  return(max(abs(Fn(I)-pnorm(I))))
}

D <- Vectorize(D)
N <- seq(from=100, to=20000, by=100)
plot(N,D(N),type='l')


#EXERCICE 4
y <- c(8,5,2,9,8,3,6,4,100) ; y2 <- c(8,5,2,9,8,3,6,4)
mean(y) ; mean(y2)
median(y) ; median(y2)


#EXERCICE 5
chickwts
help(chickwts) 
names(chickwts) 

meal <- chickwts$feed
wts <- chickwts$weight

# We split the data into different groups:
G1 <- wts[meal=='horsebean'] 
G2 <- wts[meal=='linseed']
G3 <- wts[meal=='soybean']
G4 <- wts[meal=='sunflower']
G5 <- wts[meal=='meatmeal']
G6 <- wts[meal=='casein'] 

# Finally the box plots:

boxplot(G1,G2,G3,G4,G5,G6, 
        col = "lightgray",
        ylab = "Weight at six weeks (gm)")

# G4 ou G6 ?
mean(G4) ; mean(G6)
var(G4) ; var(G6)

# Taille des moustaches
Q1 <- qnorm(0.25)
Q3 <- qnorm(0.75)

M <- function(t){
  a <- Q1 - t*(Q3-Q1)
  b <- Q3 + t*(Q3-Q1)
  P <- pnorm(b)-pnorm(a)
  return(P)
}

M(1); M(1.5); M(2)


# EXERCICE 6
?rgamma

# Question 1

# Une exponentielle E(lambda)
# = Gamma(1,rate=lambda)
# = Gamma(1,scale=1/lambda)
# NB : 'dgamma' utilise rate par défaut, et non scale 

# Question 2
x <- seq(0,5,0.001)
col <- rainbow(5)
plot(x,dgamma(x,0.5,1),type='l', col=col[1], ylim=c(0,2))
lines(x,dgamma(x,1,1),type='l', col=col[2])
lines(x,dgamma(x,2,1),type='l', col=col[3])
lines(x,dgamma(x,2,2),type='l', col=col[4])
lines(x,dgamma(x,2,3),type='l', col=col[5])

# Question 3
X <- rexp(1000,2) + rexp(1000,2) + rexp(1000,2) 

hist(X,breaks=50,freq = FALSE)
lines(x,dgamma(x,3,2),type='l', col="red")
lines(x,dgamma(x,1,2),type='l', col="orange")
lines(x,dgamma(x,10,2),type='l', col="green")

qqplot(X,rgamma(1000,3,rate=2)) # pas très précis...

library(car)
?qqPlot

qqPlot(X,distribution="gamma", 3, rate=2) # Mieux !
qqPlot(X,distribution="gamma", 1, rate=2)
qqPlot(X,distribution="gamma", 10, rate=2)


# Alternative :
# library(lattice)
# f <- function(p){qgamma(p,3,scale=1/2)}
# qqmath(X,distribution=f)


# EXERCICE 7

# NB : téléchargez le jeu de données 
# dans votre "working directory"

Vote <- read.table("Intention_Vote.txt")
names(Vote)
n <- length(Vote$data) ; n
Xbar <- mean(Vote$data)
Xbar # prédiction

# précision d'approximation à 5% :
1/(2*sqrt(n*0.05))

# FIN _________
