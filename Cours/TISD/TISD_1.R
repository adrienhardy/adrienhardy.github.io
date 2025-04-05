#################
##  TISD TP 1  ##
#################

getwd()
# setwd("/Users/adrien/R/Data")

# Dans R Studio on peut enregistrer ce choix 
# en allant dans Tools/Global Options/

# 1 - Structure de données
# 1.1 - Vecteurs

#1.1.1
a <- c(10,5,3,6,21)
a ; a[2] ; a[c(1,3)]

#1.1.2
b <- seq(from=1, to=10, by=2) ; b
2:7
is.vector(a) ; is.vector(b)

#1.1.3
2*a+b+1 ; cos(a) ; exp(a) #somme vectorielle, vecteur image par cos et exp
sum(a); cumsum(a); mean(a) #somme vectorielle, sommes cumulées, moyenne

# 1.2 Matrices

#1.2.1
diag(c(1,2,3))

#1.2.2
M <- matrix(c(1:6), nrow=3) ; M
matrix(c(1:6), ncol=3)
matrix(c(1:6), nrow=3, byrow=TRUE)
N <- matrix(c(1,11,0,5,0,2), nrow=2, byrow=TRUE); N
N[2,3]

#1.2.3
N%*%M
U <- diag(c(1,2)) ; V <- matrix(1:4,nrow=2)
U; V; U*V ; U%*%V;

#1.2.4
A <- matrix(c(1,0,0,0,1,3,0,2,1), ncol =3)
b <- c(1, 2, 1)
det(A)
x <- solve(A,b) ; A%*%x
A_inv <- solve(A) #alernative : "matrix.inverse"
A%*%A_inv ; A_inv%*%A

#1.2.5
E <- eigen(A); E
names(E)
E$values
E$vectors
D <- diag(E$values)
P <- matrix(E$vectors, ncol = 3); P
P%*%D%*%solve(P) 

# 1.3 Tableaux

#1.3.1
x <- array(data=c(15,3,12,2,1), dim=c(1,5)); x
y <- c(15,3,12,2,1); y;
nrow(x) ; ncol(x) ; dim(x) 
nrow(y) ; ncol(y) ; dim(y) 

#1.3.2
x <- array(c(1,0,0,0,1,3,0,2,1), dim=c(3,3)); x
nrow(x) ; ncol(x) ; dim(x) 
nrow(A) ; ncol(A) ; dim(A) 
is.matrix(x)

#1.3.3
x <- array(1:8, dim=c(2,2,2)); x

# 1.4 - Listes et data frames

#1.4.1 
L <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(9,7,4))
L
L[[1]]
L$name
L[[4]]
L$child.age[3]

#1.4.2
?iris
iris
data(iris)
class(iris)
colnames(iris)
iris$Sepal.Length
write.table(iris, "Iris.txt")
x <- read.table("Iris.txt")
x

# 2 - Graphiques : la commande "plot"  

#2.1 
x <- seq(from = 0, to = 2*pi, by = 0.1)
plot(x,sin(x),type="l",col="blue", ylim = c(-1,1))

#2.2
lines(x,cos(x),col='red')


# 5 - Lois de probabilité

#5.1
E <- rnorm(100000)
hist(E)
hist(E, breaks=50)
hist(E, breaks=50, freq = FALSE)
mean(E) 
E2 <- rnorm(10000, mean=3, vd=5)
hist(E2, breaks=50)
mean(E2)

#5.2
dbinom(3,10,0.7)
pbinom(8,10,0.7) 
1-pbinom(3,10,0.7) 
# on a aussi l'alternative :
pbinom(3,10,0.7,lower.tail=FALSE) 

#5.3
hist(rexp(5000,2), breaks=100, freq = FALSE)
qexp(0.05,2) #cette valeur, c'est u : le 95-ème centile de X

#5.4
E1 <- rnorm(1000) ; E2 <- rnorm(1000)
plot(E2,E1,pch=3)
x <- seq(from = 0, to = 2*pi, by = 0.1) 
lines(cos(x),sin(x), col="blue")
lines(2*cos(x),2*sin(x), col="blue")

# Bonus :
# La densité du vecteur (E1,E2) est le produit 
# de deux gaussiennes (car indépendantes): 
# e^(-x^2/2)e^(-y^2/2)dxdy/2pi. En coordonnées 
# polaires, ça donne : re^(-r^2/2)dr dO/2pi, donc
# le produit de la loi uniforme sur [0,2*pi] et 
# d'une loi sur les rayons qu'on étudie maintenant:

Z <- (E1^2+E2^2)^{1/2} #rayons
hist(Z, breaks=40, freq=FALSE) #histogramme des rayons
u <- seq(0,4,0.1); v <- u*exp(-u^2/2) 
lines(u,v,col="blue")
# on superpose le résultat théorique à l'histogramme 


# 3 - Boucles

#3.1 Boucle "for"

for (i in 1:10) {print(i)}

#3.2 Boucle "if"

A <- matrix( ,8,8)
for (i in 1:8){
  for (j in 1:8){
    if (i==j) {A[i,j] <- 0} else {A[i,j]<- i+j}
  }
}
A

#3.3 Boucle "while"

seuil <- 1000; n <- 0; s <- 0
while (s<= seuil){n <- n+1; s <- s+n}
c(n,s)

# n est le plus petit entier naturel tel que 
# s=1+2+...+n est supérieur ou égal à 1000.
  
# 4 - Fonctions

S <- function(seuil){
  n <- 0; s <- 0
  while (s<= seuil){
    n <- n+1; s <- s+n
  }
  return(c(n,s))
}
S(987)
# n=S(987)-1=43

#4.1
F <- function(a, b) {
  R <- a+b 
  return(R)
}
F(12,24)

#4.2 
# Méthode 1
Fibo <- function(n){
 u <- vector()
 u[1] <- 0
 u[2] <- 1
 for (j in 3:n) {u[j] <- u[j-1]+u[j-2]}
 return(u[n])
}
# pour vérifier
for (j in 3:10) {print(Fibo(j))} 

# Méthode 2 (plus économique !)
Fibo2 <- function(n){
  u <- 0; v <- 1; w <- 1;
  for (i in 1:n-2){
    w <- u+v; u <- v; v <- w }
  return(w)
}
for (i in 1:8){print(Fibo2(i))}

### FIN

