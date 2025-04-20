# EXERCICE 10 (PMA - M1)

# (a)
Z1=rnorm(100,0,1) ; Z2=rnorm(100,0,1)
#plot(Z1,Z2,pch=16)
plot(Z1,Z2,xlim=c(-4,4),ylim=c(-4,4),pch=16,col='blue')


# (b)
Sigma=diag(c(2,7)) ; Sigma
Sigma_demi=sqrt(Sigma) ; Sigma_demi 
# Attention, c'est la racine carrée "entrée par entrée"
Sigma_demi %*% Sigma_demi 

X100=matrix(,ncol=2,nrow=100)
for (i in 1:100){
  X=Sigma_demi %*% rnorm(2,0,1)
  X100[i,1]=X[1,1]  
  X100[i,2]=X[2,1]
}
# plot(X100,pch=16)
plot(X100,xlim=c(-10,10),ylim=c(-10,10),pch=16, col='blue')
var(X100)  # matrice de covariance empirique


# (c) 
Sigma=matrix(c(1,-2,-2,5), nrow=2) ; Sigma

# Sigma^(1/2) avec décomposition spectrale (on pourrait aussi utiliser Choleski)
O=eigen(Sigma)$vector ; O
D=diag(eigen(Sigma)$val) ; D
Sigma_demi=O %*% sqrt(D) %*% t(O)
Sigma_demi%*%Sigma_demi # check

X100=matrix(,ncol=2,nrow=100)
for (i in 1:100){
  X=Sigma_demi %*% rnorm(2,0,1)
  X100[i,1]=X[1,1]  
  X100[i,2]=X[2,1]
}
# plot(X100,pch=16)
plot(X100,xlim=c(-7,7),ylim=c(-7,7),pch=16,col='blue')
var(X100)

abline(b=O[2,1]/O[1,1],a=0) # droite engendrée par le premier vecteur propre
abline(b=O[2,2]/O[1,2],a=0) # droite engendrée par le deuxième vecteur propre

solve(Sigma) # Matrice Sigma^{-1} pour calculer la densité


# (d)
Sigma=matrix(c(1,2,2,4),nrow=2) ; Sigma

# Sigma^1/2 : décomposition spectrale 
O=eigen(Sigma)$vector ; O
D=diag(eigen(Sigma)$val) ; D
Sigma_demi=O %*% sqrt(D) %*% t(O)
Sigma_demi%*%Sigma_demi # check

X100=matrix(,ncol=2,nrow=100)
for (i in 1:100){
  X=Sigma_demi %*% rnorm(2,0,1)
  X100[i,1]=X[1,1]  
  X100[i,2]=X[2,1]
}
# plot(X100,pch=16)
plot(X100,xlim=c(-7,7),ylim=c(-7,7),pch=16,col='blue')

abline(b=O[2,1]/O[1,1],a=0) # droite engendrée par le premier vecteur propre
abline(b=O[2,2]/O[1,2],a=0) # droite engendrée par le deuxième vecteur propre
