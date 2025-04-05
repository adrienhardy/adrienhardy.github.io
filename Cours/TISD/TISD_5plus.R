setwd("/Users/adrien/R/Data")

# Exercice 1

Eu <- EuStockMarkets
Eu
help(EuStockMarkets)
names(Eu)
Eu

DAX <- Eu[,1] 
SMI <- Eu[,2]
CAC <- Eu[,3]
FTSE <- Eu[,4]

RLM <- lm(DAX ~ SMI + CAC + FTSE)
summary(RLM)
names(summary(RLM))

?predict.lm

x <- data.frame(SMI=1000, CAC=1500, FTSE=1200)
x
predict.lm(RLM, x, interval="prediction")


RLM23 <- lm(DAX ~ SMI + CAC)
summary(RLM23)$adj.r.squared
# names(summary(RLM23))
AIC(RLM23)

RLM24 <- lm(DAX ~ SMI+FTSE)
summary(RLM24)$adj.r.squared
AIC(RLM24)

RLM34 <- lm(DAX ~ CAC+FTSE)
summary(RLM34)$adj.r.squared
AIC(RLM34)

RLM2 <- lm(DAX ~ SMI)
summary(RLM2)$adj.r.squared
AIC(RLM2)

RLM3 <- lm(DAX ~ CAC)
summary(RLM3)$adj.r.squared
AIC(RLM3)

RLM4 <- lm(DAX ~ FTSE)
summary(RLM4)$adj.r.squared
AIC(RLM4)

# Le Dax semble déjà bien expliqué par le SMI uniquement

# Remarque : RLM avec toutes les variables
lm(DAX ~ ., data=Eu) 

# bruit...
res <- RLM$residuals
shapiro.test(res)
Box.test(res)
qqnorm(res)








