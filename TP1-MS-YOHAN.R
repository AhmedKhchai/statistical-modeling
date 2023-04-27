#### TP1#####################################
ozone=read.table("ozone.txt",header=T) 

plot(ozone$T12,ozone$maxO3)   
# on observe une tendance linéaire : les points sont concentrés autour d'une ligne droite
# ce qui justifie l'utilisation d'une régression linéaire

cor(ozone$T12,ozone$maxO3)   
# on obtient un coefficient de corrélation élevé (0.78 qui est proche de 1)
# donc les deux variables sont fortement corrélées

reg=lm(maxO3~T12,data=ozone)
summary(reg)
abline(reg,col="red")

confint(reg)

par(mfrow=c(2,2))
plot(reg)

dev.off()   # pour fermer la matrice graphique

residus=residuals(reg)
shapiro.test(residus)   

# division de données en apprentissage/test
ozone.app=ozone[1:86,]
ozone.test=ozone[87:112,]

# construction du modèle avec les données d'apprentissage
reg1=lm(maxO3~T12,data=ozone.app)
summary(reg1)
# le modèle est significatif, et il a un bon pouvoir explicatif (59%)

# prédiction sur les données de test
fit1=predict(reg1,ozone.test)

mean(abs(fit1-ozone.test$maxO3))  
# l'ecart absolu moyen est de 9.29 unités
# en moyenne, on a une erreur de prédiction de 9.29 unités pour chaque observation

summary(ozone.test$maxO3)

# sinon on peut les déterminer à l'aide de la librairie Metrics
install.packages("Metrics")
library(Metrics)
mae(fit1,ozone.test$maxO3)

reg2=lm(maxO3~maxO3v,data=ozone.app)
summary(reg2)
fit2=predict(reg2,ozone.test)
mean(abs(fit2-ozone.test$maxO3))
# ou aussi mae(fit2,ozone.test$maxO3)
# si on utilise maxO3v au lieu de T12, l'écart absolu moyen s'élève à 10.84
# donc le modèle précédent a un meilleur pouvoir prédictif
