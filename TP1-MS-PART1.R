# Charger les données depuis le fichier "ozone.txt"
ozone=read.table("ozone.txt",header=T)

# Créer un nuage de points de la concentration maximale d'ozone vs. la température à midi
plot(ozone$T12,ozone$maxO3)

# Calculer le coefficient de corrélation entre la température à midi et la concentration maximale d'ozone
cor_coefficient <- cor(ozone$T12, ozone$maxO3)
print(cor_coefficient)

# Construire un modèle de régression linéaire avec la concentration maximale d'ozone comme variable réponse et la température à midi comme prédicteur
reg<-lm(maxO3~T12,data=ozone)

# Afficher un résumé du modèle de régression
summary(reg)

# Ajouter la ligne de régression au nuage de points
abline(reg,col="red")

# Calculer les intervalles de confiance à 95% pour les coefficients de régression
confint(reg)

# Afficher les graphiques de diagnostic pour le modèle de régression
par(mfrow=c(2,2))
plot(reg)

# Diviser les données en un ensemble d'entraînement (les 86 premières observations) et un ensemble de test (les observations restantes)
ozone.app=ozone[1:86,]
ozone.test=ozone[87:112,]

# Construire un modèle de régression linéaire en utilisant les données d'entraînement
reg.app=lm(maxO3~T12,data=ozone.app)

# Afficher un résumé du modèle d'entraînement
summary(reg.app)

# Prédire la concentration maximale d'ozone pour les données de test
fit1=predict(reg.app,ozone.test)

# Calculer l'erreur absolue moyenne (MAE) des prédictions
mean(abs(fit1-ozone.test$maxO3))

# Afficher un résumé de la concentration maximale d'ozone dans les données de test
summary(ozone.test$maxO3)

# Construire un modèle de régression linéaire avec la concentration maximale d'ozone comme variable réponse et la concentration maximale d'ozone de la veille comme prédicteur
reg2=lm(maxO3~maxO3v,data=ozone.app)

# Afficher un résumé de ce modèle
summary(reg2)

# Prédire la concentration maximale d'ozone pour les données de test en utilisant le second modèle
fit2=predict(reg2,ozone.test)

# Calculer le MAE des prédictions du second modèle
mean(abs(fit2-ozone.test$maxO3))
