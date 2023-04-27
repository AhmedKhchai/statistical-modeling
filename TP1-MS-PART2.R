# Lire le fichier CSV et stocker les données dans une variable nommée GPA
GPA = read.csv("GPA.csv", header = T, dec = ",", sep = ";")

# Afficher un résumé statistique des données
summary(GPA)

# Créer un graphique de type boîtes à moustache pour les notes secondaires et universitaires
boxplot(GPA$high_GPA, GPA$univ_GPA, names = c("Note secondaire", "Note universitaire"))

# Diviser les données en un ensemble d'apprentissage et un ensemble de test
GPA_app = GPA[1:75, ]
GPA_test = GPA[76:105, ]

# Tracer le nuage de points des données d'apprentissage
plot(GPA_app$high_GPA, GPA_app$univ_GPA)

# Calculer le coefficient de corrélation entre les notes secondaires et universitaires
cor_coefficient <- cor(GPA_app$high_GPA, GPA_app$univ_GPA)
print(cor_coefficient)

# Construire le modèle de régression linéaire
reg <- lm(univ_GPA ~ high_GPA, data = GPA_app)

# Afficher un résumé du modèle de régression
summary(reg)

# Ajouter la droite de régression au nuage de points
abline(reg, col = "red")

# Afficher le résumé du modèle pour vérifier la significativité
summary(reg)

# Calculer le pourcentage de variabilité expliquée par le modèle
summary(reg)$r.squared

# Utiliser le modèle de régression pour prédire les notes universitaires des données de test
predictions <- predict(reg, GPA_test)

# Calculer l'erreur absolue moyenne des prédictions
MAE <- mean(abs(predictions - GPA_test$univ_GPA))
print(MAE)
