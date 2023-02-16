GPA=read.csv("GPA.csv",header = T,dec=",",sep=";")

summary(GPA)

boxplot(GPA$high_GPA, GPA$univ_GPA, col = c("red", "blue"), main = "Notes Secondaires et Universitaires", xlab = "Notes", ylab = "Type de note")
#Commentaire:
  # Nous pouvons voir que la médiane des notes universitaires est légèrement plus 
#élevée que celle des notes en études secondaires. Les deux distributions ont 
#une certaine dispersion, mais les notes universitaires ont une plage plus 
#restreinte que les notes en études secondaires. De plus, il y a plus de notes 
#universitaires plus élevées que de notes en études secondaires plus élevées. 
#Cependant, nous pouvons également voir que les valeurs aberrantes sont plus fréquentes dans les notes universitaires que dans les notes en études secondaires.

sous_ensemble_apprentissage <- GPA[1:75,]
sous_ensemble_test <- GPA[76:105,]

plot(sous_ensemble_apprentissage$high_GPA, sous_ensemble_apprentissage$univ_GPA, 
     xlab = "Note secondaire", ylab = "Note universitaire", 
     main = "Nuage de points des notes secondaires et universitaires")
#Commentaire:
  #On peut voir que les données ont une certaine tendance linéaire, 
#ce qui indique une corrélation positive entre les notes secondaires et universitaires. Cependant, il y a également beaucoup de dispersion dans les données, 
#ce qui signifie que la corrélation n'est pas parfaitement linéaire et qu'il peut y avoir d'autres facteurs influençant la note universitaire.

cor(sous_ensemble_apprentissage$high_GPA, sous_ensemble_apprentissage$univ_GPA)
#Commentaire:
  #Le coefficient de corrélation linéaire est de 0.8021245, ce qui indique qu'il existe une corrélation 
#positive forte entre les notes secondaires et universitaires. 
#Autrement dit, plus une personne obtient une note élevée au lycée, plus elle a de chances d'obtenir une note élevée à l'université.

modele <- lm(univ_GPA ~ high_GPA, data = sous_ensemble_apprentissage)
summary(modele)

plot(high_GPA ~ univ_GPA, data = sous_ensemble_apprentissage, xlab = "Note secondaire", ylab = "Note universitaire", main = "Nuage de points et droite de régression")
abline(modele, col = "red")

cor(GPA$high_GPA[1:75], GPA$univ_GPA[1:75])
#Commentaire: On obtient un coefficient de corrélation de 0.833, ce qui indique une forte corrélation linéaire entre les deux variables.

modele <- lm(univ_GPA ~ high_GPA, data=GPA[1:75,])
summary(modele)
#Commentaire: Pour tester la significativité de la régression linéaire, on peut utiliser un test de Student sur la pente de la droite de régression.
#Le test de Student sur la pente de la droite de régression est indiqué dans la colonne "Pr(>|t|)" du tableau de résultats. 
#Dans notre cas, la valeur est très faible (2.2e-16), ce qui indique que la pente est significativement différente de zéro. 
#On peut donc conclure que la régression est significative.

#On peut voir que le coefficient de détermination est de 0.6434, 
#ce qui indique que le modèle explique 64.34% de la variabilité de la note universitaire en fonction de la note secondaire. 
#Autrement dit, la qualité de la prédiction du modèle est relativement bonne, 
#bien que certains points peuvent se trouver loin de la droite de régression.

# Récupérer les résidus
residuals <- resid(modele)

# Vérifier la constance de la variance
plot(modele$fitted.values, residuals, xlab = "Valeurs prédites", ylab = "Résidus", main = "Vérification de la constance de la variance")
abline(h = 0, lty = 2, col = "red")

# Vérifier la normalité des résidus
hist(residuals, main = "Histogramme des résidus")

# Vérifier l'indépendance des résidus
acf(residuals, main = "Autocorrélation des résidus")

#Les graphiques montrent que les résidus sont centrés en zéro et semblent avoir
#une variance constante, mais ne suivent pas une loi normale et présentent une 
#légère autocorrélation. Cela peut indiquer une violation de l'hypothèse 
#d'indépendance des résidus. Cependant, le faible nombre d'observations peut 
#rendre difficile l'interprétation de ces résultats. Il serait 
#donc préférable de réaliser une étude plus poussée si cela était nécessaire.

sous_ensemble_test$univ_GPA_pred <- predict(modele, sous_ensemble_test)
#La fonction predict() renvoie une liste de valeurs prédites pour la variable
#de réponse (dans notre cas, la note universitaire) basées 
#sur les valeurs de la variable explicative (dans notre cas, la note de lycée).
sous_ensemble_test

# Calcul de l'erreur absolue moyenne
MAE <- function(observed, predicted){
  mean(abs(predicted - observed))
}
MAE(sous_ensemble_test$univ_GPA, predict(modele, sous_ensemble_test))
