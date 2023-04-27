# Charger les données depuis le fichier "ozone.txt"
ozone=read.table("ozone.txt",header=T)

# Sélection des colonnes d'intérêt, c'est-à-dire les colonnes 1 à 11.
ozone.m=ozone[, 1:11]

# Création d'un sous-ensemble d'apprentissage avec les 86 premières observations
ozone_app = ozone.m[1:86, ]

# Création d'un sous-ensemble de test avec les 26 dernières observations
ozone_test = ozone.m[87:112, ]

# Construction du modèle de régression linéaire multiple en utilisant toutes les variables du sous-ensemble d'apprentissage
reg_multi = lm(maxO3 ~ ., data = ozone_app)

# Affichage d'un résumé statistique du modèle. Cela inclut l'erreur standard, les valeurs t, les p-valeurs, le R-carré, etc.
summary(reg_multi)

# Prédiction de la concentration maximale d'ozone en utilisant le modèle de régression multiple et les données de test
predictions <- predict(reg_multi, ozone_test)

# Calcul de l'erreur absolue en soustrayant la vraie concentration maximale d'ozone des prédictions
erreur_absolue <- abs(predictions - ozone_test$maxO3)

# Calcul de l'erreur absolue moyenne (MAE), qui est la moyenne des erreurs absolues
MAE <- mean(erreur_absolue)
print(MAE)

# Construction d'un modèle de régression linéaire simplifié en utilisant uniquement maxO3v et Ne9 comme variables explicatives
reg_multi_simplified <- lm(maxO3 ~ maxO3v + Ne9, data = ozone_app)

# Affichage d'un résumé statistique du modèle simplifié
summary(reg_multi_simplified)

# Prédiction de la concentration maximale d'ozone en utilisant le modèle de régression simplifié et les données de test
predictions_simplified <- predict(reg_multi_simplified, ozone_test)

# Calcul de l'erreur absolue en utilisant le modèle simplifié
erreur_absolue_simplified <- abs(predictions_simplified - ozone_test$maxO3)

# Calcul de l'erreur absolue moyenne (MAE) en utilisant le modèle simplifié
MAE_simplified <- mean(erreur_absolue_simplified)
print(MAE_simplified)

# Installation et chargement du package "leaps" pour la sélection de variables
install.packages("leaps")
library(leaps)

# Utilisation de la fonction regsubsets pour effectuer une sélection descendante des variables explicatives
choix <- regsubsets(maxO3 ~ ., data = ozone_app, nbest = 1, nvmax = 11, method = "backward")

# Affichage d'un graphique montrant le critère BIC pour chaque nombre de variables
plot(choix, scale = "bic")

# Identification des variables sélectionnées par regsubsets
vars_selected <- summary(choix)$which[which.min(summary(choix)$bic), ]

# Suppression de l'intercept de la liste des variables sélectionnées
vars_selected <- vars_selected[!names(vars_selected) %in% "(Intercept)"]

# Construction d'un nouveau modèle de régression multiple en utilisant uniquement les variables sélectionnées
formula <- as.formula(paste("maxO3 ~ ", paste(names(vars_selected)[vars_selected], collapse = " + ")))
reg_multi_backward <- lm(formula, data = ozone_app)

# Ce modèle est construit avec les variables sélectionnées par la méthode de sélection descendante.

# Affichage d'un résumé statistique du nouveau modèle de régression. Cela donne des informations sur la significativité des variables explicatives et le pourcentage de variabilité expliquée par le modèle.
summary(reg_multi_backward)

# Prédiction de la concentration maximale d'ozone en utilisant le nouveau modèle de régression et les données de test
predictions_backward <- predict(reg_multi_backward, ozone_test)

# Calcul de l'erreur absolue en utilisant le nouveau modèle
erreur_absolue_backward <- abs(predictions_backward - ozone_test$maxO3)

# Calcul de l'erreur absolue moyenne (MAE) en utilisant le nouveau modèle
MAE_backward <- mean(erreur_absolue_backward)
print(MAE_backward)
