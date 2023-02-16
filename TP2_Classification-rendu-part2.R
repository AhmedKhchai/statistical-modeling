ozone=read.csv("ozone.csv",header = T,sep=";")
# conversion des variables Pic et STATION en facteur (variable catégorielle)
ozone$Pic=as.factor(ozone$Pic)
ozone$STATION=as.factor(ozone$STATION)
# puis discrétisation de la variable STATION pour être utilisée par kmeans
ozone$STATION=as.integer(ozone$STATION)

# nombre d'observations dans le jeu de données
n <- nrow(ozone)

# indices des observations à inclure dans l'ensemble d'apprentissage
idx_train <- sample(1:n, round(0.7*n), replace = FALSE)

# ensemble d'apprentissage
apprentissage <- ozone[idx_train, ]

# ensemble de test
test <- ozone[-idx_train, ]

# Charger le package rpart
library(rpart)
arbre_ozone <- rpart(Pic ~ ., data = apprentissage)

# Prédire les valeurs de la variable réponse pour l'ensemble de test
predictions <- predict(arbre_ozone, newdata = test, type = "class")

# Matrice de confusion
table(predictions, test$Pic)

# Calcul des métriques de performance
precision <- sum(predictions == test$Pic) / length(predictions)
sensibilite <- sum(predictions[test$Pic == "Yes"] == "Yes") / sum(test$Pic == "Yes")
specificite <- sum(predictions[test$Pic == "No"] == "No") / sum(test$Pic == "No")

# Affichage des métriques de performance
cat("Précision: ", precision, "\n")
cat("Sensibilité: ", sensibilite, "\n")
cat("Spécificité: ", specificite, "\n")


# Appliquer la méthode k-means sur l'ensemble de test
kmeans_test <- kmeans(test[, 2], centers = 2)

table(kmeans_test$cluster, test$Pic)
