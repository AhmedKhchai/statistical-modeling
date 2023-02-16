library(rpart)
data(iris)
set.seed(10) # nombre à modifier par chaque utilisateur
my.iris=iris[sample(nrow(iris)),]
my.iris.app=my.iris[1:100,]
my.iris.test=my.iris[101:150,]

# Construction du modèle d'arbre de décision sur l'échantillon d'apprentissage
modele <- rpart(Species ~ ., data = my.iris.app)

# Prédiction des espèces dans l'échantillon de test
pred <- predict(modele, my.iris.test, type = "class")

# Calcul de l'accuracy
accuracy <- sum(pred == my.iris.test$Species) / nrow(my.iris.test)

# Affichage de l'accuracy
accuracy

#Le résultat obtenu est : 0.9
#Cela signifie que le modèle d'arbre de décision a prédit correctement l'espèce 
#de 45 des 50 fleurs dans l'échantillon de test, ce qui correspond à une 
#précision de 90%.