GPA = read.csv("GPA.csv", header = T, dec = ",", sep = ";")

summary(GPA)

boxplot(GPA$high_GPA, GPA$univ_GPA, names = c("Note secondaire", "Note universitaire"))

GPA_app = GPA[1:75, ]
GPA_test = GPA[76:105, ]

plot(GPA_app$high_GPA, GPA_app$univ_GPA)

cor_coefficient <- cor(GPA_app$high_GPA, GPA_app$univ_GPA)
print(cor_coefficient)

reg <- lm(univ_GPA ~ high_GPA, data = GPA_app)

summary(reg)

abline(reg, col = "red")

summary(reg)

summary(reg)$r.squared

predictions <- predict(reg, GPA_test)

MAE <- mean(abs(predictions - GPA_test$univ_GPA))
print(MAE)
