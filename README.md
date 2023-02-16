# statistical-modeling-1


University grades
The file "GPA.csv" contains high school and university grades for graduates
in computer science. Our objective is to determine the model that linearly explains
a student's university grade by his or her high school grade.
1. Reading the data
GPA=read.csv("GPA.csv",header = T,dec=",",sep=";")
2. Give a statistical summary of the variables studied.
3. Show the boxplots of two variables on the same graph. Comment on this.
4. Construct a training subset with the first 75 observations, and a test subset with the remaining 30.
with the remaining 30 observations.
5. Considering the training data:
(a) Plot the scatterplot of the data. Comment.
(b) Is there a strong correlation between the two variables studied?
(c) Determine the regression line and superimpose it on the previous scatter plot.
(d) Is this regression significant?
(e) What percentage of the variability is explained by the model?
(f) Check the assumptions about the residuals.
6. Use the regression model to predict the academic grades of the test data.
7. Calculate the mean absolute error of these predictions.
