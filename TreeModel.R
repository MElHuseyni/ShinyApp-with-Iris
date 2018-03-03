data("iris") 
set.seed(42) 
indexes <- sample(x=1:150,size = 100) 
print(indexes)

train <- iris[indexes,]
test <- iris[-indexes,]

library(tree)
model <- tree(formula = Species ~ .,data = train)
summary(model)
#Visualize the model 
plot(model)
text(model)

library(RColorBrewer)

palette <- brewer.pal(3,"Set2")

#Scatter Plot 
plot(x = iris$Petal.Length,y=iris$Petal.Width,pch=19,col=palette[as.numeric(iris$Species)],
     main = "Iris Petal Length vs. Width",xlab = "Petal Width (cm)",ylab = "Petal Width(cm)")

#Now we plot the Decision Tree boundaries 

partition.tree(tree = model,label = "Species",add = TRUE)

predictions <- predict(object = model,newdata = test,type = "class")

#Confusion Matrix 
table(x=predictions,y=test$Species)

library(ggplot2)
library(lattice)
library(caret)

confusionMatrix(data = predictions,reference = test$Species)

save(model,file = "Tree.RData")


