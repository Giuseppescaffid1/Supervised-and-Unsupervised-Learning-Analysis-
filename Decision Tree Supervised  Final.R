#DECISION TREE 
install.packages("tree")
install.packages("rattle")
install.packages("ineq")
library(tree)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(ineq)


set.seed(0, sample.kind = "Rounding")

tree_hf <- rpart(DEATH_EVENT ~.,
                 data = hftrain,
                 method = "class")

fancyRpartPlot(tree_hf)

# Using desition tree for prediction
prediction_tree <- predict(tree_hf, hftest, type = "class")


# Save the solution to a dataframe with two columns: death event and prediction
solution <- data.frame(DEATH_EVENT = hftest$DEATH_EVENT, prediction = prediction_tree)
solution
solution <- solution %>% mutate(comparison = if_else(DEATH_EVENT == prediction, 1, 0))

accuracy <- round((sum(solution$comparison)/nrow(solution)*100),2)
accuracy


# setting a random seed
set.seed(456, sample.kind = "Rounding")
# Random Forest Model
rf_model <- randomForest(DEATH_EVENT ~., data = hftrain)
rf_model
# Show model error
plot(rf_model, ylim=c(0,0.5))



# Get importance
importance    <- rf_model$importance
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,],2))
# Create a rank variable based on importance
ranking <- varImportance %>% arrange(desc(Importance))
# Use ggplot2 to visualize the relative importance of variables
ggplot(ranking, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip()





