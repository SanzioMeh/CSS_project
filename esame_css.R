library(tree)
library(randomForest)
#Data loading and modifications
country_data <- read.csv("country_stats.csv")

country_data$pil_change_2017 <- (country_data$PIL2017- country_data$PIL2016) / country_data$PIL2016
country_data$pil_change_total <- (country_data$PIL2017- country_data$PIL2014) / country_data$PIL2014

#Tree
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

basic_tree <- tree(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017 + poverty_risk_perc,
                   data=country_data, control = control)
summary(basic_tree)

plot(basic_tree)
text(basic_tree, pretty=0)
title(main="Unpruned regression basic_tree")

#We use cross validation to determine the best tree size and prune the tree accordingly
set.seed(1234)
cv_basic <- cv.tree(basic_tree)
plot(cv_basic$size, cv_basic$dev, type="b")

prune_tree <- prune.tree(basic_tree, best=5)
plot(prune_tree)
text(prune_tree, pretty=0)
title(main="Boston: Pruned regression tree")

#Random forest

set.seed(1234)
n_pred <- ncol(country_data) - 5
(rand_forest <- randomForest(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017 + poverty_risk_perc,
                           data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(rand_forest)
varImpPlot(rand_forest)

(rand_forest <- randomForest(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_2017 + poverty_risk_perc,
                             data=country_data, importance=TRUE))
importance(rand_forest)
