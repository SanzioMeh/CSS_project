library(tree)
library(randomForest)
library(gbm)
#Data loading and modifications
country_data <- read.csv("country_stats.csv")

country_data$pil_change_2017 <- (country_data$PIL2017- country_data$PIL2016) / country_data$PIL2016
country_data$pil_change_total <- (country_data$PIL2017- country_data$PIL2014) / country_data$PIL2014

country_data$difference <- country_data$perceived_proportion - country_data$actual_proportion

#Tree
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

basic_tree <- tree(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017
                   + poverty_risk_perc + percentage_educated + informed,
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

#Tree diff
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

diff_tree <- tree(difference ~ PIL2017 + pil_change_total + pil_change_2017
                   + poverty_risk_perc + percentage_educated + informed,
                   data=country_data, control = control)
summary(diff_tree)

plot(diff_tree)
text(diff_tree, pretty=0)
title(main="Unpruned regression diff_tree")

#We use cross validation to determine the best tree size and prune the tree accordingly
set.seed(1234)
cv_basic <- cv.tree(diff_tree)
plot(cv_basic$size, cv_basic$dev, type="b")

prune_tree <- prune.tree(diff_tree, best=5)
plot(prune_tree)
text(prune_tree, pretty=0)
title(main="Boston: Pruned regression tree")






#Random forest

set.seed(1234)
n_pred <- ncol(country_data) - 6
(rand_forest <- randomForest(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017 
                             + poverty_risk_perc + percentage_educated + informed,
                           data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(rand_forest)


#Random forest diff

set.seed(1234)
n_pred <- ncol(country_data) - 7
(diff_forest <- randomForest(difference ~ PIL2017 + pil_change_total + pil_change_2017 
                             + poverty_risk_perc + percentage_educated + informed,
                             data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(rand_forest)

#boosting
set.seed(1234)
boosted <- gbm(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017
               + poverty_risk_perc + percentage_educated + informed, data=country_data, distribution="gaussian",
               n.trees=1000, interaction.depth=3,  bag.fraction = 0.5, n.minobsinnode = 4)
knitr::kable(summary(boosted))

plot(boosted, i="percentage_educated")
plot(boosted, i="pil_change_2017")
