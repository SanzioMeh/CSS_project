library(tree)
library(randomForest)
library(gbm)

compute_mse <- function(preds, truth) {
  mean((preds - truth)^2)
}
#Data loading and modifications
country_data <- read.csv("country_stats.csv")

country_data$pil_change_2017 <- (country_data$PIL2017- country_data$PIL2016) / country_data$PIL2016
country_data$pil_change_total <- (country_data$PIL2017- country_data$PIL2014) / country_data$PIL2014

country_data$difference <- country_data$perceived_proportion - country_data$actual_proportion

#Tree
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

basic_tree <- tree(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017
                   + poverty_risk_perc + percentage_educated + informed + friends_family + factor(religion),
                   data=country_data, control = control)
summary(basic_tree)

plot(basic_tree)
text(basic_tree, pretty=0)
title(main="Unpruned regression proportion tree")

#We use cross validation to determine the best tree size and prune the tree accordingly
set.seed(1)
cv_basic <- cv.tree(basic_tree)
plot(cv_basic$size, cv_basic$dev, type="b")

prune_tree <- prune.tree(basic_tree, best = 4)
plot(prune_tree)
text(prune_tree, pretty=0)
title(main="Pruned tree: immigrant proportion")

#Tree diff
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

diff_tree <- tree(difference ~ PIL2017 + pil_change_total + pil_change_2017
                   + poverty_risk_perc + percentage_educated + informed + friends_family + factor(religion),
                   data=country_data, control = control)
summary(diff_tree)

plot(diff_tree)
text(diff_tree, pretty=0)
title(main="Unpruned regression diff_tree")

#We use cross validation to determine the best tree size and prune the tree accordingly
set.seed(1234)
cv_basic <- cv.tree(diff_tree)
plot(cv_basic$size, cv_basic$dev, type="b")

prune_tree <- prune.tree(diff_tree, best=3)
plot(prune_tree)
text(prune_tree, pretty=0)
title(main="Pruned regression tree")






#Random forest

set.seed(1234)
n_pred <- ncol(country_data) - 6
(rand_forest <- randomForest(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017 
                             + poverty_risk_perc + percentage_educated + informed + friends_family + religion,
                           data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(rand_forest)


#Random forest diff

set.seed(1234)
n_pred <- ncol(country_data) - 7
(diff_forest <- randomForest(difference ~ PIL2017 + pil_change_total + pil_change_2017 
                             + poverty_risk_perc + percentage_educated + informed + friends_family + religion,
                             data=country_data, importance=TRUE, ntrees=5000)) #the mtry argument is left empty so it defaults to a standard n/3
importance(diff_forest)

set.seed(1234)
(diff_forest2 <- randomForest(difference ~ percentage_educated + friends_family + religion + informed,
                             data=country_data, importance=TRUE, ntrees=5000)) #the mtry argument is left empty so it defaults to a standard n/3
importance(diff_forest2)
#boosting
set.seed(1234)
boosted <- gbm(perceived_proportion ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017
               + poverty_risk_perc + percentage_educated + informed + friends_family + factor(religion), data=country_data, distribution="gaussian",
               n.trees=5000, interaction.depth=3,  bag.fraction = 0.5, n.minobsinnode = 4)
knitr::kable(summary(boosted))

plot(boosted, i="percentage_educated")
plot(boosted, i="pil_change_2017")

#diff boosting
#boosting
set.seed(1234)
(diff_boosted <- gbm(difference  ~ PIL2017 + pil_change_total + pil_change_2017
               + poverty_risk_perc + percentage_educated + informed + friends_family + factor(religion), data=country_data, distribution="gaussian",
               n.trees=5000, interaction.depth=3,  bag.fraction = 0.5, n.minobsinnode = 4))
knitr::kable(summary(diff_boosted))

plot(boosted, i="percentage_educated",ylab = "Difference in proportion", xlab = "Percentage of educated (ISCED level 3 or higher)")
plot(boosted, i="friends_family", ylab = "Difference in proportion", xlab = "Percentage that reported immigrants as friends or family", col="red")



###Perception of immigration as a problem
#Tree

#Tree
control <- tree.control(nobs = 28, mincut = 3, minsize = 6, mindev = 0.01) #We change the settings to allow more splits
#the tree is going to be pruned later on so it should not matter

basic_tree <- tree(problem ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017
                   + poverty_risk_perc + percentage_educated + informed + friends_family + factor(religion) + actual_proportion,
                   data=country_data, control = control)
summary(basic_tree)

plot(basic_tree)
text(basic_tree, pretty=0)
title(main="Unpruned regression basic_tree")

#We use cross validation to determine the best tree size and prune the tree accordingly
set.seed(1234)
cv_basic <- cv.tree(basic_tree)
plot(cv_basic$size, cv_basic$dev, type="b")

prune_tree <- prune.tree(basic_tree, best=2)
plot(prune_tree)
text(prune_tree, pretty=0)
title(main="Pruned tree: perception of immigration as dangerous")

#Random forest

set.seed(1234)
n_pred <- ncol(country_data) - 6
(prob_forest <- randomForest(problem ~ PIL2017 + actual_proportion + pil_change_total + pil_change_2017 
                             + poverty_risk_perc + percentage_educated + informed + friends_family + religion + actual_proportion,
                             data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(prob_forest)

(prob_forest2 <- randomForest(problem ~ PIL2017 + pil_change_total +
                              friends_family,
                             data=country_data, importance=TRUE)) #the mtry argument is left empty so it defaults to a standard n/3
importance(prob_forest2)



#boosting
set.seed(1234)
(prob_boosted <- gbm(problem  ~ PIL2017 + pil_change_total + pil_change_2017 + actual_proportion
                    + poverty_risk_perc + informed + friends_family + factor(religion), data=country_data, distribution="gaussian",
                    n.trees=1000, interaction.depth=3,  bag.fraction = 0.5, n.minobsinnode = 4))
knitr::kable(summary(prob_boosted))


yhat.boost <- predict(prob_boosted, newdata=country_data, n.trees=5000)
compute_mse(yhat.boost, country_data$problem)

plot(boosted, i="percentage_educated",xlab = "Difference in proportion", ylab = "Percentage of educated (ISCED level 3 or higher)")
plot(boosted, i="pil_change_2017")


