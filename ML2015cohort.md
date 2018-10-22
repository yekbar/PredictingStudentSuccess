---
title: "Machine Learning & Analysis for 2015 Cohort"
author: "kat"
date: "October 17, 2018"
output:
  html_document:
    keep_md: yes
---




#Purpose
- The purpose of this file is to continue analysis from the cleaned ninth2015 file. This file uses the cleaned 2015 freshman and conducts predictive analytics using Random Forests, logistic regression, decision trees and statistical analysis using chi square.

### Loading in data

```r
nine_nona <- readRDS("nine_nona.RDS")
```

### Random Forest

```r
library(randomForest)

#less personID
nine_nona_ready <- nine_nona[,-c(1,10, 12)] # get rid of Person ID, Engineering, Grade

saveRDS(nine_nona_ready, "nine_nona_ready.rds")

nine_complete <- nine_nona_ready[complete.cases(nine_nona_ready),]
# split into 70/30 random
set.seed(100)
train <- sample(nrow(nine_complete), 0.7*nrow(nine_complete), replace = FALSE)
TrainSet <- nine_complete[train,]
TestSet <- nine_complete[-train,]
```

### Create Model
- changing mtry from 3 to 5 increases training accuracy, but not overall testing accuracy

```r
### random forest reference https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#overview
model1 <- randomForest(physicsSemPassed9 ~., data = TrainSet, interact=1, mtry=3, imp=1, mdim2nd=0,mdim2nd=3,  importance=TRUE, ntree=400, do.trace=100)
```

```
## ntree      OOB      1      2      3      4      5
##   100:  37.12% 47.22% 70.00% 96.55% 87.50%  3.47%
##   200:  35.28% 47.22% 62.50%100.00% 83.33%  2.31%
##   300:  35.28% 47.22% 65.00%100.00% 83.33%  1.73%
##   400:  35.89% 47.22% 67.50%100.00% 83.33%  2.31%
```

```r
model1
```

```
## 
## Call:
##  randomForest(formula = physicsSemPassed9 ~ ., data = TrainSet,      interact = 1, mtry = 3, imp = 1, mdim2nd = 0, mdim2nd = 3,      importance = TRUE, ntree = 400, do.trace = 100) 
##                Type of random forest: classification
##                      Number of trees: 400
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 35.89%
## Confusion matrix:
##    0  1 2 3   4 class.error
## 0 19  8 1 1   7  0.47222222
## 1 10 13 4 4   9  0.67500000
## 2  4 13 0 0  12  1.00000000
## 3  2  7 2 8  29  0.83333333
## 4  1  1 2 0 169  0.02312139
```

```r
plot(model1)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#getTree(model1)
#model1$importance
varImpPlot(model1)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
#predict on testing

predTrain <- predict(model1, TrainSet, type="class")
predtable <- table(predTrain, TrainSet$physicsSemPassed9)
sum(diag(predtable))/sum(predtable) #overall accuracy
```

```
## [1] 0.8773006
```

```r
1-sum(diag(predtable))/sum(predtable) #incorrect classification 
```

```
## [1] 0.1226994
```

```r
sum(diag(predtable))
```

```
## [1] 286
```

```r
sum(predtable)
```

```
## [1] 326
```


```r
importance(model1)
```

```
##                              0          1            2           3
## physicsType9         8.4669972  0.6819748 -1.033018059  2.09300876
## englishType9         4.3540594  6.4435753  0.253610972  2.95671727
## englishSemPassed9   14.8842140  5.8782136  0.590161002 -1.49368295
## historyType9         5.6216146  3.0577392 -4.117323270  0.40718465
## historySemPassed9   15.9228329  9.6666512 -1.045932659 -6.99486045
## mathType9            7.7067140  3.9342071 -0.349053407  2.80102927
## mathSemPassed9      17.6795170 13.1434287  5.779956989 -3.54524665
## Gender               4.0469669  2.6321352 -3.610816060 -0.77195483
## RaceEthnicityState   0.8567279  3.6424540 -0.005284939 -1.06225455
## MealStatus           0.8714268  1.4164739 -1.976078373  0.02438732
## Homeless            -0.7601914 -6.0563544 -1.811573879 -2.90312591
## SpecialEdDisability  4.7795487 -2.5849147 -3.302750497 -1.47377216
## Section504           0.0000000  0.0000000  0.000000000  0.00000000
## ELL                  4.2785407  0.4724597 -2.246622283  0.94309744
## Gifted               4.3155843 -0.9083770  2.215700517  3.00799168
## immigrant            0.0000000  0.0000000  0.000000000  0.00000000
## IEP                  4.9989387 -3.8595559 -2.414348232 -0.66315148
## EconDis             -0.1835214  1.8119585 -3.160915806  1.28778495
## HomePrimaryLanguage  5.2442356  0.7488810 -0.061725761  0.70706845
##                              4 MeanDecreaseAccuracy MeanDecreaseGini
## physicsType9        -5.2793709             0.598099       4.60603403
## englishType9         3.3125809             7.115613       8.57985193
## englishSemPassed9   21.9990358            22.629987      26.27601915
## historyType9         1.6775080             3.449341       3.14250548
## historySemPassed9   18.8481335            20.887797      24.90096917
## mathType9           -2.1258415             2.981513       3.69219901
## mathSemPassed9      18.0448299            22.330348      30.20015123
## Gender               2.1572706             3.394162       5.40260888
## RaceEthnicityState   1.9122207             2.740106      10.47877983
## MealStatus           5.2700533             3.909405       6.91211886
## Homeless             8.3212581             2.230232       3.48016572
## SpecialEdDisability  9.6152411             7.689907       4.34038267
## Section504           0.0000000             0.000000       0.42161473
## ELL                  1.3711158             2.932633       3.02523517
## Gifted              -0.7378166             1.899425       1.02370178
## immigrant            0.0000000             0.000000       0.01870466
## IEP                  7.0534514             6.046039       2.92602775
## EconDis              6.5598099             5.439987       3.70131901
## HomePrimaryLanguage  0.6781946             3.483252       4.57291588
```



```r
# NOT WORKING??
library(forestFloor)
class(model1)
forestFloor(rf.fit=model1, X=TrainSet, Xtest=TestSet, keep.inbag=T)
```

### Testing Set
- 68% accuracy

```r
predTest <- predict(model1, TestSet, type="class")
predtable2 <- table(predTest, TestSet$physicsSemPassed9)
predtable2
```

```
##         
## predTest  0  1  2  3  4
##        0 15  3  2  2  1
##        1  6  3  3  2  0
##        2  0  0  2  1  0
##        3  0  0  1  0  2
##        4  1  3  6 12 76
```

```r
sum(diag(predtable2))/sum(predtable2) #overall accuracy
```

```
## [1] 0.6808511
```

```r
1-sum(diag(predtable2))/sum(predtable2) #incorrect classification 
```

```
## [1] 0.3191489
```


```r
### TRY THIS?? https://chandramanitiwary.wordpress.com/2014/03/17/r-tips-part2-rocr-example-with-randomforest/
library("ROCR")
#prepare model for ROC Curve
test.forest <- predict(model1, type = 'prob', newdata = TestSet)
forestpred <- prediction(test.forest[,2], TestSet$class)
forestperf <- performance(forestpred, 'tpr', 'fpr')
plot(perf, main='ROC', colorize=T)
plot(bagperf, col=2, add=TRUE)
plot(perf, col=1, add=TRUE)
plot(forestperf, col=3, add=TRUE)
#legend(0.6, 0.6, c('ctree’, ‘bagging’, ‘rforest’), 1:3)
```

### Testing for which mtry model is best
- see note on 5 above

```r
#code https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(physicsSemPassed9 ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predTest <- predict(model3, TestSet, type = "class")
  a[i-2] = mean(predTest == TestSet$physicsSemPassed9)
}
a
```

```
## [1] 0.6737589 0.6808511 0.6808511 0.6737589 0.6879433 0.6879433
```

```r
plot(3:8,a)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Plotting Trees

```r
#plotting https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(model1, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(model1, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
					repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
```


```r
tree_func(model1, tree_num=5)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



### Create binary predictor variable for physics (p/F)

```r
#nine_nona_ready
# create new binary column
nine_nona_ready$predPhy[nine_nona_ready$physicsSemPassed9 == 4] <- "p"
nine_nona_ready$predPhy[nine_nona_ready$physicsSemPassed9 != 4] <- "f"

#convert to factor
nine_nona_ready[] <- lapply(nine_nona_ready, factor)
nine_nona_binary <- nine_nona_ready[,-2] # remove physics sem passed
#get rid of NAs
nine_nona_binary <- nine_nona_binary[complete.cases(nine_nona_binary),]

# split train/test
# create model
# split into 70/30 random
set.seed(100)
train <- sample(nrow(nine_nona_binary), 0.7*nrow(nine_nona_binary), replace = FALSE)
TrainSet2 <- nine_nona_binary[train,]
TestSet2 <- nine_nona_binary[-train,]


model2 <- randomForest(predPhy ~., data = TrainSet2, mtry=3, importance=TRUE, ntree=400)
model2
```

```
## 
## Call:
##  randomForest(formula = predPhy ~ ., data = TrainSet2, mtry = 3,      importance = TRUE, ntree = 400) 
##                Type of random forest: classification
##                      Number of trees: 400
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 14.42%
## Confusion matrix:
##     f   p class.error
## f 121  32   0.2091503
## p  15 158   0.0867052
```

```r
plot(model2)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#getTree(model2)
#model2$importance
varImpPlot(model2)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
#predict on testing

predTrain2 <- predict(model2, TrainSet2, type="class")
predtable2 <- table(predTrain2, TrainSet2$predPhy)
sum(diag(predtable2))/sum(predtable2) #overall accuracy
```

```
## [1] 0.9233129
```

```r
1-sum(diag(predtable2))/sum(predtable2) #incorrect classification 
```

```
## [1] 0.07668712
```

### Testing Set
- 86.5% accuracy

```r
predTest2 <- predict(model2, TestSet2, type="class")
predtable3 <- table(predTest2, TestSet2$predPhy)
predtable3
```

```
##          
## predTest2  f  p
##         f 52  9
##         p 10 70
```

```r
sum(diag(predtable3))/sum(predtable3) #overall accuracy
```

```
## [1] 0.8652482
```

```r
1-sum(diag(predtable3))/sum(predtable3) #incorrect classification 
```

```
## [1] 0.1347518
```


###Testing for which mtry model is best
- see note on 5 above

```r
#code https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(predPhy ~ ., data = TrainSet2, ntree = 400, mtry = i, importance = TRUE)
  predTest <- predict(model3, TestSet2, type = "class")
  a[i-2] = mean(predTest == TestSet2$predPhy)
}
a
```

```
## [1] 0.8510638 0.8652482 0.8652482 0.8510638 0.8652482 0.8439716
```

```r
plot(3:8,a)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
#plotting https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(model2, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(model2, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
					repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
```


```r
tree_func(model2, tree_num=5)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

### Cross Validation wtih caret

```r
library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(predPhy ~ ., data = TrainSet2, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

```
## CART 
## 
## 326 samples
##  19 predictor
##   2 classes: 'f', 'p' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 293, 294, 293, 293, 294, 293, ... 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy   Kappa    
##   0.01  0.8165720  0.6274933
##   0.02  0.8164773  0.6272421
##   0.03  0.8196023  0.6376384
##   0.04  0.8196023  0.6376384
##   0.05  0.8196023  0.6376384
##   0.06  0.8196023  0.6376384
##   0.07  0.8196023  0.6376384
##   0.08  0.8196023  0.6376384
##   0.09  0.8196023  0.6376384
##   0.10  0.8196023  0.6376384
##   0.11  0.8196023  0.6376384
##   0.12  0.8196023  0.6376384
##   0.13  0.8196023  0.6376384
##   0.14  0.8196023  0.6376384
##   0.15  0.8196023  0.6376384
##   0.16  0.8196023  0.6376384
##   0.17  0.8196023  0.6376384
##   0.18  0.8196023  0.6376384
##   0.19  0.8196023  0.6376384
##   0.20  0.8196023  0.6376384
##   0.21  0.8196023  0.6376384
##   0.22  0.8196023  0.6376384
##   0.23  0.8196023  0.6376384
##   0.24  0.8196023  0.6376384
##   0.25  0.8196023  0.6376384
##   0.26  0.8196023  0.6376384
##   0.27  0.8196023  0.6376384
##   0.28  0.8196023  0.6376384
##   0.29  0.8196023  0.6376384
##   0.30  0.8196023  0.6376384
##   0.31  0.8196023  0.6376384
##   0.32  0.8196023  0.6376384
##   0.33  0.8196023  0.6376384
##   0.34  0.8196023  0.6376384
##   0.35  0.8196023  0.6376384
##   0.36  0.8196023  0.6376384
##   0.37  0.8196023  0.6376384
##   0.38  0.8196023  0.6376384
##   0.39  0.8196023  0.6376384
##   0.40  0.8196023  0.6376384
##   0.41  0.8196023  0.6376384
##   0.42  0.8196023  0.6376384
##   0.43  0.8196023  0.6376384
##   0.44  0.8196023  0.6376384
##   0.45  0.8196023  0.6376384
##   0.46  0.8196023  0.6376384
##   0.47  0.8196023  0.6376384
##   0.48  0.8196023  0.6376384
##   0.49  0.8196023  0.6376384
##   0.50  0.8196023  0.6376384
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.5.
```

### TRAINSET 2 is 85% accuracy with rpart RandomForest


```r
library(rpart)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
rpartTree <- rpart(predPhy ~., data=TrainSet2, method="class", cp=.02, parms= list(split="gini"))
predictionCV <- predict(rpartTree, newdata=TestSet2, type="class")
table2 <- table(TestSet2$predPhy, predictionCV)


sum(diag(table2))/sum(table2) #overall accuracy
```

```
## [1] 0.8510638
```

```r
1-sum(diag(table2))/sum(table2) #incorrect classification 
```

```
## [1] 0.1489362
```

```r
library(rpart.plot)
prp(rpartTree)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
fancyRpartPlot(rpartTree)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-17-2.png)<!-- -->



### Only top predictors model

```r
colnames(nine_nona_ready)
```

```
##  [1] "physicsType9"        "physicsSemPassed9"   "englishType9"       
##  [4] "englishSemPassed9"   "historyType9"        "historySemPassed9"  
##  [7] "mathType9"           "mathSemPassed9"      "Gender"             
## [10] "RaceEthnicityState"  "MealStatus"          "Homeless"           
## [13] "SpecialEdDisability" "Section504"          "ELL"                
## [16] "Gifted"              "immigrant"           "IEP"                
## [19] "EconDis"             "HomePrimaryLanguage" "predPhy"
```

```r
mostimportant <- nine_nona_ready[c( 4, 6, 8, 21)]
#mostimportant

mostimportant <- mostimportant[complete.cases(mostimportant),]

# split train/test
# create model
# split into 70/30 random
set.seed(100)
train2 <- sample(nrow(mostimportant), 0.7*nrow(mostimportant), replace = FALSE)
TrainSet4 <- mostimportant[train2,]
TestSet4 <- mostimportant[-train2,]

model4 <- randomForest(predPhy ~., data = TrainSet4, mtry=3, importance=TRUE, ntree=200, do.trace=TRUE)
```

```
## ntree      OOB      1      2
##     1:  16.06% 20.31% 12.33%
##     2:  16.99% 23.33% 12.07%
##     3:  18.22% 24.14% 13.38%
##     4:  18.28% 25.40% 12.80%
##     5:  17.70% 25.00% 12.14%
##     6:  19.18% 28.68% 12.09%
##     7:  18.65% 31.21%  9.14%
##     8:  18.21% 31.25%  8.38%
##     9:  17.35% 29.05%  8.33%
##    10:  17.30% 28.38%  8.81%
##    11:  17.84% 27.70% 10.31%
##    12:  18.66% 28.19% 11.34%
##    13:  18.37% 28.86% 10.31%
##    14:  18.95% 28.86% 11.34%
##    15:  18.66% 29.53% 10.31%
##    16:  18.66% 29.53% 10.31%
##    17:  18.66% 28.86% 10.82%
##    18:  18.37% 28.19% 10.82%
##    19:  19.24% 29.53% 11.34%
##    20:  18.37% 28.19% 10.82%
##    21:  18.66% 28.86% 10.82%
##    22:  18.37% 27.52% 11.34%
##    23:  18.08% 27.52% 10.82%
##    24:  18.08% 27.52% 10.82%
##    25:  18.95% 28.86% 11.34%
##    26:  18.37% 27.52% 11.34%
##    27:  18.08% 26.85% 11.34%
##    28:  18.66% 28.19% 11.34%
##    29:  18.37% 27.52% 11.34%
##    30:  18.66% 27.52% 11.86%
##    31:  17.78% 26.85% 10.82%
##    32:  17.78% 26.85% 10.82%
##    33:  17.20% 26.17% 10.31%
##    34:  17.20% 26.17% 10.31%
##    35:  16.91% 26.17%  9.79%
##    36:  16.91% 26.17%  9.79%
##    37:  16.33% 26.17%  8.76%
##    38:  16.91% 26.85%  9.28%
##    39:  16.91% 26.17%  9.79%
##    40:  17.20% 26.17% 10.31%
##    41:  16.91% 26.85%  9.28%
##    42:  16.62% 26.85%  8.76%
##    43:  17.20% 26.85%  9.79%
##    44:  16.91% 26.85%  9.28%
##    45:  17.49% 26.85% 10.31%
##    46:  17.49% 27.52%  9.79%
##    47:  16.91% 27.52%  8.76%
##    48:  17.20% 27.52%  9.28%
##    49:  17.49% 27.52%  9.79%
##    50:  18.08% 28.86%  9.79%
##    51:  18.08% 28.86%  9.79%
##    52:  18.08% 28.86%  9.79%
##    53:  18.08% 28.86%  9.79%
##    54:  17.78% 28.86%  9.28%
##    55:  18.08% 28.86%  9.79%
##    56:  18.08% 28.86%  9.79%
##    57:  18.08% 28.86%  9.79%
##    58:  18.37% 28.86% 10.31%
##    59:  17.49% 28.86%  8.76%
##    60:  17.49% 28.86%  8.76%
##    61:  17.78% 28.86%  9.28%
##    62:  17.78% 28.86%  9.28%
##    63:  17.78% 28.86%  9.28%
##    64:  18.08% 29.53%  9.28%
##    65:  17.78% 28.86%  9.28%
##    66:  17.78% 28.86%  9.28%
##    67:  17.49% 28.86%  8.76%
##    68:  17.49% 28.86%  8.76%
##    69:  17.78% 28.86%  9.28%
##    70:  17.78% 29.53%  8.76%
##    71:  18.08% 29.53%  9.28%
##    72:  18.08% 29.53%  9.28%
##    73:  18.08% 29.53%  9.28%
##    74:  18.08% 29.53%  9.28%
##    75:  18.08% 29.53%  9.28%
##    76:  18.08% 29.53%  9.28%
##    77:  18.08% 28.86%  9.79%
##    78:  18.08% 29.53%  9.28%
##    79:  18.37% 29.53%  9.79%
##    80:  18.37% 29.53%  9.79%
##    81:  18.66% 30.20%  9.79%
##    82:  18.08% 29.53%  9.28%
##    83:  18.37% 30.20%  9.28%
##    84:  18.37% 30.20%  9.28%
##    85:  18.37% 30.20%  9.28%
##    86:  18.37% 30.20%  9.28%
##    87:  18.37% 30.20%  9.28%
##    88:  18.37% 30.20%  9.28%
##    89:  18.66% 30.20%  9.79%
##    90:  18.37% 30.20%  9.28%
##    91:  18.37% 30.20%  9.28%
##    92:  18.37% 30.20%  9.28%
##    93:  18.66% 30.87%  9.28%
##    94:  18.37% 30.20%  9.28%
##    95:  18.66% 30.87%  9.28%
##    96:  18.66% 30.20%  9.79%
##    97:  18.37% 30.20%  9.28%
##    98:  18.66% 30.87%  9.28%
##    99:  18.08% 30.20%  8.76%
##   100:  18.37% 30.20%  9.28%
##   101:  18.08% 30.20%  8.76%
##   102:  18.08% 30.20%  8.76%
##   103:  18.08% 30.20%  8.76%
##   104:  18.08% 30.20%  8.76%
##   105:  18.08% 30.20%  8.76%
##   106:  18.08% 30.20%  8.76%
##   107:  18.08% 30.20%  8.76%
##   108:  18.08% 30.20%  8.76%
##   109:  18.08% 30.20%  8.76%
##   110:  18.08% 30.20%  8.76%
##   111:  18.08% 30.20%  8.76%
##   112:  18.08% 30.20%  8.76%
##   113:  18.08% 30.20%  8.76%
##   114:  18.08% 30.20%  8.76%
##   115:  18.08% 30.20%  8.76%
##   116:  18.08% 30.20%  8.76%
##   117:  18.08% 30.20%  8.76%
##   118:  18.08% 30.20%  8.76%
##   119:  18.08% 30.20%  8.76%
##   120:  18.08% 30.20%  8.76%
##   121:  18.08% 30.20%  8.76%
##   122:  18.08% 30.20%  8.76%
##   123:  18.08% 30.20%  8.76%
##   124:  18.08% 30.20%  8.76%
##   125:  18.08% 30.20%  8.76%
##   126:  18.08% 30.20%  8.76%
##   127:  18.08% 30.20%  8.76%
##   128:  18.08% 30.20%  8.76%
##   129:  18.37% 30.87%  8.76%
##   130:  18.37% 30.87%  8.76%
##   131:  18.37% 30.87%  8.76%
##   132:  18.37% 30.87%  8.76%
##   133:  18.08% 30.20%  8.76%
##   134:  18.08% 30.20%  8.76%
##   135:  18.37% 30.87%  8.76%
##   136:  18.37% 30.87%  8.76%
##   137:  18.08% 30.20%  8.76%
##   138:  18.37% 30.87%  8.76%
##   139:  17.78% 29.53%  8.76%
##   140:  18.08% 30.20%  8.76%
##   141:  18.08% 30.20%  8.76%
##   142:  18.08% 30.20%  8.76%
##   143:  18.08% 30.20%  8.76%
##   144:  18.08% 30.20%  8.76%
##   145:  18.08% 30.20%  8.76%
##   146:  18.08% 30.20%  8.76%
##   147:  18.66% 30.87%  9.28%
##   148:  18.08% 30.20%  8.76%
##   149:  18.37% 30.20%  9.28%
##   150:  18.08% 29.53%  9.28%
##   151:  18.37% 30.20%  9.28%
##   152:  18.37% 30.20%  9.28%
##   153:  18.08% 29.53%  9.28%
##   154:  17.78% 29.53%  8.76%
##   155:  18.08% 29.53%  9.28%
##   156:  18.37% 30.20%  9.28%
##   157:  18.66% 30.87%  9.28%
##   158:  18.66% 30.87%  9.28%
##   159:  18.66% 30.87%  9.28%
##   160:  18.66% 30.87%  9.28%
##   161:  18.66% 30.87%  9.28%
##   162:  18.95% 31.54%  9.28%
##   163:  18.95% 31.54%  9.28%
##   164:  18.66% 30.87%  9.28%
##   165:  18.66% 30.87%  9.28%
##   166:  19.24% 31.54%  9.79%
##   167:  18.95% 30.87%  9.79%
##   168:  18.95% 30.87%  9.79%
##   169:  18.66% 30.20%  9.79%
##   170:  18.66% 30.20%  9.79%
##   171:  18.66% 30.20%  9.79%
##   172:  18.95% 30.87%  9.79%
##   173:  18.95% 30.87%  9.79%
##   174:  19.24% 31.54%  9.79%
##   175:  19.24% 31.54%  9.79%
##   176:  19.24% 31.54%  9.79%
##   177:  19.24% 31.54%  9.79%
##   178:  19.24% 31.54%  9.79%
##   179:  19.24% 31.54%  9.79%
##   180:  18.95% 30.87%  9.79%
##   181:  18.95% 30.87%  9.79%
##   182:  18.95% 30.87%  9.79%
##   183:  18.95% 30.87%  9.79%
##   184:  18.95% 30.87%  9.79%
##   185:  18.95% 30.87%  9.79%
##   186:  18.95% 30.87%  9.79%
##   187:  18.95% 30.87%  9.79%
##   188:  18.95% 30.87%  9.79%
##   189:  18.95% 30.87%  9.79%
##   190:  18.66% 30.87%  9.28%
##   191:  18.95% 30.87%  9.79%
##   192:  18.37% 30.20%  9.28%
##   193:  18.66% 30.87%  9.28%
##   194:  18.66% 30.87%  9.28%
##   195:  18.66% 30.20%  9.79%
##   196:  18.95% 30.87%  9.79%
##   197:  18.66% 30.87%  9.28%
##   198:  18.37% 30.20%  9.28%
##   199:  18.66% 30.87%  9.28%
##   200:  18.66% 30.87%  9.28%
```

```r
model4
```

```
## 
## Call:
##  randomForest(formula = predPhy ~ ., data = TrainSet4, mtry = 3,      importance = TRUE, ntree = 200, do.trace = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 200
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 18.66%
## Confusion matrix:
##     f   p class.error
## f 103  46  0.30872483
## p  18 176  0.09278351
```

```r
plot(model4)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
varImpPlot(model4)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```r
#predict on training
predTrain4 <- predict(model4, TrainSet4, type="class")
predtable4 <- table(predTrain4, TrainSet4$predPhy)
sum(diag(predtable4))/sum(predtable4) #overall accuracy
```

```
## [1] 0.8804665
```

```r
1-sum(diag(predtable4))/sum(predtable4) #incorrect classification
```

```
## [1] 0.1195335
```

```r
# predict on testing
predTest44 <- predict(model4, TestSet4, type="class")
predtable44 <- table(predTest44, TestSet4$predPhy)
predtable44
```

```
##           
## predTest44  f  p
##          f 61 10
##          p 16 61
```

```r
sum(diag(predtable44))/sum(predtable44) #overall accuracy
```

```
## [1] 0.8243243
```

```r
1-sum(diag(predtable44))/sum(predtable44) #incorrect classification 
```

```
## [1] 0.1756757
```

### TrainSet4 Model
- 82% accuracy

```r
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
rpartTree4 <- rpart(predPhy ~., data=TrainSet4, method="class", cp=.02)
predictionCV4 <- predict(rpartTree4, newdata=TestSet4, type="class")
table4 <- table(TestSet4$predPhy, predictionCV4)


sum(diag(table4))/sum(table4) #overall accuracy
```

```
## [1] 0.8175676
```

```r
1-sum(diag(table4))/sum(table4) #incorrect classification 
```

```
## [1] 0.1824324
```

```r
library(rpart.plot)
prp(rpartTree4)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
fancyRpartPlot(rpartTree4)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-19-2.png)<!-- -->



```r
#plotting https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(model4, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(model4, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
					repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
```


```r
tree_func(model4, tree_num=3)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-21-1.png)<!-- -->



### Can freshman grades & physics predict success in chemisty?
- Get chemistry classes in 10th grade & Bio 11th

### Try to predict Chemistry grade with Freshman year


```r
nineTen <- readRDS("nineTen.rds")
```


```r
# split train/test
# create model
# split into 70/30 random
set.seed(100)
train <- sample(nrow(nineTen), 0.7*nrow(nineTen), replace = FALSE)
TrainSet3 <- nineTen[train,]
TestSet3 <- nineTen[-train,]

summary(TrainSet3)
```

```
##        physicsType9 physicsSemPassed9 englishType9 englishSemPassed9
##  honorsphy   : 74   0: 21             ELDEng: 17   0: 34            
##  physics     :197   1: 25             HEng  : 84   1: 26            
##  sheltphysics:  2   2: 23             regEng:172   2: 29            
##                     3: 43                          3: 25            
##                     4:161                          4:159            
##                                                                     
##                                                                     
##    historyType9 historySemPassed9    mathType9   mathSemPassed9 Gender 
##  APgeo   : 58   0: 12             core1   :214   0: 25          F:137  
##  geo     :210   1: 23             hcore2  : 58   1: 26          M:136  
##  Sheltgeo:  5   2: 31             honorsC3:  1   2: 31                 
##                 3: 38                            3: 34                 
##                 4:169                            4:157                 
##                                                                        
##                                                                        
##  RaceEthnicityState MealStatus Homeless SpecialEdDisability Section504
##  1:  3              0:130      0:261    0:253               0:272     
##  2: 16              1:110      1: 12    1:  5               1:  1     
##  3: 59              2: 33      2:  0    2: 14                         
##  4:112                                  3:  1                         
##  5: 63                                                                
##  6:  1                                                                
##  7: 19                                                                
##  ELL     Gifted  immigrant IEP     EconDis HomePrimaryLanguage predChem
##  0:178   0:252   0:272     0:253   0:110   0:178               f:119   
##  1: 95   1: 21   1:  1     1: 20   1:163   1: 77               p:154   
##                                            2:  5                       
##                                            3: 13                       
##                                                                        
##                                                                        
## 
```

```r
summary(TestSet3)
```

```
##        physicsType9 physicsSemPassed9 englishType9 englishSemPassed9
##  honorsphy   :26    0:13              ELDEng:16    0:12             
##  physics     :89    1: 7              HEng  :31    1:12             
##  sheltphysics: 2    2: 9              regEng:70    2:10             
##                     3:16                           3: 9             
##                     4:72                           4:74             
##                                                                     
##                                                                     
##    historyType9 historySemPassed9    mathType9  mathSemPassed9 Gender
##  APgeo   :26    0: 1              core1   :90   0:13           F:54  
##  geo     :90    1:11              hcore2  :27   1:12           M:63  
##  Sheltgeo: 1    2:11              honorsC3: 0   2:14                 
##                 3:17                            3:11                 
##                 4:77                            4:67                 
##                                                                      
##                                                                      
##  RaceEthnicityState MealStatus Homeless SpecialEdDisability Section504
##  1: 0               0:65       0:111    0:109               0:117     
##  2: 6               1:43       1:  6    1:  0               1:  0     
##  3:25               2: 9       2:  0    2:  8                         
##  4:54                                   3:  0                         
##  5:27                                                                 
##  6: 1                                                                 
##  7: 4                                                                 
##  ELL    Gifted  immigrant IEP     EconDis HomePrimaryLanguage predChem
##  0:73   0:106   0:116     0:109   0:43    0:73                f:44    
##  1:44   1: 11   1:  1     1:  8   1:74    1:37                p:73    
##                                           2: 5                        
##                                           3: 2                        
##                                                                       
##                                                                       
## 
```

```r
model9_10 <- randomForest(predChem ~., data = TrainSet3, mtry=3, importance=TRUE, ntree=400)
model9_10
```

```
## 
## Call:
##  randomForest(formula = predChem ~ ., data = TrainSet3, mtry = 3,      importance = TRUE, ntree = 400) 
##                Type of random forest: classification
##                      Number of trees: 400
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 27.84%
## Confusion matrix:
##    f   p class.error
## f 70  49   0.4117647
## p 27 127   0.1753247
```

```r
plot(model9_10)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
#getTree(model9_10)
#model2$importance
varImpPlot(model9_10)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

```r
#predict on testing

predTrain9_10 <- predict(model9_10, TrainSet3, type="class")
predtable9_10 <- table(predTrain9_10, TrainSet3$predChem)
sum(diag(predtable9_10))/sum(predtable9_10) #overall accuracy
```

```
## [1] 0.9304029
```

```r
1-sum(diag(predtable9_10))/sum(predtable9_10) #incorrect classification 
```

```
## [1] 0.06959707
```

### TESTING SET
- Freshman year grades and chemistry grades  predict 83% accuracy

```r
predTest9_10 <- predict(model9_10, TestSet3, type="class")
predtable9_10 <- table(predTest9_10, TestSet3$predChem)
predtable9_10
```

```
##             
## predTest9_10  f  p
##            f 35 11
##            p  9 62
```

```r
sum(diag(predtable9_10))/sum(predtable9_10) #overall accuracy
```

```
## [1] 0.8290598
```

```r
1-sum(diag(predtable9_10))/sum(predtable9_10) #incorrect classification 
```

```
## [1] 0.1709402
```

```r
# reference https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html
```

### RPART FOR VISUAL
- Trainset3 has Chem

```r
library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(predChem ~ ., data = TrainSet3, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

```
## CART 
## 
## 273 samples
##  20 predictor
##   2 classes: 'f', 'p' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 246, 246, 246, 246, 245, 246, ... 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy   Kappa    
##   0.01  0.7111111  0.3983349
##   0.02  0.7038360  0.3822669
##   0.03  0.6966931  0.3758062
##   0.04  0.7038360  0.3923186
##   0.05  0.7038360  0.3923186
##   0.06  0.7038360  0.3923186
##   0.07  0.7038360  0.3923186
##   0.08  0.7038360  0.3923186
##   0.09  0.7038360  0.3923186
##   0.10  0.7038360  0.3923186
##   0.11  0.7038360  0.3923186
##   0.12  0.7038360  0.3923186
##   0.13  0.7038360  0.3923186
##   0.14  0.7038360  0.3923186
##   0.15  0.7038360  0.3923186
##   0.16  0.7038360  0.3923186
##   0.17  0.7038360  0.3923186
##   0.18  0.7038360  0.3923186
##   0.19  0.7038360  0.3923186
##   0.20  0.7038360  0.3923186
##   0.21  0.7038360  0.3923186
##   0.22  0.7038360  0.3923186
##   0.23  0.7038360  0.3923186
##   0.24  0.7038360  0.3923186
##   0.25  0.7038360  0.3923186
##   0.26  0.7038360  0.3923186
##   0.27  0.7038360  0.3923186
##   0.28  0.7038360  0.3923186
##   0.29  0.7038360  0.3923186
##   0.30  0.7038360  0.3923186
##   0.31  0.7038360  0.3923186
##   0.32  0.7038360  0.3923186
##   0.33  0.7038360  0.3923186
##   0.34  0.7038360  0.3923186
##   0.35  0.7038360  0.3923186
##   0.36  0.7038360  0.3923186
##   0.37  0.7038360  0.3923186
##   0.38  0.7038360  0.3923186
##   0.39  0.6890212  0.3424513
##   0.40  0.6667989  0.2874513
##   0.41  0.6482804  0.2403924
##   0.42  0.5675926  0.0172043
##   0.43  0.5640212  0.0000000
##   0.44  0.5640212  0.0000000
##   0.45  0.5640212  0.0000000
##   0.46  0.5640212  0.0000000
##   0.47  0.5640212  0.0000000
##   0.48  0.5640212  0.0000000
##   0.49  0.5640212  0.0000000
##   0.50  0.5640212  0.0000000
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.01.
```


### Trainset3 for Chemistry
- adding loss penalty decreased accuracy

```r
# penalize false positive (kid actually failed but model thought they would pass)
#lossmatrix <- matrix(c(0,2,1,0), byrow=TRUE, nrow=2)

library(rpart)
library(caret)
rpartTree5 <- rpart(predChem ~., data=TrainSet3, method="class", cp=.02)
                    #parms=list(loss=lossmatrix))
predictionCV5 <- predict(rpartTree5, newdata=TestSet3, type="class")
table5 <- table(actual=TestSet3$predChem, prediction=predictionCV5)
table5
```

```
##       prediction
## actual  f  p
##      f 31 13
##      p 13 60
```

```r
confusionMatrix(TestSet3$predChem, predictionCV5)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  f  p
##          f 31 13
##          p 13 60
##                                           
##                Accuracy : 0.7778          
##                  95% CI : (0.6916, 0.8494)
##     No Information Rate : 0.6239          
##     P-Value [Acc > NIR] : 0.0002738       
##                                           
##                   Kappa : 0.5265          
##  Mcnemar's Test P-Value : 1.0000000       
##                                           
##             Sensitivity : 0.7045          
##             Specificity : 0.8219          
##          Pos Pred Value : 0.7045          
##          Neg Pred Value : 0.8219          
##              Prevalence : 0.3761          
##          Detection Rate : 0.2650          
##    Detection Prevalence : 0.3761          
##       Balanced Accuracy : 0.7632          
##                                           
##        'Positive' Class : f               
## 
```

```r
#see principle components
printcp(rpartTree5)
```

```
## 
## Classification tree:
## rpart(formula = predChem ~ ., data = TrainSet3, method = "class", 
##     cp = 0.02)
## 
## Variables actually used in tree construction:
## [1] englishSemPassed9 historySemPassed9 mathSemPassed9    physicsSemPassed9
## 
## Root node error: 119/273 = 0.4359
## 
## n= 273 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.403361      0   1.00000 1.00000 0.068850
## 2 0.025210      1   0.59664 0.63866 0.062232
## 3 0.021008      3   0.54622 0.68067 0.063426
## 4 0.020000      5   0.50420 0.68067 0.063426
```

```r
sum(diag(table5))/sum(table5) #overall accuracy
```

```
## [1] 0.7777778
```

```r
1-sum(diag(table5))/sum(table5) #incorrect classification 
```

```
## [1] 0.2222222
```

```r
library(rpart.plot)
library(RColorBrewer)
library(rattle)
#prp(rpartTree5)
fancyRpartPlot(rpartTree5)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-26-1.png)<!-- -->



```r
as.data.frame(rpartTree5$variable.importance)
```

```
##                     rpartTree5$variable.importance
## mathSemPassed9                          31.2965337
## physicsSemPassed9                       24.1120223
## englishSemPassed9                       20.2745947
## historySemPassed9                       17.5307986
## englishType9                             5.3634969
## SpecialEdDisability                      4.5972830
## RaceEthnicityState                       1.2812561
## physicsType9                             0.6520101
## mathType9                                0.5588658
## Gender                                   0.5087942
```


### TrainSet3 RPart
- 77% accuracy

```r
# Cross Validation wtih caret
library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(predChem ~ ., data = TrainSet3, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

```
## CART 
## 
## 273 samples
##  20 predictor
##   2 classes: 'f', 'p' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 246, 246, 246, 245, 246, 247, ... 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy   Kappa     
##   0.01  0.7183659  0.42387134
##   0.02  0.7003765  0.38234987
##   0.03  0.6895299  0.36475408
##   0.04  0.6895299  0.36604918
##   0.05  0.6895299  0.36604918
##   0.06  0.6895299  0.36604918
##   0.07  0.6895299  0.36604918
##   0.08  0.6895299  0.36604918
##   0.09  0.6895299  0.36604918
##   0.10  0.6895299  0.36604918
##   0.11  0.6895299  0.36604918
##   0.12  0.6895299  0.36604918
##   0.13  0.6895299  0.36604918
##   0.14  0.6895299  0.36604918
##   0.15  0.6895299  0.36604918
##   0.16  0.6895299  0.36604918
##   0.17  0.6895299  0.36604918
##   0.18  0.6895299  0.36604918
##   0.19  0.6895299  0.36604918
##   0.20  0.6895299  0.36604918
##   0.21  0.6895299  0.36604918
##   0.22  0.6895299  0.36604918
##   0.23  0.6895299  0.36604918
##   0.24  0.6895299  0.36604918
##   0.25  0.6895299  0.36604918
##   0.26  0.6895299  0.36604918
##   0.27  0.6895299  0.36604918
##   0.28  0.6895299  0.36604918
##   0.29  0.6895299  0.36604918
##   0.30  0.6895299  0.36604918
##   0.31  0.6895299  0.36604918
##   0.32  0.6895299  0.36604918
##   0.33  0.6895299  0.36604918
##   0.34  0.6895299  0.36604918
##   0.35  0.6895299  0.36604918
##   0.36  0.6895299  0.36604918
##   0.37  0.6549145  0.28180675
##   0.38  0.6549145  0.28180675
##   0.39  0.6549145  0.28180675
##   0.40  0.6263431  0.19852408
##   0.41  0.6045177  0.13290709
##   0.42  0.5897029  0.09290709
##   0.43  0.5677452  0.01538462
##   0.44  0.5640415  0.00000000
##   0.45  0.5640415  0.00000000
##   0.46  0.5640415  0.00000000
##   0.47  0.5640415  0.00000000
##   0.48  0.5640415  0.00000000
##   0.49  0.5640415  0.00000000
##   0.50  0.5640415  0.00000000
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.01.
```

```r
library(rpart)
rpartTree9_10 <- rpart(predChem ~., data=TrainSet3, method="class", cp=.01)
predictionCV9_10 <- predict(rpartTree9_10, newdata=TestSet3, type="class")
table9_10 <- table(TestSet3$predChem, predictionCV9_10)


sum(diag(table9_10))/sum(table9_10) #overall accuracy
```

```
## [1] 0.7777778
```

```r
1-sum(diag(table9_10))/sum(table9_10) #incorrect classification 
```

```
## [1] 0.2222222
```

```r
library(rpart.plot)
prp(rpartTree9_10)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-28-1.png)<!-- -->


### Chi Square between combos of classes
- ALL ARE independent of each other!!! 

```r
nine_tenChem <- readRDS("nine_tenChem.rds") #9/10 cohort classes
colnames(nine_tenChem)
```

```
##  [1] "personID"            "physicsType9"        "physicsSemPassed9"  
##  [4] "englishType9"        "englishSemPassed9"   "historyType9"       
##  [7] "historySemPassed9"   "mathType9"           "mathSemPassed9"     
## [10] "engineering"         "Gender"              "Grade"              
## [13] "RaceEthnicityState"  "MealStatus"          "Homeless"           
## [16] "SpecialEdDisability" "Section504"          "ELL"                
## [19] "Gifted"              "immigrant"           "IEP"                
## [22] "EconDis"             "HomePrimaryLanguage" "ChemType10"         
## [25] "ChemSemPassed10"
```

```r
nine_ten_chi <- nine_tenChem[c(1, 3, 5, 7, 9, 25)]
```


```r
# Physics & math
chisq.test(nine_ten_chi$physicsSemPassed9, nine_ten_chi$mathSemPassed9)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  nine_ten_chi$physicsSemPassed9 and nine_ten_chi$mathSemPassed9
## X-squared = 365.79, df = 16, p-value < 2.2e-16
```

```r
# Physics & Chem
chisq.test(nine_ten_chi$physicsSemPassed9, nine_ten_chi$ChemSemPassed10, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$physicsSemPassed9 and nine_ten_chi$ChemSemPassed10
## X-squared = 150.9, df = NA, p-value = 0.0004998
```

```r
#physics & history
chisq.test(nine_ten_chi$physicsSemPassed9, nine_ten_chi$historySemPassed9, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$physicsSemPassed9 and nine_ten_chi$historySemPassed9
## X-squared = 304.11, df = NA, p-value = 0.0004998
```

```r
#physics & english
chisq.test(nine_ten_chi$physicsSemPassed9, nine_ten_chi$englishSemPassed9, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$physicsSemPassed9 and nine_ten_chi$englishSemPassed9
## X-squared = 285.82, df = NA, p-value = 0.0004998
```

```r
# Chem & math
chisq.test(nine_ten_chi$mathSemPassed9, nine_ten_chi$ChemSemPassed10, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$mathSemPassed9 and nine_ten_chi$ChemSemPassed10
## X-squared = 202.67, df = NA, p-value = 0.0004998
```

```r
#Chem & history
chisq.test(nine_ten_chi$historySemPassed9, nine_ten_chi$ChemSemPassed10, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$historySemPassed9 and nine_ten_chi$ChemSemPassed10
## X-squared = 150.28, df = NA, p-value = 0.0004998
```

```r
#Chem & english
chisq.test(nine_ten_chi$englishSemPassed9, nine_ten_chi$ChemSemPassed10, simulate.p.value = TRUE) 
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$englishSemPassed9 and nine_ten_chi$ChemSemPassed10
## X-squared = 140.59, df = NA, p-value = 0.0004998
```

```r
#math & english
chisq.test(nine_ten_chi$mathSemPassed9, nine_ten_chi$englishSemPassed9, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$mathSemPassed9 and nine_ten_chi$englishSemPassed9
## X-squared = 305.9, df = NA, p-value = 0.0004998
```

```r
#math & history
chisq.test(nine_ten_chi$mathSemPassed9, nine_ten_chi$historySemPassed9, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$mathSemPassed9 and nine_ten_chi$historySemPassed9
## X-squared = 332.04, df = NA, p-value = 0.0004998
```

```r
# history & engish
chisq.test(nine_ten_chi$englishSemPassed9, nine_ten_chi$historySemPassed9, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_chi$englishSemPassed9 and nine_ten_chi$historySemPassed9
## X-squared = 303.12, df = NA, p-value = 0.0004998
```

### Chi square of engineering & physics & chem
- Chi square for engineering & phsyics were LOWER than the other 9th grade classes!
- BUT, this is a smaller sample size

```r
colnames(nine_tenChem)
```

```
##  [1] "personID"            "physicsType9"        "physicsSemPassed9"  
##  [4] "englishType9"        "englishSemPassed9"   "historyType9"       
##  [7] "historySemPassed9"   "mathType9"           "mathSemPassed9"     
## [10] "engineering"         "Gender"              "Grade"              
## [13] "RaceEthnicityState"  "MealStatus"          "Homeless"           
## [16] "SpecialEdDisability" "Section504"          "ELL"                
## [19] "Gifted"              "immigrant"           "IEP"                
## [22] "EconDis"             "HomePrimaryLanguage" "ChemType10"         
## [25] "ChemSemPassed10"
```

```r
# personID, physicsSemPassed9, engineering, ChemSemPassed10""
nine_ten_eng <- nine_tenChem[c(1, 3, 10, 25)]

#engineering & Physics
chisq.test(nine_ten_eng$physicsSemPassed9, nine_ten_eng$engineering, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_eng$physicsSemPassed9 and nine_ten_eng$engineering
## X-squared = 106.17, df = NA, p-value = 0.0004998
```

```r
#engineering & chem
chisq.test(nine_ten_eng$ChemSemPassed10, nine_ten_eng$engineering, simulate.p.value = TRUE)
```

```
## 
## 	Pearson's Chi-squared test with simulated p-value (based on 2000
## 	replicates)
## 
## data:  nine_ten_eng$ChemSemPassed10 and nine_ten_eng$engineering
## X-squared = 28.197, df = NA, p-value = 0.006997
```

```r
# physics & engineering
library(ggplot2)
ggplot(nine_ten_eng, aes(physicsSemPassed9, engineering)) +
  geom_count()+
  geom_jitter(width=.2, height=.2) 
```

![](ML2015cohort_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
  #geom_jitter() # or try default


# chem & engineering
library(ggplot2)
ggplot(nine_ten_eng, aes(ChemSemPassed10, engineering)) +
  geom_count()+
  geom_jitter(width=.2, height=.2) 
```

![](ML2015cohort_files/figure-html/unnamed-chunk-31-2.png)<!-- -->

```r
  #geom_jitter() # or try default
```

### QUESTION:
- Can 9th grade grades predict success or failure in Biology?

```r
rm(list=ls())
nine_Ten_Eleven <- readRDS("nine_10_11_clean_2015.rds")
```


```r
# only complete cases
#nine_Ten_Eleven2 <- nine_Ten_Eleven[!(is.na(nine_Ten_Eleven$BioSemPassed11) & is.na(nine_Ten_Eleven$BioSemPassed11)),]
nine_Ten_Eleven2<- nine_Ten_Eleven[complete.cases(nine_Ten_Eleven),]

#remove Bio semesters passed11
nine_Ten_Eleven2 <- nine_Ten_Eleven2[-c(23,24)]
```


### Rpart decision tree For Chemistry Prediction

```r
# split train/test
# split into 70/30 random
set.seed(100)
train <- sample(nrow(nine_Ten_Eleven2), 0.7*nrow(nine_Ten_Eleven2), replace = FALSE)
TrainSet_11 <- nine_Ten_Eleven2[train,]
TestSet_11 <- nine_Ten_Eleven2[-train,]

#summary(TrainSet_11)
#summary(TestSet_11)
```
### Tuning the Complexity Parameter (cp) and using cross validation
- Using 10 fold cross validation
- Image for reference # https://www.edureka.co/blog/implementation-of-decision-tree/
- Want to select the cp that minimizes cross-validated error and maximizes accuracy
- Kappa compares how closely machine matches data labeled as truth, controlling for the accuracy of a random classificted as measured by the expected accuracy  # Reference https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
    + You can directly compare Kappa statistics

```r
library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(predBio ~ ., data = TrainSet_11, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

```
## CART 
## 
## 202 samples
##  22 predictor
##   2 classes: 'f', 'p' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 182, 181, 183, 182, 181, 182, ... 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy   Kappa     
##   0.01  0.7032331  0.32137077
##   0.02  0.7127569  0.33286848
##   0.03  0.7227569  0.35756543
##   0.04  0.7275188  0.37012700
##   0.05  0.7327820  0.38867881
##   0.06  0.7327820  0.38867881
##   0.07  0.7427820  0.41980846
##   0.08  0.7427820  0.41980846
##   0.09  0.7475439  0.43262897
##   0.10  0.7475439  0.43262897
##   0.11  0.7475439  0.43262897
##   0.12  0.7475439  0.43262897
##   0.13  0.7475439  0.43262897
##   0.14  0.7475439  0.43262897
##   0.15  0.7475439  0.43262897
##   0.16  0.7475439  0.43262897
##   0.17  0.7475439  0.43262897
##   0.18  0.7475439  0.43262897
##   0.19  0.7475439  0.43262897
##   0.20  0.7475439  0.43262897
##   0.21  0.7475439  0.43262897
##   0.22  0.7475439  0.43262897
##   0.23  0.7475439  0.43262897
##   0.24  0.7475439  0.43262897
##   0.25  0.7475439  0.43262897
##   0.26  0.7475439  0.43262897
##   0.27  0.7475439  0.43262897
##   0.28  0.7234962  0.33655054
##   0.29  0.7077068  0.27151987
##   0.30  0.6927068  0.21275698
##   0.31  0.6631830  0.09124620
##   0.32  0.6631830  0.09124620
##   0.33  0.6584211  0.05553191
##   0.34  0.6584211  0.00000000
##   0.35  0.6584211  0.00000000
##   0.36  0.6584211  0.00000000
##   0.37  0.6584211  0.00000000
##   0.38  0.6584211  0.00000000
##   0.39  0.6584211  0.00000000
##   0.40  0.6584211  0.00000000
##   0.41  0.6584211  0.00000000
##   0.42  0.6584211  0.00000000
##   0.43  0.6584211  0.00000000
##   0.44  0.6584211  0.00000000
##   0.45  0.6584211  0.00000000
##   0.46  0.6584211  0.00000000
##   0.47  0.6584211  0.00000000
##   0.48  0.6584211  0.00000000
##   0.49  0.6584211  0.00000000
##   0.50  0.6584211  0.00000000
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.27.
```

### Decision tree Model for Bio


```r
library(rpart)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
#model
rpartTree_11 <- rpart(predBio ~., data=TrainSet_11, method="class", cp=.27, parms= list(split="gini"))
#prediction
predictionCV_11 <- predict(rpartTree_11, newdata=TestSet_11, type="class")
table11 <- table(TestSet_11$predBio, predictionCV_11)


printcp(rpartTree_11)
```

```
## 
## Classification tree:
## rpart(formula = predBio ~ ., data = TrainSet_11, method = "class", 
##     parms = list(split = "gini"), cp = 0.27)
## 
## Variables actually used in tree construction:
## [1] ChemSemPassed10
## 
## Root node error: 69/202 = 0.34158
## 
## n= 202 
## 
##        CP nsplit rel error  xerror     xstd
## 1 0.30435      0   1.00000 1.00000 0.097684
## 2 0.27000      1   0.69565 0.94203 0.096226
```

```r
confusionMatrix(TestSet_11$predBio, predictionCV_11)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  f  p
##          f 22 12
##          p  7 46
##                                           
##                Accuracy : 0.7816          
##                  95% CI : (0.6802, 0.8631)
##     No Information Rate : 0.6667          
##     P-Value [Acc > NIR] : 0.0132          
##                                           
##                   Kappa : 0.5289          
##  Mcnemar's Test P-Value : 0.3588          
##                                           
##             Sensitivity : 0.7586          
##             Specificity : 0.7931          
##          Pos Pred Value : 0.6471          
##          Neg Pred Value : 0.8679          
##              Prevalence : 0.3333          
##          Detection Rate : 0.2529          
##    Detection Prevalence : 0.3908          
##       Balanced Accuracy : 0.7759          
##                                           
##        'Positive' Class : f               
## 
```

```r
sum(diag(table11))/sum(table11) #overall accuracy
```

```
## [1] 0.7816092
```

```r
1-sum(diag(table11))/sum(table11) #incorrect classification 
```

```
## [1] 0.2183908
```

```r
library(rpart.plot)
prp(rpartTree_11)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

```r
fancyRpartPlot(rpartTree_11)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-36-2.png)<!-- -->

### Can We predict Chemistry success from 9th grade classes ONLY (not including Chem)


```r
#remove Chem columns
nine_Ten_Eleven2_noChem <- nine_Ten_Eleven2[-c(21,22)]
```

### decision tree
- No CHEM trying to predict Bio


```r
# split train/test
# split into 70/30 random
set.seed(100)
train <- sample(nrow(nine_Ten_Eleven2_noChem), 0.7*nrow(nine_Ten_Eleven2_noChem), replace = FALSE)
TrainSet_11_2 <- nine_Ten_Eleven2_noChem[train,]
TestSet_11_2 <- nine_Ten_Eleven2_noChem[-train,]

#summary(TrainSet_11_2)
#summary(TestSet_11_2)
```



```r
library(caret)
library(e1071)

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(predBio ~ ., data = TestSet_11_2, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
```

```
## CART 
## 
## 87 samples
## 20 predictors
##  2 classes: 'f', 'p' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 79, 77, 78, 79, 78, 79, ... 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy   Kappa     
##   0.01  0.6694444  0.31222689
##   0.02  0.6694444  0.30151261
##   0.03  0.6694444  0.30151261
##   0.04  0.6583333  0.25074337
##   0.05  0.6583333  0.25074337
##   0.06  0.6583333  0.25074337
##   0.07  0.6583333  0.25074337
##   0.08  0.6583333  0.25074337
##   0.09  0.6583333  0.25074337
##   0.10  0.6583333  0.25074337
##   0.11  0.6583333  0.25074337
##   0.12  0.6583333  0.25074337
##   0.13  0.6583333  0.25074337
##   0.14  0.6583333  0.25074337
##   0.15  0.6583333  0.25074337
##   0.16  0.6583333  0.25074337
##   0.17  0.6583333  0.25074337
##   0.18  0.6583333  0.25074337
##   0.19  0.6583333  0.25074337
##   0.20  0.6138889  0.15074337
##   0.21  0.6138889  0.15074337
##   0.22  0.6138889  0.15074337
##   0.23  0.6138889  0.15074337
##   0.24  0.6138889  0.15074337
##   0.25  0.6013889  0.09780220
##   0.26  0.6013889  0.09780220
##   0.27  0.6013889  0.09780220
##   0.28  0.6013889  0.09780220
##   0.29  0.6013889  0.09780220
##   0.30  0.6013889  0.09780220
##   0.31  0.6013889  0.09780220
##   0.32  0.6013889  0.09780220
##   0.33  0.6000000  0.04857143
##   0.34  0.6000000  0.03857143
##   0.35  0.6000000  0.03857143
##   0.36  0.6000000  0.01000000
##   0.37  0.6000000  0.00000000
##   0.38  0.6000000  0.00000000
##   0.39  0.6000000  0.00000000
##   0.40  0.6100000  0.00000000
##   0.41  0.6100000  0.00000000
##   0.42  0.6100000  0.00000000
##   0.43  0.6100000  0.00000000
##   0.44  0.6100000  0.00000000
##   0.45  0.6100000  0.00000000
##   0.46  0.6100000  0.00000000
##   0.47  0.6100000  0.00000000
##   0.48  0.6100000  0.00000000
##   0.49  0.6100000  0.00000000
##   0.50  0.6100000  0.00000000
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.03.
```
### Building predicting Bio no Chem Model
- 67% accuracy
- Kappa is .234 - so better than predicting at random
- Neg preditive value is 87% is better than 35% positive predictor - better at determining students that will fail that pass
-  two best predictors are english semesters passed and physicstype9
- This model is WORSE in accuracy than using chem to predict success

```r
library(rpart)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
#model
rpartTree_11_2 <- rpart(predBio ~., data=TrainSet_11_2, method="class", cp=.03, parms= list(split="gini"))
#prediction
predictionCV_11_2 <- predict(rpartTree_11_2, newdata=TestSet_11_2, type="class")
table11_2 <- table(TestSet_11$predBio, predictionCV_11_2)


printcp(rpartTree_11_2)
```

```
## 
## Classification tree:
## rpart(formula = predBio ~ ., data = TrainSet_11_2, method = "class", 
##     parms = list(split = "gini"), cp = 0.03)
## 
## Variables actually used in tree construction:
## [1] englishSemPassed9 physicsType9     
## 
## Root node error: 69/202 = 0.34158
## 
## n= 202 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.318841      0   1.00000 1.00000 0.097684
## 2 0.072464      1   0.68116 0.73913 0.089485
## 3 0.030000      2   0.60870 0.73913 0.089485
```

```r
confusionMatrix(TestSet_11_2$predBio, predictionCV_11_2)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  f  p
##          f 12 22
##          p  7 46
##                                           
##                Accuracy : 0.6667          
##                  95% CI : (0.5575, 0.7642)
##     No Information Rate : 0.7816          
##     P-Value [Acc > NIR] : 0.99538         
##                                           
##                   Kappa : 0.2398          
##  Mcnemar's Test P-Value : 0.00933         
##                                           
##             Sensitivity : 0.6316          
##             Specificity : 0.6765          
##          Pos Pred Value : 0.3529          
##          Neg Pred Value : 0.8679          
##              Prevalence : 0.2184          
##          Detection Rate : 0.1379          
##    Detection Prevalence : 0.3908          
##       Balanced Accuracy : 0.6540          
##                                           
##        'Positive' Class : f               
## 
```

```r
sum(diag(table11_2))/sum(table11_2) #overall accuracy
```

```
## [1] 0.6666667
```

```r
1-sum(diag(table11_2))/sum(table11_2) #incorrect classification 
```

```
## [1] 0.3333333
```

```r
library(rpart.plot)
prp(rpartTree_11_2)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

```r
fancyRpartPlot(rpartTree_11_2)
```

![](ML2015cohort_files/figure-html/unnamed-chunk-40-2.png)<!-- -->
