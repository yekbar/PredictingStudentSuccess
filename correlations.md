---
title: "Chi Square Test of Independence for classes"
author: "kat"
date: "October 11, 2018"
output:
  html_document:
    keep_md: yes
---


### Purpose
- The purpose of this document is to expore relationships between the physics, chem, and bio classes

```r
rv.keep <- readRDS("rv.keepMASTER.rds")
#rv.keep
```
### Get 11th graders in 2017-2018

```r
full_11_2017 <- rv.keep[rv.keep$startYear == 2017,]
```

## #Biology Courses
- Bio courses
    + AP Biology, Biology, Biology 1, Biology 2, Honors Biology, Gen College Biology I w/Lab, Gen College Biology II w/Lab

```r
bio <- full_11_2017[ grepl("Biology" , full_11_2017$courseName ),] 
bioclasses <- unique(bio$courseName)
```

### only rows with bio classes

```r
full_11_2017_bio <- full_11_2017[full_11_2017$courseName %in% bioclasses,]
```

### Bio Grades Bar plot

```r
table_bio <- table(full_11_2017_bio$score)
barplot(table_bio, main = "grade distribution bio")
```

![](correlations_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Cleaning up Bio 
- Remove duplicated rows, spread & merge data
- Delete duplicate rows based on courseduplicator code 
- Convert data to wide

```r
bio_notdup <- full_11_2017_bio[!duplicated(full_11_2017_bio$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
bio_short <- bio_notdup[,c(2, 50, 52)]

#Below is all 2017 juniors and their bio classes spread 
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.4.4
```

```r
bio_wide <- spread(bio_short, courseIdentifier, scoreNum, fill=TRUE)
```

```
## Warning in `[<-.factor`(`*tmp*`, is.na(ordered), value = TRUE): invalid
## factor level, NA generated
```

```r
#bio_wide
```
## #create copies of bio columns just in case

```r
### CAREFUL WITH THIS!!!!!!!!!!!!! save as copy
bio_wideCopy <- bio_wide

bio_classes2 <- colnames(bio_wide[-c(1, 39,40, 41)])

# pass = 1, fail = 0
#cover all 1's to D's otherwise below wil not rewrite correctly
for (i in bio_classes2){
bio_wide[[i]] <- ifelse(bio_wide[[i]] == 4, 1,
                  ifelse(bio_wide[[i]] == 3, 1, 
                      ifelse(bio_wide[[i]] == 2, 1, 
                        ifelse(bio_wide[[i]] == 1, 0, 
                          ifelse(bio_wide[[i]] == "NA", "NA", 0)))))
}

#bio_wide
#bio_wideCopy

#convert semester classes differently
#- 4, 3, 2 = 2 credits
#- 1, 0 = 0 credits

semesterclasses <- c("XBIO111S1-Gen College Biology I w/Lab-S1", "XBIO111S2-Gen College Biology I w/Lab-S2", "XBIO112S2-Gen College Biology II w/Lab-S2")

#cover all 1's to D's otherwise below wil not rewrite correctly
for (i in semesterclasses){
bio_wide[[i]] <- ifelse(bio_wide[[i]] == 4, 2,
                  ifelse(bio_wide[[i]] == 3, 2, 
                      ifelse(bio_wide[[i]] == 2, 2, 
                        ifelse(bio_wide[[i]] == 1, 0, 
                          ifelse(bio_wide[[i]] == "NA", "NA", 0)))))
}
```


```r
#create row sum column
bio_wide$predSumBio <- rowSums(bio_wide[-1], na.rm=TRUE)
#bio_wide
```

### Bar plot of quarters 

```r
barplot(table(bio_wide$predSumBio),  main="Quarters passed of Biology, passed is C or greater")
```

![](correlations_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
barplot(prop.table(table(bio_wide$predSumBio)))
```

![](correlations_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

### BIO NUMBER OF QUARTERS PASSED

```r
bio_SUM <- bio_wide[c(1,42)]
#bio_SUM
```

### START CHEMISTRY, 2016 SOPHOMORES
####################### 


```r
full_10_2016 <- rv.keep[rv.keep$startYear == 2016,]
```

#Get Chemistry courses & Subset only Chem classes

```r
chem <- full_10_2016[ grepl("Chemistry" , full_10_2016$courseName ),] 
chemclasses <- unique(chem$courseName)
#chemclasses

full_10_2016_chem <- full_10_2016[full_10_2016$courseName %in% chemclasses,]
```

### Chem Grades Bar plot - ALL QUARTERS


```r
table_chem <- table(full_10_2016_chem$score)
barplot(table_chem)
```

![](correlations_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
### Cleaning up Chem
- Remove duplicated rows, spread & merge data
- Delete duplicate rows based on courseduplicator code 
- Convert data to wide

```r
chem_notdup <- full_10_2016_chem[!duplicated(full_10_2016_chem$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
chem_short <- chem_notdup[,c(2, 50, 52)]
#chem_short

library(tidyr)
chem_wide <- spread(chem_short, courseIdentifier, scoreNum, fill=TRUE)
```

```
## Warning in `[<-.factor`(`*tmp*`, is.na(ordered), value = TRUE): invalid
## factor level, NA generated
```

```r
#chem_wide
```
### Recode Chem score values as Pass/Fail (2 or greater = pass)

```r
chem_wideCopy <- chem_wide
chem_classes2 <- colnames(chem_wide[-c(1)])
# pass = 1, fail = 0
#cover all 1's to D's otherwise below wil not rewrite correctly
for (i in chem_classes2){
chem_wide[[i]] <- ifelse(chem_wide[[i]] == 4, 1,
                  ifelse(chem_wide[[i]] == 3, 1, 
                      ifelse(chem_wide[[i]] == 2, 1, 
                        ifelse(chem_wide[[i]] == 1, 0, 
                          ifelse(chem_wide[[i]] == "NA", "NA", 0)))))
}

#chem_wide
#chem_wideCopy
```

### Create row sums

```r
#create row sum column
chem_wide$predSumChem <- rowSums(chem_wide[-1], na.rm=TRUE) #minus personID column
#chem_wide
```


```r
#create row sum column
bio_wide$predSumBio <- rowSums(bio_wide[-1], na.rm=TRUE)
#bio_wide
```

### Bar plot of quarters 

```r
barplot(table(chem_wide$predSumChem), main="Total Quarters Passed in Chem, passing = C")
```

![](correlations_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
barplot(prop.table(table(chem_wide$predSumChem)))
```

![](correlations_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

### CHEM NUMBER OF QUARTERS PASSED

```r
chem_SUM <- chem_wide[c(1,26)]
```

### START OF 2015 PHYSICS
##################################### 

### get 9th graders in 2015-2016 physivs

```r
full_9_2015 <- rv.keep[rv.keep$startYear == 2015,]
```


### Physics Courses


```r
physics <- full_9_2015[ grepl("Physics" , full_9_2015$courseName ),] 
physicsclasses<- unique(physics$courseName)
physicsclasses
```

```
## [1] "Conceptual Physics"             "AP Physics"                    
## [3] "Physics"                        "Physics 2"                     
## [5] "General Physics I"              "General Physics Lab 1"         
## [7] "Honors Physics 1"               "Physics 1"                     
## [9] "Physics:Algebra Based II w/Lab"
```

### only rows with physics classes

```r
full_9_2015_phy <- full_9_2015[full_9_2015$courseName %in% physicsclasses,]
```
### Bio Grades Bar plot

```r
table_phys <- table(full_9_2015_phy$score)
barplot(table_phys, main="grade distribution physics")
```

![](correlations_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
### cleaning up Physics
- Remove duplicated rows, spread & merge data
- Delete duplicate rows based on courseduplicator code 
- Convert data to wide

```r
phys_notdup <- full_9_2015_phy[!duplicated(full_9_2015_phy$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
phy_short <- phys_notdup[,c(2, 50, 52)]

#Below is all 2017 juniors and their bio classes spread 
library(tidyr)
phy_wide <- spread(phy_short, courseIdentifier, scoreNum, fill=TRUE)
```

```
## Warning in `[<-.factor`(`*tmp*`, is.na(ordered), value = TRUE): invalid
## factor level, NA generated
```

```r
#phy_wide
```

`


```r
#copy to compare scores
phy_wideCopy <- phy_wide

phy_classes2 <- colnames(phy_wide[-c(1)])

# pass = 1, fail = 0
#cover all 1's to D's otherwise below wil not rewrite correctly
for (i in phy_classes2){
phy_wide[[i]] <- ifelse(phy_wide[[i]] == 4, 1,
                  ifelse(phy_wide[[i]] == 3, 1, 
                      ifelse(phy_wide[[i]] == 2, 1, 
                        ifelse(phy_wide[[i]] == 1, 0, 
                          ifelse(phy_wide[[i]] == "NA", "NA", 0)))))
}

#phy_wide
#phy_wideCopy
```


### Create row sums

```r
#create row sum column
phy_wide$predSumPhy <- rowSums(phy_wide[-1], na.rm=TRUE) #minus personID column
#phy_wide
```

### Bar plot of quarters 

```r
barplot(table(phy_wide$predSumPhy), main="Total Quarters Passed in Physics, passing = C")
```

![](correlations_files/figure-html/unnamed-chunk-27-1.png)<!-- -->


### PHYSICS NUMBER OF QUARTERS PASSED

```r
phy_SUM <- phy_wide[c(1,29)]
```

### COMBINING ALL THREE CLASSES
############# 


```r
#phy_SUM
#bio_SUM
#chem_SUM
one <- merge(phy_SUM, bio_SUM, by="personID", all=TRUE)
two <- merge(one, chem_SUM, by="personID", all=TRUE)
#two
two_numeric <- two
```

### ONLY 319 complete cases for now!!

```r
complete <- two[complete.cases(two),]
#complete
```

### Figure out how many non-NA entries  there are
- have trouble using complete cases for this section to remove them all so using above

```r
as.data.frame(sapply(two[-1], function(x) {
  factor(x, exclude=NULL) }))
two$predSumPhy <- factor(two$predSumPhy, exclude=NULL)
two$predSumBio <- factor(two$predSumBio, exclude=NULL)
two$predSumChem <- factor(two$predSumChem, exclude=NULL)
levels(two$predSumBio)
#summary(two)
```

### getting column means & medians


```r
library(robustbase)
```

```
## Warning: package 'robustbase' was built under R version 3.4.4
```

```r
#complete
colMeans(complete[-1])
```

```
##  predSumPhy  predSumBio predSumChem 
##    3.282132    3.231975    3.194357
```

```r
colMedians(as.matrix(complete[-1]))
```

```
##  predSumPhy  predSumBio predSumChem 
##           4           4           4
```

```r
#summary(complete)
#str(complete)
```

###Ho: grade in physics is independent of grade in bio
###Ha: grade in physics is not independent of grade in bio

- Grades in Physics and Bio are not independent of each

```r
tbl <- table(complete$predSumPhy, complete$predSumBio)
chisq.test(tbl)
```

```
## Warning in chisq.test(tbl): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl
## X-squared = 80.929, df = 16, p-value = 1.131e-10
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0   4   3   0   7   5
##   1   3   2   4   4   5
##   2   1   7   4   5   9
##   3   5   4   9   5  24
##   4   6   6  10  28 159
```

```r
ctbl <- cbind(tbl[,"0"]+ tbl[,"1"], tbl[,"2"] , tbl[,"3"] ,tbl[,"4"])
ctbl <- rbind(ctbl["0",]+ ctbl["1",], ctbl["2",] ,ctbl["3",] , ctbl["4",])
ctbl
```

```
##      [,1] [,2] [,3] [,4]
## [1,]   12    4   11   10
## [2,]    8    4    5    9
## [3,]    9    9    5   24
## [4,]   12   10   28  159
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0   4   3   0   7   5
##   1   3   2   4   4   5
##   2   1   7   4   5   9
##   3   5   4   9   5  24
##   4   6   6  10  28 159
```

```r
chisq.test(ctbl)
```

```
## Warning in chisq.test(ctbl): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  ctbl
## X-squared = 62.596, df = 9, p-value = 4.224e-10
```

### PHysics & Bio WITH ALL VALUES NOT JUST complete cases



```r
tbl2 <- table(two_numeric$predSumPhy, two_numeric$predSumBio)
chisq.test(tbl2)
```

```
## Warning in chisq.test(tbl2): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl2
## X-squared = 82.993, df = 16, p-value = 4.785e-11
```


### Physics and Chemistry are not independent of each other
- More so than Phy & Bio

```r
tbl <- table(complete$predSumPhy, complete$predSumChem)
chisq.test(tbl)
```

```
## Warning in chisq.test(tbl): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl
## X-squared = 111.53, df = 16, p-value = 2.297e-16
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0   9   3   1   2   4
##   1   4   2   4   4   4
##   2   2   4   7   4   9
##   3   6   7   5   5  24
##   4   4   5  11  23 166
```

```r
ctbl <- cbind(tbl[,"0"]+ tbl[,"1"]+ tbl[,"2"] , tbl[,"3"] ,tbl[,"4"])
ctbl <- rbind(ctbl["0",]+ ctbl["1",]+ctbl["2",] ,ctbl["3",] , ctbl["4",])
sum(ctbl)
```

```
## [1] 319
```

```r
ctbl
```

```
##      [,1] [,2] [,3]
## [1,]   36   10   17
## [2,]   18    5   24
## [3,]   20   23  166
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0   9   3   1   2   4
##   1   4   2   4   4   4
##   2   2   4   7   4   9
##   3   6   7   5   5  24
##   4   4   5  11  23 166
```

```r
chisq.test(ctbl)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  ctbl
## X-squared = 75.825, df = 4, p-value = 1.333e-15
```
### Chem & Bio
- Not independent of each other 


```r
tbl <- table(complete$predSumChem, complete$predSumBio)
chisq.test(tbl)
```

```
## Warning in chisq.test(tbl): Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tbl
## X-squared = 175.52, df = 16, p-value < 2.2e-16
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0  13   3   1   6   2
##   1   2   5   3   3   8
##   2   3   5   5   8   7
##   3   1   3   4  12  18
##   4   0   6  14  20 167
```

```r
ctbl <- cbind(tbl[,"0"]+ tbl[,"1"]+ tbl[,"2"] , tbl[,"3"] ,tbl[,"4"])
ctbl <- rbind(ctbl["0",]+ ctbl["1",] + ctbl["2",] ,ctbl["3",] , ctbl["4",])
ctbl
```

```
##      [,1] [,2] [,3]
## [1,]   40   17   17
## [2,]    8   12   18
## [3,]   20   20  167
```

```r
sum(ctbl)
```

```
## [1] 319
```

```r
tbl
```

```
##    
##       0   1   2   3   4
##   0  13   3   1   6   2
##   1   2   5   3   3   8
##   2   3   5   5   8   7
##   3   1   3   4  12  18
##   4   0   6  14  20 167
```

```r
chisq.test(ctbl)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  ctbl
## X-squared = 94.47, df = 4, p-value < 2.2e-16
```


```r
chem.bio <- as.matrix(tbl)
rownames(chem.bio) <- paste('chem',0:4)
colnames(chem.bio) <- paste('bio',0:4)
chem.bio
```

```
##         
##          bio 0 bio 1 bio 2 bio 3 bio 4
##   chem 0    13     3     1     6     2
##   chem 1     2     5     3     3     8
##   chem 2     3     5     5     8     7
##   chem 3     1     3     4    12    18
##   chem 4     0     6    14    20   167
```


```r
library(rcompanion)
```

```
## Warning: package 'rcompanion' was built under R version 3.4.4
```

```r
results <- pairwiseNominalIndependence(chem.bio, fisher=FALSE, gtest=FALSE, chisq = TRUE, method="fdr")
```

```
## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect
```

```
## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect

## Warning in chisq.test(Dataz, ...): Chi-squared approximation may be
## incorrect
```

```r
results[results$p.adj.Chisq < .05,]
```

```
##         Comparison  p.Chisq p.adj.Chisq
## 1  chem 0 : chem 1 7.54e-03    1.26e-02
## 2  chem 0 : chem 2 1.49e-02    2.13e-02
## 3  chem 0 : chem 3 4.42e-05    1.10e-04
## 4  chem 0 : chem 4 9.19e-28    9.19e-27
## 7  chem 1 : chem 4 8.76e-09    2.92e-08
## 9  chem 2 : chem 4 2.61e-11    1.30e-10
## 10 chem 3 : chem 4 5.59e-05    1.12e-04
```

```r
results[!results$p.adj.Chisq < .05,]
```

```
##        Comparison p.Chisq p.adj.Chisq
## 5 chem 1 : chem 2   0.721       0.721
## 6 chem 1 : chem 3   0.214       0.238
## 8 chem 2 : chem 3   0.209       0.238
```

### THIS PLOTS ALL STUDENTS, NOT JUST COMPLETE CASES!!!


```r
phybioTable <- table(two$predSumPhy, two$predSumBio)
sum(phybioTable)
```

```
## [1] 321
```

```r
twocomplete <- two[complete.cases(two$predSumPhy & two$predSumBio),]
#How many students did better in Physics than Bio? 80
sum(twocomplete$predSumPhy > twocomplete$predSumBio, na.rm=TRUE)
```

```
## [1] 80
```

```r
# How many students did better in Bio than Physics 66
sum(twocomplete$predSumPhy < twocomplete$predSumBio, na.rm=TRUE)
```

```
## [1] 66
```

```r
phybioTable <- cbind(phybioTable[,"0"]+ phybioTable[,"1"] + phybioTable[,"2"] , phybioTable[,"3"] ,phybioTable[,"4"])
phybioTable <- rbind(phybioTable["0",]+ phybioTable["1",] + phybioTable["2",] ,phybioTable["3",] , phybioTable["4",])
phybioTable
```

```
##      [,1] [,2] [,3]
## [1,]   30   16   19
## [2,]   18    5   24
## [3,]   22   28  159
```


```r
#reference https://ggplot2.tidyverse.org/reference/geom_count.html
# reference https://ggplot2.tidyverse.org/reference/geom_point.html
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
ggplot(two, aes(predSumChem, predSumBio)) +
  geom_count(colour="gray")+
  geom_point(colour="gray")+
  geom_jitter(width=.2, height=.2) +
  geom_abline(intercept=0, slope=1, color="red", size=2)+
 geom_jitter(width=.2, height=.2, colour=ifelse((two$predSumBio==2 & two$predSumChem==4) |
                                                  (two$predSumBio==3 & two$predSumChem==4) |
                                                  (two$predSumBio==4 & two$predSumChem==3) |
                                                   (two$predSumBio==4 & two$predSumChem==4) , "blue", "gray"))
```

```
## Warning: Removed 588 rows containing non-finite values (stat_sum).
```

```
## Warning: Removed 588 rows containing missing values (geom_point).

## Warning: Removed 588 rows containing missing values (geom_point).

## Warning: Removed 588 rows containing missing values (geom_point).
```

![](correlations_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

```r
ggplot(two, aes(predSumChem, predSumPhy)) +
  geom_count(colour="gray")+
  geom_point(colour="gray") +
  geom_abline(intercept=0, slope=1, color="red", size=2)+
  geom_jitter(width=.2, height=.2, colour=ifelse((two$predSumPhy==4 & two$predSumChem==4) |
                                                  (two$predSumPhy==3 & two$predSumChem==4) |
                                                  (two$predSumPhy==4 & two$predSumChem==3) |
                                                   (two$predSumPhy==4 & two$predSumChem==2) , "blue", "gray")) 
```

```
## Warning: Removed 570 rows containing non-finite values (stat_sum).
```

```
## Warning: Removed 570 rows containing missing values (geom_point).

## Warning: Removed 570 rows containing missing values (geom_point).
```

![](correlations_files/figure-html/unnamed-chunk-40-2.png)<!-- -->

```r
ggplot(two, aes(predSumPhy, predSumBio)) +
  geom_point(colour="gray")+
  geom_count(colour="gray")+
  geom_abline(intercept=0, slope=1, color="red", size=2)+
  geom_jitter(width=.2, height=.2, colour=ifelse((two$predSumPhy==4 & two$predSumBio==4) |
                                                  (two$predSumPhy==3 & two$predSumBio==4) |
                                                  (two$predSumPhy==4 & two$predSumBio==3) , "blue", "gray")) 
```

```
## Warning: Removed 683 rows containing non-finite values (stat_sum).
```

```
## Warning: Removed 683 rows containing missing values (geom_point).

## Warning: Removed 683 rows containing missing values (geom_point).
```

![](correlations_files/figure-html/unnamed-chunk-40-3.png)<!-- -->

### bar plot of grade distributions

```r
par(mfrow=c(2,2))
barplot(prop.table(table(chem_wide$predSumChem)), border="green")

barplot(prop.table(table(bio_wide$predSumBio)), col="red")
barplot(prop.table(table(phy_wide$predSumPhy)))
```

![](correlations_files/figure-html/unnamed-chunk-41-1.png)<!-- -->
