---
title: "2015 Cohort Cleaning & Preprocessing"
author: "kat"
date: "October 13, 2018"
output:
  html_document:
    keep_md: yes
---


### Purpose
- The purpose of this file is create a complete 9th grader 2015 data set for physics, Chemistry and Bio classes as well as freshman classes and demographic information. Output of this file were saved as .rds and used in ML2015cohor.rmd

```r
rv.keep <- readRDS("rv.keepMASTER.rds")
```

###9th grade- 573 Students in 2015

```r
nine <- rv.keep[rv.keep$startYear ==2015 & rv.keep$grade == 9, ]
#nine
nineU <- unique(nine$personID)
length(nineU)
```

```
## [1] 573
```

### Cleaning & Spreading Math Classes data
- 269 duplicate rows removed
- spread math classes
- change certain columns to factors
- recode grades per quarter and add together for semester passed
    + change grades to pass/fail 0, 1 = failed a semester

```r
#subset algebra classes only
algebra <- nine[grep("Alg", nine$courseName), ]
table(algebra$courseName)
```

```
## 
##               College Algebra Honors Int Algebra/Geo/Trig 3 
##                             2                            56 
## Honors Int Algebra/Geometry 2             IntegratedAlgGeo1 
##                           552                          1711 
##             IntegratedAlgGeo2 
##                            57
```

```r
#remove all duplicate rows based on whether they have teh same course duplicater code #http://www.sthda.com/english/wiki/identifying-and-removing-duplicate-data-in-r
#algebra
algrebra_nodup <- algebra[!duplicated(algebra$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
algebra_noup_short <- algrebra_nodup[,c(2, 50, 52)]

#Below is all 2015 freshman and MATH spread
library(tidyr)
nine_spread <- spread(algebra_noup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert columns to numeric from factor
cols <- c(2:25)
nine_spread[,cols] <- apply(nine_spread[cols], 2, function(x) as.numeric(as.character(x)))


#####regular COre 1
regularC1 <- c("HMA1010Q1-IntegratedAlgGeo1-NULL", "HMA1010Q2-IntegratedAlgGeo1-NULL", "HMA1010Q3-IntegratedAlgGeo1-NULL", "HMA1010Q4-IntegratedAlgGeo1-NULL")

for (i in regularC1){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 1,
                  ifelse(nine_spread[[i]] == 3, 1, 
                      ifelse(nine_spread[[i]] == 2, 1, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}

# Add regular columns together
#create Core 1 # of semesters passed
nine_spread$core1 <- rowSums(nine_spread[,c(2:5)], na.rm=TRUE)

#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread$core1[( is.na(nine_spread[[2]]) & is.na(nine_spread[[3]])  & is.na(nine_spread[[4]]) & is.na(nine_spread[[5]])  )] <- NA


#####Honors Core 2
honorsC2 <- c("HMA1012Q1-Honors Int Algebra/Geometry 2-NULL","HMA1012Q2-Honors Int Algebra/Geometry 2-NULL" , "HMA1012Q3-Honors Int Algebra/Geometry 2-NULL","HMA1012Q4-Honors Int Algebra/Geometry 2-NULL"  )

for (i in honorsC2){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 1,
                  ifelse(nine_spread[[i]] == 3, 1, 
                      ifelse(nine_spread[[i]] == 2, 1, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}
#create Honors  Core 2 # of semesters passed
nine_spread$hcore2 <- rowSums(nine_spread[,c(6:9)], na.rm=TRUE)

nine_spread$hcore2[( is.na(nine_spread[[6]]) & is.na(nine_spread[[7]])  & is.na(nine_spread[[8]]) & is.na(nine_spread[[9]])  )] <- NA


######Regular Core 2
regularC2 <- c("HMA1013Q1-IntegratedAlgGeo2-NULL", "HMA1013Q2-IntegratedAlgGeo2-NULL" ,"HMA1013Q3-IntegratedAlgGeo2-NULL", "HMA1013Q4-IntegratedAlgGeo2-NULL" )

for (i in regularC2){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 1,
                  ifelse(nine_spread[[i]] == 3, 1, 
                      ifelse(nine_spread[[i]] == 2, 1, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}

#create Regular  Core 2 # of semesters passed
nine_spread$rcore2 <- rowSums(nine_spread[,c(10 :13)], na.rm=TRUE)

nine_spread$rcore2[( is.na(nine_spread[[10]]) & is.na(nine_spread[[11]])  & is.na(nine_spread[[12]]) & is.na(nine_spread[[13]])  )] <- NA



#######Honors Core 3
honorsC3 <- c("HMA1015Q1-Honors Int Algebra/Geo/Trig 3-NULL", "HMA1015Q2-Honors Int Algebra/Geo/Trig 3-NULL", "HMA1015Q3-Honors Int Algebra/Geo/Trig 3-NULL" , "HMA1015Q4-Honors Int Algebra/Geo/Trig 3-NULL")

for (i in honorsC3){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 1,
                  ifelse(nine_spread[[i]] == 3, 1, 
                      ifelse(nine_spread[[i]] == 2, 1, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}

#create Honors Core 3 # of semesters passed
nine_spread$honorsC3 <- rowSums(nine_spread[,c(14 :17)], na.rm=TRUE)

nine_spread$honorsC3[( is.na(nine_spread[[14]]) & is.na(nine_spread[[15]])  & is.na(nine_spread[[16]]) & is.na(nine_spread[[17]])  )] <- NA


#####Sheltered Core 1
shelteredC1 <- c("HMAS1010Q1-IntegratedAlgGeo1-NULL", "HMAS1010Q2-IntegratedAlgGeo1-NULL", "HMAS1010Q3-IntegratedAlgGeo1-NULL", "HMAS1010Q4-IntegratedAlgGeo1-NULL")

for (i in shelteredC1){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 1,
                  ifelse(nine_spread[[i]] == 3, 1, 
                      ifelse(nine_spread[[i]] == 2, 1, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}

nine_spread$sheltC1 <- rowSums(nine_spread[,c(18 :21)], na.rm=TRUE)

nine_spread$sheltC1[( is.na(nine_spread[[18]]) & is.na(nine_spread[[19]])  & is.na(nine_spread[[20]]) & is.na(nine_spread[[21]])  )] <- NA


### There is 1 student in Sheltered Core 2 - Omitting this!!!

######college Alg Core 1
collegealg <- c("MATX121Q1-College Algebra-NULL", "MATX121Q2-College Algebra-NULL")


for (i in collegealg){
nine_spread[[i]] <- ifelse(nine_spread[[i]] == 4, 2,
                  ifelse(nine_spread[[i]] == 3, 2, 
                      ifelse(nine_spread[[i]] == 2, 2, 
                        ifelse(nine_spread[[i]] == 1, 0, 
                          ifelse(nine_spread[[i]] == "NA", NA, 0)))))
}
nine_spread$collegealg <- rowSums(nine_spread[,c(24 :25)], na.rm=TRUE)

nine_spread$collegealg[( is.na(nine_spread[[24]]) & is.na(nine_spread[[25]])  )] <- NA

# Taking only new columns
#- grab only only new sum columns
#- create "type" column to indicate math type class
#- nine_math df will be merged with nine_english, etc for ML later

#nine_spread
nine_spread_short <- nine_spread[,c(1, 26:31)]

# create Math type column & Total math semesters passed in 9th grade
# new math type column EMPTY
nine_spread_short$mathType9 <- NA

#create math type column
nine_spread_short$mathType9[nine_spread_short$core1 >= 0] <- "core1"
nine_spread_short$mathType9[nine_spread_short$hcore2 >= 0] <- "hcore2"
nine_spread_short$mathType9[nine_spread_short$core2 >= 0] <- "core2"
nine_spread_short$mathType9[nine_spread_short$shelteredC1 >= 0] <- "sheltC1"
nine_spread_short$mathType9[nine_spread_short$honorsC3 >= 0] <- "honorsC3"
nine_spread_short$mathType9[nine_spread_short$collegealg >= 0] <- "collegealg"

#create row sum column for "math semesters passed in 9th grade"
nine_spread_short$mathSemPassed9 <- rowSums(nine_spread_short[,-c(1,8)], na.rm=TRUE)

#############make a copy for math 9th grade
nine_math <- nine_spread_short
saveRDS(nine_math[c(1,8,9)], file="nine_math.rds")
####################
```

### START ENGLISH
############ 


```r
english <- nine[grep("English", nine$courseName), ]
table(english$courseName)
```

```
## 
##                  English 9           Honors English 9 
##                       1557                        771 
## Reading - English Skills 9 
##                         46
```

```r
#test <- algebra[grep("Honors Int Algebra/Geometry 2", algebra$courseName),]
#unique(test$personID)

# Cleaning & Spreading English Classes data

#remove all duplicate rows based on whether they have teh same course duplicater code #http://www.sthda.com/english/wiki/identifying-and-removing-duplicate-data-in-r
#english
english_nodup <- english[!duplicated(english$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
english_noup_short <- english_nodup[,c(2, 50, 52)]

#Below is all 2015 freshman and MATH spread
library(tidyr)
nine_spread_eng <- spread(english_noup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert a few columns to factor
cols <- c(2:17)
nine_spread_eng[,cols] <- apply(nine_spread_eng[cols], 2, function(x) as.numeric(as.character(x)))

#####Regular English 1
regularEng <- c("HLA1012Q1-English 9-NULL","HLA1012Q2-English 9-NULL", "HLA1012Q3-English 9-NULL",
                "HLA1012Q4-English 9-NULL")
for (i in regularEng){
nine_spread_eng[[i]] <- ifelse(nine_spread_eng[[i]] == 4, 1,
                  ifelse(nine_spread_eng[[i]] == 3, 1, 
                      ifelse(nine_spread_eng[[i]] == 2, 1, 
                        ifelse(nine_spread_eng[[i]] == 1, 0, 
                          ifelse(nine_spread_eng[[i]] == "NA", NA, 0)))))
}
#create sum # of semesters passed
nine_spread_eng$regularEng <- rowSums(nine_spread_eng[,c(2 :5)], na.rm=TRUE)

nine_spread_eng$regularEng[( is.na(nine_spread_eng[[2]]) & is.na(nine_spread_eng[[3]])  & is.na(nine_spread_eng[[4]]) & is.na(nine_spread_eng[[5]])  )] <- NA

#####Honors English 1
honorsEng <- c("HLA1022Q2-Honors English 9-NULL","HLA1022Q1-Honors English 9-NULL","HLA1022Q3-Honors English 9-NULL", "HLA1022Q4-Honors English 9-NULL" )
for (i in honorsEng){
nine_spread_eng[[i]] <- ifelse(nine_spread_eng[[i]] == 4, 1,
                  ifelse(nine_spread_eng[[i]] == 3, 1, 
                      ifelse(nine_spread_eng[[i]] == 2, 1, 
                        ifelse(nine_spread_eng[[i]] == 1, 0, 
                          ifelse(nine_spread_eng[[i]] == "NA", NA, 0)))))
}
nine_spread_eng$honorsEng <- rowSums(nine_spread_eng[,c(6 :9)], na.rm=TRUE)

nine_spread_eng$honorsEng[( is.na(nine_spread_eng[[6]]) & is.na(nine_spread_eng[[7]])  & is.na(nine_spread_eng[[8]]) & is.na(nine_spread_eng[[9]])  )] <- NA

#####ELD English 1
EldEng <- c("HLAELD1150Q1-English 9-NULL","HLAELD1150Q2-English 9-NULL", "HLAELD1150Q3-English 9-NULL", "HLAELD1150Q4-English 9-NULL"  )
for (i in EldEng){
nine_spread_eng[[i]] <- ifelse(nine_spread_eng[[i]] == 4, 1,
                  ifelse(nine_spread_eng[[i]] == 3, 1, 
                      ifelse(nine_spread_eng[[i]] == 2, 1, 
                        ifelse(nine_spread_eng[[i]] == 1, 0, 
                          ifelse(nine_spread_eng[[i]] == "NA", NA, 0)))))
}
nine_spread_eng$EldEng <- rowSums(nine_spread_eng[,c(10 :13)], na.rm=TRUE)

nine_spread_eng$EldEng[( is.na(nine_spread_eng[[10]]) & is.na(nine_spread_eng[[11]])  & is.na(nine_spread_eng[[12]]) & is.na(nine_spread_eng[[13]])  )] <- NA

#####English 1 Skills OMITTED BECAUSE THIS SUPPLEMENT TO REGULAR ENGLISH CLASS

# Taking only new columns
#- grab only only new sum columns
#- create "type" column to indicate english type class
#- nine_english df will be merged with nine_math, etc for ML later

#nine_spread_eng
nine_spread_eng_short <- nine_spread_eng[,c(1, 18:20)]

# create English type column & Total English semesters passed in 9th grade
# new English type column EMPTY
nine_spread_eng_short$englishType9 <- NA

#create English type column
nine_spread_eng_short$englishType9[nine_spread_eng_short$regularEng >= 0] <- "regEng"
nine_spread_eng_short$englishType9[nine_spread_eng_short$honorsEng >= 0] <- "HEng"
nine_spread_eng_short$englishType9[nine_spread_eng_short$EldEng >= 0] <- "ELDEng"
nine_spread_eng_short$englishType9[nine_spread_eng_short$EngSkill >= 0] <- "EngSkill"

#create row sum column for "english semesters passed in 9th grade"
nine_spread_eng_short$englishSemPassed9 <- rowSums(nine_spread_eng_short[,-c(1,5)], na.rm=TRUE)

#############make a copy for english 9th grade
nine_english <- nine_spread_eng_short
saveRDS(nine_english[c(1,5,6)], file="nine_english.rds")
####################
```
## START SOCIAL STUDIES
- Cleaning & Spreading Social Studies Classes data

```r
#unique(nine$courseName)
#nine[grep("History", nine$courseName), ]

#why did grepl work instead of grep?
history <- nine[(grepl( "AP Human Geography", nine$courseName)  | grepl( "Geography", nine$courseName)) ,]

history_nodup <- history[!duplicated(history$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
history_noup_short <- history_nodup[,c(2, 50, 52)]

#Below is all 2015 freshman and HISTORY spread
library(tidyr)
nine_spread_hist <- spread(history_noup_short, courseIdentifier, scoreNum, fill=TRUE)
#nine_spread

#convert columns to numeric from factor
cols <- c(2:ncol(nine_spread_hist))
nine_spread_hist[,cols] <- apply(nine_spread_hist[cols], 2, function(x) as.numeric(as.character(x)))


########### Geography
geo <- c("HSS1011Q1-Geography-NULL", "HSS1011Q2-Geography-NULL","HSS1011Q3-Geography-NULL", "HSS1011Q4-Geography-NULL" )

for (i in geo){
nine_spread_hist[[i]] <- ifelse(nine_spread_hist[[i]] == 4, 1,
                  ifelse(nine_spread_hist[[i]] == 3, 1, 
                      ifelse(nine_spread_hist[[i]] == 2, 1, 
                        ifelse(nine_spread_hist[[i]] == 1, 0, 
                          ifelse(nine_spread_hist[[i]] == "NA", NA, 0)))))
}
nine_spread_hist$geo <- rowSums(nine_spread_hist[,c(2:5)], na.rm=TRUE)

#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_hist$geo[( is.na(nine_spread_hist[[2]]) & is.na(nine_spread_hist[[3]])  & is.na(nine_spread_hist[[4]]) & is.na(nine_spread_hist[[5]])  )] <- NA



########### APGeography
APgeo <- c("HSS1026Q1-AP Human Geography-NULL","HSS1026Q2-AP Human Geography-NULL", "HSS1026Q3-AP Human Geography-NULL","HSS1026Q4-AP Human Geography-NULL"  )

for (i in APgeo){
nine_spread_hist[[i]] <- ifelse(nine_spread_hist[[i]] == 4, 1,
                  ifelse(nine_spread_hist[[i]] == 3, 1, 
                      ifelse(nine_spread_hist[[i]] == 2, 1, 
                        ifelse(nine_spread_hist[[i]] == 1, 0, 
                          ifelse(nine_spread_hist[[i]] == "NA", NA, 0)))))
}
nine_spread_hist$APgeo <- rowSums(nine_spread_hist[,c(6:9)], na.rm=TRUE)

#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_hist$APgeo[( is.na(nine_spread_hist[[6]]) & is.na(nine_spread_hist[[7]])  & is.na(nine_spread_hist[[8]]) & is.na(nine_spread_hist[[9]])  )] <- NA


########### Sheltered Geo
SheltGeo <- c("HSSS1011Q1-Geography-NULL","HSSS1011Q2-Geography-NULL", "HSSS1011Q3-Geography-NULL", "HSSS1011Q4-Geography-NULL" )

for (i in SheltGeo){
nine_spread_hist[[i]] <- ifelse(nine_spread_hist[[i]] == 4, 1,
                  ifelse(nine_spread_hist[[i]] == 3, 1, 
                      ifelse(nine_spread_hist[[i]] == 2, 1, 
                        ifelse(nine_spread_hist[[i]] == 1, 0, 
                          ifelse(nine_spread_hist[[i]] == "NA", NA, 0)))))
}
nine_spread_hist$SheltGeo <- rowSums(nine_spread_hist[,c(13:16)], na.rm=TRUE)

#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_hist$SheltGeo[( is.na(nine_spread_hist[[13]]) & is.na(nine_spread_hist[[14]])  & is.na(nine_spread_hist[[15]]) & is.na(nine_spread_hist[[16]])  )] <- NA


# Taking only new columns
#- grab only only new sum columns
#- create "type" column to indicate history type class
#- nine_history df will be merged with nine_math, etc for ML later

#nine_spread_hist
nine_spread_hist_short <- nine_spread_hist[,c(1, 17:19)]

# create History type column & Total History semesters passed in 9th grade
# new History type column EMPTY
nine_spread_hist_short$historyType9 <- NA

#create English type column
nine_spread_hist_short$historyType9[nine_spread_hist_short$geo >= 0] <- "geo"
nine_spread_hist_short$historyType9[nine_spread_hist_short$APgeo >= 0] <- "APgeo"
nine_spread_hist_short$historyType9[nine_spread_hist_short$SheltGeo >= 0] <- "Sheltgeo"

#create row sum column for "english semesters passed in 9th grade"
nine_spread_hist_short$historySemPassed9 <- rowSums(nine_spread_hist_short[,-c(1,5)], na.rm=TRUE)

#############make a copy for history 9th grade
nine_history <- nine_spread_hist_short
saveRDS(nine_history[c(1, 5, 6)], file="nine_history.rds")
####################
```



## Cleaning & Spreading Physics Classes data
############### Physics
- duplicates removed
- spread physics classes
- change certain columns to factors
- recode grades per quarter and add together for semester passed
    + change grades to pass/fail 0, 1 = failed a semester

```r
physics <- (nine[grep("Physics", nine$courseName), ])
table(physics$courseName)
```

```
## 
## Honors Physics 1          Physics        Physics 1 
##              506                2             1631
```

```r
physics_nodup <- physics[!duplicated(physics$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
physics_nodup_short <- physics_nodup[,c(2, 50, 52)]

#Below is all 2015 freshman and physics spread
library(tidyr)
nine_spread_phy <- spread(physics_nodup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert columns to numeric from factor
cols <- c(2:ncol(nine_spread_phy))
nine_spread_phy[,cols] <- apply(nine_spread_phy[cols], 2, function(x) as.numeric(as.character(x)))

##### Physics1
physics <- c("HSC1009Q1-Physics 1-NULL","HSC1009Q2-Physics 1-NULL", "HSC1009Q3-Physics 1-NULL", "HSC1009Q4-Physics 1-NULL"  )
for (i in physics){
nine_spread_phy[[i]] <- ifelse(nine_spread_phy[[i]] == 4, 1,
                  ifelse(nine_spread_phy[[i]] == 3, 1, 
                      ifelse(nine_spread_phy[[i]] == 2, 1, 
                        ifelse(nine_spread_phy[[i]] == 1, 0, 
                          ifelse(nine_spread_phy[[i]] == "NA", NA, 0)))))
}
nine_spread_phy$physics <- rowSums(nine_spread_phy[,c(2:5)], na.rm=TRUE)
#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_phy$physics[( is.na(nine_spread_phy[[2]]) & is.na(nine_spread_phy[[3]])  & is.na(nine_spread_phy[[4]]) & is.na(nine_spread_phy[[5]])  )] <- NA


####### Honors Physics 1
Hphysics <- c("HSC1109Q1-Honors Physics 1-NULL","HSC1109Q2-Honors Physics 1-NULL",  "HSC1109Q3-Honors Physics 1-NULL","HSC1109Q4-Honors Physics 1-NULL"  )
for (i in Hphysics){
nine_spread_phy[[i]] <- ifelse(nine_spread_phy[[i]] == 4, 1,
                  ifelse(nine_spread_phy[[i]] == 3, 1, 
                      ifelse(nine_spread_phy[[i]] == 2, 1, 
                        ifelse(nine_spread_phy[[i]] == 1, 0, 
                          ifelse(nine_spread_phy[[i]] == "NA", NA, 0)))))
}
nine_spread_phy$Hphysics <- rowSums(nine_spread_phy[,c(6:9)], na.rm=TRUE)
#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_phy$Hphysics[( is.na(nine_spread_phy[[6]]) & is.na(nine_spread_phy[[7]])  & is.na(nine_spread_phy[[8]]) & is.na(nine_spread_phy[[9]])  )] <- NA


####### Sheltered Physics 1
sheltphysics <- c("HSCS1009Q1-Physics 1-NULL", "HSCS1009Q2-Physics 1-NULL", "HSCS1009Q3-Physics 1-NULL", "HSCS1009Q4-Physics 1-NULL"   )
for (i in sheltphysics){
nine_spread_phy[[i]] <- ifelse(nine_spread_phy[[i]] == 4, 1,
                  ifelse(nine_spread_phy[[i]] == 3, 1, 
                      ifelse(nine_spread_phy[[i]] == 2, 1, 
                        ifelse(nine_spread_phy[[i]] == 1, 0, 
                          ifelse(nine_spread_phy[[i]] == "NA", NA, 0)))))
}
nine_spread_phy$sheltphysics <- rowSums(nine_spread_phy[,c(11:14)], na.rm=TRUE)
#add columns that had all NAs since adding above removed NAs and gave all NA columns a value of 0
nine_spread_phy$sheltphysics[( is.na(nine_spread_phy[[11]]) & is.na(nine_spread_phy[[12]])  & is.na(nine_spread_phy[[13]]) & is.na(nine_spread_phy[[14]])  )] <- NA


# Taking only new columns
# grab only only new sum columns
# create "type" column to indicate history type class
# nine_physics df will be merged with nine_math, etc for ML later

#nine_spread_phy
nine_spread_phy_short <- nine_spread_phy[,c(1, 15:17)]

# create Physics type column & Total Physics semesters passed in 9th grade
# new Physics type column EMPTY
nine_spread_phy_short$physicsType9 <- NA

#create Physics type column
nine_spread_phy_short$physicsType9[nine_spread_phy_short$physics >= 0] <- "physics"
nine_spread_phy_short$physicsType9[nine_spread_phy_short$Hphysics >= 0] <- "honorsphy"
nine_spread_phy_short$physicsType9[nine_spread_phy_short$sheltphysics >= 0] <- "sheltphysics"


#create row sum column for "physics semesters passed in 9th grade"
nine_spread_phy_short$physicsSemPassed9 <- rowSums(nine_spread_phy_short[,-c(1,5)], na.rm=TRUE)

#############make a copy for history 9th grade
nine_physics <- nine_spread_phy_short

# Save physics to RDS
saveRDS(nine_physics[c(1, 5,6)], file="nine_physics.rds")
####################
```


### TO DO:
- FRESHMAN SEMINAR - Did not exist in 2015
- MAYBE PE


```r
engineering <- nine[grep( "Engineering", nine$courseName) ,]
unique(engineering$courseName)
```

```
## [1] "Intro Engineering and Design"
```

```r
engineering_nodup <- engineering[!duplicated(engineering$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
engineering_nodup_short <- engineering_nodup[,c(2, 50, 52)]

library(tidyr)
nine_spread_engineer <- spread(engineering_nodup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert columns to numeric from factor
cols <- c(2:ncol(nine_spread_engineer))
nine_spread_engineer[,cols] <- apply(nine_spread_engineer[cols], 2, function(x) as.numeric(as.character(x)))


engineer <- c("HET1020Q1-Intro Engineering and Design-NULL","HET1020Q2-Intro Engineering and Design-NULL", "HET1020Q3-Intro Engineering and Design-NULL", "HET1020Q4-Intro Engineering and Design-NULL" )

for (i in engineer){
nine_spread_engineer[[i]] <- ifelse(nine_spread_engineer[[i]] == 4, 1,
                  ifelse(nine_spread_engineer[[i]] == 3, 1, 
                      ifelse(nine_spread_engineer[[i]] == 2, 1, 
                        ifelse(nine_spread_engineer[[i]] == 1, 0, 
                          ifelse(nine_spread_engineer[[i]] == "NA", NA, 0)))))
}
nine_spread_engineer$engineering <- rowSums(nine_spread_engineer[,c(2:5)], na.rm=TRUE)
nine_spread_engineer$engineering[( is.na(nine_spread_engineer[[2]]) & is.na(nine_spread_engineer[[3]])  & is.na(nine_spread_engineer[[4]]) & is.na(nine_spread_engineer[[5]])  )] <- NA

### SAVE ENGINEERING
#nine_spread_engineer
saveRDS(nine_spread_engineer[c(1, 6)], file="nine_engineer.rds")
```
### COMBINE all courses + demographics

```r
physics <- readRDS("nine_physics.rds")
english <- readRDS("nine_english.rds")
history <- readRDS("nine_history.rds")
math <- readRDS("nine_math.rds")
engineering <- readRDS("nine_engineer.rds")


one <- merge(physics, english, by="personID", all=TRUE)
one <- merge(one, history, by="personID", all=TRUE)
one <- merge(one, math, by="personID", all=TRUE)
one <- merge(one, engineering, by="personID", all=TRUE)

#pull only important demographic columns
keep <- c("personID", "Gender", "Grade", "RaceEthnicityState", "MealStatus", "Homeless", "SpecialEdDisability", "Section504", "ELL", "Gifted", "immigrant", "IEP", "EconDis", "HomePrimaryLanguage" )
nine_demo <- nine[keep]
nine_demo <- nine_demo[!duplicated(nine_demo$personID),]

nine_complete <- merge(one, nine_demo, by="personID", all=TRUE)
saveRDS(nine_complete, "nine_complete.rds")
```

### 58 students did not take physics Freshman year
- some took Honors Chem, etc
- will be removed from regression


```r
nine_complete <- readRDS("nine_complete.rds")
# one student with physics type NA is not working for some reason
nine_nona <- nine_complete[!(is.na(nine_complete$physicsSemPassed9) & is.na(nine_complete$physicsType9)),]
#convert to factor
nine_nona[] <- lapply(nine_nona, factor)
saveRDS(nine_nona, "nine_nona.RDS")
```


### Adding Chemistry



```r
#rv.keep

ten <- rv.keep[rv.keep$startYear ==2016 & rv.keep$grade == 10, ]
chem <- ten[grep("Chemistry", ten$courseName), ]
table(chem$courseName)
```

```
## 
##     AP Chemistry        Chemistry      Chemistry 1 Honors Chemistry 
##               57                1             1270              734
```



```r
chem_nodup <- chem[!duplicated(chem$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
chem_nodup_short <- chem_nodup[,c(2, 50, 52)]


#Below is all 2016 sophomores and CHEM
library(tidyr)
ten_spread <- spread(chem_nodup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert columns to numeric from factor
cols <- c(2:ncol(ten_spread))
ten_spread[,cols] <- apply(ten_spread[cols], 2, function(x) as.numeric(as.character(x)))

#####regular Chem
regularChem <- c("HSC2012Q1-Chemistry 1-NULL", "HSC2012Q2-Chemistry 1-NULL", "HSC2012Q3-Chemistry 1-NULL" , "HSC2012Q4-Chemistry 1-NULL")

for (i in regularChem){
ten_spread[[i]] <- ifelse(ten_spread[[i]] == 4, 1,
                  ifelse(ten_spread[[i]] == 3, 1, 
                      ifelse(ten_spread[[i]] == 2, 1, 
                        ifelse(ten_spread[[i]] == 1, 0, 
                          ifelse(ten_spread[[i]] == "NA", NA, 0)))))
}
ten_spread$regChem <- rowSums(ten_spread[,c(2:5)], na.rm=TRUE)
ten_spread$regChem[( is.na(ten_spread[[2]]) & is.na(ten_spread[[3]])  & is.na(ten_spread[[4]]) & is.na(ten_spread[[5]])  )] <- NA

#####Honors Chem
HonorsChem <- c("HSC2040Q1-Honors Chemistry-NULL","HSC2040Q2-Honors Chemistry-NULL", "HSC2040Q3-Honors Chemistry-NULL", "HSC2040Q4-Honors Chemistry-NULL" )

for (i in HonorsChem){
ten_spread[[i]] <- ifelse(ten_spread[[i]] == 4, 1,
                  ifelse(ten_spread[[i]] == 3, 1, 
                      ifelse(ten_spread[[i]] == 2, 1, 
                        ifelse(ten_spread[[i]] == 1, 0, 
                          ifelse(ten_spread[[i]] == "NA", NA, 0)))))
}
ten_spread$HChem <- rowSums(ten_spread[,c(6:9)], na.rm=TRUE)
ten_spread$HChem[( is.na(ten_spread[[6]]) & is.na(ten_spread[[7]])  & is.na(ten_spread[[8]]) & is.na(ten_spread[[9]])  )] <- NA

#####AP Chem
APChem <- c("HSC2042Q1-AP Chemistry-NULL","HSC2042Q2-AP Chemistry-NULL", "HSC2042Q3-AP Chemistry-NULL", "HSC2042Q4-AP Chemistry-NULL" )

for (i in APChem){
ten_spread[[i]] <- ifelse(ten_spread[[i]] == 4, 1,
                  ifelse(ten_spread[[i]] == 3, 1, 
                      ifelse(ten_spread[[i]] == 2, 1, 
                        ifelse(ten_spread[[i]] == 1, 0, 
                          ifelse(ten_spread[[i]] == "NA", NA, 0)))))
}
ten_spread$APChem <- rowSums(ten_spread[,c(10:13)], na.rm=TRUE)
ten_spread$APChem[( is.na(ten_spread[[10]]) & is.na(ten_spread[[11]])  & is.na(ten_spread[[12]]) & is.na(ten_spread[[13]])  )] <- NA

#####Shelt Chem
SheltChem <- c("HSCS2012Q1-Chemistry 1-NULL", "HSCS2012Q2-Chemistry 1-NULL","HSCS2012Q3-Chemistry 1-NULL","HSCS2012Q4-Chemistry 1-NULL"  )

for (i in SheltChem){
ten_spread[[i]] <- ifelse(ten_spread[[i]] == 4, 1,
                  ifelse(ten_spread[[i]] == 3, 1, 
                      ifelse(ten_spread[[i]] == 2, 1, 
                        ifelse(ten_spread[[i]] == 1, 0, 
                          ifelse(ten_spread[[i]] == "NA", NA, 0)))))
}
ten_spread$SheltChem <- rowSums(ten_spread[,c(15:18)], na.rm=TRUE)
ten_spread$SheltChem[( is.na(ten_spread[[15]]) & is.na(ten_spread[[16]])  & is.na(ten_spread[[17]]) & is.na(ten_spread[[18]])  )] <- NA
```




```r
ten_spread_short <- ten_spread[,c(1, 19:22)]

ten_spread_short$ChemType10 <- NA
ten_spread_short$ChemType10[ten_spread_short$regChem >= 0] <- "regChem"
ten_spread_short$ChemType10[ten_spread_short$HChem >= 0] <- "HonorsChem"
ten_spread_short$ChemType10[ten_spread_short$APChem >= 0] <- "APChem"
ten_spread_short$ChemType10[ten_spread_short$SheltChem >= 0] <- "SheltChem"

ten_spread_short$ChemSemPassed10 <- rowSums(ten_spread_short[,-c(1,6)], na.rm=TRUE)

saveRDS(ten_spread_short[c(1,6,7)], file="ten_chem.rds")

ten_chem <- readRDS("ten_chem.rds")
nine_complete <- readRDS("nine_complete.rds")
# one student with physics type NA is not working for some reason
nine_nona <- nine_complete[!(is.na(nine_complete$physicsSemPassed9) & is.na(nine_complete$physicsType9)),]

#merge nine complete with tenchem
nine_tenChem <- merge(nine_nona, ten_chem, by="personID", all=TRUE)

#convert to factor
nine_tenChem[] <- lapply(nine_tenChem, factor)
saveRDS(nine_tenChem, "nine_tenChem.rds")

#remove grade & personID & engineering
nine_tenChem_clean<- nine_tenChem[,-c(1,12,10)]

nine_tenChem_clean$predChem[nine_tenChem_clean$ChemSemPassed10 == 4] <- "p"
nine_tenChem_clean$predChem[nine_tenChem_clean$ChemSemPassed10 != 4] <- "f"
nine_tenChem_clean[] <- lapply(nine_tenChem_clean, factor)
#get rid of NAs
nine_tenChem_clean <- nine_tenChem_clean[complete.cases(nine_tenChem_clean),]
# get rid of ChemType10, ChemSemPassed10
nineTen <- nine_tenChem_clean[,-c(21,22)]
saveRDS(nineTen, "nineTen.rds")
```

# Adding Biology to dataset for prediction


```r
eleven <- rv.keep[rv.keep$startYear ==2017 & rv.keep$grade == 11, ]
#eleven
elevenU <- unique(eleven$personID)
length(elevenU)
```

```
## [1] 533
```


```r
#subset algebra classes only
biology <- eleven[grep("Biology", eleven$courseName), ]
table(biology$courseName)
```

```
## 
##                  AP Biology                     Biology 
##                         281                           1 
##                   Biology 1                   Biology 2 
##                         863                          34 
## Gen College Biology I w/Lab              Honors Biology 
##                           1                         518
```

```r
#remove all duplicate rows based on whether they have teh same course duplicater code
```


```r
biology_nodup <- biology[!duplicated(biology$courseduplicater),]
#take only personID,  scoreNum, courseIdentifier  ***numerical score
biology_nodup_short <- biology_nodup[,c(2, 50, 52)]

#Below is all 2015 freshman and MATH spread
library(tidyr)
eleven_spread <- spread(biology_nodup_short, courseIdentifier, scoreNum, fill=TRUE)

#convert columns to numeric from factor
cols <- c(2:ncol(eleven_spread))
eleven_spread[,cols] <- apply(eleven_spread[cols], 2, function(x) as.numeric(as.character(x)))
```



```r
#####Regular Bio
Bio <- c("HSC1042-Biology 1-Q1", "HSC1042-Biology 1-Q2", "HSC1042-Biology 1-Q3", "HSC1042-Biology 1-Q4")

for (i in Bio){
eleven_spread[[i]] <- ifelse(eleven_spread[[i]] == 4, 1,
                  ifelse(eleven_spread[[i]] == 3, 1, 
                      ifelse(eleven_spread[[i]] == 2, 1, 
                        ifelse(eleven_spread[[i]] == 1, 0, 
                          ifelse(eleven_spread[[i]] == "NA", NA, 0)))))
}
eleven_spread$Bio <- rowSums(eleven_spread[,c(2:5)], na.rm=TRUE)
eleven_spread$Bio[( is.na(eleven_spread[[2]]) & is.na(eleven_spread[[3]])  & is.na(eleven_spread[[4]]) & is.na(eleven_spread[[5]])  )] <- NA

####### Honors Biology
Honors <- c("HSC1062-Honors Biology-Q1", "HSC1062-Honors Biology-Q2", "HSC1062-Honors Biology-Q3", "HSC1062-Honors Biology-Q4")

for (i in Honors){
eleven_spread[[i]] <- ifelse(eleven_spread[[i]] == 4, 1,
                  ifelse(eleven_spread[[i]] == 3, 1, 
                      ifelse(eleven_spread[[i]] == 2, 1, 
                        ifelse(eleven_spread[[i]] == 1, 0, 
                          ifelse(eleven_spread[[i]] == "NA", NA, 0)))))
}

eleven_spread$HonorsBio <- rowSums(eleven_spread[,c(6:9)], na.rm=TRUE)
eleven_spread$HonorsBio[( is.na(eleven_spread[[6]]) & is.na(eleven_spread[[7]])  & is.na(eleven_spread[[8]]) & is.na(eleven_spread[[9]])  )] <- NA


######## AP Bio
APBio <- c("HSC1082-AP Biology-Q1", "HSC1082-AP Biology-Q2", "HSC1082-AP Biology-Q3", "HSC1082-AP Biology-Q4")

for (i in APBio){
eleven_spread[[i]] <- ifelse(eleven_spread[[i]] == 4, 1,
                  ifelse(eleven_spread[[i]] == 3, 1, 
                      ifelse(eleven_spread[[i]] == 2, 1, 
                        ifelse(eleven_spread[[i]] == 1, 0, 
                          ifelse(eleven_spread[[i]] == "NA", NA, 0)))))
}
eleven_spread$APBio <- rowSums(eleven_spread[,c(14:17)], na.rm=TRUE)
eleven_spread$APBio[( is.na(eleven_spread[[14]]) & is.na(eleven_spread[[15]])  & is.na(eleven_spread[[16]]) & is.na(eleven_spread[[17]])  )] <- NA


######## Bio2
Bio2 <- c("HSC1071-Biology 2-Q1", "HSC1071-Biology 2-Q2", "HSC1071-Biology 2-Q3", "HSC1071-Biology 2-Q4")

for (i in Bio2){
eleven_spread[[i]] <- ifelse(eleven_spread[[i]] == 4, 1,
                  ifelse(eleven_spread[[i]] == 3, 1, 
                      ifelse(eleven_spread[[i]] == 2, 1, 
                        ifelse(eleven_spread[[i]] == 1, 0, 
                          ifelse(eleven_spread[[i]] == "NA", NA, 0)))))
}

eleven_spread$Bio2 <- rowSums(eleven_spread[,c(10:13)], na.rm=TRUE)
eleven_spread$Bio2[( is.na(eleven_spread[[10]]) & is.na(eleven_spread[[11]])  & is.na(eleven_spread[[12]]) & is.na(eleven_spread[[13]])  )] <- NA


##### Sheltered
Sheltered <- c("HSCS1042-Biology 1-Q1","HSCS1042-Biology 1-Q2", "HSCS1042-Biology 1-Q3", "HSCS1042-Biology 1-Q4" )

for (i in Sheltered){
eleven_spread[[i]] <- ifelse(eleven_spread[[i]] == 4, 1,
                  ifelse(eleven_spread[[i]] == 3, 1, 
                      ifelse(eleven_spread[[i]] == 2, 1, 
                        ifelse(eleven_spread[[i]] == 1, 0, 
                          ifelse(eleven_spread[[i]] == "NA", NA, 0)))))
}

eleven_spread$Sheltered <- rowSums(eleven_spread[,c(19:22)], na.rm=TRUE)
eleven_spread$Sheltered[( is.na(eleven_spread[[19]]) & is.na(eleven_spread[[20]])  & is.na(eleven_spread[[21]]) & is.na(eleven_spread[[22]])  )] <- NA
```



```r
eleven_spread_short <- eleven_spread[,c(1, 24:28)]
eleven_spread_short$BioType11 <- NA
eleven_spread_short$BioType11[eleven_spread_short$Bio >= 0] <- "Bio"
eleven_spread_short$BioType11[eleven_spread_short$HonorsBio >= 0] <- "HonorsBio"
eleven_spread_short$BioType11[eleven_spread_short$APBio >= 0] <- "APBio"
eleven_spread_short$BioType11[eleven_spread_short$Bio2 >= 0] <- "Bio2"
eleven_spread_short$BioType11[eleven_spread_short$Sheltered >= 0] <- "Sheltered"

eleven_spread_short$BioSemPassed11 <- rowSums(eleven_spread_short[,-c(1,7)], na.rm=TRUE)

#eleven_spread_short

#############make a copy for bio 11th grade
eleven_bio <- eleven_spread_short
saveRDS(eleven_bio[c(1,7,8)], file="eleven_bio.rds")
####################
```

# Combine all 9th+10 grade 2015 cohort classes with Chemisty in 2017


```r
nine_tenChem <- readRDS("nine_tenChem.rds") #9/10 cohort classes
eleven_bio <- readRDS(file="eleven_bio.rds")

nine_10_11_clean <- merge(nine_tenChem, eleven_bio, by="personID", all=TRUE)

#remove grade & personID & engineering
nine_10_11_clean<- nine_10_11_clean[,-c(1,12,10)]

nine_10_11_clean$predBio[nine_10_11_clean$BioSemPassed11 == 4] <- "p"
nine_10_11_clean$predBio[nine_10_11_clean$BioSemPassed11 != 4] <- "f"
nine_10_11_clean[] <- lapply(nine_10_11_clean, factor)

#nine_10_11_clean
saveRDS(nine_10_11_clean, "nine_10_11_clean_2015.rds")
```

