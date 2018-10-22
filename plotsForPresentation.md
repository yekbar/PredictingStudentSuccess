---
title: "Graphs for presentation"
author: "kat"
date: "October 16, 2018"
output:
  html_document:
    keep_md: yes
---




```r
rv.keep <- readRDS("rv.keepMASTER.rds")
```

### 9th grade- 573 Students in 2015


```r
nine <- rv.keep[rv.keep$startYear ==2015 & rv.keep$grade == 9, ]
#nine
nineU <- unique(nine$personID)
length(nineU)
```

```
## [1] 573
```
# Create Plot of 2015 Courses Most Taken By Freshman

```r
length(unique(nine$personID))
```

```
## [1] 573
```

```r
courses <- as.data.frame(table(nine$courseName))
courses$class <- courses$Freq / 4
coursesh <- courses[courses$class >=100,]
coursesh <- coursesh[order(coursesh$class),]
#sort order
order <- coursesh$Var1

coursesh
```

```
##                             Var1 Freq  class
## 43              Honors Physics 1  506 126.50
## 42 Honors Int Algebra/Geometry 2  552 138.00
## 7             AP Human Geography  577 144.25
## 53      Intro to PC Applications  667 166.75
## 86                     Spanish 1  687 171.75
## 40              Honors English 9  771 192.75
## 36  Health and Activity for Life  933 233.25
## 76                   Peer Mentor 1176 294.00
## 29                     English 9 1557 389.25
## 81                     Physics 1 1631 407.75
## 47             IntegratedAlgGeo1 1711 427.75
## 33                     Geography 1783 445.75
```

```r
physicslabel <- c("Physics", "Other", "Other", NA,NA,"Other", NA, NA, "Other", "Physics" , NA, NA )
coursesh <- cbind(coursesh, physicslabel)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
ggplot(coursesh, aes(x=Var1, y=class, fill=as.factor(coursesh$physicslabel))) +
  geom_col(fill=c("navyblue", "slategray1", "slategray1", "Grey87", "Grey87", "slategray1", "Grey87", "Grey87" , "slategray1", "navyblue", "slategray1", "slategray1" )) +
  coord_flip() +
  scale_x_discrete(limits=order)+
  theme(panel.background = element_blank(), 
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = .13)) +
  xlab("") +
  ylab("") +
  ggtitle("2015 Most Taken Courses By Freshman")
```

![](plotsForPresentation_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
### Plotting All Science courses

```r
# 9th graders Physics breakdown
phygraph <- nine[grep("Physics", nine$courseName), ]
phygraph_nodup <- phygraph[!duplicated(phygraph$courseduplicater),]
phygraph <- as.data.frame(table(phygraph_nodup$courseName))
phygraph2 <- phygraph[phygraph$Freq >1,]
phygraph2$nFreq <- phygraph2$Freq /4
phygraph2
```

```
##               Var1 Freq  nFreq
## 1 Honors Physics 1  465 116.25
## 3        Physics 1 1448 362.00
```

```r
# 10th graders Chemistry breakdown
ten <- rv.keep[rv.keep$startYear ==2016 & rv.keep$grade == 10, ]
phygraph <- ten[grep("Chemistry", ten$courseName), ]
phygraph_nodup <- phygraph[!duplicated(phygraph$courseduplicater),]
phygraph <- as.data.frame(table(phygraph_nodup$courseName))
phygraph1 <- phygraph[phygraph$Freq >1,]
phygraph1$nFreq <- phygraph1$Freq /4
phygraph1
```

```
##               Var1 Freq  nFreq
## 1     AP Chemistry   57  14.25
## 3      Chemistry 1 1175 293.75
## 4 Honors Chemistry  693 173.25
```

```r
## Juniors
eleven <- rv.keep[rv.keep$startYear ==2017 & rv.keep$grade == 11, ]
phygraph <- eleven[grep("Biology", eleven$courseName), ]
phygraph_nodup <- phygraph[!duplicated(phygraph$courseduplicater),]
phygraph <- as.data.frame(table(phygraph_nodup$courseName))
phygraph3 <- phygraph[phygraph$Freq >1,]
phygraph3$nFreq <- phygraph3$Freq /4
phygraph3
```

```
##             Var1 Freq nFreq
## 1     AP Biology  274  68.5
## 3      Biology 1  796 199.0
## 4      Biology 2   30   7.5
## 6 Honors Biology  496 124.0
```



```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.4
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
bind1<- rbind(phygraph2, phygraph1)
bind1 <- rbind(bind1, phygraph3)
#bind1
c <- as.factor(c("P", "P", "C", "C", "C", "B", "B", "B", "B"))
bind1 <- cbind(bind1, c)
bind1 <- bind1[order(-bind1$nFreq),]

#bind1
bind1_ordered <- bind1 %>% arrange(c, nFreq)
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

```r
order <- bind1_ordered$Var1
ggplot(bind1, aes(x=Var1, y=nFreq, fill=c))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits=order)+
  xlab("")+
  ylab("")+
  ggtitle("Science Course Options By Grade Level")+
  theme(panel.background = element_blank(), 
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = .17))+
   scale_fill_manual(values=c("navyblue", "slategray1", "Grey87"), 
                     labels=c("Junior", "Sophomore", "Freshman"),
                     name="Grade Level")+
  guides(fill=guide_legend(reverse=TRUE))
```

![](plotsForPresentation_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

