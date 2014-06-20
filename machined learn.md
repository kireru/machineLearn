 Manner of the Exercise Prediction 
========================================================
Date: 2014-06-18
 
Author: Andrew Kireru 
Purpose: prediction of exercise 
R functions and coding paradigms we will use throughout.
This includes loading, viewing, and cleaning raw data; as well as
some basic visualization. This specific case we will use data from netlfik.
Data Used: http://groupware.les.inf.puc-rio.br

Packages Used: ggplot2, plyr, scales

### Pre-processing

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(ggplot2)
library(plyr)
library(scales)
```
 
### loading data set
 

```r
training <- read.csv("pml-training.csv", header=TRUE)
testing <- read.csv("pml-testing.csv", header=TRUE)
outcome = factor(training$classe)    #save the outcome apart
training = subset(training,select=-classe)
dim(training)
```

```
## [1] 19622   159
```

### functions


```r
mae <- function(x,y)
{
  sum( abs(x-y) ) /length(x)
}
```

preprocess continued


```r
num = sapply(training,is.numeric)

tmp = as.data.frame(lapply(training[!num],function(x) gsub('#DIV/0!','',x)),stringsAsFactors=F)
dim(tmp)
```

```
## [1] 19622    36
```

```r
head(tmp[,1:4])    #the first 3 columns should be still as character
```

```
##   user_name   cvtd_timestamp new_window kurtosis_roll_belt
## 1  carlitos 05/12/2011 11:23         no                   
## 2  carlitos 05/12/2011 11:23         no                   
## 3  carlitos 05/12/2011 11:23         no                   
## 4  carlitos 05/12/2011 11:23         no                   
## 5  carlitos 05/12/2011 11:23         no                   
## 6  carlitos 05/12/2011 11:23         no
```

```r
tmp[4:36] = as.data.frame(lapply(tmp[,4:36],as.numeric))    #convert
training[!num] = tmp
```

Remove variables with a missing rate >= 0.95.


```r
na_rate = apply(training,2,function(x) sum(is.na(x))/length(x))    #rate of missingness
training = training[,na_rate<0.95]
dim(training)
```

```
## [1] 19622    59
```

Then remove non-numeric variables and the first column which is the sequence number. Remove near-zero variables.


```r
num = sapply(training,is.numeric)
num[1] = FALSE    #also remove the first column
training = training[num]

nzv = nearZeroVar(training,saveMetrics=T)
all(!nzv$nzv)    #none of the variables are near to zero
```

```
## [1] TRUE
```

```r
dim(training)
```

```
## [1] 19622    55
```

Then standardize the variables.


```r
pre_pro1 = preProcess(training,method=c('center','scale','pca'),thresh=0.9)
train_std = predict(pre_pro1,training)
```

Take a subset of training set, since the whole dataset is too big...


```r
set.seed(123)

in_train = createDataPartition(y=outcome,p=0.2,list=F)
sub_train = train_std[in_train,]
sub_out = outcome[in_train]
dim(sub_train)
```

```
## [1] 3927   20
```

### Analyze and evaluate

Splice the data into 10-fold subsets. Train and cross validate.


```r
fitControl <- trainControl(method="repeatedcv",number=10,repeats=1)    #control of fitting
par_grid <- expand.grid(mtry=c(2,5,10,20))    #parameter grid
model <- train(sub_out~.,data=sub_train,method='rf',trControl=fitControl,tuneGrid=par_grid)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

The final model is


```r
model
```

```
## Random Forest 
## 
## 3927 samples
##   19 predictors
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 1 times) 
## 
## Summary of sample sizes: 3533, 3536, 3535, 3534, 3535, 3535, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     0.9       0.9    0.01         0.01    
##   5     0.9       0.9    0.01         0.01    
##   10    0.9       0.9    0.02         0.02    
##   20    0.9       0.9    0.02         0.02    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

```r
model$results
```

```
##   mtry Accuracy  Kappa AccuracySD KappaSD
## 1    2   0.9065 0.8818    0.01121 0.01410
## 2    5   0.9045 0.8792    0.01138 0.01439
## 3   10   0.8989 0.8721    0.01695 0.02142
## 4   20   0.8852 0.8547    0.01677 0.02131
```
 
sample error
======
with the hyper-parameter mtry=2, and the corresponding **out-of-sample error** is (1-Accuracy), which is 0.0935.

## Testing

```r
testing = read.csv('pml-testing.csv')
dim(testing)
```

```
## [1]  20 160
```

Pre-process the data as what was done for the training set.


```r
testing = testing[names(training)]
dim(testing)
```

```
## [1] 20 55
```

```r
test_std = predict(pre_pro1,testing)
```

Then make predictions.


```r
pred = predict(model,test_std)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
pred
```

```
##  [1] B A A A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
