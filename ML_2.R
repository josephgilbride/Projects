---
output: pdf_document
---

For this assignment, the following libraries are required, please install and load them by running the following:

```{r, message=FALSE, warning=FALSE}

required_packages = c("rpart", "rpart.plot", "psych", "randomForest")
packages_to_install = setdiff(required_packages, installed.packages()[,"Package"])

if (length(packages_to_install)!=0) {
  install.packages(packages_to_install)
}

library(rpart)
library(rpart.plot)
library(psych)
library(randomForest)
library(bbmle)
library(Deriv)

set.seed(0)

```

## Maximum Likelihood

The MLE process involves estimating the likelihood that a given, randomized, set of data falls into a certain probability. The goal is to represent a randomized sample as best as possible - the derivative of log likelihood is used, as it eliminates the deviation of the population. MLE can be categorized as inferential statistics.

### Probability bias:

```{r}

# read in coin toss data and store data in a vector

coin_toss = read.csv("coin_toss.csv")
vec = unlist(coin_toss[,1])
vec

# write negative log-likelihood function for bernoulli trial

log_likelihood = function(pi) {-log((pi^(sum(vec[vec==1])))*((1-pi)^(sum(vec==0))))}
log_likelihood2 = function(pi) {-log((pi^(sum(vec2[vec2==1])))*((1-pi)^(sum(vec2==0))))}
log_likelihood3 = function(pi) {-log((pi^(sum(vec3[vec3==1])))*((1-pi)^(sum(vec3==0))))}


# use "optimize" to find the optimal parameter

opt = optimize(log_likelihood, interval = c(0,1))


# report the optimal parameter

opt
```

```{r}
# using only the first 50 coin tosses, repeat the process

vec2 = unlist(coin_toss[1:50,])
opt_50 = optimize(log_likelihood2, interval = c(0,1))

opt_50
             
# using only the first 100 coin tosses, repeat the process

vec3 = unlist(coin_toss[1:100,])
opt_100 = optimize(log_likelihood3, interval = c(0,1))
opt_100

```

### Maximum likelihood estimator:

```{r}
# mle estimate of all the data

MLE = mle2(log_likelihood, start=list(pi=.5))
summary(MLE)
MLE

# mle estimate of random 50 points

MLE2 = mle2(log_likelihood2, start=list(pi=.6))
summary(MLE2)

# mle estimate of random 100 points

MLE3 = mle2(log_likelihood3, start=list(pi=.7))
summary(MLE3)

```

With 50 flips of the coin, the MLE is very high (\> 1). This is because the probability of predicting the outcome is high, considering the first few flips are heads (or 1). As more flips occur, the chances of guessing the side approaches 50%.

The estimates of 50 flips compared to 100 flips is noted as large, since the sample size is increase, and probability is evening out. The probability of successfully predicting the outcome asymptotically approaches 50%, as seen in the plot graph of pi(y) and number of outcomes(x).

Overall, it appears that the threshold of accuracy on the dataset is about 578 flips, even though the MLE is closer to .5 with 100 flips of the coin.

## Decision Tree

For this question, we will need to download the Cleveland Clinic heart disease data set from UCI Machine Learning Repository. We'll use decision tree to predict the probability of heart disease.

### Preparation & Exploration:

```{r}

# read in the data from UCI Machine Learning Repository

heart_disease_url = 'http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data'

heart = read.csv(
  url(heart_disease_url),
  header = F, 
  col.names = c("age", "sex", "cp", "trestbps", "chol",
                "fbs", "restecg", "thalach", "exang", "oldpeak", "slope",
                "ca", "thal", "num"))

# let's look at the summary of the data

summary(heart)

# Notice that the variables "ca" and "thal" have "?" in the data. 
# Let's remove those from our dataset.

heart[heart=="?"] = 0
heart = sapply(heart, as.numeric)
heart = data.frame(heart)

# use multi.hist() to plot histograms of all of the variables. 
# Notice that the data is required to have numeric values. 
# To convert your data to numeric, you can use sapply() and is.numeric().

multi.hist(heart)

```

Our target feature should be an indicator that tells us if an individual has a heart disease (1) or not (0). To do this, we will need to transform the feature "num" to 1 if it's greater than zero and 0 if it's zero. Remove the "num" feature after the transformation. Let's label our target feature "disease".

```{r}
# create target variable

heart$num[heart$num>0] = 1
heart$num[heart$num==0] = 0
heart$disease = heart$num
heart$disease = as.factor(heart$disease)

# remove "num"

heart$num = NULL

```

### Data Splitting:

Let's split our data into three sets: training (40%), validation (20%), and testing (40%). In other words, we're using 60% of our data to train and tune our model.

```{r}
# split data into training, validation, and testing sets

split = sample(1:3, size=nrow(heart), prob=c(0.4,0.2,0.4), replace = TRUE)
train = heart[split==1,]
valid = heart[split==2,] 
test = heart[split==3,]


```

### Decision Tree Model Fitting & Parameter Tuning:

We want to train a decision tree model that generalizes well. Let's vary two parameters in our decision tree model, maxdepth and minsplit, and see how they perform on the validation set. Let's vary maxdepth from 1 to 5 and minsplit from 10 to 30.

```{r}

# loop through the parameter set and store the parameters, models, 
# and accuracies on the validation set

library(caret)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
tuneGrid = expand.grid(maxdepth=seq(1,5))

#find best maxdepth

cvModel = train(disease~.,data=train,trControl = fitControl,method="rpart2",tuneGrid = tuneGrid)

cvModel$bestTune

#find best minsplit

x = 1
results2 = data.frame()
while (x<22){
    
cvModel2 = train(disease~.,data=train,method="rpart2", trControl = fitControl,   control=rpart.control(cp = .01, minsplit=9+x,maxdepth = cvModel$bestTune))
results = data.frame(max(cvModel2$result$Accuracy))
results2 = rbind(results, results2)
                         
x=1+x
}


results2$minsplit = 10:30
results3 <-results2[order(results2$max.cvModel2.result.Accuracy., results2$minsplit, decreasing = TRUE),]

# report the best parameters (there can be more than one set, select one of them)

minsplit_best = results3[1, 2]
maxdepth_best = as.numeric(cvModel$bestTune)

# report the best score from the validation set

cvModel = train(disease~.,data=train,method="rpart2", trControl = fitControl,  control=rpart.control(cp = .01, minsplit=minsplit_best,maxdepth = maxdepth_best))

pred = predict(cvModel, valid)
pred = data.frame(pred)
confmat = cbind(pred, valid$disease)
confmat

mean(confmat$pred==confmat$`valid$disease`)
```

Using the parameters above, let's compare how our model performs with different split metrics, information and ginni.

```{r}
# train using information metric

cvModel_info = train(disease~.,data=train,method="rpart2", trControl = fitControl,  control=rpart.control(cp = .01, minsplit=minsplit_best,maxdepth = maxdepth_best, parms=list(split='information')))

# report accuracy on validation set 

pred = predict(cvModel_info, valid)
pred = data.frame(pred)
confmat = cbind(pred, valid$disease)
confmat

mean(confmat$pred==confmat$`valid$disease`)

# train using ginni metric

cvModel_gini = train(disease~.,data=train,method="rpart2",trControl = fitControl,   control=rpart.control(cp = .01, minsplit=minsplit_best,maxdepth = maxdepth_best, parms=list(split='gini')))

# report accuracy on validation set 

pred = predict(cvModel_gini, valid)
pred = data.frame(pred)
confmat = cbind(pred, valid$disease)
confmat

mean(confmat$pred==confmat$`valid$disease`)

```

```{r}

# visualize our best model using rpart.plot()

treeCV = rpart(disease~.,data=train, 
               control=rpart.control(maxdepth = minsplit_best, minsplit=maxdepth_best, cp=.01))

rpart.plot(treeCV)

# visualize the confusion matrix of our actual vs. predicted for our test data

pred = predict(treeCV, test, type = 'class')
pred = data.frame(pred)
confmat = cbind(pred, test$disease)
confmat

# compute test accuracy

mean(confmat$pred==confmat$`test$disease`)

```

## Random Forest

```{r}
# read in "iris" using data()

data(iris)
iris = iris

# split your data in to a features ("Sepal Length", "Sepal Width", "Petal Length",
# and "Petal Width") and target ("Species)

split2 = createDataPartition(y=iris$Species,p = 0.7,list = F,groups = 100)
train2 = iris[split2,]
test2 = iris[-split2,]


# preparing data for training the model

```

### Random Forest Learning Tree:

```{r}
# generating the random forest learning model. Start with 50 tress

forest = randomForest(Species~.,data=train2,ntree = 50)

```

### Evaluate the model on the test data and check accuracy:

```{r}
# evalute the model on the test data

predForest = predict(forest,newdata=test2)
  
# check the accuracy

(1- sum(forest$confusion[, 'class.error']))
(sum(forest$predicted == train2$Species))/nrow(train2)

```

### Tuning ntree:

```{r}
# generating the random forest learning model. Start with 500 tress

forest2 = randomForest(Species~.,data=train2,ntree = 500)


# check the accuracy

(1-sum(forest2$confusion[, 'class.error']))
(sum(forest2$predicted == train2$Species))/nrow(train2)

```

## Numerically find the minimum solution of the function below:

$$F \left( x \right)= \frac{1}{2} \left (4x_1^2 +4x_1x_2+2x_1x_3+5x_2^2+6x_2x_3+7x_3^2+x_1x_4+x_2x_5\right)+2x_1-8x_2+9x_3$$

### initial guess = 2 for $x_1-x_5$ and the learning rate =0.01 and epsilon=0.4. (4 pts)

```{r}

# original formula 

func = function(x_1, x_2, x_3, x_4, x_5) {(1/2)*(4*(x_1)^2+4*(x_1*x_2)+2*(x_1*x_3)+5*((x_2)^2)+6*(x_2*x_3)+(x_1*x_4)+(x_2*x_5))+2*(x_1)-8*(x_2) + 9*(x_3)}


# define the initial value 

x=data.frame(c(2,2,2,2,2))

# define the alpha value (learning rate)

learning_rate = .01

# define the epsilon value, maximum iteration allowed 

epsilon = .4

  # gradient descent

Der = Deriv::Deriv(func)

grad =  data.frame(x = 2, value = func(2,2,2,2,2))
grad = 2 - learning_rate * Der(2,2,2,2,2)
grad =  data.frame( x = 2, value = func(2,2,2,2,2))
# record keeping 

while (abs(x[1,]) > epsilon)
{x = mean(x[1,], x[2,], x[3,], x[4,], x[5,]) - learning_rate *Der(x[1,], x[2,], x[3,], x[4,], x[5,]) 
x = data.frame(x)
grad = rbind(grad, func(x[1,], x[2,], x[3,], x[4,], x[5,]))}

grad = grad[,2]


# create the data points' dataframe

x

dd <- expand.grid(x_1=0:10, x_2=0:10, x_3 = 0:10, x_4 = 0:10, x_5=0:10)
results = do.call(mapply, c(func, unname(dd)))
df = cbind(dd, data.frame(results))

```

### Function value vs. number of iterations:

We can increase the number of iterations or/and decrease the step length to improve the algorithm

```{r}
# draw the function value vs. #iterations

plo = seq(-10,10, by=.01)
plot(func(plo,plo,plo,plo,plo))

```

### Iterations:

```{r}
# try different initial points, repeat, and observe convergence

x=data.frame(c(1,1,1,1,1))

# define the alpha value (learning rate)

learning_rate = .0001

# define the epsilon value, maximum iteration allowed 

epsilon = .000001

  # gradient descent

Der = Deriv::Deriv(func)

grad =  data.frame(x = 1, value = func(1,1,1,1,1))
grad = 1 - learning_rate * Der(1,1,1,1,1)
grad =  data.frame( x = 2, value = func(1,1,1,1,1))
# record keeping 

while (abs(x[1,]) > epsilon)
{x = mean(x[1,], x[2,], x[3,], x[4,], x[5,]) - learning_rate *Der(x[1,], x[2,], x[3,], x[4,], x[5,]) 
x = data.frame(x)
grad = rbind(grad, func(x[1,], x[2,], x[3,], x[4,], x[5,]))}

grad = grad[,2]

grad
x
scale(x)

#lowest value
func(-0.23086200,1.41731946,-1.38499114, 0.09930804,0.09922564)

```

As displayed by above, adjusting the parameters for learning process (learning rate, epsilon) are the most important factors to improve convergence rate. Starting, guessed values are less important (even though guessing closer to the best value limits CPU time).
