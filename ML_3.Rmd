---
output: html_document
---

```{r, message=FALSE, warning=FALSE}

required_packages = c("mlbench", "ggplot2", "e1071", "ROCR")
packages_to_install = setdiff(required_packages, installed.packages()[,"Package"])

if (length(packages_to_install)!=0) {
  install.packages(packages_to_install)
}

library(mlbench)
library(ggplot2)
library(e1071)
library(tidyverse)  
library(modelr)    
library(broom)      
library(ROCR)
library(ISLR)

set.seed(0)

```

## Support Vector Machine

```{r}

# load in PimaIndiansDiabetes

data(PimaIndiansDiabetes)


# binarize "pos" (1 = 'pos', 0 ='neg') in diabetes variable

PimaIndiansDiabetes$diabetes = as.character(PimaIndiansDiabetes$diabetes)
PimaIndiansDiabetes$diabetes[PimaIndiansDiabetes$diabetes == "pos"] = 1
PimaIndiansDiabetes$diabetes[PimaIndiansDiabetes$diabetes == "neg"] =0
PimaIndiansDiabetes$diabetes = factor(PimaIndiansDiabetes$diabetes)
PimaIndiansDiabetes$pregnant = NULL
PimaIndiansDiabetes$pressure = NULL
PimaIndiansDiabetes$triceps = NULL
PimaIndiansDiabetes$insulin = NULL
PimaIndiansDiabetes$pedigree = NULL
PimaIndiansDiabetes$age = NULL

# scatter plot glucose vs mass, coloring the points by diabetes

ggplot(PimaIndiansDiabetes, aes(glucose, mass, colour = diabetes)) +
  geom_point()

```

```{r}

# split data into training & testing (70/30 split)

library(caret)
split = createDataPartition(y=PimaIndiansDiabetes$diabetes,p = 0.7,list = F,groups = 100)
train = PimaIndiansDiabetes[split,]
test = PimaIndiansDiabetes[-split,]

```

### SVM & Varying Degree of Polynomial Kernel:

```{r}

# function to plot decision boundary

# make grid
make.grid = function(x, n = 200) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)}

grid = make.grid(train)
  
```

```{r,fig.align='center'}

# fit using "svm", with "polynomial" kernel, degree = 1, cost = 5, type = "C", and scale = F

svmPoly = svm(diabetes~glucose+mass,data = train, kernel='polynomial',scale=F,type='C-classification',degree=1, cost=5)

# visualize the decision boundary of your SVM model

plot(svmPoly, train)

# assess the accuracy of your prediction on the testing set

confmat = table(predict(svmPoly), train$diabetes, dnn=c("Prediction", "Actual"))


```

```{r}
# fit using "svm", with "polynomial" kernel, degree = 2, cost = 5, type = "C", and scale = F

svmPoly2 = svm(diabetes~glucose+mass,data = train, kernel='polynomial',scale=F,type='C-classification',degree=2, cost=5)


# visualize the decision boundary of your SVM model
plot(svmPoly2, train)

# assess the accuracy of your prediction on the testing set
confmat2 = table(predict(svmPoly2), train$diabetes, dnn=c("Prediction", "Actual"))


```

```{r}
# fit using "svm", with "polynomial" kernel, degree = 3, cost = 5, type = "C", and scale = F

svmPoly3 = svm(diabetes~glucose+mass,data = train, kernel='polynomial',scale=F,type='C-classification',degree=3, cost=5)


# visualize the decision boundary of your SVM model

plot(svmPoly3, train)

# assess the accuracy of your prediction on the testing set

confmat3 = table(predict(svmPoly3), train$diabetes, dnn=c("Prediction", "Actual"))

```

The degree is a tuning of hyperplane. It takes on a radial shape, such that boundaries are more accurate. The number of iterations increases with each degree. The vectors are generally optimal at a degree of 3, where the clusters seemingly contain more correctly placed points.

### SVM & Varying Cost:

```{r}
# fit using "svm", with "polynomial" kernel, degree = 3, cost = 50, type = "C", and scale = F

svmPoly4 = svm(diabetes~glucose+mass,data = train, kernel='polynomial',scale=F,type='C-classification',degree=3, cost=50)

# visualize the decision boundary of your SVM model

plot(svmPoly4, train)


# assess the accuracy of your prediction on the testing set

confmat4 = table(predict(svmPoly4), train$diabetes, dnn=c("Prediction", "Actual"))

```

Cost = cost of constraints violation, cost of misclassification. The hyperplane of the model takes on a different shape that classifies more points correctly. However, predictions are less accurate.

### SVM Parameter Tuning:

```{r}

# tune your SVM model

svmLinear = tune.svm(diabetes~glucose+mass,data = train, kernel='linear',scale=F,type='C-classification',degree=3, cost=c(4,8,16,32))

# report the accuracy of your best performing model

confmat5 = table(predict(svmLinear$best.model), train$diabetes, dnn=c("Prediction", "Actual"))

# visualize the decision boundary of your SVM model

plot(svmLinear$best.model, train)

```

## Logistic Regression

```{r}
# load dataset
data(Default)
Default

Default$default = as.character(Default$default)
Default$default[Default$default== "Yes"] = 1
Default$default[Default$default== "No"] = 0
Default$default = as.numeric(Default$default)

```

```{r}
# split data into training & testing (70/30 split)
set.seed(123)
split = createDataPartition(y=Default$default,p = 0.7,list = F,groups = 100)
train2 = Default[split,]
test2 = Default[-split,]
```

### Logistic regression:

```{r}
#use glm function 

fit = glm(default~ balance, train2, family = "binomial")

```

```{r}
#plot logistic function

ggplot(Default, aes(balance, default))+
  geom_point()+
  stat_smooth(method = 'glm', method.args=list(family="binomial"), se=FALSE)

```

```{r}
#predict the probability of defaulting

pred = predict(fit, newdata=data.frame(balance = c(1000,2000)), type="response")

pred

```

```{r}
#Test the predicted target varaiable vs. the observed values

pred2 = predict(fit, newdata=test2, type="response")
predictions=ifelse(pred2>0.5,"Yes","No")
test2$default = ifelse(test2$default>=0.5,"Yes","No")

```

```{r}
#Form the confusion matrix

table(test2$default,predictions,dnn=c("Actual","Predicted"))

```

```{r}
#misclassification rate

round(mean(predictions!=test2$default),4)
```
