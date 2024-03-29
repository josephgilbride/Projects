---
output: pdf_document
---

```{r, message=FALSE, warning=FALSE}

required_packages = c( "tidyverse", "corrplot", "gridExtra",
                       "GGally", "cluster", "factoextra", 
                       "mlbench", "class", "ggplot2", "FNN",
                       "caret", "plot3D", "Rtsne")

packages_to_install = setdiff(required_packages, installed.packages()[,"Package"])

if (length(packages_to_install)!=0) {
  install.packages(packages_to_install)
}

library(mlbench)
library(ggplot2)
library(class)
library(FNN)
library(caret)
library(dplyr)
library(tidyr)
library(corrplot)
library(cluster)
library(plot3D)
library(Rtsne)
library(GGally)
library(factoextra)

# let's set the seed to 0 for consistency
set.seed(0)

```

## Data Visualization

Working with wine dataset from UCI Machine Learning Repository. The data can be downloaded directly from the repository (see code below).

### Read in the data and report its dimension:

```{r cars}
# read in the data using the following
wine = read.csv(
  url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
  header = F, 
  col.names = c("Wine", "Alcohol", "Malic.acid", "Ash", 
                "Acl", "Mg", "Phenols", "Flavanoid",
                "Nonflavanoid.phenols", "Proanth", "Color.int", 
                "Hue", "OD", "Proline"))

# display the first few columns using head()

head(wine)

# report dimension

dim(wine)

```

### Summary statistics of the dataset:

```{r}
# show summary statistics of the data

summary(wine)

```

### Pairwise relationship of the first five variables:

```{r}
# pair plot of wine

pairs(wine)

ggpairs(wine)


```

### Histogram of each attribute:

```{r}

# histogram plot of each attribute

ggplot(gather(wine), aes(value)) + 
        geom_histogram(bins = 10) + 
       facet_wrap(~key, scales = 'free_x')

```

### Correlation matrix:

```{r}
# correlation plot

corrplot(cor(wine))

```

## Clustering

```{r}

# read in "iris" using data()

data(iris)

# split your data in to a features ("Sepal Length", "Sepal Width", "Petal Length",
# and "Petal Width") and target ("Species)

iris2 = subset(iris,select=-c(Species))

# normalize your features

iris3 = scale(iris2)

# fit a k-means model with k = 3

km = kmeans(iris3,centers = 3,nstart=25)
fviz_cluster(km, data = iris2)

# compare your cluster results with the target variable using table()

table(iris$Species)


```

### 2D clustering plot:

```{r}
# plot a 2D clustering plot using clustplot()

clusters <- pam(iris2, 3)$clustering
clusplot(iris2, clusters)

```

### Draw a 3D plot using scatter3D:

```{r}
# draw a 3D plot using scatter3D

scatter3D(iris2$Sepal.Length, iris2$Sepal.Width, iris2$Petal.Length, iris2$Petal.Width)


```

### Visualize high-dimensional:

```{r}
# transform your features data into a matrix

mat = as.matrix(iris2)

# use Rtsne() to reduce your dimensions, set perplexity = 20, theta = 0.5, dims = 2

tsne = Rtsne(mat, perplexity = 20, theta = 0.5, dims = 2, check_duplicates = FALSE)

# display the results of t-SNE colored by the cluster labels

dtsne = as.data.frame(tsne$Y)
fviz_cluster(km, dtsne)
```

## kNN Classification

```{r}

# read in the data, store the data as "df"

data(PimaIndiansDiabetes)
df = PimaIndiansDiabetes

# describe the data using summary()

summary(df)

# plot histogram of age

hist(df$age)

# proportion of individuals with diabetes

count(subset(df, diabetes=='pos'))/nrow(df)*100

```

The distribution of "age" variable is skewed with more younger individuals than older.

### Scatter plot:

```{r}
# convert "pos" in diabetes variable to factor

df$diabetes = as.factor(df$diabetes)

# plot glucose vs mass, colored by diabetes

ggplot(df, aes(glucose, mass, colour = diabetes)) +
  geom_point()

```

### k-NN classifier:

```{r}

# function to plot decision boundary
decision_boundary_plot <- function(model, data, x1_var, x2_var, y_var, resolution = 100, title) {
  
  x1 = data[, x1_var]
  x2 = data[, x2_var]
  y = data[, y_var]
  
  # make grid
  xs1 <- seq(min(x1), max(x1), length.out = resolution)
  xs2 <- seq(min(x2), max(x2), length.out = resolution)
  g <- cbind(rep(xs1, each=resolution), rep(xs2, time = resolution))
  g <- as.data.frame(g)
  names(g) = c('glucose', 'mass')
  
  p <- predict(model, g, type = "class")
  
    plt = ggplot() + 
    geom_point(aes(g[, 1], g[, 2], col = p), size = 0.1) +
    geom_point(aes(x1, x2, col = y), size = 2) + 
    geom_contour(
      aes(x = g[, 1], y = g[, 2],z = as.integer(p)), 
      col = 'black', size = 0.1) +
    xlab('glucose') + ylab('mass') + scale_colour_discrete(name='diabetes') +
    ggtitle(title)
  
  return(plt)
}
```

```{r}
# build a k-NN classifier, setting k to 2

knnmod = knn3(formula=diabetes~mass+glucose, data=df, k=2)

# visualize the decision boundary of k-NN, k = 2

decision_boundary_plot(knnmod, df, 2, 6, 9, resolution = 100, title= "K=2")

# build a k-NN classifier, setting k to 50

knnmod2 = knn3(formula=diabetes~mass+glucose, data=df, k=50)

# visualize the decision boundary of k-NN, k = 50

decision_boundary_plot(knnmod2, df, 2, 6, 9, resolution = 100, title= "K=50")

# build a k-NN classifier, setting k to 200

knnmod3 = knn3(formula=diabetes~mass+glucose, data=df, k=200)

# visualize the decision boundary of k-NN, k = 200

decision_boundary_plot(knnmod3, df, 2, 6, 9, resolution = 100, title= "K=200")

```

As k increases, the boundary becomes less defined, since there are more neighbors considered in the knn calculation. From the visualizations, it appears that as k between 2 \<\> 50 is optimal.

## Regression

```{r}

# read in "airquality"

data(airquality)

# store the data as df

dfair = airquality

# describe the data using summary()

summary(dfair)

# observe the pairwise relationships between the variables 

pairs(dfair)
ggpairs(dfair)

```

From the pair plot, most of the relationships appear to be nonlinear.

### Regress:

```{r}

# read in airquality


# subset the data to include only "Ozone" and "Temp". Remove missing values.

dfair2 = cbind(dfair[,1], dfair[,4])
dfair2 = na.omit(dfair2)
dfair2 = data.frame(dfair2)
names(dfair2) = c('Ozone', 'Temp')
# regress Temp (y) on Ozone (x) using linear regression

airmod = lm(Temp~Ozone, dfair2)

ggplot(dfair2, aes(x = Ozone, y = Temp)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# regression Temp (y) on Ozone (x) using 3rd-degree polynomial regression

airmod2 = lm(Temp~poly(Ozone), dfair2)

ggplot(dfair2, aes(x = Ozone, y = Temp)) + 
  geom_point() +
  stat_smooth(method = "loess", col = "red")

# plot Temp vs Ozone with the fitted regression line

ggplot(dfair2, aes(x = Ozone, y = Temp)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_smooth(method = "loess", col = "orange")

```

Visually, polynomial regression fits the data better.

### k-NN regression:

```{r}
# fit a kNN regression with k = 2

knnregmod = knn.reg(dfair2, y=dfair2$Temp, k=2)
dfplot = cbind(dfair2, data.frame(knnregmod$pred))
dfplot

# plot Temp vs Ozone with kNN predictions, k = 2

ggplot(dfair2, aes(x = Ozone, y = Temp))+
  geom_point()+
  geom_point(aes(dfplot$knnregmod.pred, col = "orange"))+
  scale_colour_discrete(name='pred') +
  stat_smooth(method = "loess", col = "red")+
   stat_smooth(method = "lm", col = "yellow")
# fit a kNN regression with k = 10

knnregmod2 = knn.reg(dfair2, y=dfair2$Temp, k=10)
dfplot2 = cbind(dfair2, data.frame(knnregmod2$pred))
dfplot2

# plot Temp vs Ozone with kNN predictions, k = 10

ggplot(dfair2, aes(x = Ozone, y = Temp))+
  geom_point()+
  geom_point(aes(dfplot2$knnregmod2.pred, col = 'orange'))+
  scale_colour_discrete(name='pred') +
  stat_smooth(method = "loess", col = "red")+
  stat_smooth(method = "lm", col = "yellow")


# fit a kNN regression with k = 100

knnregmod3 = knn.reg(dfair2, y=dfair2$Temp, k=100)
dfplot3 = cbind(dfair2, data.frame(knnregmod3$pred))
dfplot3


# plot Temp vs Ozone with kNN predictions, k = 100

ggplot(dfair2, aes(x = Ozone, y = Temp))+
  geom_point()+
  geom_point(aes(dfplot3$knnregmod3.pred, col = 'orange'))+
  scale_colour_discrete(name='pred') +
  stat_smooth(method = "loess", col = "red") +
  stat_smooth(method = "lm", col = "yellow")

```

As k increases, or number of neighbors, predictions become less staggered. (The predictions observed actually align with the bend in the polynomial regression with higher k values). In this situation, the lower the k value, the better (k\<2). Nearest neighbors in this situation has a max value of 116, or nrows.

kNN Predictions will be more accurate with more data points. Analysis of kNN regression vs. linear regression would be more appropriate then.
