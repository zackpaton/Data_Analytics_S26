################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)
library(class)
library(cluster)



## read data
abalone <- read.csv("~/Data Analytics/GitHubRepo/Lab3/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
#featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally (colour by class column age.group)
ggpairs(train, ggplot2::aes(colour = age.group))





# Exercise 1

train.y <- train$age.group
test.y  <- test$age.group


## Model 1: features 1-3
pp.a <- preProcess(train[, 1:3], method = c("center", "scale"))
train.a <- predict(pp.a, train[, 1:3])
test.a  <- predict(pp.a, test[, 1:3])


# Model 2: metrics 4-6
pp.b <- preProcess(train[, 4:6], method = c("center", "scale"))
train.b <- predict(pp.b, train[, 4:6])
test.b  <- predict(pp.b, test[, 4:6])


# Set initial k value to 5, find predicted vs actual
k0 <- 5
pred.a <- knn(train.a, test.a, train.y, k = k0)
pred.b <- knn(train.b, test.b, train.y, k = k0)

acc.a <- mean(pred.a == test.y)
acc.b <- mean(pred.b == test.y)

table(Predicted = pred.a, Actual = test.y)
table(Predicted = pred.b, Actual = test.y)


# Find the better performing model based on k=5 (Model 2 during my testing)
if (acc.a >= acc.b) {
  tr <- train.a
  te <- test.a
} else {
  tr <- train.b
  te <- test.b
}


# Test a large range of k values (k=1 to k=80)
k.max <- min(80, nrow(tr) - 1)
acc.k <- sapply(seq_len(k.max), function(k) mean(knn(tr, te, train.y, k = k) == test.y))
best.k <- which.max(acc.k)


# Print summary table for optimal k (k=27 during my testing)
pred.tuned <- knn(tr, te, train.y, k = best.k)
table(Predicted = pred.tuned, Actual = test.y)





# Exercise 2

# Use the same subset as Model 2 from Exercise 1 as that was optimal
pp.ex2 <- preProcess(train[, 4:6], method = c("center", "scale"))
Z <- as.matrix(predict(pp.ex2, abalone.sub[, 4:6, drop = FALSE]))
d <- dist(Z)

# Use a range of k=2 to k=10
k.range <- 2:10


# Test the range of k values to find the best k value (k=2 during my testing)
avg.sil.km <- sapply(k.range, function(k) {
  km <- kmeans(Z, centers = k, nstart = 25)
  mean(silhouette(km$cluster, d)[, 3])
})
best.k.km <- k.range[which.max(avg.sil.km)]


# Test the range of k values to find the best k value (k=2 during my testing)
avg.sil.pam <- sapply(k.range, function(k) {
  cl <- pam(d, k, pamonce = 5, cluster.only = TRUE)
  mean(silhouette(cl, d)[, 3])
})
best.k.pam <- k.range[which.max(avg.sil.pam)]


# Plot silhouette plots for both models with their optimum k (k=2 during my testing)
km.opt <- kmeans(Z, centers = best.k.km, nstart = 25)
plot(silhouette(km.opt$cluster, d))

pam.opt <- pam(d, k = best.k.pam, pamonce = 5)
plot(silhouette(pam.opt))


## EOF ##