##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## set working directory so that files can be referenced without the full path
#setwd("~/Courses/Data Analytics/Fall25/labs/lab 4/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

X <- wine[,-1]
Y <- wine$Type


# Identify the principal components
X_scaled <- scale(X)
pca_wine <- princomp(X_scaled, cor = FALSE)

# PC1 and PC2 for the first two principal components
pc_plot_df <- data.frame(
  PC1 = pca_wine$scores[, 1],
  PC2 = pca_wine$scores[, 2],
  Type = Y
)

# Plot the dataset using the first two principal components
ggplot(pc_plot_df, aes(x = PC1, y = PC2, color = Type)) +
  geom_point(size = 2, alpha = 0.85) +
  labs(x = "PC1", y = "PC2", title = "First two PCs")

## Determine which variables contribute most to PC1
load_pc1 <- as.matrix(pca_wine$loadings)[, 1]
names(load_pc1) <- colnames(X)

# Order variables by level of contribution to PC1 
pc1_by_abs_loading <- sort(abs(load_pc1), decreasing = TRUE)
print(pc1_by_abs_loading)

# Train / Test split for comparison of two kNN models
n_obs <- nrow(wine)
train_idx <- sample.int(n_obs, size = floor(0.7 * n_obs))
test_idx <- setdiff(seq_len(n_obs), train_idx)

# kNN on four of the original variables
var_subset <- c("Alcohol", "Flavanoids", "Malic acid", "Ash")
train_X_sub <- wine[train_idx, var_subset, drop = FALSE]
test_X_sub  <- wine[test_idx,  var_subset, drop = FALSE]
pred_knn_sub <- knn(train_X_sub, test_X_sub, cl = Y[train_idx], k = 5)

# kNN on first two PCs
train_scores12 <- pca_wine$scores[train_idx, 1:2, drop = FALSE]
test_scores12  <- pca_wine$scores[test_idx,  1:2, drop = FALSE]
pred_knn_pc <- knn(train_scores12, test_scores12, cl = Y[train_idx], k = 5)

# Compare models: contingency tables, then confusion matrix for precision / recall / F1
contingency_knn_sub <- table(Actual = Y[test_idx], Predicted = pred_knn_sub)
contingency_knn_pc  <- table(Actual = Y[test_idx], Predicted = pred_knn_pc)

print(contingency_knn_sub)
print(confusionMatrix(pred_knn_sub, Y[test_idx]))

print(contingency_knn_pc)
print(confusionMatrix(pred_knn_pc, Y[test_idx]))
