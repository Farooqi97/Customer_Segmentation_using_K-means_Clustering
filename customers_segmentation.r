# Load the required libraries
library(ggplot2)
library(dplyr)
library(stats)
library(corrplot)
library(cluster)
library(factoextra)

# Load your data (assuming you have a CSV file)
data <- read.csv("C:/Users/Mall_Customers.csv")

# Load the data from a CSV file into a DataFrame
customer_data <- read.csv("C:/Users/Mall_Customers.csv")

# You can access and manipulate the DataFrame as needed
# For example, to view the first few rows of the DataFrame:
head(customer_data)

# View the first 5 rows of the DataFrame
head(customer_data, 5)

# Get information about the dataset's structure
str(customer_data)

# Check for missing values and count them in each column
col_missing <- sapply(customer_data, function(x) sum(is.na(x)))

# Display the count of missing values
col_missing

# Select columns 3 and 4 and extract their values as a matrix
X <- customer_data[, c(4, 5)]

# Print the matrix X
print(X)

plot(X, main = 'Customer Data set' , xlab='Annual Income', ylab='Spending Scores', frame=FALSE)

# Initialize an empty vector to store WCSS values
wcss <- vector("numeric", length = 10)

# Calculate WCSS for different numbers of clusters
for (i in 1:10) {
  kmeans_result <- kmeans(X, centers = i, nstart = 10)
  cluster_assignments <- kmeans_result$cluster
  cluster_centers <- kmeans_result$centers
  wcss[i] <- sum((X - cluster_centers[cluster_assignments, ])^2)
  print(wcss[i])
}

# Create an elbow plot
elbow_plot <- data.frame(Clusters = 1:10, WCSS = wcss)
ggplot(elbow_plot, aes(x = Clusters, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(title = "The Elbow Point Graph",
       x = "Number of Clusters",
       y = "WCSS") +
  theme_minimal()

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


# Perform K-means clustering with 5 clusters
kmeans <- kmeans(X, centers = 5, nstart = 10)

print(kmeans)

# Get cluster labels for each data point
Y <- kmeans$cluster

# Print the cluster labels
print(Y)


# Create a data frame with your data
df <- data.frame(X1 = X[, 1], X2 = X[, 2], Cluster = factor(Y))

# Create the scatter plot
plot <- ggplot(df, aes(x = X1, y = X2, color = Cluster)) +
  geom_point(size = 2) +
  geom_point(data = df %>% group_by(Cluster) %>% summarize(X1 = mean(X1), X2 = mean(X2)),
             aes(x = X1, y = X2), color = "black", size = 4, shape = 3) +
  labs(title = "Customer Groups",
       x = "Annual Income",
       y = "Spending Score",
       color = "Cluster") +
  scale_color_manual(values = c('violet', 'red', 'purple', 'orange', 'yellow')) +
  theme_minimal()
print(plot)