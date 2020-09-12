#Both the packages are used for Data Visualization

#Downloading the package
# install.packages("dplyr")
library(dplyr)
library(ggplot2)


#Load data from internet location.


PATH <- "C:\\Users\\ravs\\Development\\ML\\IIM\\L5\\Cluster\\Computers.csv"
df <- read.csv(PATH) %>%
  select(-c(X, cd, multi, premium))

colnames(df)

head(df)
glimpse(df)

summary(df)


#Creating a function that will rescale the data.
# The formula used in scaling is (x-mean)/sd

rescale_data_df <- df %>%
  mutate(
    scale_price = scale(price),
    scale_hd = scale(hd),
    scale_ram = scale(ram),
    scale_screen = scale(screen),
    scale_ads = scale(ads),
    scale_trend = scale(trend)
  ) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

head(rescale_data_df)

# kmeans(df, k)
# arguments:
#   -df: dataset used to run the algorithm
# -k: Number of clusters



#Actual K Means
computer_cluster <- kmeans(rescale_data_df, 5)


# The list pc_cluster contains following important elements:
# computer_cluster$cluster: Indicates the cluster of each observation
# computer_cluster$centers: The cluster centres
# computer_cluster$withinss: Within sum of square. The number of components return is equal to `k`
# computer_cluster$size: Number of observation within each cluster


## How to find the Optimal value of K
# Elbow mwthod is used the explain the variance by each cluster number


################################################################################3
# You can construct the elbow graph and find the optimal k as follow:
#
# Step 1: Construct a function to compute the total within clusters sum of squares
# Step 2: Run the algorithm n times
# Step 3: Create a data frame with the results of the algorithm
# Step 4: Plot the results


#Step 1) Construct a function to compute the total within clusters sum of squares

kmeanCluster_withinss <- function(k) {
  cluster <- kmeans(rescale_data_df, k)
  return (cluster$tot.withinss)
}

#You can test the function with equals 5
kmeanCluster_withinss(2)

#Step 2 - Run the algorithm n times

# Set maximum cluster
max_k <- 15

#----------------------------------------------------------

#apply
#apply(x, MARGIN, FUN)
# -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
#   -MARGIN=1`: the manipulation is performed on rows
# -MARGIN=2`: the manipulation is performed on columns
# -MARGIN=c(1,2)` the manipulation is performed on rows and columns
# -FUN: tells which function to apply.
#Apply a function to the rows or columns or both
#Data frame or matrix
#vector, list, array



#lapply
#lapply(X, FUN)
#Apply a function to all the elements of the input
#List, vector or data frame
#list

#sapply
#sappy(X FUN)
#Apply a function to all the elements of the input
#List, vector or data frame
#vector or matrix

#----------------------------------------------------------







# Run algorithm over a range of k
wss <- sapply(2:max_k, kmeanCluster_withinss)

wss

elbow <- data.frame(2:max_k, wss)

elbow

#Plot the graph to visualize where is the elbow point
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 15, by = 1))

##8 is more optimal value of K from graph

#Examining the cluster
pc_cluster_2 <- kmeans(rescale_data_df, 8)


pc_cluster_2$cluster
pc_cluster_2$centers
pc_cluster_2$size

result <- cbind(df, pc_cluster_2$cluster)
tail(result)
