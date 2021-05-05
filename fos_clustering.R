library(tidyverse)
library(cluster)
library(factoextra)

#Set WD and load csv
setwd('C:/Users/pkmnn/Documents/School/CS-504-001/Sprints and Project/Datasets/Imputed Data')
college_data <- read.csv('FOS_ALG2.csv')

#Create Dataframe
college_df <- data.frame(row.names = college_data$cipdesc,
  debt = college_data$debt_all_stgp_any_mdn,
  wage = college_data$earn_mdn_hi_1yr,
  working = college_data$earn_count_wne_hi_1yr,
  not_working = college_data$earn_count_nwne_hi_1yr
)

#Scale the data
college_df <- scale(college_df)

#Clustering Distance Measures
distance <- get_dist(college_df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(college_df, centers = 2, nstart = 25)
str(k2)
k2

s2 = fviz_cluster(k4, geom = "point", data = college_df) + ggtitle("Field of Study - 4 Clusters")

k3 <- kmeans(college_df, centers = 3, nstart = 25)
k4 <- kmeans(college_df, centers = 4, nstart = 25)
k5 <- kmeans(college_df, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = college_df) + ggtitle("clusters = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = college_df) + ggtitle("clusters = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = college_df) + ggtitle("clusters = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = college_df) + ggtitle("cluster = 5")

library(gridExtra)
grid.arrange(s1,s2, nrow=1)

#Check how many clusters work best with different emethods.
#Elbow method

set.seed(123)
fviz_nbclust(college_df, kmeans, method = "wss")

#Average Silhouette Method
s1 = fviz_nbclust(college_df, kmeans, method = "silhouette") + ggtitle("Field of Study - optimal clusters")

#Gap Static Method
set.seed(123)
gap_stat <- clusGap(college_df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 2
set.seed(123)
final <- kmeans(college_df, 2, nstart = 25)
print(final)
fviz_cluster(final, data = college_df)
