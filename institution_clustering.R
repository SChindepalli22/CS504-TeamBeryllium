library(tidyverse)
library(cluster)
library(factoextra)

#Set WD and load csv
setwd('C:/Users/pkmnn/Documents/School/CS-504-001/Sprints and Project/Datasets/Imputed Data')
college_data <- read.csv('institution_alg.csv')

#Create Dataframe
college_df <- data.frame(row.names = c(college_data$INSTNM),
                         sat = college_data$sat_avg,
                         tuition = college_data$tuitionfee_in,
                         debt = college_data$debt_mdn,
                         wages = college_data$mn_earn_wne_p10
                         )


#Scale the data
college_df <- scale(college_df)

summary(college_df)

#Clustering Distance Measures
distance <- get_dist(college_df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(college_df, centers = 2, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = college_df)

k3 <- kmeans(college_df, centers = 3, nstart = 25)
k4 <- kmeans(college_df, centers = 4, nstart = 25)
k5 <- kmeans(college_df, centers = 5, nstart = 25)

k4

p1 <- fviz_cluster(k2, geom = "point", data = college_df) + ggtitle("Institution - 2 clusters")
p2 <- fviz_cluster(k3, geom = "point",  data = college_df) + ggtitle("clusters = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = college_df) + ggtitle("clusters = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = college_df) + ggtitle("clusters = 5")

library(gridExtra)
grid.arrange(s1, p1,nrow = 1)

#Check how many clusters work best with different emethods.
#Elbow method

set.seed(123)
fviz_nbclust(college_df, kmeans, method = "wss")

#Average Silhouette Method
s1 <- fviz_nbclust(college_df, kmeans, method = "silhouette") + ggtitle("Institution - silhouette method")

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

