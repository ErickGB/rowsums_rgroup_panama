cat("\014")
# ****************************************************
# Created by: Erick Gordon - erick.gordon@rowsums.com
# ****************************************************
# Load libraries ----
# install.packages(c("factoextra", "fpc", "NbClust"))
library(tidyverse)    # data manipulation
library(janitor)      # basic manipulation 
library(DataExplorer) # general data explorer
# for clustering
library(factoextra)
library(fpc)
library(NbClust)
# ****************************************************
# Excluding the column "Species" at position 5
data_tbl <- iris[, -5]
# Standardize
data_tbl <- scale(data_tbl)


data_tbl %>% 
	glimpse()

# ****************************************************
#Exploratory for find the best numbers of groups (k) 
# Setup for k-means loop 
km_out <- list()
sil_out <- list()
x <- vector()
y <- vector()
min_clust <- 2      # Hypothesized minimum number of segments
max_clust <- 30    # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
num_cols <- ncol(data_tbl) - 1 # start in 2
for (centr in min_clust:max_clust) {
        i <- centr-(min_clust-1) # relevels start as 1, and increases with centr
        print(i)
        set.seed(777) # For reproducibility
        km_out[i] <- list(kmeans(data_tbl, centers = centr, iter.max=1000000, nstart=1))
        sil_out[i] <- list(cluster::silhouette(km_out[[i]][[1]], dist(data_tbl)))
        # Used for plotting silhouette average widths
        x[i] = centr  # value of k
        y[i] = summary(sil_out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

data.frame(x, y) %>% 
	filter(y > 0.25) %>% 
	arrange(desc(y)) %>% 
	head(10)
# ****************************************************


# K-means clustering
km_res <- eclust(data_tbl, "kmeans", k = 3, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km_res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


# Hierarchical clustering
hc_res <- eclust(data_tbl, "hclust", k = 3, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc_res, show_labels = FALSE,
         palette = "jco", as.ggplot = TRUE)


# Silhouette information
silinfo <- km_res$silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km_res$size

fviz_silhouette(km_res, palette = "jco", ggtheme = theme_classic()) 


# Silhouette width of observation
sil <- km_res$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]


