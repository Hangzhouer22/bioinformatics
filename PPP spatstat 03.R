library(dplyr)
library(tidyverse)
library(GEOquery)
install.packages(geoquery)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
getwd()

library(EBImage)

image_path <- "F10Dd.jpg"
img <- load.image(image_path)

gray_img <- grayscale(img)
threshold_value <- 0.50
binary_img <- threshold(gray_img, threshold_value)
plot(binary_img, main = "Binary Image")
labeled_img <- label(binary_img)

# Ensure labeled image is 2D
labeled_img <- as.Image(labeled_img)

dim(labeled_img)
labeled_img_2d <- labeled_img[,,1,1]
dim(labeled_img_2d)

features <- computeFeatures.moment(labeled_img_2d)
centroids_x <- features[, "m.cx"]
centroids_y <- features[, "m.cy"]
centroids_df <- data.frame(x = centroids_x, y = centroids_y)
display(gray_img, method = "raster")
points(centroids_df$x, centroids_df$y, col = "red", pch = 19)







#
#
#
#
#



# Extract the coordinates of the centroids
#centroids <- as.data.frame(centroid(labeled_img))
coordinates <- centroids_df %>% select(x, y)

# Convert coordinates to a data frame
coordinates <- data.frame(x = coordinates$x, y = coordinates$y)

# Display the centroids
plot(coordinates$x, coordinates$y, main = "Centroids of Stained Cells", xlab = "X", ylab = "Y", pch = 19, col = "red")

# Define the observation window (use the dimensions of the image)
window <- owin(c(0, dim(img)[2]), c(0, dim(img)[1]))

# Create the point pattern object
ppp_data <- ppp(coordinates$x, coordinates$y, window = window)

# Basic visualization
plot(ppp_data, main = "Point Pattern of Stained Cells")

# Intensity estimation
intensity_estimate <- density(ppp_data, sigma = bw.diggle)
plot(intensity_estimate, main = "Intensity Estimation")

# Cluster analysis using Ripley's K function
k_estimate <- Kest(ppp_data)
plot(k_estimate, main = "Ripley's K Function")

# Compare to theoretical Poisson process
envelope_k <- envelope(ppp_data, Kest, nsim = 99)
plot(envelope_k, main = "K Function with Envelopes")

