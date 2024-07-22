
library(EBImage)
library(imager)
library(spatstat)
tiff_file <- "F10C.jpg"
image <- load.image(tiff_file)

gray_img <- grayscale(image)
threshold_value <- 0.7
binary_img <- threshold(gray_img, threshold_value)
plot(binary_img, main = "Binary Image")






# Label connected components in the binary image
labeled_img <- label(binary_img)

# Extract the unique labels (excluding background)
labels <- unique(labeled_img[labeled_img > 0])






# Label connected components in the pixel image
#labeled_img <- connected(im)

# Check the structure of the labeled image
#str(labeled_img)


# Extract the component labels
#labels <- labeled_img$v  # Extract the matrix of labels

# Ensure labels is a vector
#labels_vector <- as.vector(labels)

# Get unique labels
#unique_labels <- unique(labels_vector)


# Compute centroids of labeled components
# Compute centroids of labeled components
centroids <- sapply(labels, function(label) {
  subset_points <- which(labeled_img == label, arr.ind = TRUE)
  centroid_x <- mean(subset_points[, 2])  # x-coordinate
  centroid_y <- mean(subset_points[, 1])  # y-coordinate
  c(centroid_x, centroid_y)
})

# Convert centroids to data frame for easier handling
centroids_df <- as.data.frame(t(centroids))
colnames(centroids_df) <- c("x", "y")

# Plot the original grayscale image and overlay the centroids
plot(imager::as.cimg(gray_img), main = "Centroids Overlay")
points(centroids_df$x, centroids_df$y, col = "red", pch = 19)



# Define the observation window based on the image dimensions
window <- owin(xrange = c(1, dim(gray_img)[2]), yrange = c(1, dim(gray_img)[1]))
# Create the PPP object
ppp_object <- ppp(x = centroids_df$x, y = centroids_df$y, window = window)


# Define colors for the points
n_points <- nrow(centroids_df)
point_colors <- rainbow(n_points)  # You can choose a different palette if you prefer


# Plot the PPP object with different colors for the points
plot(ppp_object, main = "Point Pattern Plot of Centroids")


# Overlay the centroids with different colors
# Define colors for the points
n_points <- nrow(centroids_df)
point_colors <- rainbow(n_points)  # You can choose a different palette if you prefer

# Overlay the centroids with different colors
points(ppp_object, col = point_colors, pch = 19)


# Intensity estimation
intensity_estimate <- density(ppp_data, sigma = bw.diggle)
plot(intensity_estimate, main = "Intensity Estimation")

# Cluster analysis using Ripley's K function
k_estimate <- Kest(ppp_data)
plot(k_estimate, main = "Ripley's K Function")

# Compare to theoretical Poisson process
envelope_k <- envelope(ppp_data, Kest, nsim = 99)
plot(envelope_k, main = "K Function with Envelopes")

