library(spdep)

setwd("c:/Users/QUXINHAO/Desktop/STAT293B/Final project")
data <- read.csv("Data sets/boston_c.csv")

coords <- cbind(data$LON, data$LAT)
knn <- knearneigh(coords, k = 6)
nb <- knn2nb(knn)
listw <- nb2listw(nb, style = "W")

moran_test <- moran.test(data$CMEDV, listw)
print(moran_test)

# Moran scatterplot
png("moran_scatterplot.png", width = 600, height = 600, res = 120)
moran.plot(data$CMEDV, listw, 
           xlab = "Standardized CMEDV", 
           ylab = "Spatially Lagged CMEDV",
           main = "Moran's I Scatterplot for Boston Housing")
dev.off()

# CMEDV spatial distribution
z <- scale(data$CMEDV)[, 1]
wz <- lag.listw(listw, z)
quadrant <- rep(NA, length(z))
quadrant[z >= 0 & wz >= 0] <- "HH"
quadrant[z <  0 & wz <  0] <- "LL"
quadrant[z >= 0 & wz <  0] <- "HL"
quadrant[z <  0 & wz >= 0] <- "LH"
quadrant <- factor(quadrant, levels = c("HH", "LH", "LL", "HL"))

col_quad <- c("HH" = "#E41A1C", "LH" = "#92C5DE", "LL" = "#2166AC", "HL" = "#F4A582")

png("spatial_distribution_CMEDV.png", width = 700, height = 600, res = 120)
par(mar = c(4, 4, 3, 6), xpd = TRUE)
plot(data$LON, data$LAT,
     col = col_quad[quadrant],
     pch = 19, cex = 0.8,
     xlab = "Longitude", ylab = "Latitude",
     main = "Spatial Distribution of CMEDV")
legend("topright", inset = c(-0.22, 0), legend = c("HH", "LH", "LL", "HL"),
       fill = col_quad, bty = "n", title = "Quadrant")
dev.off()