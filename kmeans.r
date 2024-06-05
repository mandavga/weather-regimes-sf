library(raster)
library(stringr)

level = 1000

table_wide <- readRDS(sprintf('data/hgt-SA-%d-anomaly.rds', level))

pca <- prcomp(table_wide, rank = 10, scale. = TRUE)

summary(pca)

pca_ords <- as.data.frame(pca$x)
pca_ords$date <- seq(as.Date("1979-01-01"), as.Date("2022-06-30"), by="days")

write.csv(pca_ords, sprintf("models/simple-kmeans-%d/pca-ords.csv", level), row.names=FALSE)

## # To see if the data looks similar on discarding the higher PC values
## table_appx <- t(t(pca$x %*% t(pca$rotation)) * pca$scale + pca$center)

## table_appx[1:4,1:5]
## table_wide[1:4,1:5]

## table_appx_long <- tidyr::pivot_longer(as.data.frame(table_appx), cols = -c(), names_to = "lonlat", values_to = "delta_hht")

## head(table_appx_long)
## org_raster <- rasterFromXYZ()

km <- kmeans(pca$x, centers = 6, nstart = 100)

kmeans_centers <- km$centers
kmeans_revert <- t(t(kmeans_centers %*% t(pca$rotation)) * pca$scale + pca$center)

regimes <- data.frame(date = pca_ords$date, clusters = km$cluster)

## regimes$date <- stringr::str_trunc(stringr::str_replace_all(regimes$date, "_", "-"), 10, ellipsis = "")
head(regimes)
write.csv(regimes, "new-out/kmeans-clusters.csv")


get_raster <- function(df) {
    df <- as.data.frame(df)
    df$lat <- 0
    df$lon <- 0

    for (i in 1:length(df$lonlat)){
        ll <- df$lonlat[i]
        ll_split <- str_split_fixed(ll, ",", n=2)
        lon <- as.numeric(ll_split[1])
        lat <- as.numeric(ll_split[2])
        df$lat[i] <- lat
        df$lon[i] <- lon
    }

    xyz <- subset(df, select = c("lon", "lat", "geopotential_ht"))

    dfr <- rasterFromXYZ(xyz = xyz)
    return(dfr)
}



for (N in 1:6){
    means_N <- data.frame(lonlat = names(kmeans_revert[N,]), geopotential_ht=as.numeric(kmeans_revert[N,]))
    dfr1 <- get_raster(means_N)
    writeRaster(dfr1, sprintf("new-out/rasters/kluster-%d.tif", N))
    ## plot(dfr1)
}
