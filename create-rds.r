library(tidyr)

level = 1000
tabular <- read.csv(sprintf("csvs/daily-%dhpa-anomaly.csv", level))

head(tabular)
tabular$lon <- tabular$lon-360*(tabular$lon > 180)
tabular$lonlat <- sprintf("%+.2f,%+.2f", tabular$lon, tabular$lat)


dropped_df <-subset(tabular, select = c(time, lonlat, anomaly))
head(dropped_df)


table_wide <- tidyr::spread(dropped_df,
                     key=lonlat,
                     value=anomaly)

rownames(table_wide) <- str(table_wide$time)
table_wide <- subset(table_wide, select = -c(time))
table_wide <- table_wide
head(table_wide, 1)

## save this for scott's code
saveRDS(table_wide, sprintf('data/hgt-SA-%d-anomaly.rds', level))
## table_wide <- readRDS('data/hgt-SA-anomaly.rds')
