library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("report")
library("lsr")
library('plot.matrix')
library("ggpubr")
library(zoo)
library(multcompView) #Tukey
library(tidyquant)
library(patchwork)
library(gridExtra)

regimes <-
  read.csv("./new-out/weather-regimes.csv") %>%   ###kmeans-clusters ###weather-regimes
  dplyr::select(-X)# Name pattern can be changed, x is unneccesary
## clusters as factors because clusters are not number
regimes$clusters <- as.factor(regimes$clusters)
regimes$date <- as.Date(regimes$date)
head(regimes)
# monthly histogram of clusters
regimes$month <- month(regimes$date)
ggplot(regimes, aes(x = month, fill = clusters)) + geom_bar()

# look at the histogram of clusters
ggplot(regimes, aes(x = clusters, fill = clusters)) + geom_bar()

ggplot(regimes, aes(x = clusters, fill = clusters)) +
  geom_bar(stat = "count")
#count per year and continuos plot

# Group by year, month, and cluster, and calculate the count of observations
cluster_counts <- regimes %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date), clusters) %>%
  summarise(count = n())

cluster_counts_all <- regimes %>%
  group_by(clusters) %>%
  summarise(count = n())

#only for count
cluster_counts_peryear <- regimes %>%
  group_by(year = lubridate::year(date), clusters) %>%
  summarise(count = n())%>%
  group_by(clusters) %>%
  summarise(count = mean(count))
##

ggplot(cluster_counts, aes(x = month, fill = clusters)) +
  geom_density(alpha = 0.8, position="fill") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Density", fill = "clusters") +
  theme_minimal()

ggplot(cluster_counts, aes(x = month, fill = clusters)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Density", fill = "Cluster") +
  theme_minimal() +
  facet_wrap(~ clusters, scales = "free_y")
# Plot each of them separately
# Define a custom color vector based on your descriptions
scales:: hue_pal()(6)
custom_colors <- c("#F8766D", "#B79F00" , "#00BA38",  "#00BFC4",  "#619CFF",  "#F564E3")
regimes_filtered <- regimes %>%
  filter(format(date, "%m") %in% c("04", "05", "06", "07", "08"))

# Plot bar plot for entire time period
ggplot(regimes_filtered, aes(x = clusters)) +
  geom_bar() +
  labs(title = "Cluster Distribution (April to August, Entire Time Period)",
       x = "Clusters", y = "Frequency")

# Function to filter by decade
filter_by_decade <- function(data, start_year, end_year) {
  data %>%
    filter(year(date) >= start_year, year(date) <= end_year)
}

# Function to plot bar plot for a specific decade with custom colors
plot_decade <- function(data, start_year, end_year, colors) {
  filtered_data <- filter_by_decade(data, start_year, end_year)
  ggplot(filtered_data, aes(x = clusters, fill = factor(clusters))) +
    geom_bar() +
    scale_fill_manual(values = colors) +
    guides(fill = FALSE) +  # Remove legend
    coord_cartesian(ylim = c(0, 600)) +  # Set y-axis limits
    theme(axis.title.x = element_blank(),  # Remove individual axis titles
          axis.title.y = element_blank()) +
    labs(title = paste0(start_year, " - ", end_year)   )
}

# Create plots for each decade
plot_1979_1989 <- plot_decade(regimes_filtered, 1979, 1989, custom_colors)
plot_1990_1999 <- plot_decade(regimes_filtered, 1990, 1999, custom_colors)
plot_2000_2009 <- plot_decade(regimes_filtered, 2000, 2009, custom_colors)
plot_2010_2021 <- plot_decade(regimes_filtered, 2010, 2021, custom_colors)

# Combine plots into a collage with shared y-axis
combined_plots <- grid.arrange(plot_1979_1989, plot_1990_1999, plot_2000_2009, plot_2010_2021,
                               ncol = 2, top= "WR Frequency (April to August)", bottom="WR", left="Frequency")

# Filter for WR1
wr1_data <- filter(regimes, clusters == 1)

# Extract year and month from the date
wr1_data$Year <- format(wr1_data$date, "%Y")
wr1_data$Month <- format(wr1_data$date, "%m")

# Aggregate data to count occurrences of WR1 for each month and year
wr1_counts <- wr1_data %>%
filter(format(date, "%m") %in% c("04", "05", "06", "07", "08", "09"))%>%
  group_by(Year, Month) %>%
  summarise(count = n())

# Calculate the average count for each month across all years
average_counts <- wr1_counts %>%
  group_by(Month) %>%
  summarise(avg_count = mean(count))

# Merge the average counts with the original data
wr1_counts <- merge(wr1_counts, average_counts, by = "Month")

# Create a new column to indicate if the count is higher or lower than the average
wr1_counts$color <- ifelse(wr1_counts$count > wr1_counts$avg_count, "blue", "red")

# Create the matrix plot
ggplot(data = wr1_counts, aes(x = Year, y = Month, fill = color)) +
  geom_tile(color = "black") +
  scale_fill_identity() +
  labs(title = "WR1 Counts Compared to Overall Average for the month",
       x = "Year",
       y = "Month",
       fill = "Count vs Average") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Initialize a data frame to store cluster counts per year
cluster_year_counts <- data.frame()

# Loop through clusters and create and save each plot
unique_clusters <- unique(cluster_counts$clusters)

# for (i in 1:6) {
#   cluster <- i
#   plot_data <- cluster_counts %>%
#     filter(clusters == cluster)
#
#   # Create a density plot for the current cluster with a specific color
#   plot <- ggplot(plot_data, aes(x = month)) +
#     geom_density(alpha = 0.8, fill = "grey") +
#     scale_x_continuous(limits=c(1,12), breaks = seq(1,12,2), labels = seq(1,12,2)) +
#     scale_y_continuous(limits = c(0,0.15), breaks = c(0, 0.05, 0.1, 0.15)) +
#      #labs(x = "Month", y = "Density", fill = "Cluster") +
#     theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12),
#       plot.background = element_rect(fill = "white"))+  # Set white background
#
#         guides(fill = "none") #+
#     #scale_fill_manual(values = custom_colors[i])  # Assign a specific color to the cluster
#
#   # Save each plot with a unique filename
#   filename <- paste("../plots/monthly_density/density_plot_cluster_", cluster, ".pdf", sep = "")
#   ggsave(filename, plot, width = 3, height = 1.5)  # Adjust width and height as needed
#
#   # Store cluster counts per year in cluster_year_counts data frame
#   year_counts <- plot_data %>%
#     group_by(year,month) %>%
#     summarise(cluster = cluster, count = sum(count))
#
# }



#look at yearly pattern
regimes$year <- year(regimes$date)
ggplot(regimes, aes(x = year, fill = clusters)) + geom_bar()

# regimes annual count
ggplot(regimes, aes(x = year, color = clusters)) + geom_line(stat='count') #+ geom_ma(ma_fun = SMA, n = 10)

regimes.y<-regimes%>%
  group_by(year,clusters)%>%
  summarize(count = n())

regimes.10y<-regimes.y %>%
  group_by(clusters) %>%
  mutate(moving_count10 = rollmean(count, k=10, fill=NA, align='center'))

ggplot(regimes.10y, aes(x = year, y=moving_count10 ,color = clusters)) + geom_line() + ylab('Frequency (days/year)')+
  ggtitle("Ten year running means of frequency of occurence per year for each weather regime")

# #Separately save
# for (cluster in 1:6) {
#   plot_data <- regimes.10y %>%
#     filter(clusters == cluster)
#
#   plot <- ggplot(plot_data, aes(x = year, y = moving_count10)) +
#     geom_line() +
#     ylab('Frequency (days/year)') +
#     scale_y_continuous(limits = c(20,115))+
#     theme(axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12),
#           plot.background = element_rect(fill = "white"))
#
#   # Save each plot with a unique filename
#   filename <- paste("../plots/runningmean/cluster_", cluster, "_plot.pdf", sep = "")
#   ggsave(filename, plot, width = 3, height = 1.5)  # Adjust width and height as needed
# }

# look at the histogram of clusters
ggplot(regimes, aes(x = clusters, fill = clusters)) + geom_bar()

#combined

# Read the data for precip_eerste and precip_sabie
precip_eerste <- read.csv("D:/UNESCO_southafrica/weathergenerator/Gaurav_codes/ncep (1)/ncep/data/average-precip-eerste-dump.csv") %>%
  select(Date, mean) %>%
  mutate(precip = ifelse(is.na(mean), 0, mean),
         date = as.Date(Date),
         basin = "Eerste (SW region)")

precip_sabie <- read.csv("D:/UNESCO_southafrica/weathergenerator/Gaurav_codes/ncep (1)/ncep/data/average-precip-sabie-dump.csv") %>%
  select(Date, mean) %>%
  mutate(precip = ifelse(is.na(mean), 0, mean),
         date = as.Date(Date),
         basin = "Sabie (NE region)")

precip_luvuvhu <- read.csv("D:/UNESCO_southafrica/weathergenerator/Gaurav_codes/ncep (1)/ncep/data/average-precip-luvuvhu.csv") %>%
  select(Date, mean) %>%
  mutate(precip = ifelse(is.na(mean), 0, mean),
         date = as.Date(Date),
         basin = "Luvuvhu (N region)")

joined_eerste <- inner_join(regimes, precip_eerste, by = "date")
joined_sabie <- inner_join(regimes, precip_sabie, by = "date")
joined_luvuvhu <- inner_join(regimes, precip_luvuvhu, by = "date")
# Transform 0 values to the lowest nonzero value times 0.01 for both datasets
smallest_nonzero <- min(min(joined_eerste$precip[joined_eerste$precip > 0]),
                        min(joined_sabie$precip[joined_sabie$precip > 0]),
                        min(joined_luvuvhu$precip[joined_luvuvhu$precip > 0]))
joined_eerste$precip.l<-joined_eerste$precip
joined_eerste$precip.l[joined_eerste$precip.l == 0] <- smallest_nonzero * 0.01
joined_sabie$precip.l<-joined_sabie$precip
joined_sabie$precip.l[joined_sabie$precip.l == 0] <- smallest_nonzero * 0.01
joined_luvuvhu$precip.l<-joined_luvuvhu$precip
joined_luvuvhu$precip.l[joined_luvuvhu$precip.l == 0] <- smallest_nonzero * 0.01
# Calculate mean precipitation for each cluster
mean_precipitation <- rbind(
  joined_eerste %>% group_by(clusters) %>% summarise(mean_precip = mean(precip)),
  joined_sabie %>% group_by(clusters) %>% summarise(mean_precip = mean(precip)),
  joined_luvuvhu %>% group_by(clusters) %>% summarise(mean_precip = mean(precip))
)
# Plot
ggplot(rbind(joined_eerste, joined_sabie, joined_luvuvhu), aes(x = clusters, y = log(precip.l), fill = clusters)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black", aes(group = clusters)) + # Add mean points
  #geom_hline(data = mean_precipitation, aes(yintercept = log(mean_precip)), color = "blue", linetype = "dashed") + # Add mean line
  labs(x = "WRs", y = "Log(Precipitation)", title = "Boxplot of Log(Precipitation) by WRs") +
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~ basin, ncol = 1)


# Create a boxplot for log-transformed precipitation for Eerste and Sabie
ggplot(rbind(joined_eerste, joined_sabie, joined_luvuvhu), aes(x = clusters, y = log(precip.l), fill = clusters)) +
  geom_boxplot() +
  labs(x = "WRs", y = "Log(Precipitation)", title = "Boxplot of Log(Precipitation) by WRs") +
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~ basin, ncol=1)  # Two columns for Eerste and Sabie
