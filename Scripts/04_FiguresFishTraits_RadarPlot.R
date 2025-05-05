# Load required libraries
library(fmsb)
library(ggplot2)
library(gridExtra)
library(tidyverse)

#2 plots categories ####
# Create sample data (replace this with your actual data)
dat <- read.csv("Data/Raw/Radar_FishTraits_4R.csv")
data <- dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="Species")
traits <- colnames(data)
species <- rownames(data)


dat <- read.csv("Data/Raw/Radar_FishTraits_4R.csv")
data <- dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="Species")

# Function to create a single spider plot
create_spider_plot <- function(data, title, colors) {
  # Add max and min rows
  data <- rbind(rep(5, 6), rep(1, 6), data)
  
  # Create the plot
  radarchart(data, 
             axistype = 1,
             pcol = adjustcolor(colors, alpha.f = 0.8),
             pfcol = adjustcolor(colors, alpha.f = 0.3),
             plwd = 2,
             cglcol = "grey",
             cglty = 1,
             axislabcol = "grey",
             caxislabels = seq(1, 5, 1),
             cglwd = 0.8,
             vlcex = 0.8)
  
  # Add a legend
  legend("bottom", 
         legend = rownames(data[-c(1, 2),]), 
         bty = "n", pch = 20, col = colors, 
         text.col = "black", cex = 0.8, pt.cex = 2)
}

# Define color palette
colors1 <- c("#4363d8", "#e6194B", "#3cb44b", "#ffe119")
colors2 <- c("#f58231", "#911eb4", "#42d4f4", "#f032e6")

# Set up the plotting area
par(mfrow = c(1, 2), mar = c(5, 2, 4, 2), oma = c(0, 0, 0, 0))  # Adjust margins

# Create two plots
tiff("Figures/RadarPlots.jpg", units= "in", width = 10, height = 8, res = 600)

plot.new()
# Create the left plot with extra space on the right
par(fig = c(0, 0.5, 0, 1), new = TRUE, mar = c(5, 4, 4, 2))
create_spider_plot(data[1:4,], "Species 1-4", colors1)

# Create the right plot with extra space on the left
par(fig = c(0.5, 1, 0, 1), new = TRUE, mar = c(5, 2, 4, 4))
create_spider_plot(data[5:8,], "Species 5-8", colors2)

dev.off()

# Reset plot parameters
par(mfrow = c(1, 1))

#1 plot categories ####
# Load required libraries
library(fmsb)

# Create sample data (replace this with your actual data)
dat <- read.csv("Data/Raw/Radar_FishTraits_4R.csv")
data <- dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="Species")
traits <- colnames(data)
species <- rownames(data)

# Add max and min rows for scaling
data <- rbind(rep(5, 6), rep(1, 6), data)

# Color-blind friendly palette
colors <- c("#4363d8", "#e6194B", "#3cb44b", "#ffe119", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "black")

# Create the radar chart
radarchart(data, 
           axistype = 1,
           pcol = adjustcolor(colors, alpha.f = 0.8),
           pfcol = adjustcolor(colors, alpha.f = 0.3),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(1, 5, 1),
           cglwd = 0.8,
           vlcex = 0.8)

# Add a legend
legend("topright", 
       legend = rownames(data[-c(1, 2),]), 
       bty = "n", pch = 20, col = colors, 
       text.col = "black", cex = 0.8, pt.cex = 2)

#2 plots raw USE THIS ONE####
# Read the data
dat <- read.csv("Data/Raw/Radar_RawData.csv") %>% 
  mutate(Species = case_when(
    Species == "Channel Catfish" ~ "Channel Catfish (E)",
    Species == "Common Carp" ~ "Common Carp (I)",
    Species == "Fathead Minnow" ~ "Fathead Minnow (I)",
    Species == "Flathead Chub" ~ "Flathead Chub (O)",
    Species == "Red Shiner" ~ "Red Shiner (O)",
    Species == "RG Silvery Minnow" ~ "RG Silvery Minnow (O)",
    Species == "River Carpsucker" ~ "River Carpsucker (O)",
    Species == "Western Mosquitofish" ~ "Western Mosquitofish (I)",
    TRUE ~ Species  # Default case to handle any species not specified above
  ))
data1 <- dat %>% 
  remove_rownames %>% 
  column_to_rownames(var="Species") 
traits <- colnames(data1)
species <- as.character(rownames(data1))

species1 <- species[c(1,2,4,6,7)]
species2 <- species[c(3,5,8)]

# Define a min-max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply the scaling function to all columns
df_scaled <- as.data.frame(lapply(data1, min_max_scale))%>% 
  dplyr::rename("Maximum Length" = Max.TL, "Serial" = Spawning.Periodicity,
         "Season" = Spawning.Season, "Parental Care" = Parental.Care,
         "Lifespan" = Longevity, "Appearance" = Juvenile.Appearance.Rank)

# Reorder columns according to the specified order
desired_order <- c("Maturation", "Appearance", "Season", "Parental Care",  
                   "Serial","Lifespan",   "Fecundity", "Maximum Length")
df_scaled <- df_scaled[, desired_order]

# Function to create a single spider plot
create_spider_plot <- function(data, title, colors, species_names) {
  # Add max and min rows
  data <- rbind(rep(1, ncol(data)), rep(0, ncol(data)), data)
  
  # Ensure the first two rows are exactly 1 and 0
  data[1,] <- 1
  data[2,] <- 0
  
  # Create the plot
  radarchart(data, 
             axistype = 1,
             pcol = adjustcolor(colors, alpha.f = 0.8),
             pfcol = adjustcolor(colors, alpha.f = 0.3),
             plwd = 2,
             cglcol = "black",
             cglty = 1,
             axislabcol = "black",
             cglwd = 0.8,
             vlcex = 1.5, 
             cex.axis = 2)
  
  # Add a legend
  legend("topleft", 
         legend = species_names, 
         bty = "n", pch = 20, col = colors, 
         text.col = "black", cex = 1.5, pt.cex = 2)
}

# Define color palette
colors1 <- c("#911eb4", "#42d4f4",  "#f58231", "#ffe119",  "#3cb44b","#e6194B")
colors2 <- c( "#4363d8", "#f032e6", "black")


# Create two plots
tiff("Figures/RadarPlots_ScaledRawV3.jpg", units= "in", width = 14, height = 12, res = 600)

plot.new()
# Create the left plot with extra space on the right
par(fig = c(0, 1, 0.5, 1), new = TRUE, mar = c(2, 2, 2, 2))
create_spider_plot(df_scaled[c(1,2,4,6,7),], "Species 1-4", colors1, species1)

# Create the right plot with extra space on the left
par(fig = c(0, 1, 0, 0.5), new = TRUE, mar = c(2, 2, 2, 2))
create_spider_plot(df_scaled[c(3,5,8),], "Species 5-8", colors2, species2)

dev.off()

