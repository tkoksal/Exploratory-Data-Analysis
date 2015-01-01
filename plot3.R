#################################################
#######             PLOT 3          #############
#################################################

# Read data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Check the number of duplicates in nei
dim(nei[duplicated(nei),])
dim(scc[duplicated(scc),]) # no duplicates found

# Duplicates removed and the resulting dataset assigned to 'unei'
unei <- unique(nei)

# Convert unei$SCC from character to a factor type
unei$SCC <- factor(unei$SCC)

# Merge 'unei' and 'scc' into 'munei'
munei <- merge(unei, scc, by = "SCC")

# Convert pollutant source 'type' from character to a factor variable
munei$type <- factor(munei$type)

# Select cases specific to Baltimore City and create 4 different datasets
# filtered with respect to 4 different pollutant sources, which are
# aggregate by year with computed total emissions across years 

library(dplyr)

# POINT
t1 <- munei %>%
    filter(fips == "24510" & type == "POINT") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# NONPOINT
t2 <- munei %>%
    filter(fips == "24510" & type == "NONPOINT") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions))

# ON-ROAD
t3 <- munei %>%
    filter(fips == "24510" & type == "ON-ROAD") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# NON-ROAD
t4 <- munei %>%
    filter(fips == "24510" & type == "NON-ROAD") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 


library(ggplot2)

# Open PNG device; create 'plot3.png' in the working directory
png(file = "plot3.png")

ggplot(t1, aes(year, totalemiss)) + geom_line(aes(color = "POINT")) + 
    scale_x_continuous(breaks = t1$year) +
    geom_line(data = t2, aes(year, totalemiss, color = "NONPOINT")) +
    geom_line(data = t3, aes(year, totalemiss, color = "ON-ROAD")) +
    geom_line(data = t4, aes(year, totalemiss, color = "NON-ROAD")) +
    scale_color_manual("Pollutant Source Type", 
                       values = c("POINT" = "red", 
                                  "NONPOINT" = "blue",
                                  "ON-ROAD" = "green",
                                  "NON-ROAD" = "yellow")) +
    geom_point(data = t1, aes(year, totalemiss)) +
    geom_point(data = t2, aes(year, totalemiss)) +
    geom_point(data = t3, aes(year, totalemiss)) +
    geom_point(data = t4, aes(year, totalemiss)) +
    labs(title = "Total Emissions from PM2.5 by Source Type, 
         Baltimore City, MD", x = "Year", 
         y = "Total Emissions (PM2.5 in tons)") +
    theme(legend.position=c(.85,.85))

dev.off() # close PNG file device


