#################################################
#######             PLOT 6          #############
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


# Based on the forum discussions, it seems that the best approach to select
# cases related to emissions from motor vehicle sources sources, is to select 
# cases based on 'type' whose value is either 'ON-ROAD' or 'NON-ROAD'.

# Select cases related to emissions from motor vehicle sources and 
# specific to Baltimore City in one dataset and specific to Los Angeles in
# another dataset. Then both of these datasets are aggregated by year and 
# total emissions are computed across years.


library(dplyr)

# emissions from motor vehicle sources in Baltimore City
m1 <- munei %>%
    filter(type == "ON-ROAD" | type == "NON-ROAD") %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# emissions from motor vehicle sources in Los Angeles
m2 <- munei %>%
    filter(type == "ON-ROAD" | type == "NON-ROAD") %>%
    filter(fips == "06037") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 


library(ggplot2)

# Open PNG device; create 'plot6.png' in the working directory
png(file = "plot6.png")

ggplot(m1, aes(year, totalemiss)) + geom_line(aes(color = "Baltimore")) + 
    scale_x_continuous(breaks = m1$year) +
    geom_line(data = m2, aes(year, totalemiss, color = "Los Angeles")) +
    scale_color_manual("City", 
                       values = c("Baltimore" = "red", 
                                  "Los Angeles" = "green")) +
    geom_point(data = m1, aes(year, totalemiss)) +
    geom_point(data = m2, aes(year, totalemiss)) +
    labs(title = "Total Emissions from Motor Vehicle Sources", x = "Year", 
         y = "Total Emissions (PM2.5 in tons)") +
    theme(legend.position=c(.9,.9))

dev.off() # close PNG file device

