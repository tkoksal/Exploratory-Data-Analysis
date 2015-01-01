#################################################
#######             PLOT 5          #############
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
# cases related to emissions from motor vehicle sources, is to select 
# cases based on 'type' whose value is either 'ON-ROAD' or 'NON-ROAD'.

# Select cases related to emissions from motor vehicle sources as well as 
# specific to Baltimore City and then aggregate by year and compute total 
# emissions across years

library(dplyr)
p5 <- munei %>%
    filter(type == "ON-ROAD" | type == "NON-ROAD") %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# Open PNG device; create 'plot5.png' in the working directory
png(file = "plot5.png")
with(p5, {
    plot(year, totalemiss, type = "l", col = "red", xaxt = 'n', 
         main = "Total Emissions from Motor Vehicle Sources, Baltimore City, MD",
         ylab = "Total Emissions (PM2.5 in tons)",
         xlab = "Year")
    axis(1, at = year)
    points(year, totalemiss, pch = 20)
})
dev.off() # close PNG file device
