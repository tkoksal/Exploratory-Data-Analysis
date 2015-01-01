#################################################
#######             PLOT 2          #############
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

# Select cases specific to Baltimore City and then
# aggregate by year and compute total emissions across years 
library(dplyr)
p2 <- munei %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# Open PNG device; create 'plot2.png' in the working directory
png(file = "plot2.png")
with(p2, {
    plot(year, totalemiss, type = "l", col = "red", xaxt = 'n', 
         main = "Total Emissions from PM2.5 in Baltimore City, MD",
         ylab = "Total Emissions (PM2.5 in tons)",
         xlab = "Year")
    axis(1, at = year)
    points(year, totalemiss, pch = 20)
})
dev.off() # close PNG file device
