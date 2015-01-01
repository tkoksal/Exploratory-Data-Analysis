#################################################
#######             PLOT 1          #############
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

# Aggregate data 'munei' by year and compute total emissions across years 
library(dplyr)
p1 <- munei %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# Open PNG device; create 'plot1.png' in the working directory
png(file = "plot1.png")
with(p1, {
    plot(year, totalemiss, type = "l", col = "red", xaxt = 'n', 
         main = "Total Emissions from PM2.5 in the United States",
         ylab = "Total Emissions (PM2.5 in tons)",
         xlab = "Year")
    axis(1, at = year)
    points(year, totalemiss, pch = 20)
})
dev.off() # close PNG file device
