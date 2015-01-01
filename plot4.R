#################################################
#######             PLOT 4          #############
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
# cases regarding emissions from coal combustion-related sources is to select 
# cases based on 'EI.Sector' whose value contains both 'comb' and 'coal'.

p4 <- munei[grepl("coal", munei$EI.Sector, ignore.case = TRUE) &
                    grepl("comb", munei$EI.Sector, ignore.case = TRUE) , ]

# Aggregate p4 by year and compute total emissions across years
library(dplyr)
p4 <- p4 %>%
    group_by(year) %>%
    summarize(totalemiss = sum(Emissions)) 

# Open PNG device; create 'plot4.png' in the working directory
png(file = "plot4.png")
with(p4, {
    plot(year, totalemiss, type = "l", col = "red", xaxt = 'n', 
         main = "Total Emissions from Coal Combustion-Related Sources 
                in the United States",
         ylab = "Total Emissions (PM2.5 in tons)",
         xlab = "Year")
    axis(1, at = year)
    points(year, totalemiss, pch = 20)
})
dev.off() # close PNG file device
