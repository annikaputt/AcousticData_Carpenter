############################
# Acoustic_TagLife.R
# AcousticData_Carpenter.Rproj

# Uploads a table (copied from Acoustic Data Entry foder) with the release date, the
# days of batter life, and the death date of all acoustic tagged fish in carpenter
# Creates a table with the count of live fish by day

# Created November 22, 2016
# A Putt
#############################

library(plyr)
library(lubridate)

#############################

# Read in TagLife table
TagLife <- read.csv("AcousticLiveDead.csv",head=TRUE)
print("Need to adjust code if tags with life other than 730 days are used")

TagLife$Released <- as.POSIXlt(strptime(TagLife$Released,format="%m/%d/%y",tz="America/Los_Angeles"))
TagLife$Dead     <- as.POSIXlt(strptime(TagLife$Dead,    format="%m/%d/%y",tz="America/Los_Angeles"))
TagLife$Released  <- as.POSIXct(TagLife$Released)
TagLife$Dead      <- as.POSIXct(TagLife$Dead)

TagLife          <- subset(TagLife,LifeDays == 730) # Subset only tags that live for 730 days just in case

# Expand the table to that days are expanded between released and dead and Transmitter name is repeated each day it is alive
DatesExpanded <- TagLife %>%
  rowwise() %>%
  do(data.frame(Transmitters=.$Transmitters, date=seq(.$Released,.$Dead,by="day")))

DatesExpanded <- as.data.frame(DatesExpanded) # ? Not really sure what the format output of pipe is but here I turn it into a df
DatesExpanded$count <- 1 # Turn each transmitter occurance into a count of 1 so that transmitters can be summed to get the live count

# The transmitters are repeated for each date they are alive so summarize by date and count each transmitter to get the total
AliveTable <- ddply(DatesExpanded, c("date"), summarise,
               CountAlive=sum(count)
)

# Check the graphic to make sure it makes sense; adding ~20 tags per year
plot(CountAlive~date,AliveTable,type="b")
                        