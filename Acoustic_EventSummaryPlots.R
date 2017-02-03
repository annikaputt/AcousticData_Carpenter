############################
# Acoustic_EventummaryPlots.R
# AcousticData_Carpenter.Rproj

# Reads in the event summary table and the live dead tag plots
# Creates a table that has the number of fish in each zone per day
# Creates plots through time of the summaries of counts and proportions

# Created August 28, 2015
# A Putt
#############################

source("Acoustic_LoadLibraries.R")
source("Acoustic_LiveDead.R")
Events <- read.csv("EventsSummaryTable.csv",head=TRUE)
Events <- Events[order(Events$start),]

# Need to create a new table that fills in the dates so that you know the number of fish in each area on each day
# Get rid of the times associated with the dates
Events$start <- strptime(ymd_hms(Events$start),format="%Y-%m-%d")
Events$end   <- strptime(ymd_hms(Events$end),format="%Y-%m-%d")
Events$start <- as.POSIXct(Events$start)
Events$end   <- as.POSIXct(Events$end)

###########################
# Fill Location Funcitons #
###########################

# I want to add in all dates between events and fill in the location based on the start and end locations
# at each event. i.e., if I know where they finsihed after an event, I know they must have been there every day between that event and the next 
# Any NA in a particular column is filled with the last non-NA
# !!!!!!! It is important to note that because fish sometimes slip through the gates this table would
# Look different if I filled backwards using the START location rather than filling forward using the END locations.

fillNAgapsForward <- function(x, firstBack=FALSE) {
  # NA's in a vector or factor are replaced with last non-NA values
  # If firstBack is TRUE, it will fill in leading NA's with the first non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  # These are the non-NA values from x only; Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  fillIdx <- cumsum(goodIdx)+1   # these offsets of goodVals. Add 1 to avoid indexing to zero.
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  x
}

######################
# Run Fill NA ########
######################

# Create a function that pulls one transmitter and fills in missing dates
FillTransmitterNA <- function(TransmitterName) {
  TransmitterData <- subset(Events,Transmitter == TransmitterName)
  TransmitterData$start.2 <- TransmitterData$start
  TransmitterStartDate <- strptime(subset(TagLife,Transmitters == TransmitterName)$Released,format="%Y-%m-%d")
  # DateSequence <- seq(TransmitterStartDate,max(TransmitterData$end),by="day") # This will fill within events
  DateSequence <- seq(TransmitterStartDate,as.POSIXct("2016-04-07"),by="day")
  DateDF <- data.frame(start=DateSequence)
  AllDates <- merge(TransmitterData,DateDF,by="start",all.y="TRUE")
  AllDates$Transmitter <- TransmitterData$Transmitter[1]

  # Fill NAs. For the end gate, end location and event number, fill based on the previous value.
  # i.e., where they end is going to be there end location until a new end location occurs.
  AllDates$end.gate <- fillNAgapsForward(AllDates$end.gate,firstBack=FALSE)
  AllDates$eventnumbers <- fillNAgapsForward(AllDates$eventnumbers,firstBack=FALSE) 
  AllDates$end.location <- fillNAgapsForward(AllDates$end.location,firstBack=FALSE)
  
  # All of the NAs at the start can be CR.W because all fish are tagged there and therefore begin there
  AllDates$end.location[is.na(AllDates$end.location)] <- "CR.W"
  return(AllDates)
}  


# Run the function for all the transmitters

TransmitterList <- levels(Events$Transmitter)
FillNAList <- list()

for (i in 1:length(TransmitterList)) {
  TransmitterName <- TransmitterList[i]
  Temp <- FillTransmitterNA(TransmitterName=TransmitterName)
  FillNAList[[i]] <- Temp
}

ExpandedEvents <- do.call("rbind",FillNAList)
  
##################################
# Plot all Transmitter Locations #
##################################

# Plot the location for all transmitters
windows()
ggplot(ExpandedEvents, aes(start, Transmitter, height=0.7)) + 
  geom_tile(aes(fill = end.location)) +
  theme_bw() + xlab("") + ylab("") + ggtitle("Transmitter Location") + 
  theme(legend.title = element_blank(), legend.position="bottom", legend.direction="vertical") +
  scale_fill_manual(values = c("grey", "black", "red"),
                    breaks=c("CR.E","CR.W","MBR"), labels=c("Carpenter East","Carpenter West","Middle Bridge River")) +
  scale_x_datetime(labels=date_format("%b-%y"),breaks=date_breaks("1 month")) +
  # The below lines just add dots onto the plot at the start and end of every event. We don't see when the event start but 
  # we can at least see when the fish encounters a gate
  geom_point(aes(end,Transmitter), color="white") + 
  geom_point(aes(start.2,Transmitter),color="yellow")


######################################
# Create Table of Proportions ########
######################################

# and plot

# Truncate the expanded events data frame to only have the location information according to 
# the end gate as determined previously in this file
ExpandedEvents.trunc <- data.frame(date=ExpandedEvents$start,transmitter=ExpandedEvents$Transmitter,
                                  location=ExpandedEvents$end.location)

Counts <- ddply(ExpandedEvents.trunc, c("date","location"), summarise,
                     count=length(transmitter))
#Counts$date <- as.Date(Counts$date) # change to date so that I can merge. Would be nice to swich to posix but I keep retaining the time :(
Counts <- merge(Counts,AliveTable,by="date",all.x=TRUE)

# The proportions will not add up to 1 because some fish were never encountered again and have no event to build on
# These fish need to be acounted for.
# Create a table with the count of fish never observed again
Counts.NoEvents <- ddply(Counts,"date",summarize,
                         location="CR.W: no event",
                         count=CountAlive[1]-sum(count),
                         CountAlive=CountAlive[1])

Counts <- rbind(Counts,Counts.NoEvents)

Counts$proportion <- Counts$count/Counts$CountAlive
Counts$plotdate <- as.POSIXct(Counts$date)
      
ggplot(Counts,aes(x = plotdate, y=proportion, fill=location)) +
  geom_bar(stat="identity") +
  theme_bw() + xlab("") + ylab("Prortion Tagged Fish") + ggtitle("Proportion Tagged Fish by Location") + 
  theme(legend.title = element_blank(), legend.position="bottom", legend.direction="vertical") +
  scale_fill_manual(values = c("grey", "black", "red","grey13"),
                    breaks=c("CR.E","MBR","CR.W","CR.W: no event"), labels=c("Carpenter East","Middle Bridge River","Carpenter West","Carpenter West: No Event")) +
  scale_x_datetime(labels=date_format("%b-%y"),breaks=date_breaks("1 month"))
