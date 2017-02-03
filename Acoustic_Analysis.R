############################
# Acoustic_Analysis.R
# AcousticData_Carpenter.Rproj

# Create a function that determines how long a fish was in the vicinity of each door of the gates
# From this table we can infer the direction of travel

# Created August 31, 2016
# A Putt
#############################

source("Acoustic_LoadLibraries.R")

#############################

acoustic <- read.csv("Acoustic_RawTransmitterData/fullacoustic.csv",head=TRUE) # fullacoustic is the data frame created in the upload file
acoustic$fulldateUTC <- ymd_hms(acoustic$fulldateUTC,tz="UTC")
acoustic$fulldatePDT <- ymd_hms(acoustic$fulldatePDT,tz="America/Los_Angeles")

#######################
# Time Gap Definition #
#######################

# The time gap is the space between detections that defines the start of a new event
# This analysis examines the space between detections and determines when a new event detection occurs based on the time gap
tgapdays <- 1
tgapmin  <- tgapdays*24*60 # Number of minutes between events; if a fish is not detected for 5 minutes the event will end and a new one will begin next time it's detected
tgap     <- tgapmin * 60 # Convert to seconds

###################      
# EventFunction ###
###################

# This function determines the time spacing between detection events and creates an event number based on 
# a user-defined time gap ("tgap"). The time gap signifies when to consider a detection part of a new event
# The function is pretty complicated because it creates a bunch of vectors related to the position and timing of detections.
# I've chosen to do it this way because a loop going through the whole table (probably cleaner code) would take an 
# extrememly long time to run.
# To run the function you need to select a particular fish ID as well as define the time gap that will signify a new event
# tgap is in seconds
# Returns a table of one ID that has event numbers added in a new column
# The names of the event numbers are pretty irrelevant, they just separate the table into event blocks

EventFunction <- function(data,transmitterID,tgap) {
  oneID <- subset(data,Transmitter==transmitterID) # Subset out the transmitter ID from the acoustic table
  ordered <- oneID[order(oneID$fulldatePDT),] # MUST order the table by date and time so that I can then determine the time from the first detection
  
  # By using time from first detection I can use seconds (a number) instead of "time" to determine events (way faster)
  # Determine the number of seconds between each detection and the first detection in the table
  secfrom1 <- difftime( ordered$fulldatePDT[1] , ordered$fulldatePDT[ 2:length(ordered$fulldatePDT) ] , units="secs" )
  secfrom1 <- c(0,abs(secfrom1)) # Turn the vector positive, also add a zero because the very first detection is 0 sec from itself
  
  # Determine the number of seconds between detections. These values represent the time from the previous detection
  # All I have to do is subtract the secfrom1 vector from itself, but I need to add a zero and an NA at the start and end to obtain the correct positioning.
  # The first number will be 0 again because the first detection is 0 sec from the previous (nonexistant) detection
  # The last number will be an NA. I need to remove it becuase it is just a dummy value to allow the calculation to run (the vec is one number too long)
  secfromprev <- c(secfrom1,NA)-c(0,secfrom1) 
  secfromprev  <- secfromprev[1:(length(secfromprev)-1)] # Get rid of NA at the end
  # If there was only one detection we need to rewrite the seconds from previous (it's really the difftime function that messes things up)
  if (nrow(ordered)==1) secfromprev <- 0
  
  # Now we need to use the seconds from the previous detection to create unique event names and turn them into a vector that can be
  # added to the original acoustic table.
  # 1. Pull the row numbers from the sec from prev that have seconds that are greater than the user-defined tgap
  rows <- c(1,which(secfromprev > tgap))
  # 2. Create a loop (and a dummy vector the loop will populate) that will create a vector of unique event names that matches the row
  # positions in the rows vector
  eventnumbers <- vector()
  
  # This loop won't work for a fish that was only detected once so I need to create an if statement to prevent it from working
  if (length(rows) > 1) {
    for (i in 1:(length(rows)-1)) { # I delete the last row so that the loop doesn't crash on the last iteration
      firstdetection <- rows[i] # The first detection in an event
      finaldetection <- rows[i+1] - 1 # I subtract 1 because the "rows" defines each NEW event. Eventposition[i+1] is the next event
      eventnumbers[firstdetection:finaldetection] <- sprintf("event%s",i) # Create event names and populate into the eventnumber vector
    } 
  } else {
    eventnumbers[1] <- "event1" 
  }
  
  # It is important to note that an event may consist of detections from more than one gate. Obviously the only other possible
  # gate is the neighbouring gate because the fish can't move that fast. In order to acount for this I could subset
  # the ordered table into events and create intermediate events OR I could just leave it as it is and make sure to 
  # print both the starting and ending gates in the summary table. The receiver number doesn't really matter, just the gate
  
  # 3. In order to prevent the loop from crashing on the last iteration I removed the last event, which now needs to be added
  rows.lastevent   <- rows[length(rows)] # This is the row signifying the beginning of the last event
  lastevent.length <- length(secfromprev)-length(eventnumbers) # This is how many rows are missing from the eventnumbers vec that must belong to the last event
  eventnumbers     <- c( eventnumbers , rep(sprintf("event%s",length(rows)),lastevent.length) ) # Add in the final event and name it so that it is the length of rows. 
  # In the loop it would have been the final iteration that I removed.
  
  # This vector represents the event names for the acoutstic table (subsetted for one ID) ordered by date and time
  ordered$secfromprev  <- secfromprev
  ordered$eventnumbers <- eventnumbers
  return(ordered) # One ID with new event numbers added according to the tgap 
}
  
##########################################
# Run EventFunction for all transmitters #
##########################################

# Example run
# example <- EventFunction(data=acoustic,transmitterID = "A69-1601-58082", tgap = tgap)

# Run a loop through all transmitters and create a list of data frames that can then be bound together
EventFunction.List <- list()

for (i in 1:length(levels(acoustic$Transmitter))) {
  TransmitterID <- levels(acoustic$Transmitter)[i]
  temp.df       <- EventFunction(data=acoustic,transmitterID=TransmitterID,tgap=tgap)
  EventFunction.List[[i]] <- temp.df
}

# Bind the data frames together
acoustic.events <- do.call("rbind",EventFunction.List) # This is a huge data frame; don't really want to print it

################################
# Location Table ###############
################################

# Create a table that translates the gate of first or last detection into what section of the reservoir the fish is in
LocationTable      <- data.frame(gate=c("CR.East","CR.West","MBR.East","MBR.West"),location=c("CR.E","CR.W","CR.W","MBR"))
StartLocationTable <- data.frame(start.gate=c("CR.East","CR.West","MBR.East","MBR.West"),start.location=c("CR.E","CR.W","CR.W","MBR"))
EndLocationTable   <- data.frame(end.gate=c("CR.East","CR.West","MBR.East","MBR.West"),end.location=c("CR.E","CR.W","CR.W","MBR"))

#################################
# Event Summary Table ###########
#################################

##########################################################
# Code for event summary table using ddplyr
# This is faster (I think) than the loop but may have some risk because dates can't be used
# The dates need to be character values for the code to work; because of the date format ordering numerically orders by date
# Could include Transmitter or not depending on what you want
# An addition is needed to determine the duration (it's not so easy because the values are character now)
# Also a bit worried about first and last; does it preserve the right order during summarize?

EventSummaryFunc <- function(oneID) {
  oneID <- oneID[order(oneID$fulldateUTC),] # MUST order the table by date and time so that I can then determine the time from the first detection
  oneID$fulldateUTC <- as.character(oneID$fulldateUTC)
  oneID$fulldatePDT <- as.character(oneID$fulldatePDT)
  OneFishEvents <- ddply(oneID, c("Transmitter","eventnumbers"), summarise,
                          start = min(fulldateUTC), # This is the max and min of Character(!) vectors
                          end   = max(fulldateUTC),
                          start.gate = first(Gate),
                          end.gate   = last(Gate))
  
  OneFishEvents <- merge(OneFishEvents,StartLocationTable,by="start.gate")
  OneFishEvents <- merge(OneFishEvents,EndLocationTable,by="end.gate")
  OneFishEvents <- OneFishEvents[order(OneFishEvents$start),]
}

############################################
# Run Event Summary Table for All Fish #####
############################################

# Pare down the acoustic events by transmitter
# Run a loop through all transmitters and create a list of data frames that can then be bound together
EventSummary.List <- list()

for (i in 1:length(levels(acoustic.events$Transmitter))) {
  TransmitterID <- levels(acoustic.events$Transmitter)[i]
  oneID.loop    <- subset(acoustic.events,Transmitter==TransmitterID)
  temp.df       <- EventSummaryFunc(oneID=oneID.loop)
  EventSummary.List[[i]] <- temp.df
}

events.summary <- do.call("rbind",EventSummary.List)
write.csv(events.summary,"EventsSummaryTable.csv",row.names=FALSE)
