############################
# Acoustic_Analysis.R
# AcousticData_Carpenter.Rproj

# Create a function that determines how long a fish was in the vicinity of each door of the gates
# From this table we can infer the direction of travel

# Created August 31, 2016
# A Putt
#############################

source("Acoustic_DataUpload.R") # acoustic is the data frame created in the upload file

#######################
# Time Gap Definition #
#######################

# The time gap is the space between detections that defines the start of a new event
# This analysis examines the space between detections and determines when a new event detection occurs based on the time gap
tgapmin <- 5 # Number of minutes between events
tgap    <- tgapmin * 60 # Convert to seconds

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
  ordered <- oneID[order(oneID$fulldateUTC),] # MUST order the table by date and time so that I can then determine the time from the first detection
  
  # By using time from first detection I can use seconds (a number) instead of "time" to determine events (way faster)
  # Determine the number of seconds between each detection and the first detection in the table
  secfrom1 <- difftime( ordered$fulldateUTC[1] , ordered$fulldateUTC[ 2:length(ordered$fulldateUTC) ] , units="secs" )
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
example <- EventFunction(data=acoustic,transmitterID = "A69-1601-58082", tgap = tgap)

# Run a loop through all transmitters and create a list of data frames that can then be bound together
EventFunction.List <- list()

for (i in 1:length(levels(acoustic$Transmitter))) {
  TransmitterID <- levels(acoustic$Transmitter)[i]
  temp.df       <- EventFunction(data=acoustic,transmitterID=TransmitterID,tgap=tgap)
  EventFunction.List[[i]] <- temp.df
}

# Bind the data frames together
acoustic.events <- do.call("rbind",EventFunction.List)


#################################
# Event Summary Table ###########
#################################

oneID      <- subset(acoustic.events,Transmitter=TransmitterID)
events     <- levels(as.factor((oneID$eventnumbers)))
# Note, can't use ddply because the date format has too many attributes


####! START HERE! I'm trying to make an attributes table

eventstart <- min(example$fulldateUTC) 
eventend   <- max(example$fulldateUTC)
duration   <- difftime(eventstart,eventend,units="secs")
