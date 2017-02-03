############################
# Acoustic_DataUpload.R
# AcousticData_Carpenter.Rproj

# Uploads all VR2Ws from the Acoustic_RUploads folder in the working directory 
# and prepares them for manipulation/analysis by adding some extra date columns, etc.
# This code works ok right now, but having all of the the heavy date formats will eventually make it
# very slow and it will need to be adjusted.

# Times are in UTC!!!


# Created August 26, 2015
# A Putt
#############################

source("Acoustic_LoadLibraries.R")
source("Acoustic_LiveDead.R")


# Upload all csv files from the Acoustic_DataUpload folder
file_path  <- list.files(path="Acoustic_DataUpload", pattern="*.csv") # Pull all files
file_names <- substr(file_path,start=1,stop=22) # Pull the file names without .csv
file_list  <- list()

for(i in 1:length(file_path)){
  temp_df <- read.csv(sprintf("Acoustic_DataUpload/%s",file_path[i]),head=TRUE) # Read in the data
  file_list[[i]] <- temp_df
}

# The loop creates a list of data frames, all with the same attributes so they can be bound together to make one large table
acoustic <- do.call("rbind",file_list)
str(acoustic)

# Get rid of the columns that we don't have any information for
acoustic <- acoustic[,1:3]

# Create a PDT time zone column
# Create columns (as factors) with time information that takes up less memory than the date columns
# May be useful for plotting or analysis later on
names(acoustic)[1]   <- "fulldateUTC"
acoustic$fulldateUTC <- ymd_hms(acoustic$fulldateUTC,tz="UTC")
acoustic$fulldatePDT <- with_tz(acoustic$fulldateUTC,tz="America/Los_Angeles") # convert to PDT
# acoustic$fulldateUTC <- as.POSIXlt(acoustic$fulldateUTC)

###########################
# Remove False Detections #
###########################

# There are a bunch of codes that don't belong to tags that I implanted
# All tags that I implanted are in the TagLife table that I upload in the Acoustic_LiveDead.R file
TransmitterList <- levels(TagLife$Transmitters)

# Pull out the false detections
# This will also get rid of the test tag (A69-1601-58082) because it's not in the TagLife table
FalseDetections <- subset(acoustic, !Transmitter %in% TransmitterList)

# Remove false detections by only selecting Trasmitters that appear in the TagLife table
acoustic <- subset(acoustic, Transmitter %in% TransmitterList)
acoustic$Transmitter <- factor(acoustic$Transmitter) # Drop the unused factor levels

##########################
# Define Gate Names ######
##########################

# I want to group my recievers into gates so that I can see which gate detected which fish
# Create a table to merge with the acoustic table

Gates <- data.frame(Receiver=c("VR2W-127257","VR2W-127260","VR2W-127258","VR2W-127255","VR2W-127259","VR2W-127256","VR2W-128829","VR2W-127261","VR2W-128830","VR2W-127262"),
                    Gate=c(rep("CR.West",3),rep("CR.East",3),rep("MBR.West",2),rep("MBR.East",2)))

# Merge with the acoustic table
acoustic.temp <- merge(acoustic,Gates,by="Receiver")
if ( nrow(acoustic)!=nrow(acoustic.temp) ) "Merge failed" # Double check to make sure the merge worked properly
acoustic <- acoustic.temp

#####################################
# Get rid of duplicate lines ########
#####################################

# Mulitple receivers pick up the same signal
# To reduce the amount of data we can get rid of the duplicates as long as we retain one of the receivers for each gate
# Actually still a ton of duplicate values based on dd,mm,yy, hh,mm i.e., only the seconds are different
# I'll leave those in for now

# Show the duplicated values
duplicatedValues <- acoustic[duplicated(acoustic[,2:5]),]
duplicatedValues <- duplicatedValues[order(duplicatedValues$fulldatePDT),]

# Remove the duplicate values
acoustic <- acoustic[!duplicated(acoustic[,2:5]),]
acoustic <- acoustic[order(acoustic$fulldatePDT),]

######################################
# Create an excel file for each fish #
######################################

for (i in 1:length(levels(acoustic$Transmitter))) {
  temp <- subset(acoustic,Transmitter==levels(acoustic$Transmitter)[i])
  name <- as.character(temp$Transmitter[1])
  temp$fulldatePDT <- as.character(temp$fulldatePDT)
  temp$fulldateUTC <- as.character(temp$fulldateUTC)
  write.csv(temp,sprintf("Acoustic_RawTransmitterData/%s.csv",name),row.names=FALSE)
}

# Also write the acoustic data to csv so that the upload file doesn't need to run each time
write.csv(acoustic,"Acoustic_RawTransmitterData/fullacoustic.csv",row.names=FALSE)
                         