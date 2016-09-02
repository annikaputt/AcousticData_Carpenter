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
acoustic$fulldateUTC <- as.POSIXct(acoustic$fulldateUTC,tz="UTC")
acoustic$fulldatePDT <- as.POSIXlt(format(acoustic$fulldateUTC,tz="America/Los_Angeles",usetz=TRUE)) # convert to PDT
acoustic$fulldateUTC <- as.POSIXlt(acoustic$fulldateUTC)
acoustic$utcyear     <- as.factor(format(acoustic$fulldateUTC,"%Y"))
acoustic$utcmonth    <- as.factor(format(acoustic$fulldateUTC,"%m"))
acoustic$utcday      <- as.factor(format(acoustic$fulldateUTC,"%d"))
acoustic$pdtyear     <- as.factor(format(acoustic$fulldatePDT,"%Y"))
acoustic$pdtmonth    <- as.factor(format(acoustic$fulldatePDT,"%m"))
acoustic$pdtday      <- as.factor(format(acoustic$fulldatePDT,"%d"))

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
