############################
# Acoustic_DataUpload.R
# AcousticData_Carpenter.Rproj

# Uploads all VR2Ws from the Acoustic_RUploads folder in the working directory 
# and prepares them for manipulation/analysis by adding some extra date columns, etc.

# Created August 26, 2015
# A Putt
#############################

##########
# Upload all csv files from the Acoustic_DataUpload folder
file_path  <- list.files(path="Acoustic_DataUpload", pattern="*.csv") # Pull all files
file_names <- substr(file_list,start=1,stop=22) # Pull the file names without .csv
file_list  <- list()

for(i in 1:length(file_path)){
  temp_df <- read.csv(sprintf("Acoustic_DataUpload/%s",file_path[i]),head=TRUE) # Read in the data
  file_list[[i]] <- temp_df
}

acoustic <- do.call("rbind",file_list)
str(acoustic)
names(acoustic)[1] <- "fulldateUTC"
acoustic$fulldateUTC <- as.POSIXlt(acoustic$fulldateUTC,tz="UTC")
#! Time zone isn't working. Confused. Need it to work to change the time zone later)

