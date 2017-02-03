############################
# Acoustic_tsplot.R
# AcousticData_Carpenter.Rproj

# Creates a time series plot of detections by fish id and reciever unit.

# Created August 28, 2015
# A Putt
#############################

source("Acoustic_LoadLibraries.R") # acoustic is the data frame created in the upload file

#############################
# Load the acoustic data from the upload file and put dates back in

acoustic <- read.csv("Acoustic_RawTransmitterData/fullacoustic.csv",head=TRUE) # fullacoustic is the data frame created in the upload file
acoustic$fulldateUTC <- as.POSIXct(acoustic$fulldateUTC,tz="UTC")
acoustic$fulldatePDT <- as.POSIXct(acoustic$fulldatePDT,tz="America/Los_Angeles")

# Create some lists to filter/subset by
# Pull the lists of receivers and tags
receivers    <- levels(acoustic$Receiver)
transmitters <- levels(acoustic$Transmitter)
gates        <- levels(acoustic$Gate)

###########################
# Set Date Range ##########
###########################

# Set a range of dates to include in the plots
# I could add this into the function calls as well, but i figured I may as well plot
# everything over one range anyway for it all to be the same

print(sprintf("First date in the date frame is %s", min(acoustic$fulldatePDT)))
print(sprintf("Last date in the data frame is %s", max(acoustic$fulldatePDT)))

MinDate <- as.POSIXct("2015-06-10")
MaxDate <- as.POSIXct("2016-04-06")

sprintf("Plotting date range is %s %s", MinDate, MaxDate)

# Set the date range
acoustic <- subset (acoustic, fulldatePDT > MinDate & fulldatePDT < MaxDate)

#################################
# Plot detection at One Receiver#
#################################

# Create a function to plot a time series of detections at one receiver
OneReceiverTimeSeriesPlot <- function(ReceiverName) {
  plotdata <- subset(acoustic,Receiver==ReceiverName)
  # windows()
  Plot <- ggplot(plotdata, aes(fulldatePDT, Transmitter)) + geom_point() + 
    theme_bw() + scale_alpha_manual(values=c(0, 1), breaks=c(FALSE, TRUE), guide='none') +
    xlab("\nDate in PDT") + ylab("Transmitter Code\n") +
    ggtitle(paste(sprintf("%s",ReceiverName),"\n")) +
    scale_x_datetime(labels=date_format("%b-%y"),limits=c(MinDate,MaxDate))
              
  ggsave(filename=sprintf("%s.%s.%s.png",ReceiverName,MinDate,MaxDate),plot=Plot,path="Acoustic_TSPlot")
}

# Create a function to plot a time series of detections at one gate
OneGateTimeSeriesPlot <- function(GateName) {
  plotdata <- subset(acoustic,Gate==GateName)
  # windows()
  Plot <- ggplot(plotdata, aes(fulldatePDT, Transmitter)) + geom_point() + 
    theme_bw() + scale_alpha_manual(values=c(0, 1), breaks=c(FALSE, TRUE), guide='none') +
    xlab("\nDate in PDT") + ylab("Transmitter Code\n") +
    ggtitle(paste(sprintf("%s",GateName),"\n")) +
    scale_x_datetime(labels=date_format("%b-%y"),limits=c(MinDate,MaxDate))
  
  ggsave(filename=sprintf("%s.%s.%s.png",GateName,MinDate,MaxDate),plot=Plot,path="Acoustic_TSPlot")
}


# Run the functions. Still running super slow
# OneReceiverTimeSeriesPlot("VR2W-127255")

for (i in 1:length(receivers)) {
  OneReceiverTimeSeriesPlot(ReceiverName=receivers[i])
}

for (i in 1:length(gates)) {
  OneGateTimeSeriesPlot(GateName=gates[i])
}

###########################
# Pot Recievers by Fish ###
###########################

OneTransmitterTimeSeriesPlot <- function(TransmitterName) {
  plotdata <- subset(acoustic,Transmitter==TransmitterName)
  # windows()
  Plot <- ggplot(plotdata, aes(fulldatePDT, Receiver)) + geom_point() + 
    theme_bw() + scale_alpha_manual(values=c(0, 1), breaks=c(FALSE, TRUE), guide='none') +
    xlab("\nDate in PDT") + ylab("Receiver\n") +
    ggtitle(paste(sprintf("%s",TransmitterName),"\n")) +
    scale_x_datetime(labels=date_format("%b-%y"),limits=c(MinDate,MaxDate))
  
  ggsave(filename=sprintf("%s.%s.%s.receivers.png",TransmitterName,MinDate,MaxDate),plot=Plot,path="Acoustic_TSPlot")
}

OneTransmitterTimeSeriesPlot.Gate <- function(TransmitterName) {
  plotdata <- subset(acoustic,Transmitter==TransmitterName)
  # windows()
  Plot <- ggplot(plotdata, aes(fulldatePDT, Gate)) + geom_point() + 
    theme_bw() + scale_alpha_manual(values=c(0, 1), breaks=c(FALSE, TRUE), guide='none') +
    xlab("\nDate in PDT") + ylab("Gate\n") +
    ggtitle(paste(sprintf("%s",TransmitterName),"\n")) +
    scale_x_datetime(labels=date_format("%b-%y"),limits=c(MinDate,MaxDate))
  
  ggsave(filename=sprintf("%s.%s.%s.gates.png",TransmitterName,MinDate,MaxDate),plot=Plot,path="Acoustic_TSPlot")
}

# OneTransmitterTimeSeriesPlot.Gate("A69-1601-34779")

# # Run both functions for both gates and recievers

# for (i in 1:length(transmitters)) {
#   OneTransmitterTimeSeriesPlot(TransmitterName=transmitters[i])
# }

for (i in 1:length(transmitters)) {
  OneTransmitterTimeSeriesPlot.Gate(TransmitterName=transmitters[i])
}

