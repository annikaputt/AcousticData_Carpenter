############################
# Acoustic_tsplot.R
# AcousticData_Carpenter.Rproj

# Creates a time series plot of detections by fish id and reciever unit.

# Created August 28, 2015
# A Putt
#############################
library(ggplot2)
library(lattice)

source("Acoustic_DataUpload.R") # acoustic is the data frame created in the upload file

# Create some lists to filter/subset by
# Pull the lists of receivers and tags
receivers    <- levels(acoustic$Receiver)
transmitters <- levels(acoustic$Transmitter)

# Create a function to plot a time series of detections at one receiver
OneReceiverTimeSeriesPlot <- function(ReceiverName) {
  plotdata <- subset(acoustic,Receiver==ReceiverName)
  windows()
  Plot <- ggplot(plotdata, aes(fulldateUTC, Transmitter)) + geom_point() + 
    theme_bw() + scale_alpha_manual(values=c(0, 1), breaks=c(FALSE, TRUE), guide='none') +
    xlab("\nDate in UTC") + ylab("Transmitter Code\n") +
    ggtitle(paste(sprintf("%s",ReceiverName),"\n"))
  print(Plot)
}

# Re-create the function in lattice to see if it works faster....it doesn't :(
OneReceiverTimeSeriesPlot.Lattice <- function(ReceiverName) {
  plotdata <- subset(acoustic,Receiver==ReceiverName)
  windows()
  Plot <- xyplot(Transmitter ~ as.POSIXct(fulldateUTC), plotdata, 
                 panel = function(x, y, ...) {
                   panel.abline(h = seq(0:length(receivers)),col="lightgrey")
                   panel.xyplot(x, y) },
                 xlab="Date UTC",ylab="Transmitter Code",
                 main=ReceiverName,
                 scales=list(y = list(tck=c(1,0)),x=list(tck=c(1,0))),
                 par.settings=list(plot.symbol=list(col="black",fill="red",pch=19))
  )
  print(Plot)
}

# Run the function. Still running super slow
OneReceiverTimeSeriesPlot("VR2W-127255")
#OneReceiverTimeSeriesPlot.Lattice("VR2W-127255")

