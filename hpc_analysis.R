library(dplyr)

#Set working directory
setwd("C://Users/kris/Documents/GitHub/ExploratoryDataAnalysisW1")

filename <- "data.zip"

# Checking if archive already exists locally, downloads if not present.
if (!file.exists(filename)){
  fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if file exists locally, unzipping if not
if (!file.exists("household_power_consumption")) { 
  unzip(filename) 
}

# Read table from text, assign classes, define seperator, define NA values, filter values from 2/1/07-2/2/07
hpc <- read.table("household_power_consumption.txt", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'), sep=";", na.strings = "?",skip=66637,nrows=2880)
head(hpc,1) # Check
tail(hpc,1) # Check

## Format date to type date
hpc$V1 <- as.Date(hpc$V1, "%d/%m/%Y")
head(hpc,1) # Check

## Remove incomplete observation
hpc <- hpc[complete.cases(hpc),]

## Combine date(v1) and time(V2) column, remove old columns, and set column names
hpc<-cbind(paste(hpc$V1,hpc$V2),hpc)
hpc <- hpc[ ,!(names(hpc) %in% c("V1","V2"))]
hpc<-setNames(hpc,c("Date","GAP","GRP","Voltage","GI","SM1","SM2","SM3"))
head(hpc,1) # Check

hpc$Date <- as.POSIXct(hpc$Date)
head(hpc,1) # Check

## Create Plot 1
hist(hpc$GAP, main="Global Active Power", xlab = "Global Active Power (kilowatts)",ylab = "Frequency", col="red")
# Save file if not already in existence
if (!file.exists("plot1.png")){
  dev.copy(png,"plot1.png", width=480, height=480)
  dev.off()
}  

## Create Plot 2
plot(hpc$GAP~hpc$Date, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  # Save file if not already in existence
if (!file.exists("plot2.png")){
  dev.copy(png,"plot2.png", width=480, height=480)
  dev.off()
}  

## Create Plot 3
with(hpc, {
  plot(SM1~Date, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  lines(SM2~Date,col='Red')
  lines(SM3~Date,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), c("Sub Metering 1", "Sub Metering 2", "Sub Metering 3"))
  # Save file if not already in existence
if (!file.exists("plot3.png")){
  dev.copy(png,"plot3.png", width=480, height=480)
  dev.off()
}  

## Create Plot 4
  # Layout
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(hpc, {
  # Plot1
  plot(GAP~Date, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  # Plot2
  plot(Voltage~Date, type="l", ylab="Voltage (volt)", xlab="")
  # Plot3
  plot(SM1~Date, type="l", ylab="Global Active Power (kilowatts)", xlab="")
  lines(SM2~Date,col='Red')
  lines(SM3~Date,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub Metering 1", "Sub Metering 2", "Sub Metering 3"))
  # Plot4
  plot(GAP~Date, type="l", ylab="Global Rective Power (kilowatts)",xlab="")
})
  # Save file if not already in existence
if (!file.exists("plot4.png")){
  dev.copy(png,"plot4.png", width=480, height=480)
  dev.off()
}  

