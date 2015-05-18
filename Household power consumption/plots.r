# Author: Chisheng Li
# We will only be using data from the dates 2007-02-01 and 2007-02-02. 
# Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
# You may find it useful to convert the Date and Time variables to Date/Time classes 
# in R using thestrptime() and as.Date() functions.
# Construct each plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# Name each of the plot files as plot1.png, plot2.png, etc.

library(data.table)
houseEnergy <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", strip.white = TRUE, 
    stringsAsFactors = FALSE, na.strings = "?" )
houseEnergy <- data.table(houseEnergy)

houseEnergyFeb <- houseEnergy[Date %in% c("1/2/2007","2/2/2007")]

houseEnergyFeb$Date <- as.Date(houseEnergyFeb$Date, format = "%d/%m/%Y")
houseEnergyFeb$DateTime <- as.POSIXct(paste(houseEnergyFeb$Date, houseEnergyFeb$Time))

#Plot 1
png("plot1.png", height=480, width=480)
hist(houseEnergyFeb$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", 
    ylab = "Frequency", main = "Global Active Power")
dev.off()

#Plot 2
png("plot2.png", height=480, width=480)
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Global_active_power, type = "l", xlab = "", 
    ylab = "Global Active Power (kilowatts)")
dev.off()

# Plot 3
png("plot3.png", height=480, width=480)
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_1, type = "l", xlab = "",ylab = "Energy sub metering")
lines(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_2, type = "l", col = "red")
lines(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_3, type = "l", col = "blue")
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ))
dev.off()

# Plot 4
png("plot4.png", height=480, width=480)
par(mfrow = c(2,2))
#1st plot
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
#2nd plot
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
#3rd plot
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_2, col = "red")
lines(houseEnergyFeb$DateTime, houseEnergyFeb$Sub_metering_3, col = "blue")
legend("topright", lty = 1, bty="n", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ))
#4th plot
plot(houseEnergyFeb$DateTime, houseEnergyFeb$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
dev.off()
