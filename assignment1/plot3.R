library(data.table)
getData<-function(fileOpen="outcome",direct="assignment1"){
      
      output<-data.table()
      print(paste("The present working directory is:",getwd()))
      output<- tryCatch(
            {
                  fread("./household_power_consumption.txt",sep=";",header=F,data.table = TRUE,colClasses = c("Date","Time","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings=c("?"),skip="1/2/2007",nrows=2880)# verbose=T skip="2/1/2007",
                  #read.csv(fileList[fileIndex[1]],colClasses = "character")
            },
            error=function(cond){
                  message(paste("file does not seem to exist:", fileOpen))
                  message("Here's the original error message:")
                  message(cond)
                  return(2)
                  
            },
            warning=function(cond) {
                  message(paste("file caused a warning:", fileOpen))
                  message("Here's the original warning message:")
                  message(cond)
                  return(3)
            },
            finally={
                  message(paste("Processed file: ", fileOpen," was opened successfully"))
            }
      )
      output
}

plot3<-function(){
      MyData<-getData()
      setnames(MyData, c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      x <- strptime(paste(MyData$Date,MyData$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
      png(file = "Plot3.png")
      par(mfrow=c(1,1))
      with(MyData, plot(x,Sub_metering_1,type = "n",ylab = "Energy sub metering",xlab=""))
      with(MyData,lines(x,Sub_metering_1,col="black"))
      with(MyData,lines(x,Sub_metering_2,col="red"))
      with(MyData,lines(x,Sub_metering_3,col="blue"))
      legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),border="black",lty=1)
      dev.off()

}

