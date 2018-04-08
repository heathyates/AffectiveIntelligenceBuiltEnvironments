#Load the library and file path 
library(ggplot2)
participantFilePath <- "C:/Users/heath/OneDrive/Documents/Research/Current/Dissertation/2. WAMK/WAMK/6. Appendix - Processed Data/yates master/master annotated csv/participant_11_annotated_master.csv"

#Load the data
participantMasterData <- read.csv(participantFilePath, header = TRUE)

#Look at the biometrics for this data 
participantID <- participantMasterData$PARTICIPANTID[1]
observations <- as.numeric(rownames(participantMasterData))
ggplot(participantMasterData, aes(observations, participantMasterData$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
ggplot(participantMasterData, aes(observations, participantMasterData$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
ggplot(participantMasterData, aes(observations, participantMasterData$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

#Look at data between start and end 
startRow <- as.numeric(rownames(participantMasterData[participantMasterData$ZONE == "Start",]))
endRow <- as.numeric(rownames(participantMasterData[participantMasterData$ZONE == "End",])) 
participantMasterStartToEndData <- participantMasterData[startRow:endRow, ]

#participantID <- participantMasterData$PARTICIPANTID[1]
observations <- as.numeric(rownames(participantMasterStartToEndData))
ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
ggplot(participantMasterStartToEndData, aes(observations, participantMasterStartToEndData$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))



#Explore potential outliers 

#Remove the outliers 
outlierCutOff <- 1414 #This is the end 
participantMasterDataWithoutOutliers <- participantMasterData[1:outlierCutOff - 1,]


#Observe the results 
observations <- as.numeric(rownames(participantMasterDataWithoutOutliers))
ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$HRAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("HR") + ggtitle(paste0("Participant ", participantID, " HR from E4"))
ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$EDAAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("EDA") + ggtitle(paste0("Participant ", participantID, " EDA from E4"))
ggplot(participantMasterDataWithoutOutliers, aes(observations, participantMasterDataWithoutOutliers$TEMPAVG)) + geom_line() + xlab("Observations(1 per sec)") + ylab("TEMP") + ggtitle(paste0("Participant ", participantID, " TEMP from E4"))

#No need to cutoff because before end (check END is not there) 
endRow <- as.numeric(rownames(participantMasterDataWithoutOutliers[participantMasterDataWithoutOutliers$ZONE == "End",]))
tail(participantMasterDataWithoutOutliers$ZONE, 200)