cleanUpData <- function() {
        labResults <- read.csv("M:/1. Data/2. Data Set Processing/Under5_PfLDH/Lab Results.csv", as.is=TRUE)
        numRows = length(unique(labResults$Sample))
        print(numRows)
        print(length(labResults$Sample))
        labResults$Date <- as.Date(as.character(labResults$Date), format="%m/%d/%Y")
        output <- data.frame(Date=character(numRows), Sample=character(numRows), Result=numeric(numRows), Pf_MeanCt=numeric(numRows), MeanSQ=numeric(numRows))
        output$Date = as.Date(output$Date, format="%Y-%m-%d")
        output$Sample = as.character(output$Sample)
        rowCount = 1
        
        for (row in 1:nrow(labResults)) {
                if (labResults$Sample[row] %in% output$Sample == TRUE) {
                        sampleRow = which(labResults$Sample==labResults$Sample[row], arr.ind=TRUE)[1]
                        output$Date[sampleRow] = labResults$Date[row]
                        output$Result[sampleRow] = labResults$Result[row]
                        output$Pf_MeanCt[sampleRow] = labResults$Pf_MeanCt[row]
                        output$MeanSQ[sampleRow] = labResults$MeanSQ[row]
                }
                else {
                        output$Date[rowCount] = labResults$Date[row]
                        output$Sample[rowCount] = labResults$Sample[row]
                        output$Result[rowCount] = labResults$Result[row]
                        output$Pf_MeanCt[rowCount] = labResults$Pf_MeanCt[row]
                        output$MeanSQ[rowCount] = labResults$MeanSQ[row]
                        rowCount=rowCount+1
                }
        }

        write.csv(output, "M:/1. Data/2. Data Set Processing/Under5_PfLDH/CongoDatabase.csv")
}