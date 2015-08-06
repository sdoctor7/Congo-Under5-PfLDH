# congo-read-pfldh

## dataProcessingCode1.1: 
* Read in CSV outputs from qPCR machine
* Sort data, exclude standard curve, format
* Transform to one sample per line
* Determine if samples are positive or negative, record results and Ct values
* Add to existing "LabResults" database

## dataProcessingCode2:
* Read in "LabResults"
* Determine if sample has been run before
* Publish cleaned-up data to "CongoDatabase" as unique sample or retest
