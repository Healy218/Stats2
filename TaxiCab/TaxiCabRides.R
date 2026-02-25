## Matt Healy ##


## 1.  Decide if there is a difference between the average trip distance of ...
## the yellow taxis in January and July (Hint: Use only 2025 data).

## 2.  Before you do (1) what do you expect to be the case? If the data in 
## (1) supports your thoughts, what is another test you might conduct. 
##
## I expect the trip distances in July to be longer. There is a higher 
## likely hood as the weather gets warmer that people walk further distances.

## 3.  Find a 95% prediction interval for the fare amount. 

## 4.  Could you estimate the average amount made by a yellow taxi on a random 
#day in January 2025? Explain how you would do it based on the dataset you have.

## 5. I cannot ask you to find a CI for the variance of trip distance.  
## Because the data does not seem to follow a normal distribution.  
## Plot a histogram that shows this.  No need to generate a QQ-plot

## Initialization ##
library(arrow)

# TaxiCab is the folder we are in then we "/" to go down into the folder
# and read the parquet files for January and July 2025. 
# Then when we read the parquet files we assign them to janTrips and julyTrips.
# JanTrips and julyTrips are data frames that contain the data from the parquet files.
janTrips<-read_parquet('TaxiCab/janTaxi2025.parquet')
julyTrips<-read_parquet('TaxiCab/julyTaxi2025.parquet')


summary(janTrips$trip_distance)
summary(julyTrips$trip_distance)



