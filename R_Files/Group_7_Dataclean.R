#Detailed Analysis of Train Accident Data
#Will only use variables which are 'pre-crash' (ie: not involved #trains derailed)

#Two/three responses to choose from:
# 1) Accident Type : A categorical varaible for the type of accident that could happen given the circumstances

# 2) Equipment Type : A categorical variable for the type of equipment expected to fail given the circumstances

# 3) Total Cost Damages : A combination of equipment and track costs. 
#   Note: Have a variable called Total Costs but often includes values such as payouts to victims
#         thanks to casualties.

library(dplyr)
library(lubridate)
library(tidyr)

rail_data <- read.csv("E:\\CloudComputingProject\\Data\\Rail_Equipment_Accident_Incident_Data.csv", header=TRUE, stringsAsFactors=FALSE)
length(names(rail_data))

rail_data = separate(rail_data, 'Date', c('Month', 'Day', 'Year'), sep='/')
rail_data$Year = as.integer(rail_data$Year)
rail_data$DayHour = hour(parse_date_time(rail_data$Time, '%I:%M %p'))
rail_data$DayHour = as.character(rail_data$DayHour)
rail_data = rail_data[between(rail_data$Latitude, 25.84, 49.38),]
rail_data = rail_data[between(rail_data$Longitude, -124.77, -66.95),]

sum(rail_data$Positive.Alcohol.Tests == 1, na.rm=T)
sum(rail_data$Positive.Alcohol.Tests == 0, na.rm=T)

#Cleaning Data
#Main problems:

#1) Much of the data is missing, with many having NaN value in a row
#2) Many of these missing datapieces are listed as ''-value instead of NaN (must be checked for)
#3) Cannot convert chracters to factors until these have been resolved
#4) Track.Density is currently a string, wish for it to be a countinous-numeric-variable
#5) Deciding what to do abut hours of day, month, year, ect.
#      Could set certain factors into new factors (fusing certain groups to make new ones)

#Data below creates the dataframes for the data. Will then create copies in order to experiment with 
#changing factors into smaller ones,

appliable_pre = c('Month', 'Year', 'DayHour', 'Temperature', 'Visibility', 'Weather.Condition', 'Track.Type', 'Track.Class',
                  'Track.Density','Train.Direction', 'Train.Speed', 'Recorded.Estimated.Speed', 'Maximum.Speed', 'Gross.Tonnage',
                  'Signalization', 'Method.of.Operation', 'Positive.Alcohol.Tests', 'Positive.Drug.Tests', 'Passengers.Transported',
                  'Hazmat.Cars', 'Head.End.Locomotives', 'Mid.Train.Manual.Locomotives', 'Mid.Train.Remote.Locomotives',
                  'Rear.End.Manual.Locomotives','Rear.End.Remote.Locomotives','Loaded.Freight.Cars','Loaded.Passenger.Cars',
                  'Empty.Freight.Cars','Empty.Passenger.Cars', 'Cabooses', 'Engineers.On.Duty', 'Firemen.On.Duty', 'Brakemen.On.Duty',
                  'Conductors.On.Duty', 'Hours.Engineers.On.Duty', 'Hours.Conductors.On.Duty', 'Minutes.Engineers.On.Duty',
                  'Minutes.Conductors.On.Duty', 'Latitude', 'Longitude', 'Joint.Track.Class', 'Class.Code', 'Reporting.Railroad.Class',
                  'Reporting.Railroad.SMT.Grouping')

possible_responses = c('Accident.Type', 'Equipment.Type', 'Equipment.Damage.Cost', 'Track.Damage.Cost', 'Total.Damage.Cost')

damage_cost_data = rail_data[,c(appliable_pre, 'Equipment.Damage.Cost', 'Track.Damage.Cost', 'Total.Damage.Cost')]


#Initial Cleaning of total_cost_data subset, removing NA values, ''-values, and ensuring within bounds
dim(damage_cost_data)
damage_cost_data$Track.Density = as.numeric(damage_cost_data$Track.Density)
damage_cost_data = na.omit(damage_cost_data)
damage_cost_data = damage_cost_data[damage_cost_data$Track.Density > 0,]
dim(damage_cost_data)
damage_cost_data = damage_cost_data[between(damage_cost_data$Latitude, 25.84, 49.38),]
damage_cost_data = damage_cost_data[between(damage_cost_data$Longitude, -124.77, -66.95),]
dim(damage_cost_data)
#Now make index of those without values less than 0, and not equal to ''.

is_valid_dcd = logical(dim(damage_cost_data)[1])

for (i in 1:length(is_valid_dcd)){
  validator = apply(damage_cost_data[i, -c(39,40)], 1, function(x) all(x >= 0))
  is_valid_dcd[i] = validator
}
sum(is_valid_dcd)
damage_cost_data = damage_cost_data[is_valid_dcd,]
dim(damage_cost_data)
#1269 rows and 47 variables
#Creating variable that is sum of 
damage_cost_data$summed_cost = damage_cost_data$Equipment.Damage.Cost + damage_cost_data$Track.Damage.Cost
sum(damage_cost_data$summed_cost == damage_cost_data$Total.Damage.Cost)
damage_cost_data$Postive.DA.Tests = ifelse((damage_cost_data$Positive.Alcohol.Tests == 1) | 
                                             (damage_cost_data$Positive.Drug.Tests == 1), 'Yes', 'No')
#so 949 rows have equal damages and total cost, rest could be assumed to be insurance payouts/ect
#I leave it up to you to make the judgement on these

#Write Data
write.csv(damage_cost_data, 'C:\\Users\\alden\\Documents\\GRAD_SPRING_2023\\Analytics\\Project\\cleaned_train_data_1.csv', row.names=FALSE)

#Note: Seeing other potential factors.
for (i in names(damage_cost_data)){
  if(typeof(damage_cost_data[,i]) == 'character'){
    print(i)
    print(unique(damage_cost_data[,i]))
    cat('\n')
  }
}