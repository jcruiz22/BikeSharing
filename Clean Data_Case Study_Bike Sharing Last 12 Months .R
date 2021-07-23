#Main libraries 
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(dplyr)

#Importing the data of the past 12 months 
BikeTrips_Apr2020<-read_csv("CSV's Proyecto Final/202004-divvy-tripdata.csv")
BikeTrips_May2020<-read_csv("CSV's Proyecto Final/202005-divvy-tripdata.csv")
BikeTrips_Jun2020<-read_csv("CSV's Proyecto Final/202006-divvy-tripdata.csv")
BikeTrips_Jul2020<-read_csv("CSV's Proyecto Final/202007-divvy-tripdata.csv")
BikeTrips_Aug2020<-read_csv("CSV's Proyecto Final/202008-divvy-tripdata.csv")
BikeTrips_Sept2020<-read_csv("CSV's Proyecto Final/202009-divvy-tripdata.csv")
BikeTrips_Oct2020<-read_csv("CSV's Proyecto Final/202010-divvy-tripdata.csv")
BikeTrips_Nov2020<-read_csv("CSV's Proyecto Final/202011-divvy-tripdata.csv")
BikeTrips_Dic2020<-read_csv("CSV's Proyecto Final/202012-divvy-tripdata.csv")
BikeTrips_Jan2021<-read_csv("CSV's Proyecto Final/202101-divvy-tripdata.csv")
BikeTrips_Feb2021<-read_csv("CSV's Proyecto Final/202102-divvy-tripdata.csv")
BikeTrips_Mar2021<-read_csv("CSV's Proyecto Final/202103-divvy-tripdata.csv")
BikeTrips_Apr2021<-read_csv("CSV's Proyecto Final/202104-divvy-tripdata.csv")

#Comparing columns with compare_df_cols_same as we see we need to put end_station_id & start_station_id as char variable 
compare_df_cols_same(BikeTrips_Apr2020,BikeTrips_May2020,BikeTrips_Jun2020,BikeTrips_Jul2020,BikeTrips_Aug2020,BikeTrips_Sept2020,BikeTrips_Oct2020,
                     BikeTrips_Nov2020,BikeTrips_Dic2020,BikeTrips_Jan2021,BikeTrips_Feb2021,BikeTrips_Mar2021,BikeTrips_Apr2021)

#Convert the variables mention above
BikeTrips_Apr2020<-mutate(BikeTrips_Apr2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_May2020<-mutate(BikeTrips_May2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Jun2020<-mutate(BikeTrips_Jun2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Jul2020<-mutate(BikeTrips_Jul2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Aug2020<-mutate(BikeTrips_Aug2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Sept2020<-mutate(BikeTrips_Sept2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Oct2020<-mutate(BikeTrips_Oct2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Nov2020<-mutate(BikeTrips_Nov2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Dic2020<-mutate(BikeTrips_Dic2020,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Jan2021<-mutate(BikeTrips_Jan2021,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Feb2021<-mutate(BikeTrips_Feb2021,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Mar2021<-mutate(BikeTrips_Mar2021,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))
BikeTrips_Apr2021<-mutate(BikeTrips_Apr2021,end_station_id=as.character(end_station_id),start_station_id=as.character(start_station_id))

#Comparing columns with compare_df_cols_same as we see we need to put end_station_id & start_station_id as char variable 
compare_df_cols_same(BikeTrips_Apr2020,BikeTrips_May2020,BikeTrips_Jun2020,BikeTrips_Jul2020,BikeTrips_Aug2020,BikeTrips_Sept2020,BikeTrips_Oct2020,
                     BikeTrips_Nov2020,BikeTrips_Dic2020,BikeTrips_Jan2021,BikeTrips_Feb2021,BikeTrips_Mar2021,BikeTrips_Apr2021)

#Merging all 12 in to one 
all_trips<-bind_rows(BikeTrips_Apr2020,BikeTrips_May2020,BikeTrips_Jun2020,BikeTrips_Jul2020,BikeTrips_Aug2020,BikeTrips_Sept2020,BikeTrips_Oct2020,
                                              BikeTrips_Nov2020,BikeTrips_Dic2020,BikeTrips_Jan2021,BikeTrips_Feb2021,BikeTrips_Mar2021,BikeTrips_Apr2021)
#Removing Duplicates 
all_trips_no_dups<-all_trips[!duplicated(all_trips$ride_id), ]
print(paste("Deletated",nrow(all_trips)-nrow(all_trips_no_dups),"of x2 rows")) 

#adding date-time 
all_trips_no_dups$started_at<-as.POSIXct(all_trips_no_dups$started_at,"%Y-%m-%d %H:%M:%S")
all_trips_no_dups$ended_at<-as.POSIXct(all_trips_no_dups$ended_at,"%Y-%m-%d %H:%M:%S")

#addin columns 
#adding column with ride time in minutes
all_trips_no_dups<-all_trips_no_dups %>% 
  mutate(ride_time_m=as.numeric(all_trips_no_dups$ended_at-all_trips_no_dups$started_at)/60)
summary(all_trips_no_dups$ride_time_m)

#column with year(Y)&month(M) 
all_trips_no_dups<-all_trips_no_dups %>% 
  mutate(year_month=paste(strftime(all_trips_no_dups$started_at,"%Y"),"-",strftime(all_trips_no_dups$started_at,"%m"),paste("(",strftime(all_trips_no_dups$started_at,"%b"),")",sep="")))
unique(all_trips_no_dups$year_month)

#weekday
all_trips_no_dups<- all_trips_no_dups %>% 
  mutate(weekday = paste(strftime(all_trips_no_dups$ended_at,"%u"),"-",strftime(all_trips_no_dups$ended_at,"%a")))
unique(all_trips_no_dups$weekday)

#start_hour
all_trips_no_dups<-all_trips_no_dups %>% 
  mutate(start_hour=strftime(all_trips_no_dups$ended_at,"%H"))
unique(all_trips_no_dups$start_hour)

#Remove Unnecesary Columns 
all_trips_no_dups<-all_trips_no_dups %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))

all_trips_no_dups %>% 
  write.csv("Clean Data Bike Trips")

install.packages("writexl")
library("writexl")

all_trips_no_dups %>%
  write_xlsx("Clean Data Bike Trips In Excel")
