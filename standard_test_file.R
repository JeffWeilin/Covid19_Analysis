data = read.csv("/Users/Tony/Desktop/DATA3888/Week4/owid-covid-data-160222 (1).csv")
summary(data)

library (readr)
# urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c3_cancel_public_events.csv"
# 
# cancel_public_events<-read_csv(url(urlfile))
# 
# start = data$date[1]
# end = data$date[length(data$date)]
# 
# urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c6_stay_at_home_requirements.csv"
# stay_at_home = read_csv(url(urlfile))





library(tidyverse)
library(dplyr)
library(tidyr)
data$date <- as.Date(data$date, format = "%Y-%m-%d")

### process:
# Using a for loop, get filepath https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/
# this will be the same but with the end being the different filenames https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_flag.csv
# then in the for loop we want to get this done by koining the data by the dates and the country code
files = c("c1_school_closing.csv", "c2_workplace_closing.csv", "c3_cancel_public_events.csv", "c4_restrictions_on_gatherings.csv",
          "c5_close_public_transport.csv", "c6_stay_at_home_requirements.csv", "c7_movementrestrictions.csv", "e1_income_support.csv",
          "e2_debtrelief.csv", "h1_public_information_campaigns.csv", "h2_testing_policy.csv", "h3_contact_tracing.csv",
          "h6_facial_coverings.csv", "h7_vaccination_policy.csv", "h8_protection_of_elderly_people.csv")


pth = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/"
ls = c()
for (i in 1:length(files)){
  print(i)
  f = paste(pth, files[i], sep = "/")
  f = read_csv(url(f))
  subb = str_sub(files[i], 4, -5)
  ls = append(ls, subb)
  f = f[,!names(f) %in% c("...1", "country_name")]
  f = f%>%
    pivot_longer(!country_code, names_to = "date", values_to = subb)
  f$date <- as.Date(f$date, "%d%b%y")
  f = rename(f, iso_code = country_code)
  data = merge(x = data, y = f, by = c("iso_code", "date")) # works but need to verify if this is correct
}

x  = read.csv("/Users/Tony/Desktop/DATA3888/Week4/owid-covid-data-160222 (1).csv")

# colnames(stay_at_home)
# 
# in_data_but_not_in_oxford = setdiff(data$iso_code, stay_at_home$country_code) # Island nations, Africa and EU
# intersection_data_oxford = intersect(data$iso_code, stay_at_home$country_code) # Good intersection
# #
# stay_at_home = stay_at_home[,!names(stay_at_home) %in% c("...1", "country_name")]
# 
# stay_at_home = stay_at_home%>%
#   pivot_longer(!country_code, names_to = "date", values_to = "stay at home")
# 
# ### Goal convert stay_at_home$date to a date format first then convert this format to the same as in data$date
# # or alternatively do it in 1 step
# 
# stay_at_home$date <- as.Date(stay_at_home$date, "%d%b%y")
# 
# ### Then combine datasets using left join on country name and dates
# stay_at_home = rename(stay_at_home, iso_code = country_code)
# 
# data = merge(x = data, y = stay_at_home, by = c("iso_code", "date")) # works but need to verify if this is correct
# 
# ### Repeat for other useful datasets
# 
# urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/h6_facial_coverings.csv"
# facial_coverings = read_csv(url(urlfile))
# facial_coverings = facial_coverings[,!names(facial_coverings) %in% c("...1", "country_name")]
# 
# facial_coverings = facial_coverings%>%
#   pivot_longer(!country_code, names_to = "date", values_to = "facial coverings")
# 
# ### Goal convert stay_at_home$date to a date format first then convert this format to the same as in data$date
# # or alternatively do it in 1 step
# 
# facial_coverings$date <- as.Date(facial_coverings$date, "%d%b%y")
# 
# ### Then combine datasets using left join on country name and dates
# facial_coverings = rename(facial_coverings, iso_code = country_code)
# 
# data = merge(x = data, y = facial_coverings, by = c("iso_code", "date")) # works but need to verify if this is correct


