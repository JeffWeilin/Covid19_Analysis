# Load relevant packages and libraries to process data
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

# OWID Covid Data Path
owidURL = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

# Read COVID Data
covidData = as.data.frame(read_csv(owidURL))

# View first few observations
head(covidData)

# Convert date column to actual date values
covidData$date = as.Date(covidData$date, "%Y-%m-%d")

# Compute a general summary of data
summary(covidData)

# Create string array of all files to load in from Oxford Dataset
oxSuffixes = c("c1_school_closing.csv","c2_workplace_closing.csv","c3_cancel_public_events.csv",
               "c4_restrictions_on_gatherings.csv","c5_close_public_transport.csv",
               "c6_stay_at_home_requirements.csv","c7_movementrestrictions.csv","c8_internationaltravel.csv",
               "e1_income_support.csv","e2_debtrelief.csv","h1_public_information_campaigns.csv",
               "h2_testing_policy.csv", "h3_contact_tracing.csv", "h6_facial_coverings.csv",
               "h7_vaccination_policy.csv", "h8_protection_of_elderly_people.csv")

# Identify location of Oxford Data on Github so that the file suffixes above can be iterated through
prefixURL = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries"

vec = c()
# 
for(i in 1:length(oxSuffixes)){
  # Print which iteration the code is up to
  print(i)
  # Concatenate the link to create the URL to load data
  link = paste(prefixURL,oxSuffixes[i],sep = "/")
  # Convert concatenated strings into URL
  dataURL = url(link)
  # Load data using read_csv
  oxData = read_csv(dataURL)
  # Drop specific parts of the file name
  stringSub = str_sub(oxSuffixes[i],4,-5)
  # Create File Name
  varName = append(vec,stringSub)
  # Extract from all rows of data, the column `...1` and `country_name`
  oxData = subset(oxData, select = -c(...1,country_name))

  # Pivot Oxford Data to the same format as the OWID Data
  mergeData = oxData %>%
    pivot_longer(!country_code, names_to = "date", values_to = stringSub)
  # Amend the date values to ensure that it is in the same format
  mergeData$date <- as.Date(mergeData$date,"%d%b%Y")
  # Convert variable names to be identical to the OWID Data for merging
  mergeData = rename(mergeData,iso_code = country_code)
  # Compute a join of the data, by `iso_code` followed by `date`. Disregard values that do not sit within the intersection of the two datasets.
  covidData = merge(x = covidData, y = mergeData, by = c("iso_code","date"))
}

# Select location and HDI
selection <- covidData %>% select(location, human_development_index)

# Remove NA Values of HDI
selection <- na.omit(selection)

# Identify unique locations
uniqueLoc <- unique(selection$location)


# Create an empty data frame to append data into, set up counting variable
df2 = data.frame()
count = 1

# Perform loop to locate index of first occurrence of location
for (i in 1:length(uniqueLoc)){
  index = which(selection$location == uniqueLoc[count])[1]
  print(index)
  df2 = rbind(df2, selection[index,])
  count = count + 1
}

# Remove old index and make the locations the row names
df2 = df2 %>% remove_rownames %>% column_to_rownames(var = "location")

# Compute distance matrix using Euclidean distances
distMat = dist(df2,method = "euclidean")

# Compute hierarchal clustering
hClusts = hclust(distMat,method = "average")


# Dendrogram of heirarchal clustering
plot(hClusts)

# Construct the number of clusters and identify locations in each cluster
fit = cutree(hClusts, k = 6)
fit
table(fit)

# Add divisions to dendrogram
rect.hclust(hClusts,k = 6)

# Identify location and cluster numbers
clusterNo = data.frame(fit)

# Add location name to the dataframe
clusterNo <- rownames_to_column(clusterNo,"location")

# Compute join between the clusters and the existing covid Data
covidData2 = merge(x = covidData, y = clusterNo, by = c("location"))

# Adjust column name
colnames(covidData2)[84] <- "hdiCluster"


## Compute Maximum possible policy score for normalisation of data
maxC1 = max(covidData2$school_closing,na.rm = TRUE)
maxC2 = max(covidData2$workplace_closing,na.rm = TRUE)
maxC3 = max(covidData2$cancel_public_events,na.rm = TRUE)
maxC4 = max(covidData2$restrictions_on_gatherings,na.rm = TRUE)
maxC5 = max(covidData2$close_public_transport,na.rm = TRUE)
maxC6 = max(covidData2$stay_at_home_requirements,na.rm = TRUE)
maxC7 = max(covidData2$movementrestrictions,na.rm = TRUE)
maxC8 = max(covidData2$internationaltravel,na.rm = TRUE)
maxE1 = max(covidData2$income_support,na.rm = TRUE)
maxE2 = max(covidData2$debtrelief,na.rm = TRUE)
maxH1 = max(covidData2$public_information_campaigns,na.rm = TRUE)
maxH2 = max(covidData2$testing_policy,na.rm = TRUE)
maxH3 = max(covidData2$contact_tracing,na.rm = TRUE)
maxH6 = max(covidData2$facial_coverings,na.rm = TRUE)
maxH7 = max(covidData2$vaccination_policy,na.rm = TRUE)
maxH8 = max(covidData2$protection_of_elderly_people,na.rm = TRUE)


# Calculate normalised policy scores
covidData2$normC1 = covidData2$school_closing/maxC1
covidData2$normC2 = covidData2$workplace_closing/maxC2
covidData2$normC3 = covidData2$cancel_public_events/maxC3
covidData2$normC4 = covidData2$restrictions_on_gatherings/maxC4
covidData2$normC5 = covidData2$close_public_transport/maxC5
covidData2$normC6 = covidData2$stay_at_home_requirements/maxC6
covidData2$normC7 = covidData2$movementrestrictions/maxC7
covidData2$normC8 = covidData2$internationaltravel/maxC8
covidData2$normE1 = covidData2$income_support/maxE1
covidData2$normE2 = covidData2$debtrelief/maxE2
covidData2$normH1 = covidData2$public_information_campaigns/maxH1
covidData2$normH2 = covidData2$testing_policy/maxH2
covidData2$normH3 = covidData2$contact_tracing/maxH3
covidData2$normH6 = covidData2$facial_coverings/maxH6
covidData2$normH7 = covidData2$vaccination_policy/maxH7
covidData2$normH8 = covidData2$protection_of_elderly_people/maxH8

# Maximum possible weighted freedom score
maxFreeScore = 1*maxC8 + 1.25 * maxC1 + 1.5 * maxC2 + 1.75 * maxC3 + 2 * maxC4 + 2.25 * maxC5 + 2.5 * maxC7 + 2.75 * maxC6

## Compute Freedom Scores
covidData2$freeScore = (1 - 
                          ((1.25 * covidData2$school_closing + 1.5 * covidData2$workplace_closing
                           + 1.75 * covidData2$cancel_public_events+ 2 * covidData2$restrictions_on_gatherings
                           + 2.25 * covidData2$close_public_transport + 2.75 * covidData2$stay_at_home_requirements
                           + 2.5 * covidData2$movementrestrictions + covidData2$internationaltravel)/maxFreeScore))*100

# Compute Maximum values 
maxCPM = max(covidData2$new_cases_per_million, na.rm = TRUE)
maxDPM = max(covidData2$new_deaths_per_million, na.rm = TRUE)

# Extract cases and deaths per million
CPM = covidData2$new_cases_per_million
DPM = covidData2$new_deaths_per_million

# Calculate combined normalised values
combinedPM = CPM + 1.25 * DPM

# Find maximum and minimum combined values
maxCombined = max(combinedPM,na.rm = TRUE)
minCombined = min(combinedPM,na.rm = TRUE)

# Use a log scale to increase disparity in scores
covidData2$performanceScoreCalc = log(CPM + 1.25 * DPM)

# Extract minimum and maximum scores for performance
minPerfScore = min(covidData2$performanceScoreCalc[is.finite(covidData2$performanceScoreCalc)], na.rm = TRUE)
maxPerfScore = max(covidData2$performanceScoreCalc, na.rm = TRUE)

# Find the difference to normalise the scores
distPerfScore = maxPerfScore - minPerfScore

# Normalised score
covidData2$performanceScore = (1 - (log(CPM + 1.25 * DPM) + abs(minPerfScore))/distPerfScore) * 100

# Change negative infinity scores to 100 - log of 0 cases and deaths = a score of 100 and therefore is good
covidData2$performanceScore[which(is.infinite(covidData2$performanceScore))] = 100


# Use Boxplots to check distribution
boxplot(covidData2$freeScore)
boxplot(covidData2$performanceScore)


# Compute combined score
covidData2$overallScore = (1.25 * covidData2$performanceScore + covidData2$freeScore)/2.25

# Check distribution of scores
boxplot(covidData2$overallScore)

# Write Data to CSV
write.csv(covidData2,"covidDataScored.csv")

# Select Relevant Columns for Analysis
finalDF <- select(covidData2, c(1:16,48:50,63,68:104))
finalDF <- select(finalDF,-c(55))

# Reorder Data in chronological order
finalDF <- finalDF[order(finalDF$iso_code, finalDF$date),]

# Change column names to be meaningful
colnames(finalDF)

finalDF %>% 
  rename(
    TC = total_cases,
    NC = new_cases,
    NCS = new_cases_smoothed,
    TD = total_deaths,
    ND = new_deaths,
    NDS = new_deaths_smoothed,
    TCPM = total_cases_per_million,
    NCPM = new_cases_per_million,
    NCPMS = new_cases_smoothed_per_million,
    TDPM = total_deaths_per_million,
    NDPM = new_deaths_per_million,
    NDPMS = new_deaths_smoothed_per_million,
    Stringency = stringency_index,
    PopDensity = population_density,
    HDI = human_development_index,
    C1 = school_closing,
    C2 = workplace_closing,
    C3 = cancel_public_events,
    C4 = restrictions_on_gatherings,
    C5 = close_public_transport,
    C6 = stay_at_home_requirements,
    C7 = movementrestrictions,
    C8 = internationaltravel,
    E1 = income_support,
    E2 = debtrelief,
    H1 = public_information_campaigns,
    H2 = testing_policy,
    H3 = contact_tracing,
    H6 = facial_coverings,
    H7 = vaccination_policy,
    H8 = protection_of_elderly_people
  )

# Write Data to CSV
write.csv(finalDF,"covidC2DataFinal.csv")
