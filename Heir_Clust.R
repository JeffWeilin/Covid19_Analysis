library(dplyr)
library(tidyverse)
file = read_csv("https://raw.github.sydney.edu.au/thol6150/data3888c2/master/joinedCovidData.csv?token=AAABQQFURJUTPPF27C4C2JLCQGHEW")
dim(file)

# euclidian distance versus centroid

sel = file %>% select(location, human_development_index)

sel = na.omit(sel) # remove countries with na hdi 
#ignore population since it groups countries like Australia and Angola, Afghanistan and Argentina, Fiji and Israel

uniq_countries = unique(sel$location) # 186 countries maybe 31*6? 
# but we remove those without hdi so 174!

df = data.frame()
count = 1
for (i in 1:length(uniq_countries)){
  index = which(sel$location == uniq_countries[count])[1]
  print(index)
  df = rbind(df, sel[index,])
  count = count + 1
}

df = df %>% remove_rownames %>% column_to_rownames(var = "location") ## convert column of locations to the name of the row instead else dist_matrix not work

## dist mat
dist_matrix = dist(df, method = "euclidean")

set.seed(3888)
h_cluster = hclust(dist_matrix, method = "average")
plot(h_cluster)

fit = cutree(h_cluster, k = 6)
fit # results make sense https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index
table(fit)

# interpretation
# cluster 1 are more often than not poorer african countries like zimbabwe, uganda
## also asian countries - pakistan, afghanistan, laos
## also island nations like haiti
## these are countries around 130+ in the UN ranking

# cluster 2 
## Slavic nations like bosnia, Russia, Serbia and ukraine, some island nations in the carribean, 
# some middle eastern countries like Iran, Asian like Thailand
## kazakhstan, some south american nations like uruguay, also romania
## around top - middle of the pack at 45 - 80

# cluster 3 
## above cluster 2 between 1- 45 top 45 in hdi
## i.e. qatar, saudi arabia, slovakia
## also singapore, norway, sweden
## basically Western Europe, USA, Australia, richer middle east / arabian, japan and south korea, baltic states

# cluster 4 mixed bag
## lower than cluster 2 like 80 -120
## Former Soviet states like turkmenistan, Kyrgyzstan, Uzbekistan
## South American like Venezuela
## Also China
## African like Botswana, Tunisia, SA, Egypt
## Middle east jordan
## Pacific Tonga

# cluster 5 
# At 150 + mostly African nations
## Yemen
## Sudan, Chad, South Sudan, Eritrea, Ethiopia

# Cluster 6 
## 120 - 130
## Kiribati , Iraq, Honduras, Namibia, Nicaragua, El salvador, guyana
## Morocco, india

rect.hclust(h_cluster, k = 6)

loc = file %>% select(location)

v = data.frame(fit) 
v <- tibble::rownames_to_column(v, "location")

covidData = merge(x = file, y = v, by = c("location")) # send it!

colnames(covidData)[85] <- "hdi_cluster"

write.csv(covidData,"withclusters.csv")


