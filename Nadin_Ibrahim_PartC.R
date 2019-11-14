#I am interested in analyzing the phylum Tardigrada from the BOLD database.These small and cute creatures are known colloquially as "water bears" because of their four pairs of stubby legs. I would like to see from what countires the sampled specimens were obtained, to get an understanding of where these organisms are able to live.I would also like to create rarefaction curve to evaluate the adequacy/completeness of species sampling around the world.


######----- INSTALLING AND LOADING NEEDED PACKAGES---- 

#Uncomment and install package, if needed.
#install.packages("vegan")
#Then, load the library so that you will have access to the functions by running the following line.
library(vegan)

#install.packages("tidyverse")
library(tidyverse)


#Run this API call to work with Tardigrada data from BOLD
Tardigrada <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Tardigrada&format=tsv")

#### Alicia Major Edit 1
#The first major edit made by Alicia was to do additional data exploration and filtering at the beginning of the analysis. Being selective with the data being used for the analyses is important for ensuring that only values with the relevant information are being used to avoid issues down the line.

# What headers are there? i.e. what information do we have?
colnames(Tardigrada) %>% sort
# How many values are therer?
nrow(Tardigrada) # 1103 rows of data
# let's look at that data in a bit more detail
View(Tardigrada) # there looks like there are a LOT of NAs in this data
# How many NAs are there in each column?
sapply(Tardigrada, function(x) sum(is.na(x))) %>% sort
# there are 459 NAs in the species column and 77 in the bin column.... not great resoluation there
# 41 are also missing sequence information in the nucleotide column


# let's be a bit more selective with the data and filter out some missing data
# since the visualization for this script was based on BINs, I will filter out any rows that are missing BIN values
Tardigrada %>%
  filter(!is.na(Tardigrada$bin_uri)) -> Tardigrada_filtered
# How many unique bins do we have in the filtered data?
Tardigrada_filtered$bin_uri %>% 
  unique %>% length #215 unique bins in the filtered data

#From what countries has the phylum Tardigrada been DNA barcoded, based on the data available from BOLD?
list(unique(Tardigrada$country))


#Which countries have the most data about the phylum Tardigrada
Tardigrada %>%
  group_by(country) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  print()
#Notice that many specimen samples do not indicate the country from which they were sampled

# Alicia repeated above code for withthe filtered data
list(unique(Tardigrada_filtered$country))

Tardigrada_filtered %>%
  group_by(country) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  print()
# similar results to above

# Alicia filtered out missing country information here 
Tardigrada_filtered %>% filter(!is.na(Tardigrada_filtered$country)) -> Tardigrada_filter_country


#Create a bar graph showing the number of specimens sampled from each country, ignoring the samples without countries recorded
barplot(table(Tardigrada$country),xlab= "Countries", ylab = "Number Of Specimens", main = "Number of Specimens (Tardigrada) Sampled From Each Country", las=2, cex.names = 0.6, ylim = c(0,250))

# Alicia making edit to code formatting and with filtered data
barplot(table(Tardigrada_filter_country$country), 
        xlab= "Countries", 
        ylab = "Number Of Specimens", 
        main = "Number of Specimens (Tardigrada) Sampled From Each Country", 
        las=2, cex.names = 0.6, ylim = c(0,250))


#Community Data
#Ecological community data consist of observations of the (relative) abundance of species in different samples

#We will turn Tardigrada into a community object (comm) for analysis using functions from the vegan package. Using the bin_uri variable (BINs = Barcode Index Numbers) as a proxy for species. The columns in the community object will be BINs.

#Here, we are grouping the data by BIN and counting the number of records in each BIN. We are creating a new tibble called Tardigrada.count.by.BIN, by taking the data from Tardigrada, grouping by BIN (a proxy for species), and counting the number of specimen records (which are rows in our BOLD data) per BIN.
Tardigrada.count.by.BIN <- Tardigrada %>%
  group_by(bin_uri) %>%
  count(bin_uri)

#Now, we are using a very useful function called spread() from the tidyr package (within tidyverse suite) to get the data into the comm data object format.
commTardigrada <- spread(Tardigrada.count.by.BIN, bin_uri, n)

#Using commAssign1, build an individual-based accumulation curve using the function rarecurve()
rarecurve(commTardigrada)

#### Alicia Major Edit 2
#Since the analysis is focused on the samples' country of origin, creating an accumulation with the BINs grouped by country provides a visual representation of the data that is more relevant to the project question.

# Going to repeat Nadin 5 chunk with filtered country data with modification to group by country
commTardigrada_filter <- Tardigrada_filter_country %>% 
  group_by(country, bin_uri) %>% 
  count(bin_uri) %>%
  spread(bin_uri, n)

#we need to replace all the NAs where specific country don't have a BIN with a 0
commTardigrada_filter[is.na(commTardigrada_filter)] <- 0

#since country name is currently sitting as part of the data and not as row names, need to change that first column into the row names
commTardigrada_filter <- commTardigrada_filter %>% 
  remove_rownames %>% 
  column_to_rownames(var="country")

# species accumulation curve using the function specaccum()
specaccum(commTardigrada_filter) %>% plot

#### Alicia Major Edit 3
#Since part of the assignment was to use new function from the vegan package, Alicia has added the vegdist function to Nadin's script. The vegdist function looks at the dissimilarity in community composition between countries.
# Cluster distance matrix
vegdist(commTardigrada_filter, method = "bray") %>% 
  hclust(method = "average") -> Tardigrada_clustered

plot(Tardigrada_clustered)

#Based on the analyses above, we can see that the phylum Tardigrada has the ability to live in various parts of the world, even in very cold weather conditions. In fact, it seems that the majority of specimen sampling from BOLD was obtained from Antarctica. These tiny creatures are known to be extremely resilient and able to survive even with exposure to extreme temperatures and pressures (both high and low), air deprivation, radiation, dehydration, and starvation- things that would quickly kill most other known forms of life. Based on the specimen records from BOLD after plotting the rarefaction curve, we see that sampling size is quite large, and that the curve is just beginning to level off, indicating good sampling completeness. 
