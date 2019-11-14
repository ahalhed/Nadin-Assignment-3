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


#From what countries has the phylum Tardigrada been DNA barcoded, based on the data available from BOLD?
list(unique(Tardigrada$country))


#Which countries have the most data about the phylum Tardigrada
Tardigrada %>%
  group_by(country) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  print()
#Notice that many specimen samples do not indicate the country from which they were sampled

#Create a bar graph showing the number of specimens sampled from each country, ignoring the samples without countries recorded
barplot(table(Tardigrada$country),xlab= "Countries", ylab = "Number Of Specimens", main = "Number of Specimens (Tardigrada) Sampled From Each Country", las=2, cex.names = 0.6, ylim = c(0,250))


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



#Based on the analyses above, we can see that the phylum Tardigrada has the ability to live in various parts of the world, even in very cold weather conditions. In fact, it seems that the majority of specimen sampling from BOLD was obtained from Antarctica. These tiny creatures are known to be extremely resilient and able to survive even with exposure to extreme temperatures and pressures (both high and low), air deprivation, radiation, dehydration, and starvation- things that would quickly kill most other known forms of life. Based on the specimen records from BOLD after plotting the rarefaction curve, we see that sampling size is quite large, and that the curve is just beginning to level off, indicating good sampling completeness. 
