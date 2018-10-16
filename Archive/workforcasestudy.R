Beers <- read.csv("~/Desktop/MSDS6306-CS1/Beers.csv")
View(Beers)
Breweries <- read.csv("~/Desktop/MSDS6306-CS1/Breweries.csv")
View(Breweries)
#Question 1
breweries.by.state <- table(Breweries$State)
breweries.by.state

sum(is.na(Beers))
install.packages(plyr)

#Question 2
colnames(Beers)[colnames(Beers)=="Brewery_id"] <- "Brew_ID"
Merged_beers_breweries <- merge(Beers,Breweries, by ="Brew_ID")
colnames(Merged_beers_breweries)[colnames(Merged_beers_breweries)=="Name.x"] <- "Beer Name"
colnames(Merged_beers_breweries)[colnames(Merged_beers_breweries)=="Name.y"] <- "Brewery"
head(Merged_beers_breweries)

library(plyr)

#Question 3
sapply(Beers,function(x) sum(is.na(x)))
sapply(Breweries, function(x) sum(is.na(x)))

#Question 4

library(plyr)
ABV.medians.by.state <- ddply(Merged_beers_breweries, "State", function(Merged_beers_breweries) median(Merged_beers_breweries$ABV,na.rm=T))
colnames(ABV.medians.by.state)[colnames(ABV.medians.by.state)=="V1"] <- "ABV"
ABV.medians.by.state

IBU.medians.by.state <- ddply(Merged_beers_breweries, "State", function(Merged_beers_breweries) median(Merged_beers_breweries$IBU,na.rm=T))
colnames(IBU.medians.by.state)[colnames(IBU.medians.by.state)=="V1"] <- "IBU"
IBU.medians.by.state
