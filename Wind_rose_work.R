#Read in the data
#Reformat to long data frame (reshape)
#Calculate centroids for each region
#Calculate bearing from selected centroid to all other centroids in that region
library(reshape2)
library(ggplot2)
library(XML)
library(tidyr)
library(ggmap)
library(geosphere)
library(dplyr)

names <- c("Drive", "Cycle", "Walk")
remove <- c("United Kingdom", "Great Britain", "England", "Wales", "Scotland", "Northern Ireland", "England and Wales", "North East",
"North West", "East", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "Mainly work at or from home", 
"Offshore installation", "No fixed place", "Outside UK", "")

#TODO: Create this as a function for all three subsets of data. 
name <- read.csv("108483705.csv", skip = 9, nrows = 431)
removedestinations <- colnames(name)[2:24]
name <- gather(name, home, people, -place.of.work)
removeorigins <- name[1:23,1]
name$home <- gsub("[.]"," ",name$home)
name <- name[name$place.of.work != removorigins, ]
name <- name[name$home != removedestinations, ]
name$workloc <- lapply(as.character(name$place.of.work), function (x) FUN = geocode(x)
            

worknames <- unique(as.character(name$place.of.work))
uk <- rep(", united kingdom", times = length(worknames))
worknames <- paste0(worknames,uk)
worknames <- unique(as.character(name$place.of.work))

locations <- lapply(worknames, function (x) FUN = geocode(x))
longitudes <- sapply(locations,function(x) x[[1]])
latitudes <- sapply(locations,function(x) x[[2]])
locations <- cbind.data.frame(worknames,latitudes,longitudes)


colnames(locations)[1] <- "place.of.work"
main <- merge(x = name, y = locations, by = "place.of.work")
colnames(locations)[1] <- "home"
main <- merge(x = main, y = locations, by = "home")

colnames(main)[4:7] <- c( "destlat", "destlong", "originlat", "originlong")

#TODO: Check Bearing
bearing <- lapply(1:nrow(main), function (x) FUN = bearing(c(main$destlong[x], main$destlat[x]),c(main$originlong[x],main$originlat[x]), a=6378137, f=1/298.257223563))
bearing <- unlist(bearing)
main$bearing2 <- bearing2
bearing2 <- ifelse(bearing < 0, bearing + 360, bearing)
direction <- bearing2

main <- main[complete.cases(main),]
main <- main[ ! main$home %in% remove, ]
main <- main[ ! main$place.of.work %in% remove, ]


main$bearing2[findInterval(main$bearing2, c(0,22.5)) == 1L] <- "N"
main$bearing2[findInterval(main$bearing2, c(22.5,67.5)) == 1L] <- "NE"
main$bearing2[findInterval(main$bearing2, c(67.5,112.5)) == 1L] <- "E"
main$bearing2[findInterval(main$bearing2, c(112.5,157.5)) == 1L] <- "SE"
main$bearing2[findInterval(main$bearing2, c(157.5,202.5)) == 1L] <- "S"
main$bearing2[findInterval(main$bearing2, c(202.5,247.5)) == 1L] <- "SW"
main$bearing2[findInterval(main$bearing2, c(247.5,292.5)) == 1L] <- "W"
main$bearing2[findInterval(main$bearing2, c(292.5,337.5)) == 1L] <- "NW"
main$bearing2[findInterval(main$bearing2, c(337.5,360)) == 1L] <- "N"

ord<-c("N","NE","E","SE","S","SW","W","NW")
df <- main[main$place.of.work == sort(c("Birmingham", "Cardiff", "Oxford", "Leeds", "Liverpool", "Sheffield", "London", "Glasgow", "Bradford", "Edinburgh")),]
df$bearing2 <- factor(df$bearing2,levels=ord)
df$people <- as.numeric(df$people)
df$mode <- sample(c(TRUE,FALSE), nrow(df),TRUE)

df2 <- df %>% group_by(bearing2, mode, place.of.work) %>% summarise(n.people = sum(people))
df2 <- df2[complete.cases(df2),]





ggplot(df2, aes(x =bearing2,y = n.people, fill= mode)) + geom_bar(width =0.8, stat = "identity") + coord_polar(theta = "x", start = -0.3926991) + theme(legend.position="bottom") + scale_y_log10() + theme_bw() + facet_wrap(~ place.of.work)


