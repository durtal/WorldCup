# make sure following packages are installed
library(XML)
library(ggmap)
library(geosphere)
library(ggplot2)

# read World Cup 2014 wiki page into R (XML package)
htmlfile <- readLines("http://en.wikipedia.org/wiki/2014_FIFA_World_Cup")

# use regular expression to get latitudes & longitudes of stadiums
geopattern <- '<span class="geo">(.*?)</'
latlon <- sapply(regmatches(htmlfile[grep(geopattern, htmlfile)], 
                regexec(geopattern, htmlfile[grep(geopattern, htmlfile)])), 
                function(x) x[2])
lat <- sapply(strsplit(latlon, "; "), function(x) x[1])
lon <- sapply(strsplit(latlon, "; "), function(x) x[2]) 

# host cities
HostCity <- c("Rio de Janeiro, RJ", "Brasilia, DF", "Sao Paulo, SP",
            "Fortaleza, CE", "Belo Horizonte, MG", "Porto Alegre, RS",
            "Salvador, BA", "Recife, PE", "Cuiaba, MT", "Manaus, AM",
            "Natal, RN", "Curitiba, PR")

# combine cities, latitudes & longitudes into dataframe
locations <- data.frame(HostCity, lon=as.numeric(lon), lat=as.numeric(lat), 
                        stringsAsFactors=F)

# plot locations of host cities (ggmap package)
map <- get_map(location="Brazil", zoom=4)
map1 <- get_map(location="Brazil", zoom=4, maptype="satellite")
ggmap(map, extent="panel") + 
    theme(legend.position="none") + 
    geom_point(aes(x=lon, y=lat), color="#FFFF00", size=6, pch=20, 
               data=locations) + 
    geom_text(aes(x=lon, y=lat, label=HostCity, hjust=0), size=4.1, fontface=2, 
              vjust=c(0,0,1,rep(0,9)), color="#000000", data=locations)+ 
    labs(title="World Cup 2014: Host Cities")
ggmap(map1, extent="panel") + 
    theme(legend.position="none") + 
    geom_text(aes(x=lon, y=lat, label=HostCity, hjust=-.1), size=4, 
        vjust=c(0,0,1, rep(0,9)), color="#FFFF00", data=locations) + 
    geom_point(aes(x=lon, y=lat, size=5), color="#FFFF00", data=locations) +
	labs(title="World Cup 2014: Host Cities")

# scrape Match info: dates, ko time, teams, match no.
dateregex <- 'class="summary">(.*?)<br '
dates <- sapply(regmatches(htmlfile[grep(dateregex, htmlfile)], 
                regexec(dateregex, htmlfile[grep(dateregex, htmlfile)])), 
                function(s) s[2])
Date <- as.Date(dates, format="%d %B %Y")
KO <- unlist(strsplit(htmlfile[grep(dateregex, htmlfile)+1], "<"))[seq(1,192,3)]
DateKO <- paste(Date, KO, sep=" ")

vsregex <- '2014_FIFA_World_Cup_Group_[A-Z]#(.*?)" title'
Match <- sapply(regmatches(htmlfile[grep(vsregex, htmlfile)], 
                regexec(vsregex, htmlfile[grep(vsregex, htmlfile)])),
                function(s) s[2])
Match <- gsub("_", " ", Match)
TeamA <- unlist(strsplit(Match, " v "))[seq(1,96,2)]
TeamB <- unlist(strsplit(Match, " v "))[seq(2,96,2)]

matchNoregex <- 'Group [A-Z]">Match (.*?)</a></th>'
match.no <- sapply(regmatches(htmlfile[grep(matchNoregex, htmlfile)], 
                regexec(matchNoregex, htmlfile[grep(matchNoregex, htmlfile)])), 
                function(s) s[2])

# venues proved much more troublesome due to irregular HTML code
venueregex <- '(.*?)<br /></div'
venuetext <- htmlfile[grep("(.*)<br /></div>", htmlfile)][seq(2,128,2)]
venuetext <- unlist(strsplit(venuetext, "<br"))[seq(1,128,2)]
venuetext <- unlist(strsplit(venuetext, ">, "))[seq(2, 128, 2)]
y <- c(grep("Paulo", venuetext), grep("Rio de Janeiro", venuetext))
SPRJ <- venuetext[y]
venuetext <- venuetext[-y]
venuetext <- unlist(strsplit(venuetext, ">"))[seq(2, 102, 2)]
venuetext <- unlist(strsplit(venuetext, "<"))[seq(1, 102, 2)]
Venue <- rep("NA", 64)
Venue[y] <- SPRJ
Venue[-y] <- venuetext

abr <- c("Rio de", "Bras", "Paulo", "Forta", "Belo", "Porto", "Salva", "Recif",
        "Cuia", "Mana", "Natal", "Curit")
for(a in 1:length(abr)){
    Venue[grep(abr[a], Venue)] <- HostCity[a]
}
Group <- rep(c("A", "B", "C", "D", "E", "F", "G", "H"), each=6)

# put info of group matches (first 48 games) in dataframe
fixtures.df <- data.frame(Date=Date[1:48], KO=KO[1:48], DateKO=DateKO[1:48], 
                    match.no, HostCity=Venue[1:48], Group, Match, TeamA, TeamB, 
                    stringsAsFactors=F)

# read in team bases (source BBC) and make 2 plot (bases, & bases & venues)
nationbase <- read.delim("./R/bbcteambases.txt", sep="\t", h=T, 
                       stringsAsFactors=F)
nationbase$NBlon <- geocode(nationbase$Base)[[1]]
nationbase$NBlat <- geocode(nationbase$Base)[[2]]
ggmap(map, extent="panel") + 
    geom_point(aes(x=NBlon, y=NBlat), size=4, color="#FF0000", alpha=.6, 
               data=nationbase) +
    labs(title="World Cup 2014: Nation Base Camps")

cols <- c("Nation Base Camps" = "#FF0000", "Host Cities" = "#000000")
# change host cities color to #FFFF00 when plotting on satellite
ggmap(map, extent="panel") +
    geom_point(aes(x=NBlon, y=NBlat, col="Nation Base Camps"), size=4.5, alpha=.6, 
               data=nationbase) + 
    geom_point(aes(x=lon, y=lat, col="Host Cities"), size=3.5, data=locations) +
    labs(title="World Cup 2014: Nation Base Camps & Host Cities") +
    scale_color_manual(name="", values=cols)

# plot map showing distances from Brasilia
fromBrasilia <- round((distHaversine(locations[2,2:3], locations[,2:3]) * 
                        0.001), 0)
Brasilia <- cbind(locations, fromBrasilia)
ggmap(map1, extent="panel") +
    geom_point(aes(x=lon, y=lat), color="#FFFF00", size=3, data=Brasilia[-2,]) +
    geom_text(aes(x=lon, y=lat, hjust=0,
                  label=paste(HostCity, fromBrasilia, "km", sep=" ")),
              color="#FFFF00", size=4, vjust=c(0,1,rep(0,9)),
              data=Brasilia[-2,]) +
    geom_point(aes(x=lon, y=lat), color="#FFFFFF", size=4, data=Brasilia[2,]) +
    geom_text(aes(x=lon, y=lat, label=HostCity), hjust=-.1, color="#FFFFFF",
              size=5, data=Brasilia[2,]) +
    labs(title="World Cup 2014: Distance (in km) from Brasilia to 11 other Host Cities")

# create distance matrix, from team bases (32 rows) to host cities (12 columns)
distmatrix <- matrix(NA, ncol=12, nrow=32)
dimnames(distmatrix) <- list(nationbase$Nation, HostCity)
for(i in 1:32){
    distmatrix[i,] <- round(distHaversine(nationbase[nationbase$Nation==
                            row.names(distmatrix)[i],3:4], locations[,2:3]) 
                            * 0.001, 0)
}

# create complete data.frame with matches, travel, nationbases, venues, etc
Nations <- rep(nationbase$Nation, each=3)
Game <- rep(c(1,2,3), 32)
Complete <- c()
for(i in 1:96){
    Complete <- rbind(Complete, cbind(Nation=Nations[i], Game=Game[i], 
                                      fixtures.df[grep(Nations[i], fixtures.df$Match),][Game[i],]))
}
Complete <- merge(Complete, locations, by="HostCity")
Complete <- merge(Complete, nationbase, by="Nation")
Complete <- Complete[order(Complete$Game),]
Complete <- Complete[order(Complete$Nation),]
for(i in 1:96){
    Complete$Trip[i] <- distmatrix[Complete$Nation[i], Complete$HostCity[i]]
}
for(i in 1:32){
    Complete$Trip[Complete$Game==3][i] <- Complete$Trip[Complete$Game==3][i] + 
        (Complete$Trip[Complete$Game==1][i] * 2) + 
        (Complete$Trip[Complete$Game==2][i] * 2)
}
for(i in 1:32){
    Complete$Trip[Complete$Game==2][i] <- Complete$Trip[Complete$Game==2][i] + 
        (Complete$Trip[Complete$Game==1][i] * 2)
}
Complete$match.no <- as.numeric(Complete$match.no)
Complete$DateKO <- as.POSIXlt(Complete$DateKO)

# calculate days since prior game
Complete$DaysSince <- 0
for(i in 1:32){
    Complete$DaysSince[Complete$Game==2][i] <- Complete$DateKO[Complete$Game==2][i] - 
        Complete$DateKO[Complete$Game==1][i]
}
for(i in 1:32){
    Complete$DaysSince[Complete$Game==3][i] <- Complete$DateKO[Complete$Game==3][i] - 
        Complete$DateKO[Complete$Game==2][i]
}

# create bar plots of trips (change code accordingly for Games 2 and 3)
# change how you order to create daysince plots
Game1 <- subset(Complete, Game==1)
Game1 <- Game1[order(Game1$Trip),]
Game1$Nation <- factor(Game1$Nation, levels=Game1$Nation)
ggplot(Game1, aes(x=Nation, y=Trip, fill=Group)) + 
    geom_bar(stat="identity", color="#000000") + 
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0.2, hjust=1)) +
    labs(y="Distance (km)", 
         title="Distance traveled ahead of the 1st Group Game") +
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                               "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"))

# plot traveling of direct match opponents
ggplot(data=Complete[Complete$Game==3,], aes(x=Nation, y=Trip, fill=Group)) + 
    geom_bar(stat="identity", color="#000000") + 
    facet_wrap(~Match, scales="free_x") + 
    theme_bw() +
    scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                               "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")) +
    labs(title="Game 3: Individual matches & traveling of direct opponents")

# clean workspace leaving just dataframes of locations, fixtures and distances
rm(Date, DateKO, Game, Group, HostCity, KO, Match, Nations, SPRJ, TeamA, TeamB,
   Venue, a, abr, cols, dateregex, dates, fromBrasilia, geopattern, i, lat, lon,
   match.no, matchNoregex, venueregex, venuetext, vsregex, y, latlon)