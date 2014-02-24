plotNationtravel <- function(Nation="", show.km=FALSE, showotherbases=FALSE){
    p <- ggmap(map1, extent="panel")
    p <- p + geom_segment(aes(x=NBlon, y=NBlat, xend=lon, yend=lat),
                     color="#FFFFFF", size=1, alpha=.7,
                     data=Complete[grep(Nation, Complete$Nation),])
    p <- p + geom_point(aes(x=lon, y=lat, color=Match), size=4,
                        data=Complete[grep(Nation, Complete$Nation),]) + 
        scale_color_manual(name="Match", values=c("#0080FF", "#DF0101", "#FFFF00"))
    p <- p + labs(title=paste("Group Stage travel: ", Nation, sep=""))
    if(show.km==TRUE){
        p <- p + geom_text(aes(x=lon, y=lat, label=paste("Game ", Game, ": ", Trip, "km", sep="")),
                           color="#FFFFFF", hjust=1, vjust=0, size=4,
                           data=Complete[grep(Nation, Complete$Nation),])
    } else { p <- p}
    if(showotherbases==TRUE){
        p <- p + geom_point(aes(x=NBlon, y=NBlat), color="#FFFFFF", alpha=.3,
                            size=2, data=Complete)
    } else { p <- p}
    print(p)
}

plotGrouptravel <- function(Group="", showbyNation=FALSE){
    p <- ggmap(map1, extent="panel")
    p <- p + geom_point(aes(x=NBlon, y=NBlat, color=Nation), size=3,
                        data=Complete[grep(Group, Complete$Group),]) +
        scale_color_manual(name="Nation", values=c("#FFFFFF", "#DF0101", "#FFFF00", "#2EFE2E"))
    p <- p + geom_segment(aes(x=NBlon, y=NBlat, xend=lon, yend=lat,
                              color=Nation), size=1, alpha=.7,
                          data=Complete[grep(Group, Complete$Group),])
    if(showbyNation==TRUE){
        p <- p + facet_wrap(~Nation)
    } else { p <- p}
    print(p)
}
