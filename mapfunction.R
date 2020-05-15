

binomCI <- function(x){ 
  y=x/100
  100*sqrt(y*(1-y)/nrow(db7))}

map_f <- function(database,variable,fac){


## identify EU countries
show <- which(worldMap$NAME %in% database$Location)

## EU coordinates
showCoords <-
  lapply(show,
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
showCoords <- do.call("rbind", showCoords)

## this will be used as the background and will include non EU countries
plotCoords <-
  lapply(seq(worldMap$NAME),
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
plotCoords <- do.call("rbind", plotCoords)


## add EU identifier
plotCoords$EU <- 0
plotCoords$EU[which(plotCoords$region %in% database$Location)] <- 1

## add endemicity category
#howCoords$Trap <-
#  dbtrap$MostUsed[match(showCoords$region, dbtrap$Location)]

## for some reason, this group gives a horizontal segment across Europe
plotCoords <- plotCoords[plotCoords$group != "United States4", ]


## add endemicity category


if(fac == "yes"){
showCoords$present <-
  factor(variable[match(showCoords$region, database$Location)],levels = levels(variable))
}else{
  vv <- tapply(variable, database$Location, mean,na.rm=T)
  showCoords$present <-
    vv[match(showCoords$region, names(vv))]
}


ggplot() +
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = "white", colour = "darkgrey", size = 0.1)+
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = present),
    size = 0.1)+
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = NA, colour = "black", size = 0.1)+
  xlab("")+ylab("")+
  coord_map(xlim=c(-20,45),ylim=c(28,63.5))+
#coord_cartesian(xlim=c(-20,45),ylim=c(29,70))+
  theme_bw()+
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       #title="Person Involved", 
      # caption="Source: aimquestmaster155quest280519V1") +
      caption="") 

}





# define a custom panel function
myPanelFunc <- function(...){
  panel.likert(...)
  vals <- list(...)
  DF <- data.frame(x=vals$x, y=vals$y, groups=vals$groups)
  
  ### some convoluted calculations here...
  grps <- as.character(DF$groups)
  for(i in 1:length(origNames)){
    grps <- sub(paste0('^',origNames[i]),i,grps)
  }
  
  DF <- DF[order(DF$y,grps),]
  
  DF$correctX <- ave(DF$x,DF$y,FUN=function(x){
    x[x < 0] <- rev(cumsum(rev(x[x < 0]))) - x[x < 0]/2
    x[x > 0] <- cumsum(x[x > 0]) - x[x > 0]/2
    return(x)
  })
  
  subs <- sub(' Positive$','',DF$groups)
  collapse <- subs[-1] == subs[-length(subs)] & DF$y[-1] == DF$y[-length(DF$y)]
  DF$abs <- abs(DF$x)
  DF$abs[c(collapse,FALSE)] <- DF$abs[c(collapse,FALSE)] + DF$abs[c(FALSE,collapse)]
  DF$correctX[c(collapse,FALSE)] <- 0
  DF <- DF[c(TRUE,!collapse),]
  
  DF$perc <- ave(DF$abs,DF$y,FUN=function(x){x/sum(x) * 100})
  ###
  
  panel.text(x=DF$correctX, y=DF$y, label=paste0(round(DF$perc,1),'%'), cex=0.7)
}

