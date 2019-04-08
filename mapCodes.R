HeatMap_Fun <- function(h1bdata) {
  
  left <- data.frame(stateList)
  names(left)[1] <- "S_FULLNAME"
  
  left <- left %>% 
    filter(!(S_FULLNAME %in% c("NA", "DISTRICT OF COLUMBIA", "PUERTO RICO"))) %>% 
    arrange(S_FULLNAME)
  
  left$state <- state.abb
  
  right <-  h1bdata %>% 
    filter(!(STATES %in% c("NA", "DISTRICT OF COLUMBIA", "PUERTO RICO"))) %>% 
    group_by(STATES) %>% summarise(Count = n()) %>% arrange(STATES)
  
  names(right)[1] <- "S_FULLNAME"
  
  # right <- right %>% filter(S_FULLNAME %in% c("TEXAS", "CALIFORNIA"))
  
  merge_mapTab <- merge(x = left, y = right, by = "S_FULLNAME", all = TRUE) 
  
  merge_mapTab$Count[is.na(merge_mapTab$Count)] <-  0
  
  heatMap <- plot_usmap(data = merge_mapTab, 
                        values = "Count", labels = TRUE, 
                        lines = "red") + 
    scale_fill_continuous(
      low = "white", high = "red", name = "H1B Filing Count",
      label = scales::comma
    ) + theme(legend.position = "right")
  
  return(heatMap)
}

bubbMap <- function(h1bdata){
  h1bdata$rlat = round(h1bdata$lat,1)
  h1bdata$rlon = round(h1bdata$lon,1)
  
  mapdf <- h1bdata %>% filter(!is.na(rlat)) %>% filter(!is.na(rlon)) %>% 
    group_by(rlat, rlon) %>%
    summarise(value = length(rlat)) %>% ungroup()
  
  colnames(mapdf) <- c("lat","lon","value")
  
  bins <- c(max(mapdf$value), 150000, 100000, 50000, min(mapdf$value))
  
  pal <- colorBin("RdYlGn", domain = mapdf$value, bins = bins)
  
  bubble <- leaflet(data = mapdf) %>%
    addTiles() %>% setView(-99, 35, zoom = 4) %>%
    addCircleMarkers(
      lat=mapdf$lat, lng=mapdf$lon, radius=sqrt(mapdf$value)/10, 
      color = ~pal(mapdf$value),
      weight=1.5, opacity=0.8,
      popup= paste("<br><strong>Applications: </strong>", mapdf$value
      ))
  
  return(bubble)
}


