library(mapview)

makeMap <- function(data, zcol="AnimalID",colors,alpha=0.8) {
  outmap <- mapview(data, zcol=zcol, legend=TRUE, cex=4,lwd=1, col.regions=colors,alpha=alpha,label="acquisitiontime") +
    makeLinePointMap(data)
  outmap@map
}

makeLinePointMap <- function(sf.dat){
  
  trajectory <- sf.dat %>%
    group_by(AnimalID) %>% arrange(acquisitiontime) %>%
    dplyr::summarize(do_union=FALSE) %>%  
    st_cast("LINESTRING") %>% arrange(AnimalID)
  #points <- sf.dat %>% arrange(AnimalID)
  
  #trajectory <- trajectory[1:15,]
  
  points <- sf.dat %>%
    group_by(AnimalID) %>% arrange(acquisitiontime) %>%
    dplyr::summarize(do_union=FALSE) %>% arrange(AnimalID)
  
  nanimal <- nrow(trajectory)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  if (nanimal < length(col_vector)) colors <- sample(col_vector, nanimal) else colors <- sample(col_vector, nanimal,
                                                                                                replace=TRUE)
  
  or.table <- sf.dat  %>%  as_tibble()  %>%  dplyr::select(-geometry)
  points.a <- left_join(points,or.table, join_by(AnimalID)) # join the original attributes back, doesn't completely solve
  
  # it'd be nice to have an idea which are the first point in a animal trajectory and which is the last
  first.pts <- sf.dat %>% group_by(AnimalID) %>% slice_min(acquisitiontime,n=1)
  last.pts <- sf.dat %>% group_by(AnimalID) %>% slice_max(acquisitiontime,n=1)
  
  mapview(trajectory,zcol="AnimalID",color=colors)+mapview(sf.dat,zcol="AnimalID",label="acquisitiontime",
                                                           cex=4,col.regions=colors,alpha.regions=1,legend=FALSE) +
    mapview(first.pts,cex=4.5,alpha=1,alpha.regions=0,color='green',label="acquisitiontime",legend=FALSE)+
    mapview(last.pts,cex=4.5,alpha=1,alpha.regions=0,color='red',label="acquisitiontime",legend=FALSE)
  
}