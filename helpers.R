library(mapview)

makeMap <- function(data, zcol="AnimalID",colors,alpha=0.8) {
  outmap <- mapview(data, zcol=zcol, legend=TRUE, cex=4,lwd=1, col.regions=colors,alpha=alpha)
  outmap@map
}