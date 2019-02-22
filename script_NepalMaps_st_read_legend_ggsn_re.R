# Tests using sf (scalebar does not seem to work when selected ids are plotted)
# renatamuy@gmail.com

library(sf)
require(ggplot2)
require(ggsn)
library(raster)
library(doBy)

stmap <- st_read("D://NepalMaps-master//baselayers//NPL_adm//NPL_adm3.shp")

# some value to fill
stmap$HPI <- c(runif(1:nrow(stmap)*10))
stmap$id <- stmap$NAME_3


#################### Creating map
ggm1 <- ggplot(stmap , aes(fill = HPI )) +
  geom_sf() +
  scale_fill_continuous(low = "#fff7ec", high = "#7F0000")

############# Plot JUST with scalebar works
ggm1 +
  blank() +
  north(stmap) +
  ggsn::scalebar(stmap, dist = 100, dist_unit = "km",location = "bottomleft", st.dist = 0.05,
           st.bottom = TRUE, st.size = 3,
           transform = TRUE, model = "WGS84") 

# Getting labels for centroids with doBy and fortify 
########
nepal.adm3.shp.df <- fortify(shapefile("D://NepalMaps-master//baselayers//NPL_adm//NPL_adm3.shp") , region = "NAME_3")

nepal.adm3.shp.df <- merge(nepal.adm3.shp.df, stmap[15:16], by = "id", sort = FALSE)

txtVal <- summaryBy(long + lat + HPI  ~ id, data=nepal.adm3.shp.df, FUN=mean, keep.names=T)

# Plotting with all names without scale works normally
ggm1 + geom_text(aes(x=long, y=lat, label=id), data=txtVal, col="black", cex=3)

########### Selecting range of values of interest to show  disctrict name
txtVal$label <- txtVal$id
centroids.selected <- txtVal[txtVal$label %in% (stmap[stmap$HPI>0.8,]$id),]

# Legend does not work when I use with()
ggm1 +
  blank() +
  north(stmap) +
  ggsn::scalebar(stmap, dist = 100, dist_unit = "km",location = "bottomleft", st.dist = 0.05,
           st.bottom = TRUE, st.size = 3,
           transform = TRUE, model = "WGS84") +
  with(centroids.selected, annotate(geom="text", x = long, y = lat, label=label, size=2)) 
  
# Legend also does not work when I use geom_text()
ggm1 +
  blank() +
  north(stmap) +
  ggsn::scalebar(stmap, dist = 100, dist_unit = "km",location = "bottomleft", st.dist = 0.05,
           st.bottom = TRUE, st.size = 3,
           transform = TRUE, model = "WGS84") +
  geom_text(aes(x=long, y=lat, label=id), data=centroids.selected, col="black", cex=3)

#############



