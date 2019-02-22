###########################################################
################ Map with selected labels and scalebar (Nepal maps)
# renatamuy@gmail.com
# script adapted from 
# https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9
# Download data from https://github.com/anjesh/NepalMaps
###########################################################

rm(list = ls())

library(rgdal)
library(ggplot2)
library(dplyr)
library(ggsn) # download using Rgui, always make sure all functions are loaded by using ggsn::
require(raster)
library(rgdal)
library(rgeos)
#install.packages("doBy")
library(doBy)
require(maptools)


setwd("D://NepalMaps-master//baselayers//NPL_adm//")
# load shp
nepal.adm3.shp <- shapefile("D://NepalMaps-master//baselayers//NPL_adm//NPL_adm3.shp")
# fortify shp
nepal.adm3.shp.df <- fortify(nepal.adm3.shp, region = "NAME_3")
# create map 
map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, y = lat, group = group))

map + geom_path()

map + 
  geom_polygon(aes(fill = id)) +
  coord_fixed(1.3) +
  guides(fill = FALSE)


hpi.data <- read.csv("districts.csv")
str(hpi.data)
colnames(hpi.data) <- c("id","HPI")

# Create data to fill
hpi.data$HPI <- c(runif(1:nrow(hpi.data)*10))
# Merge to data
nepal.adm3.shp.df <- merge(nepal.adm3.shp.df, hpi.data, by ="id")

map <- ggplot(data = nepal.adm3.shp.df, aes(x = long, y = lat, group = group))

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  coord_fixed(1.3)

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3)


map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
  guides(fill=guide_colorbar(title="HP Index"))

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
  guides(fill=guide_colorbar(title="HP Index")) + 
  theme(legend.justification=c(0,0), legend.position=c(0,0))

theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  guides(fill=guide_colorbar(title="HP Index")) + 
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,0), legend.position=c(0,0)) +
  theme_bare

# Getting labels with doBy
########
head(nepal.adm3.shp.df)
txtVal <- summaryBy(long + lat + HPI + group ~ id, data=nepal.adm3.shp.df, FUN=mean, keep.names=T)
head(nepal.adm3.shp.df)
map + geom_text(aes(x=long, y=lat, label=id), data=txtVal, col="yellow", cex=3)

# Making it easier
head(txtVal)
txtVal$label <- txtVal$id
head(hpi.data)

########### Selecting range of values of interest

centroids.selected <- txtVal[txtVal$label %in% (hpi.data[hpi.data$HPI>0.8,]$id),]

########## All ids

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  guides(fill=guide_colorbar(title="HP Index")) + 
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
geom_text(aes(x=long, y=lat, label=id), data=txtVal, col="black", cex=3)


############# Selected ids: Using geom_text()
crs(nepal.adm3.shp)
bbox(nepal.adm3.shp)


map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  guides(fill=guide_colorbar(title="HP Index")) + 
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
  theme(legend.justification=c(0,0), legend.position=c(0.05,0.05)) +
 geom_text(aes(x=long, y=lat, label=id), data=centroids.selected, col="black", cex=3)+  
 theme_bare + 
  #ggsn
  ggsn::scalebar(nepal.adm3.shp.df, dist = 100,location = "bottomleft", transform = TRUE, dist_unit = "km",
                 st.dist = 0.02, st.size = 2.5, model = 'WGS84')+
  north(nepal.adm3.shp.df, scale = .08) 


######### Selected ids: Using with()

map + 
  geom_polygon(aes(fill = HPI), color = 'gray', size = 0.1) +
  ggtitle("Human Poverty Index Map") +
  guides(fill=guide_colorbar(title="HP Index")) + 
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  coord_fixed(1.3) +
  # legend position  
  #theme(legend.position='bottom')
  theme(legend.justification=c(0,-0.1), legend.position=c(0.05,0.05)) +
  with(centroids.selected, annotate(geom="text", x = long, y = lat, label=label, size=2)) +
    theme_bare +
#ggsn
  ggsn::scalebar(nepal.adm3.shp.df, dist = 100,location = "bottomleft", transform = TRUE, dist_unit = "km",
                 st.dist = 0.02, st.size = 2.5, model = 'WGS84')+
  north(nepal.adm3.shp.df, scale = .08) 

#########



