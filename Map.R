setwd("C:/Users/Juliana/Desktop")
coordinates <- read.delim("indCoordinates.txt", h = T)

library(ggmap)
library(ggplot2)
library(ggsn)

ggmap::register_google(key = "AIzaSyDRf382w7jEEzhu6pQlFcnGDnjfvdH1axk")
map <- get_map(location = c(lon = -74.010, lat = 4.80581), maptype = "satellite", zoom = 14, scale = 2)
sg <- data.frame(lon = coordinates$lat ,
                lat = coordinates$lon, id = coordinates$Code, s = coordinates$Sector)
sgA <- subset(sg, s == "A")
sgB <- subset(sg, s == "B")

mapRec <- ggmap(map) +
          scale_x_continuous(limits = c(-74.025,-73.9933), expand = c(0, 0)) +
          scale_y_continuous(limits = c(4.7985,4.815), expand = c(0, 0))

mapPoints <- mapRec + 
            geom_point(aes(x = lon, y = lat), data = sg, size = 2.5, color ="white")

finalAll <- mapPoints + scalebar(x.min = -74.015, x.max = -73.995,
         y.min = 4.81399, y.max = 4.8155, box.fill = "white",
         dist = 500, dist_unit = "m", box.color = c("white","gray50"),
         st.bottom = TRUE, st.color = "white", st.size = 6.5, st.dist = 0.5,
         transform = TRUE, model = "WGS84")

tiff("Map_all.tiff", units="in", width=10, height=7, res=300)
finalAll
dev.off()


mapA <-  get_map(location = c(lon = -73.997, lat = 4.804), maptype = "satellite", zoom = 16)
mapRecA <- ggmap(mapA) +
           scale_x_continuous(limits = c(-74.0020,-73.9930), expand = c(0, 0)) +
           scale_y_continuous(limits = c(4.799,4.807), expand = c(0, 0))
mapPointsA <- mapRecA + 
             geom_point(aes(x = lon, y = lat), data = sgA, size=2, color ="white") +
             geom_text(aes(x = lon, y = lat, label = id), vjust = -0.5, hjust=-0.2, 
                       data = subset(sgA,id != "A10" & id != "A11"), size=7.5, color ="white",
                       fontface = "bold")
finalA <- mapPointsA + scalebar(x.min = -74.00002, x.max = -73.9985,
                                y.min = 4.7998, y.max = 4.8003, box.fill = "white", height = 0.035,
                                dist = 150, dist_unit = "m", box.color = c("white","gray50"),
                                st.bottom = TRUE, st.color = "white", st.size = 8, st.dist = 0.45,
                                transform = TRUE, model = "WGS84")

tiff("Map_SectorA.tiff", units="in", width=7, height=7, res=300)
finalA
dev.off()


mapB <- get_map(location = c(lon = -74.019, lat = 4.810), maptype = "satellite", zoom = 16)
mapRecB <- ggmap(mapB) +
          scale_x_continuous(limits = c(-74.0248,-74.0135), expand = c(0, 0)) +
          scale_y_continuous(limits = c(4.8075,4.8137), expand = c(0, 0))
mapPointsB <- mapRecB + 
            geom_point(aes(x = lon, y = lat), data = sgB, size=2, color ="white") +
            geom_text(aes(x = lon, y = lat, label = id), vjust = -0.5, hjust=0, 
                      data = subset(sgB,id != "B10"), size=6, color ="white",
                      fontface = "bold")
finalB <- mapPointsB + scalebar(x.min = -74.026, x.max = -74.021,
                      y.min = 4.8082, y.max = 4.809, box.fill = "white",
                      dist = 150, dist_unit = "m", box.color = c("white","gray50"),
                      st.bottom = TRUE, st.color = "white", st.size = 8, st.dist = 0.25,
                      transform = TRUE, model = "WGS84")

tiff("Map_SectorB.tiff", units="in", width=10, height=6.5, res=300)
finalB
dev.off()
