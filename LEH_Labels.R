setwd("Z:/Terrestrial/Species/1 - Multi-Species/R_Projects/LEH Tool/Tool")
crs <- "+proj=longlat +datum=WGS84"

leh.all <- readRDS("LEH Boundaries_May24_2024.rds") %>%
  st_transform(crs)

for(i in 1:nrow(leh.all)){
  tt <- st_make_valid(leh.all[i,]) %>%
    st_buffer(10) %>%
    st_centroid()
  if(i == 1) out <- tt else({
    out <- rbind(out, tt)
  })
}

out$Lat <- st_coordinates(out)[,2]
out$Long <- st_coordinates(out)[,1]
out$Species <- leh.all$Species

out <- readRDS("LEH_LabelPoints.rds")

# not ready for this yet, stole this from BC Harvest data tool
# manual <- read_excel("MU Labels_Manual.xlsx","Sheet1")
# for(i in 1:nrow(out)){
#   if(out$LEH[i] %in% manual$MU) {
#     out$Lat[i] <- manual$Lat[manual$MU == out$LEH[i]]
#     out$Long[i] <- manual$Long[manual$MU == out$LEH[i]]
#   }
# }

saveRDS(out, "LEH_LabelPoints.rds")
