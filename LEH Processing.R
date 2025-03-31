# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

Report <- read_excel("Q:/Biometrics Unit/Wildlife/LEH/LEH Draw Hunt Code Report - 2002 to fall 2024.xlsx")
Estimates <- read_excel("Q:/Biometrics Unit/Wildlife/Survey/Estimates/LEH Survey Estimates 1984 to final 2023.xlsx")
setwd("Z:/Terrestrial/Species/1 - Multi-Species/R_Projects/LEH Tool/Tool")
Wild <- read_excel("LEH_WILD Export.xlsx") %>%
  subset(Species != "Grizzly Bear")
Wild <- Wild[order(Wild$Code),]
Wild$Zone[is.na(Wild$Zone)] <- ""
Wild$LEHZone <- paste(Wild$`LEH Hunting Area`, Wild$Zone, sep = " ")

species.table <- data.frame(Abrv = c("BEAG", "BISO", "CARI", "ELK",  "MOOS", "GOAT", "SHEE", "DEMU", "DEWT", "TURK", "BEAB"),
                            Full = c("Grizzly Bear", "Bison", "Caribou", "Elk", "Moose", "Mountain Goat", "Mountain Sheep", 
                                     "Mule Deer",  "White-tailed Deer", "Turkey", "Black Bear"))
region.table <- data.frame(Name = c("Vancouver Island", "Lower Mainland", "Thompson", "Kootenay", "Cariboo",
                                    "Skeena", "Omineca", "Peace", "Okanagan"),
                           Number = as.character(c(c(1:6), "7A", "7B", 8)))
for(i in 1:nrow(region.table)){
  Wild$Region[Wild$Region == region.table$Name[i]] <- region.table$Number[i]
}
rm(region.table)

season.date <- function(x, estimates = F){
  if(estimates) xx <- mdy(as.character(x)) else({xx <- ymd(as.character(x))})
  paste(month(xx), day(xx), sep = "-")
}
                        

for(i in 1:nrow(species.table)){
  Report$Species[Report$Species == species.table$Abrv[i]] <- species.table$Full[i]
  Estimates$Species[Estimates$Species == species.table$Abrv[i]] <- species.table$Full[i]
}


rm(i, species.table)
Wild$MU <- Wild$`LEH Hunting Area`
Wild$YearsPast <- 0
Wild$Success <- NA
Wild$PastHunters <- 0
Wild$DrawOdds <- NA
Wild$SpecInst <- gsub(pattern = "M.U.", x = Wild$`Special Instructions`, replacement = "") %>%
  tolower()
Wild$YouthOnly <- "No"


for(i in 1:nrow(Wild)){
  est <- subset(Estimates, Species == Wild$Species[i]) %>%
    subset(`LEH Hunt Area` == paste(Wild$`LEH Hunting Area`[i], Wild$Zone[i], sep = "")) %>%
    subset(tolower(`Animal Class`) == tolower(Wild$`Animal Class`[i])) %>% ## gotta deal with animal class
    subset(Year - year(Wild$`Season One Open Date`[i]) >= -5) %>% ## keeps only the last 5 years
    subset(season.date(`Season 1 Start`, estimates = T) == season.date(Wild$`Season One Open Date`[i], estimates = F)) %>%
    subset(season.date(`Season 1 End`, estimates = T) == season.date(Wild$`Season One Close Date`[i], estimates = F))
  Wild$YearsPast[i] <- nrow(est)
  if(nrow(est) > 0){
    if(sum(est$`Estimated Hunters`, na.rm = T) > 0){
      Wild$Success[i] <- round(sum(est$`Estimated Kills`, na.rm = T) / sum(est$`Estimated Hunters`, na.rm = T), 2)
      Wild$PastHunters[i] <- round(sum(est$ `Estimated Hunters`, na.rm = T), 0)
    } else(Wild$Success[i] <- NA)
  }
  rm(est)
  rep <- subset(Report, Species == Wild$Species[i]) %>%
    subset(`LEH Hunt Area` == paste(Wild$`LEH Hunting Area`[i], Wild$Zone[i], sep = "")) %>%
    subset(tolower(`Licence Description`) == tolower(Wild$`Animal Class`[i])) %>% ## gotta deal with animal class
    subset(`Draw Year` - year(Wild$`Season One Open Date`[i]) >= -5) %>% ## keeps only the last 5 years
    subset(season.date(`Season 1 Start`, estimates = F) == season.date(Wild$`Season One Open Date`[i], estimates = F)) %>%
    subset(season.date(`Season 1 End`, estimates = F) == season.date(Wild$`Season One Close Date`[i], estimates = F))
  if(nrow(rep) > 0){
    Wild$DrawOdds[i]<- mean(rep$`First Choice Odds`, na.rm = T)
    if(Wild$DrawOdds[i] < 10) Wild$DrawOdds[i] <- round(Wild$DrawOdds[i],1) else({
      Wild$DrawOdds[i] <- round(Wild$DrawOdds[i], 0)
    })
  }
  tt <- strsplit(Wild$Area[i], split = "[.]")[[1]] %>%
    strsplit(split = " ")
  if(any(tt[[1]] %in% "Y")) Wild$YouthOnly[i] <- "Yes"
}

Wild$Season <- paste(paste(month(Wild$`Season One Open Date`, label = T), day(Wild$`Season One Open Date`), sep = " "),
                     paste(month(Wild$`Season One Close Date`, label = T), day(Wild$`Season One Close Date`)), sep = "-")
tt <- !is.na(Wild$`Season Two Open Date`)
Wild$Season[tt] <- paste(Wild$Season[tt], paste(paste(month(Wild$`Season Two Open Date`[tt], label = T), 
                                                      day(Wild$`Season Two Open Date`[tt]), sep = " "),
                               paste(month(Wild$`Season Two Close Date`[tt], label = T), 
                                     day(Wild$`Season Two Close Date`[tt])), sep = "-"), sep = ", ")
Wild$LimitedData <- ifelse(Wild$PastHunters < 20, "Yes", 
                           ifelse(Wild$PastHunters < 50, "Slightly", "No"))
Wild$`Hunt Method` <- ifelse(Wild$`Hunt Method` == "Bow Only", "Yes", "No") 


Seasons <- list()
for(i in 1:nrow(Wild)){
  tt <-c(interval(start = Wild$`Season One Open Date`[i], end = Wild$`Season One Close Date`[i]),
         interval(start = Wild$`Season Two Open Date`[i], end = Wild$`Season Two Close Date`[i]))
  tt <- tt[!is.na(tt)]
  Seasons[[i]] <- as.interval(tt)
}
names(Seasons) <- Wild$Code

# saveRDS(Seasons, "Wild Seasons.rds")

date <- ymd(20241110)
date.in <- function(x) date %within% x
temp <- lapply(Seasons, function(x) date %within% x) %>%
  unlist()


# Wild$Success[Wild$Species %in% c("Mountain Sheep", "Mountain Goat", "Turkey", "Mule Deer")] <- NA

# Wild <- data.frame(Code = Wild$Code, Species = Wild$Species, 
#                    Area = Wild$LEHZone, Region = Wild$Region, Season = Wild$Season, AnimalClass = Wild$`Animal Class`, 
#                    TentativeAuthorizations = Wild$`Tentative Authorizations`, 
#                    DrawOdds = Wild$DrawOdds, HunterSuccess = Wild$Success)



Wild.Out <- data.frame(Code = Wild$Code, Species = Wild$Species, 
                   Area = Wild$LEHZone, Region = Wild$Region, Season = Wild$Season, AnimalClass = Wild$`Animal Class`, 
                   DrawOdds = Wild$DrawOdds, HunterSuccess = Wild$Success * 100, 
                 BowOnly  = Wild$`Hunt Method`, YouthOnly = Wild$YouthOnly, LimitedData = Wild$LimitedData)

writexl::write_xlsx(Wild.Out, "WILD LEH Table - For Tool.xlsx")

















#################### Processing Linework ###############
# library(sf)
# crs <- "+proj=longlat +datum=WGS84"
# leh <- st_read("W:/wlap/nel/Workarea/lvanderv/LEH_AllSpecies_May24_2024.shp") %>%
#   st_transform(crs) 
# tt <- unique(leh$LTD_ENTRY_)[]
# for(i in 1:length(tt)){
#   temp <- unique(Wild$Species[tolower(Wild$Species) == tolower(leh$LTD_ENTRY_[leh$LTD_ENTRY_ == tt[i]])])
#   if(length(temp) == 1)  leh$Species[leh$LTD_ENTRY_ == tt[i]] <- temp
# }
# leh <- subset(leh, Species != "NA") %>%
#   subset(!is.na(Species))
# 
# for(i in 1:nrow(leh)){
#   tt <- strsplit(leh$LEH_HUNT_A[i], split = "")[[1]]
#   if(length(tt) == 4) leh$LEH_HUNT_A[i] <- paste(tt[1], tt[2], tt[3], " ", tt[4], sep = "")
#   if(length(tt) == 3) leh$LEH_HUNT_A[i] <- paste(tt[1], tt[2], tt[3], sep = "")
# }
# rm(tt, i, temp)



setwd("Z:/Terrestrial/Species/1 - Multi-Species/R_Projects/LEH Tool/Tool")
Wild <- read_excel("WILD LEH Table - For Tool.xlsx")
wmus.all <- readRDS("ProvincialWMUs.rds")

leh.small <- leh[,which(names(leh) %in% c("Species", "MANAGEMENT", "LEH_HUNT_A"))] 
names(leh.small) <- c("MU", "LEH", "Species", "geometry")
wmus.small <- wmus.all[,which(names(wmus.all) %in% c("WILDLIFE_M", "REGION_RES", "MU"))]
wmus.small[,1] <- wmus.small$MU
names(wmus.small) <- names(leh.small)
wmus.small$LEH <- wmus.small$MU
wmus.small$Species <- "Any"

leh.all <- rbind(leh.small, wmus.small) ### not sure this is needed anymore


saveRDS(leh.small, "Z:/Terrestrial/Species/1 - Multi-Species/R_Projects/LEH Tool/LEH Boundaries_May24_2024.rds")
