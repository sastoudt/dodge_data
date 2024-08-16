library(tidyverse)
library(sf)
library(lubridate)
library(data.table)
library(chunked)


setwd("~/Documents/Bucknell/dodge_data_processing")

ptm <- proc.time()
tryThis <- fread("inatCleaned.csv")
proc.time() - ptm

# dim(tryThis)

inat_dat <- tryThis
rm(tryThis)

names(inat_dat)

stateAgg <- inat_dat %>%
  group_by(stateProvince) %>%
  summarise(stateTotal = n())

stateSpec <- inat_dat %>%
  group_by(stateProvince, species) %>%
  summarise(count = n())


toUse <- merge(stateSpec, stateAgg, by.x = "stateProvince", by.y = "stateProvince", all.x = T, all.y = F)

toUse <- toUse %>% mutate(prop = count / stateTotal)

state_vals <- unique(toUse$stateProvince)

hometown_hero <- underdog <- rep(NA, length(state_vals))
for (i in 1:length(state_vals)) {
  sub_data <- toUse %>% filter(stateProvince == state_vals[i])
  hometown_hero[i] <- sub_data$species[which.max(sub_data$prop)]
  underdog[i] <- sub_data$species[which.min(sub_data$prop)]
}

data <- cbind.data.frame(state = state_vals, hometown_hero = hometown_hero, underdog = underdog)

taxa <- fread("inaturalist-taxonomy.dwca/taxa.csv")

url_valH <- url_valU <- rep(NA, nrow(data))
for (i in 1:nrow(data)) {
  url_valH[i] <- taxa$identifier[which(taxa$scientificName == data$hometown_hero[i])]
  intm <- taxa$identifier[which(taxa$scientificName == data$underdog[i])]
  if (length(intm) == 0) {

  } else {
    url_valU[i] <- taxa$identifier[which(taxa$scientificName == data$underdog[i])]
  }
}

data$urlH <- url_valH
data$urlU <- url_valU

View(data)

write.csv(data, "../../../Desktop/dodge_data/ecology-data/spec-story.csv", row.names = F)

hh_data <- u_data <- vector("list", length(state_vals))
for (i in 1:length(state_vals)) {
  hh_data[[i]] <- inat_dat %>% filter(stateProvince == state_vals[i] & species == data$hometown_hero[i])
  u_data[[i]] <- inat_dat %>% filter(stateProvince == state_vals[i] & species == data$underdog[i])
}

hh_data <- do.call("rbind", hh_data)
u_data <- do.call("rbind", u_data)

write.csv(hh_data, "../../../Desktop/dodge_data/ecology-data/hometown-hero.csv", row.names = F)
write.csv(u_data, "../../../Desktop/dodge_data/ecology-data/underdog.csv", row.names = F)


test = read.csv("../../../Desktop/dodge_data/ecology-data/stateList.csv")

url_val <- rep(NA, nrow(test))
for (i in 1:nrow(test)) {
  intm = taxa$identifier[which(taxa$scientificName == test$species[i])]
  if (length(intm) == 0) {
    
  } else {
    url_val[i] <- intm[1]
  }
}

test$url_val = url_val

write.csv(test, "../../../Desktop/dodge_data/ecology-data/stateList.csv")

###

hh = read.csv("../../../Desktop/dodge_data/ecology-data/spec-story.csv")


species_look = unique(hh$hometown_hero)

sample_locs = vector("list", nrow(hh))
for(i in 1:nrow(hh)){
  subd = tryThis %>% filter(species == hh$hometown_hero[i] & stateProvince == hh$state[i])
  

  sample_locs[[i]] = subd[sample(1:nrow(subd), 30, replace = T), c("species","stateProvince", "decimalLatitude","decimalLongitude")]
}

sample_locd = do.call("rbind", sample_locs)

write.csv(sample_locd, "../../../Desktop/dodge_data/ecology-data/sample_locHH.csv", row.names= F)

