## GBIF.org (23 January 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.xh4sme
## desktop screenshot: gbif-warning-big-data
## desktop screenshot: gbif-temp-restricted
##  Between start of 2010 and end of 2023


#### packages ####
library(tidyverse)
library(sf)
library(lubridate)
library(data.table)
library(chunked)


#### slim down iNat data - do once ####
f <- function(x, pos) {
  x[, c(10, 18, 22, 23, 30)]
  ## species, decimalLatitude, decimalLongitude, stateProvince, eventDate
}

test <- read_tsv_chunked("0071744-231120084113126.csv", DataFrameCallback$new(f), chunk_size = 10000)

missing <- which(is.na(test$decimalLatitude) | is.na(test$decimalLongitude) | is.na(test$species) | is.na(test$stateProvince) | is.na(test$eventDate))

# length(missing)

test2 <- test[-missing, ]
# dim(test2)

test4 <- test2 %>% mutate(year = year(eventDate), month = month(eventDate), yday = yday(eventDate))

write.csv(test4, "inatCleaned.csv", row.names = F)


#### summarize data - start here ####
setwd("~/Documents/Bucknell/dodge_data_processing")

ptm <- proc.time()
tryThis <- fread("inatCleaned.csv")
proc.time() - ptm

# dim(tryThis)

inat_dat <- tryThis
rm(tryThis)

#### by state/year/season species richness ####

inat_dat$season <- case_when(
  inat_dat$month %in% c(12, 1, 2) ~ "winter",
  inat_dat$month %in% c(3, 4, 5) ~ "spring",
  inat_dat$month %in% c(6, 7, 8) ~ "summer",
  inat_dat$month %in% c(9, 10, 11) ~ "fall"
)

byState <- inat_dat %>%
  group_by(stateProvince) %>%
  summarise(specRichness = length(unique(species)))
byState <- byState %>% filter(stateProvince %in% state.name)

byStateSeason <- inat_dat %>%
  group_by(season, stateProvince) %>%
  summarise(specRichness = length(unique(species)))
byStateSeason <- byStateSeason %>% filter(stateProvince %in% state.name)

byStateYear <- inat_dat %>%
  group_by(year, stateProvince) %>%
  summarise(specRichness = length(unique(species)))
byStateYear <- byStateYear %>% filter(stateProvince %in% state.name)

byStateYearSeason <- inat_dat %>%
  group_by(year, season, stateProvince) %>%
  summarise(specRichness = length(unique(species)))
byStateYearSeason <- byStateYearSeason %>% filter(stateProvince %in% state.name)

write.csv(byState, "byState.csv", row.names = F)
write.csv(byStateSeason, "byStateSeason.csv", row.names = F)
write.csv(byStateYear, "byStateYear.csv", row.names = F)
write.csv(byStateYearSeason, "byStateYearSeason.csv", row.names = F)

#### Jaccard similarity ####

stateLists <- inat_dat %>%
  group_by(stateProvince, species) %>%
  summarise(count = n())
stateLists <- stateLists %>% filter(stateProvince %in% state.name)

stateSeasonList <- inat_dat %>%
  group_by(stateProvince, season, species) %>%
  summarise(count = n())
stateSeasonList <- stateSeasonList %>% filter(stateProvince %in% state.name)

stateYearList <- inat_dat %>%
  group_by(stateProvince, year, species) %>%
  summarise(count = n())
stateYearList <- stateYearList %>% filter(stateProvince %in% state.name)

stateSeasonYearList <- inat_dat %>%
  group_by(stateProvince, year, season, species) %>%
  summarise(count = n())
stateSeasonYearList <- stateSeasonYearList %>% filter(stateProvince %in% state.name)

write.csv(stateLists, "stateList.csv", row.names = F)
write.csv(stateSeasonList, "stateSeasonList.csv", row.names = F)
write.csv(stateYearList, "stateYearList.csv", row.names = F)
write.csv(stateSeasonYearList, "stateSeasonYearList.csv", row.names = F)

jaccard_sim <- function(place1, place2, data) {
  data_sub <- data %>% filter(stateProvince %in% c(place1, place2))
  byPlace <- split(data_sub, data_sub$stateProvince)
  in_both <- intersect(byPlace[[1]]$species, byPlace[[2]]$species)
  in_first <- setdiff(byPlace[[1]]$species, byPlace[[2]]$species)
  in_second <- setdiff(byPlace[[2]]$species, byPlace[[1]]$species)

  js <- length(in_both) / (length(in_both) + length(in_first) + length(in_second))
  return(js)
}

jaccard_simY <- function(place1, place2, year, data) {
  data_sub <- data %>%
    filter(stateProvince %in% c(place1, place2)) %>%
    filter(year == year)
  byPlace <- split(data_sub, data_sub$stateProvince)
  in_both <- intersect(byPlace[[1]]$species, byPlace[[2]]$species)
  in_first <- setdiff(byPlace[[1]]$species, byPlace[[2]]$species)
  in_second <- setdiff(byPlace[[2]]$species, byPlace[[1]]$species)

  js <- length(in_both) / (length(in_both) + length(in_first) + length(in_second))
  return(js)
}

jaccard_simS <- function(place1, place2, season, data) {
  data_sub <- data %>%
    filter(stateProvince %in% c(place1, place2)) %>%
    filter(season == season)
  byPlace <- split(data_sub, data_sub$stateProvince)
  in_both <- intersect(byPlace[[1]]$species, byPlace[[2]]$species)
  in_first <- setdiff(byPlace[[1]]$species, byPlace[[2]]$species)
  in_second <- setdiff(byPlace[[2]]$species, byPlace[[1]]$species)

  js <- length(in_both) / (length(in_both) + length(in_first) + length(in_second))
  return(js)
}

jaccard_simYS <- function(place1, place2, year, season, data) {
  data_sub <- data %>%
    filter(stateProvince %in% c(place1, place2)) %>%
    filter(year == year) %>%
    filter(season == season)
  byPlace <- split(data_sub, data_sub$stateProvince)
  in_both <- intersect(byPlace[[1]]$species, byPlace[[2]]$species)
  in_first <- setdiff(byPlace[[1]]$species, byPlace[[2]]$species)
  in_second <- setdiff(byPlace[[2]]$species, byPlace[[1]]$species)

  js <- length(in_both) / (length(in_both) + length(in_first) + length(in_second))
  return(js)
}


stateCombos <- expand.grid(state1 = state.name, state2 = state.name)
stateCombos <- stateCombos[-which(stateCombos$state1 == stateCombos$state2), ]

stateJaccard <- mapply(jaccard_sim, stateCombos$state1, stateCombos$state2, MoreArgs = list(data = stateLists), SIMPLIFY = T)

stateCombos$jaccard <- stateJaccard
write.csv(stateCombos, "stateSimilarity.csv", row.names = F)


stateYearCombos <- expand.grid(state1 = state.name, state2 = state.name, year = unique(inat_dat$year))
stateYearCombos <- stateYearCombos[-which(stateYearCombos$state1 == stateYearCombos$state2), ]

stateYearJaccard <- mapply(jaccard_simY, stateYearCombos$state1, stateYearCombos$state2, stateYearCombos$year, MoreArgs = list(data = stateYearList), SIMPLIFY = T)

stateYearCombos$jaccard <- stateYearJaccard
write.csv(stateYearCombos, "stateYearSimilarity.csv", row.names = F)

stateSeasonCombos <- expand.grid(state1 = state.name, state2 = state.name, season = unique(inat_dat$season))
stateSeasonCombos <- stateSeasonCombos[-which(stateSeasonCombos$state1 == stateSeasonCombos$state2), ]


stateSeasonJaccard <- mapply(jaccard_simS, stateSeasonCombos$state1, stateSeasonCombos$state2, stateSeasonCombos$season, MoreArgs = list(data = stateSeasonList), SIMPLIFY = T)

stateSeasonCombos$jaccard <- stateSeasonJaccard
write.csv(stateSeasonCombos, "stateSeasonSimilarity.csv", row.names = F)
