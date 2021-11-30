# Code for reproducing results from Evans et al. 2022
# "Modeling the Risk Reduction Benefit of Forest Management Using a Case Study in the Lake Tahoe Basin"

library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(osmdata)
library(gridExtra)
library(grid)

datadir <- "~/dropbox/research/mystudies/wildfireimpacts/laketahoe_propertypaper/data/"
## Load fire incidence data
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

fire_r1s1 <- loadRData(paste(datadir,"fire_intensity_full_scenario1_round1.RData")) ## Round 1, Scenario 1
fire_r1s2 <- loadRData(paste(datadir,"fire_intensity_full_scenario2_round1.RData")) ## Round 1, Scenario 2
fire_r1s3 <- loadRData(paste(datadir,"fire_intensity_full_scenario3_round1.RData")) ## Round 1, Scenario 3
fire_r1s4 <- loadRData(paste(datadir,"fire_intensity_full_scenario4_round1.RData")) ## Round 1, Scenario 4
fire_r1s5 <- loadRData(paste(datadir,"fire_intensity_full_scenario5_round1.RData")) ## Round 1, Scenario 5
fire_r1s5 <- fire_r1s5 %>% 
  dplyr::select(-t31:-t100) %>% 
  mutate(scenario = as.character(scenario))

fire <- bind_rows(fire_r1s1, fire_r1s2, fire_r1s3, fire_r1s4, fire_r1s5)
rm(fire_r1s1, fire_r1s2, fire_r1s3, fire_r1s4, fire_r1s5)

# Load property data (sort.map variable indicates LTB parcel)
properties <- readRDS(paste(datadir,"LTB_Properties.rds"))

prop_sf <- properties %>% 
  st_as_sf(coords = c("PropertyAddressLongitude", "PropertyAddressLatitude"), 
           crs = 4326, agr = "constant")


# Create data frame with the number of properties in each pixel
prop_sort.map <- prop_sf %>% 
  filter(PropertyTypeAgg != "Vacant Land") %>% 
  as_tibble() %>% 
  group_by(sort.map) %>% 
  tally()

prop_sf %>% 
  filter(PropertyTypeAgg != "Vacant Land") %>% 
  group_by(PropertyTypeAgg) %>% 
  tally()

# Figure 2: Home densities by census tract
ltb_border <- st_read("~/dropbox/lake tahoe west/ltb/ltb.shp")
ltb_parcel <- raster("~/dropbox/lake tahoe west/sort_map_ltb.tif")
ltb_parcel_sf = rasterToPolygons(ltb_parcel) %>% 
  st_as_sf()
tracts_ca <- st_read(paste(datadir,"tl_2019_06_tract/tl_2019_06_tract.shp"))
tracts_nv <- st_read(paste(datadir,"tl_2019_32_tract/tl_2019_32_tract.shp"))

tracts <- rbind(tracts_ca, tracts_nv)
tracts <- tracts %>% 
  st_transform(4326)

rm(tracts_ca, tracts_nv)
tm_shape(prop_sf) +tm_dots() +
tm_shape(ltb_border) + tm_borders()


HomesPerTract <- st_join(prop_sf, tracts, join = st_within)
homes_tract_count <- count(as_tibble(HomesPerTract), GEOID) %>%
  print()
tract_homes_sf <- left_join(tracts, homes_tract_count) %>% 
  filter(n>0) %>% 
  mutate(homes_sq_km = as.numeric(n / (ALAND/1000000)))


tract_homes_sf <- st_intersection(tract_homes_sf, ltb_border)
Fig2 <-
  tm_shape(ltb_border) + tm_borders()  +
  tm_shape(tract_homes_sf) + tm_fill(col="homes_sq_km", 
                                   title="Properties per Square km",
                                   breaks = c(0, 25, 50, 100, 200, 300, 400, 500, 1000, 1500),
                                   legend.hist = TRUE) +
  tm_legend(legend.outside = TRUE) +
  tm_shape(ltb_border) + tm_borders()  

# Create variables indicating whether a wildfire occurred over the 30 year time frame (1=yes, 0=no)
fire <- fire %>% 
  mutate(HighIntensity = ifelse(t1 == 3 | t2 == 3 | t3 == 3 | t4 == 3 | t5 == 3 |t6 == 3 | t7 == 3 | t8 == 3 | t9 == 3 | t10 == 3 |
                                t11 == 3 | t12 == 3 | t13 == 3 | t14 == 3 | t15 == 3 |t16 == 3 | t17 == 3 | t18 == 3 | t19 == 3 | t20 == 3 |  
                                t21 == 3 | t22 == 3 | t23 == 3 | t24 == 3 | t25 == 3 |t26 == 3 | t27 == 3 | t28 == 3 | t29 == 3 | t30 == 3, 1, 0 )) %>% 
  mutate(MedIntensity = ifelse(t1 == 2 | t2 == 2 | t3 == 2 | t4 == 2 | t5 == 2 |t6 == 2 | t7 == 2 | t8 == 2 | t9 == 2 | t10 == 2 |
                                  t11 == 2 | t12 == 2 | t13 == 2 | t14 == 2 | t15 == 2 |t16 == 2 | t17 == 2 | t18 == 2 | t19 == 2 | t20 == 2 |  
                                  t21 == 2 | t22 == 2 | t23 == 2 | t24 == 2 | t25 == 2 |t26 == 2 | t27 == 2 | t28 == 2 | t29 == 2 | t30 == 2, 1, 0 )) %>% 
  mutate(MedHighIntensity = ifelse(HighIntensity + MedIntensity > 0, 1, 0))

# Calculate fire probability for each parcel/scenario
fire_prob <- fire %>% 
  group_by(sort.map, scenario) %>% 
  summarize(HighProb = sum(HighIntensity / 10, na.rm=TRUE),
            MedProb = sum(MedIntensity / 10, na.rm=TRUE),
            MedHighProb = sum(MedHighIntensity / 10, na.rm = TRUE))


# Merge property count data to fire probability frame
fire_prob <- fire_prob %>% 
  left_join(prop_sort.map, by='sort.map') %>% 
  mutate(n = ifelse(is.na(n),0,n))

## Figure 3: Fire occurence and intensity by scenario  
# 3 x 3 plot (rows = fire intensity , col=probability threshold)
F3_MH_25 <- fire_prob %>% 
  filter(MedHighProb >= 0.25) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_M_25 <- fire_prob %>% 
  filter(MedProb >= 0.25) %>% 
  group_by(scenario) %>% 
  tally() # %>%
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  geom_text(aes(scenario, n/83241, label= round(n/83241,2)), vjust=-1) +
  ylab("") + xlab("") + ylim(0,.25) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_H_25 <- fire_prob %>% 
  filter(HighProb >= 0.25) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") + ylim(0,.25) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_MH_50 <- fire_prob %>% 
  filter(MedHighProb >= 0.5) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") + ylim(0,.25) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_M_50 <- fire_prob %>% 
  filter(MedProb >= 0.5) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_H_50 <- fire_prob %>% 
  filter(HighProb >= 0.5) %>% 
  group_by(scenario) %>% 
  tally() # %>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") + 
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_MH_75 <- fire_prob %>% 
  filter(MedHighProb >= 0.75) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("")  +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_M_75 <- fire_prob %>% 
  filter(MedProb >= 0.75) %>% 
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("")  +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

F3_H_75 <- fire_prob %>% 
  filter(HighProb >= 0.75) %>% 
  ungroup() %>% 
  add_row(sort.map=NA, scenario=as.character(5), HighProb=NA, MedProb=NA, MedHighProb=NA) %>% # Need to add this in for the graph since there are no high severity fires in this scenario
  group_by(scenario) %>% 
  tally() #%>% 
  ggplot() +
  geom_bar(aes(scenario, n/83241), stat="identity") +
  ylab("") + xlab("") + 
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 


grid.arrange( arrangeGrob(F3_M_25, F3_MH_25, F3_H_25, top=">25%"), 
              arrangeGrob(F3_M_50, F3_MH_50, F3_H_50, top=">50%"),
              arrangeGrob(F3_M_75, F3_MH_75, F3_H_75, top=">75%"), ncol=3, 
              left = textGrob("Fraction of Landscape", rot = 90, vjust = 1),
              bottom = textGrob("Scenario"),
              top = textGrob("Percent of replicates with wildfire"))

Fig3 <- grid.arrange( F3_M_25, F3_M_50, F3_M_75,
             F3_H_25, F3_H_50, F3_H_75,
             F3_MH_25, F3_MH_50, F3_MH_75,
             nrow=3, ncol=3)

Fig3 <- grid.arrange( F3_H_25, F3_H_50, F3_H_75,
                      F3_MH_25, F3_MH_50, F3_MH_75,
                      nrow=2, ncol=3)


# Figure 4: Number of homes at risk
HAR_M_25 <- fire_prob %>% 
  filter(MedProb >= 0.25) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) # %>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,20000) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

HAR_M_50 <- fire_prob %>% 
  filter(MedProb >= 0.5) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) # %>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,13500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri"))

HAR_M_75 <- fire_prob %>% 
  filter(MedProb >= 0.75) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) #%>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,4500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

HAR_H_25 <- fire_prob %>% 
  filter(HighProb >= 0.25) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) #%>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,20000) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

HAR_H_50 <- fire_prob %>% 
  filter(HighProb >= 0.5) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) #%>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,13500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri"))

HAR_H_75 <- fire_prob %>% 
  filter(HighProb >= 0.75) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE))# %>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,4500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri"))

HAR_MH_25 <- fire_prob %>% 
  filter(MedHighProb >= 0.25) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE)) # %>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,20000) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

HAR_MH_50 <- fire_prob %>% 
  filter(MedHighProb >= 0.5) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE))  #%>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,13500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri"))

HAR_MH_75 <- fire_prob %>% 
  filter(MedHighProb >= 0.75) %>% 
  group_by(scenario) %>% 
  summarize(HomesAtRisk = sum(n, na.rm=TRUE))#  %>% 
  ggplot() +
  geom_bar(aes(scenario, HomesAtRisk), stat="identity") +
  ylab("") + xlab("") + ylim(0,4500) +
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

Fig4 <- grid.arrange(HAR_M_25, HAR_M_50, HAR_M_75,
                     HAR_H_25, HAR_H_50, HAR_H_75,
                     HAR_MH_25, HAR_MH_50, HAR_MH_75,
                     nrow=3, ncol=3)

Fig4 <- grid.arrange(HAR_H_25, HAR_H_50, HAR_H_75,
                     HAR_MH_25, HAR_MH_50, HAR_MH_75,
                     nrow=2, ncol=3)



# Calculate average home value for back-of-envelope analysis
HomeValue <- read.csv(paste(datadir,"County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv"))

HomeValue <- HomeValue %>% 
  filter(State %in% c("CA", "NV")) %>% 
  filter(RegionName %in% c("El Dorado County", "Placer County", "Washoe County", "Carson City", "Douglas County")) %>% 
  dplyr::select(-X1996.01.31:-X2018.12.31) %>% 
  gather(TimePeriod, Value, X2019.01.31:X2020.10.31 ) %>% 
  mutate(Year = str_sub(TimePeriod, 2, -7))

HomeValue %>% 
  group_by(RegionName, Year) %>% 
  summarize(avgVal = mean(Value, na.rm=TRUE))

AvgHomeValue <- 507000 

CE_Analysis <- data.frame(scenario=rep(c("S1","S2","S3","S4","S5"),3),
                          destruction_rate = c(rep(.1,5), rep(0.5,5), rep(0.9,5)),
                          TotalProperty = rep(c(13235, 10982, 6055, 10568, 2622),3),
                          TreatedArea = rep(c(0,1300,5000,3050,7850),3))

CE_Analysis <- CE_Analysis %>% 
  mutate(ValueAtRisk = AvgHomeValue * TotalProperty * destruction_rate /1e6) %>% 
  mutate(RefCell = c(rep(556.7874, 5), rep(2783.9370, 5), rep(5011.0866,5))) %>% 
  mutate(RelativeBenefit = RefCell - ValueAtRisk) %>% 
  mutate(BenefitPerAcre = RelativeBenefit * 1e6 / (TreatedArea*30))


CE_Analysis %>% 
  filter(scenario %in% c("S3","S4","S5")) %>% 
  ggplot() +
  geom_bar(aes(x=scenario, y=BenefitPerAcre, fill=as.factor(destruction_rate)), position="dodge", stat="identity") +
  xlab("") + ylab("Relative Benefit ($/acre)") +
  scale_fill_manual(name = "Destruction Rate",
                    labels = c("10%", "50%", "90%"),
                    values=c("#D55E00",  "#009E73", "#E69F00")) + 
  theme_classic() +
  theme(text=element_text(size=16,  family="Calibri")) 

CE_Analysis %>% 
  filter(scenario %in% c("S3","S4","S5")) %>% 
  dplyr::select(scenario, destruction_rate, BenefitPerAcre)
