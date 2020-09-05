# Chapter 1
# 1.2 Setup


library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf") 


mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}


# define functions to create quintile maps
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")


# 1.2.1 Downloading and wrangling Census data
View(load_variables(2000,'sf3',cache = TRUE))

# Install A CENSUS API Key
census_api_key("a7edab3d7c3df571998caab5a3cc12a4ec8d8b61" , install = TRUE, overwrite  = TRUE)
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")

# request 2000 Census data
tracts00 <-  
  get_decennial(geography = "tract", variables = c("P001001","P006002","PCT025050",
                                                   "PCT025009","P053001","H056001",
                                                   "P092001"), 
                year=2000, state=42, county=101, geometry=T) %>% 
  st_transform('ESRI:102728')

# To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.??

class(tracts00)

# filter total population variable
totalPop00 <-
  tracts00 %>%
  filter(variable == "P001001")

plot(totalPop00)

# create four maps with different settings
HideA <- 
  ggplot() +
  geom_sf(data = totalPop00, aes(fill = value)) +
  theme(plot.title = element_text(size=22))

B <- 
  ggplot() +
  geom_sf(data = totalPop00, aes(fill = q5(value))) +
  theme(plot.title = element_text(size=22))

C <-
  ggplot() +
  geom_sf(data = totalPop00, aes(fill = q5(value))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop00, "value"),
                    name = "Total\nPopluation\n(Quintile Breaks)") +
  theme(plot.title = element_text(size=22))

D <- 
  ggplot() +
  geom_sf(data = totalPop00, aes(fill = q5(value))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop00, "value"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Philadelphia; 2000") +
  mapTheme() + theme(plot.title = element_text(size=22))

# transfer data frame from long data to wide data
tracts00 <- 
  dplyr::select(tracts00, -NAME) %>%
  spread(variable, value) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = P001001, Whites = P006002, MaleBachelors = PCT025009,
         FemaleBachelors = PCT025050, MedHHInc = P053001, MedRent = H056001,
         TotalPoverty = P092001) 

st_drop_geometry(tracts00)[1:3,]

# create new variables
tracts00 <- 
  tracts00 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2000") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)

# request 2017 Census data
tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state=42, county=101, geometry=T, output="wide") %>%
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, Whites = B02001_002E,
         FemaleBachelors = B15001_050E, MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

# The last step then is to combine the 2000 and 2017 tracts together, 
# stacking both layers atop one another with rbind. 
# allTracts is now a complete time/space dataset
allTracts <- rbind(tracts00,tracts17)



##############################################
#     1.2.2 Wrangling transit open data      #
##############################################


septaStops <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson") %>% 
      mutate(Line = "El") %>%
      select(Station, Line),
    st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson") %>%
      mutate(Line ="Broad_St") %>%
      select(Station, Line)) %>%
  st_transform(st_crs(tracts00))  

ggplot() + 
  geom_sf(data=st_union(tracts00)) +
  geom_sf(data=septaStops, aes(colour = Line), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("orange","blue")) +
  labs(title="Septa Stops", subtitle="Philadelphia, PA", caption="Figure 2.5") +
  mapTheme()

########################################################
#    1.2.3 Relating tracts & subway stops in space     #
########################################################
# Several overlay techniques are demonstrated below to find tracts close to subway stations

# Approach 1: only can be applied to long data
septaBuffers <- 
  rbind(
    st_buffer(septaStops, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(septaStops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

ggplot() +
  geom_sf(data=septaBuffers) +
  geom_sf(data=septaStops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# three different approaches for selecting tracts that are with 0.5 miles of subway stations are considered.
buffer <- filter(septaBuffers, Legend=="Unioned Buffer")

#  `Clip` is the worst choice as changing the geometries exasperates ecological fallacy and MAUP bias. 
clip <- 
  st_intersection(buffer, tracts00) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Spatial Selection is better, but a tract is included even if only sliver of its area touches the buffer. TOO MANY!
selection <- 
  tracts00[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Select by Centroids is my choice because it captures a concise set of tracts without altering geometries.
selectCentroids <-
  st_centroid(tracts00)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts00, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")


##############################################
#        1.3 Developing TOD Indicators       #
##############################################

##############################################
#         1.3.1 TOD indicator maps           #
##############################################

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2000", MedRent * 1.42, MedRent)) 

##############################################
#         1.3.2 TOD indicator tables         #
##############################################
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

# use `kable` to create table
kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# change table format
allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

##############################################
#         1.3.3 TOD indicator plots          #
##############################################

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")


####################################################
#    1.4 Capturing three submarkets of interest    #
####################################################

centerCity <-
  st_intersection(
    st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union(),
    st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "Center City")

el <-
  st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "El")

broad.st <-
  st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "Broad Street")

threeMarkets <- rbind(el, broad.st, centerCity) 

allTracts.threeMarkets <-
  st_join(st_centroid(allTracts), threeMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 

