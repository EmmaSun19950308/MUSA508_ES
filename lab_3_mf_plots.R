
# UGB and towns
ggplot()+
  geom_sf(data = studyAreaTowns, fill = "transparent", color = "grey", show.legend = FALSE)+
  geom_sf(data = buffersAndTowns %>%
            filter(Legend == "Inside"), aes(fill = MUNI))+
  geom_sf(data = buffersAndTowns %>%
            filter(Legend == "Outside"), aes(fill = MUNI), alpha = 0.7)+
  geom_sf_text(data = studyAreaTowns %>%
                 group_by(MUNI) %>% 
                 slice (1), aes(label = MUNI), size = 3)+
  labs(title = "Areas Within 1/2 mi of the UGB, by municipality",
       subtitle = "Lighter colored sections are within UGB") +
  mapTheme()


# Visualize them by population
ggplot()+
  geom_sf(data = insideTracts, aes(fill = popDens), color = "transparent")+
  geom_sf(data = studyAreaTowns, fill = "transparent", color = "grey")+
  geom_sf_text(data = studyAreaTowns %>%
                 group_by(MUNI) %>% 
                 slice (1), aes(label = MUNI), 
               color = "yellow",size = 3)+
  geom_sf(data = st_union(studyAreaTowns), fill = "transparent", color = "red")+
  mapTheme()
