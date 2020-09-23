
  
  # 1.3.1 Maps
  

  
  #1.4 Submarkets
  
# As a map
    
ggplot(allTracts.threeMarkets)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = Submarket))+
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Three Submarkets As Tracts") +
  mapTheme()

# As a facetted plot

st_drop_geometry(allTracts.threeMarkets) %>%
  group_by(year, Submarket) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T)) %>%
  gather(Variable, Value, -year, -Submarket) %>%
  ggplot(aes(year, Value, fill = Submarket)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("orange", "green", "blue", "black"))+
  labs(title = "Indicator differences across submarkets") +
  plotTheme() + theme(legend.position="bottom")
