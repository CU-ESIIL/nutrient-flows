#### goal: make edge lists for network analysis + viz ####
#### [0] prep ####
library(tidyverse)
library(countrycode)

nutrients_cons_og <- read_csv("output/consumption_annual_HS07_nutrients.csv")
# nutrients_cons_og <- read_csv("output/consumption_annual_global_HS07_nutrients.csv")
rni_og <- read_csv("output/dietrec_df.csv")
rni_og <- mutate(rni_og,category=str_to_title(category))

# establish filter variable
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

#### make pics/non-pics edge lists ####
# Add PICS/non-PICS IDs; NA exporter for domestic consumption
net_class <- nutrients_cons_og %>%
  filter(if_all(c(exporter_iso3c, source_country_iso3c, consumer_iso3c), ~ . != "NEI" | is.na(.))) %>% 
  # rename the iso3c columns
  rename(
    SourceCountry = source_country_iso3c,
    ExporterCountry = exporter_iso3c,
    ConsumerCountry = consumer_iso3c
  ) %>%
  # add domestic for countries too
  mutate(
    ExporterCountry = ifelse(consumption_source == "domestic", 
                             SourceCountry, 
                             ExporterCountry)
    ) %>%
  # add class columns
  mutate(
    SourceClass = ifelse(SourceCountry %in% picts, "PICs", "Non-\nPICs"),
    ExporterClass = ifelse(is.na(ExporterCountry), "Domestic", ifelse(ExporterCountry %in% picts, "PICs", "Non-\nPICs")),
    ConsumerClass = ifelse(ConsumerCountry %in% picts, "PICs", "Non-\nPICs")
  ) %>%
  # add continent columns, replacing continent with "PICs" for PICs countries
  mutate(
    SourceContinent = ifelse(SourceCountry %in% picts, "PICs", countrycode(SourceCountry, "iso3c", "continent")),
    ExporterContinent = ifelse(consumption_source == "domestic", "Domestic PICs", countrycode(ExporterCountry, "iso3c", "continent")),
    ConsumerContinent = ifelse(ConsumerCountry %in% picts, "PICs", countrycode(ConsumerCountry, "iso3c", "continent"))
  ) 

# List of nutrient columns
nutrients <- c("protein_total_g","fattyacids_total_g","calcium_total_mg",
               "zinc_total_mg","iron_total_mg","vitamina_total_mcg","vitaminb12_total_mcg")

# Aggregate data across all years for each nutrient
net_agg <- net_class %>%
  # filter(!(SourceClass == "PICs" & ExporterClass == "PICs" & ConsumerClass == "PICs")) %>%
  # group_by(SourceClass, ExporterClass, ConsumerClass) %>%
  group_by(SourceContinent, ExporterContinent, ConsumerContinent) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_total", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient)) %>%
  separate_wider_delim(Nutrient,delim="_",names=c("Nutrient","Unit")) 
# normalize by RNI
net_agg <- left_join(net_agg,rni_og,
                  by = c("Nutrient" = "category", "Unit" = "unit")) %>%
  mutate(rni_amount = Value / rec_intake_adults)


# write.csv(net_agg,"output/edge_consumption_pics_nutrients.csv",row.names = FALSE)
# write.csv(net_agg,"output/edge_consumption_global_nutrients.csv",row.names = FALSE)

  
# Aggregate data *annually* for each nutrient
net_agg_annual <- net_class %>%
  # filter(!(SourceClass == "PICs" & ExporterClass == "PICs" & ConsumerClass == "PICs")) %>%
  # group_by(SourceClass, ExporterClass, ConsumerClass,year) %>%
  group_by(SourceContinent, ExporterContinent, ConsumerContinent,year) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_total", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient)) %>%
  separate_wider_delim(Nutrient,delim="_",names=c("Nutrient","Unit"))
# normalize by RNI
net_agg_annual <- left_join(net_agg_annual,rni_og,
                     by = c("Nutrient" = "category", "Unit" = "unit")) %>%
  mutate(rni_amount = Value / rec_intake_adults)

# write.csv(net_agg_annual,"output/edge_consumption_pics_nutrients_annual.csv",row.names = FALSE)
# write.csv(net_agg_annual,"output/edge_consumption_global_nutrients_annual.csv",row.names = FALSE)


#### make country-level edge lists ####
# Aggregate data across all years for each nutrient
net_agg_countries <- net_class %>%
  # filter(!(SourceClass == "PICs" & ExporterClass == "PICs" & ConsumerClass == "PICs")) %>%
  group_by(SourceCountry, ExporterCountry, ConsumerCountry) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_total", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient)) %>%
  separate_wider_delim(Nutrient,delim="_",names=c("Nutrient","Unit")) 
# normalize by RNI
net_agg_countries <- left_join(net_agg_countries,rni_og,
                     by = c("Nutrient" = "category", "Unit" = "unit")) %>%
  mutate(rni_amount = Value / rec_intake_adults)

# save output (~edge list)
# write.csv(net_agg_countries,"output/edge_consumption_countries_nutrients.csv",row.names = FALSE)
# write.csv(net_agg_countries,"output/edge_consumption_countries_global_nutrients.csv",row.names = FALSE)


#### Aggregate data *annually* for each nutrient ####
net_agg_countries_annual <- net_class %>%
  # filter(!(SourceClass == "PICs" & ExporterClass == "PICs" & ConsumerClass == "PICs")) %>%
  group_by(SourceCountry, ExporterCountry, ConsumerCountry,year) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_total", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient)) %>%
  separate_wider_delim(Nutrient,delim="_",names=c("Nutrient","Unit")) 
# normalize by RNI
net_agg_countries_annual <- left_join(net_agg_countries_annual,rni_og,
                               by = c("Nutrient" = "category", "Unit" = "unit")) %>%
  mutate(rni_amount = Value / rec_intake_adults)

# save output (~edge list)
# write.csv(net_agg_countries_annual,"output/edge_consumption_countries_nutrients_annual.csv",row.names = FALSE)
# write.csv(net_agg_countries_annual,"output/edge_consumption_countries_global_nutrients_annual.csv",row.names = FALSE)





