#### [0] prep ####
library(tidyverse)
library(tidytext)

edge_df_og <- read_csv("output/edge_consumption_countries_nutrients.csv")
edge_df_pics <- read_csv("output/edge_consumption_pics_nutrients.csv")


#### [1] look at sources/exporters to PICs - continents ####
# [step 1] Filter for exports where ConsumerContinent is PICs
exports_to_pics <- edge_df_pics %>%
  filter(ConsumerContinent == "PICs")

# [step 2] Aggregate by ExporterContinent and Nutrient to find total exports to PICs
exporter_summary <- exports_to_pics %>%
  group_by(ExporterContinent, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Continent=ExporterContinent) %>%
  mutate(Role="Exporter",
         Continent = str_replace(Continent, "Domestic PICs", "PICs"))
source_summary <- exports_to_pics %>%
  group_by(SourceContinent, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Continent=SourceContinent) %>%
  mutate(Role="Source")
role_df <- bind_rows(exporter_summary,source_summary)

# [step 3] Rank exporters by total contributions to PICs
role_ranked <- role_df %>%
  mutate(Role = factor(Role, levels = c("Source", "Exporter"))) %>%
  group_by(Continent,Role) %>%
  mutate(RNIsum = sum(TotalRNIAmount)) %>% ungroup() %>%
  group_by(Role) %>%
  arrange(desc(TotalRNIAmount))

# plot
ggplot(role_ranked, 
       aes(x = reorder_within(Continent, -TotalRNIAmount,Role), y = TotalRNIAmount, fill = Nutrient)) +
  facet_wrap(~Role,scales = "free")+
  geom_bar(stat = "identity") +
  scale_x_reordered()+
  labs(x = NULL,
       y = "Total RNIs Exported", 
       title = "Roles Facilitating Nutrients to PICs") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(size = 16, hjust = 0.5),  # center the title
        plot.subtitle = element_text(size = 12, hjust = 0.5),  # subtitle under title
        strip.text = element_text(size = 12, face = "bold"))  # increase facet title size



#### [2] look at sources/exporters to PICs - countries ####
# [step 1] Filter for exports where ConsumerCountry is a PIC
exports_to_pics_countries <- edge_df_og %>%
  filter(ConsumerCountry %in% picts)

# [step 2] Aggregate by ExporterCountry and Nutrient to find total exports to PICs
exporter_summary_countries <- exports_to_pics_countries %>%
  group_by(ExporterCountry, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Country=ExporterCountry) %>%
  mutate(Role="Exporter")
source_summary_countries <- exports_to_pics_countries %>%
  group_by(SourceCountry, Nutrient) %>%
  summarise(
    TotalValue = sum(Value, na.rm = TRUE),          # sum of export value
    TotalRNIAmount = sum(rni_amount, na.rm = TRUE)  # sum of RNI contribution
  ) %>%
  ungroup() %>%
  rename(Country=SourceCountry) %>%
  mutate(Role="Source")
role_df_countries <- bind_rows(exporter_summary_countries,source_summary_countries)

# [step 3] Rank exporters by total contributions to PICs
role_ranked_countries <- role_df_countries %>%
  mutate(Role = factor(Role, levels = c("Source", "Exporter"))) %>%
  group_by(Country,Role) %>%
  mutate(RNIsum = sum(TotalRNIAmount)) %>% ungroup() %>%
  group_by(Role) %>%  # group by Role to apply slice within each group
  arrange(desc(RNIsum)) %>%
  slice_head(n = 50) %>%  # take top 50 per Role
  ungroup()

# plot
ggplot(role_ranked_countries, 
       aes(x = reorder_within(Country, -TotalRNIAmount,Role), y = TotalRNIAmount, fill = Nutrient)) +
  facet_wrap(~Role,scales = "free")+
  geom_bar(stat = "identity") +
  scale_x_reordered()+
  labs(x = NULL,
       y = "Total RNIs", 
       title = "Countries Facilitating Nutrients to Consuming PICs") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(size = 16, hjust = 0.5),  # center the title
        plot.subtitle = element_text(size = 12, hjust = 0.5),  # subtitle under title
        strip.text = element_text(size = 12, face = "bold"))  # increase facet title size



