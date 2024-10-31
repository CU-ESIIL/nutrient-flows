#### [0] prep ####
library(tidyverse)

#### [1] add population data for nutrients per capita ####
pop_df <- read_csv("output/pop_full.csv")
nutrients_cons_og <- read_csv("output/consumption_annual_HS07_nutrients.csv")
# nutrients_cons_og <- read_csv("output/consumption_annual_global_HS07_nutrients.csv")
rni_og <- read_csv("output/dietrec_df.csv")

## aggregate average across years (2007 and on because of HS07 choice)
pop_df_agg <- pop_df %>% 
  filter(Year > 2006) %>%
  group_by(Country_iso) %>% summarize(Pop_avg = mean(Pop))
valid_countries <- unique(pop_df_agg$Country_iso)

## calculate per capita nutrients
percap_df <- nutrients_cons_og %>%
  select(
    source_country_iso3c, exporter_iso3c,consumer_iso3c,dom_source,consumption_source,
    protein_total_g, fattyacids_total_g, calcium_total_mg,
    zinc_total_mg, iron_total_mg, vitamina_total_mcg, vitaminb12_total_mcg
  ) %>%
  # join with source country population
  left_join(pop_df_agg, by = c("source_country_iso3c" = "Country_iso"), relationship = "many-to-many") %>%
  rename(Pop_source = Pop_avg) %>%  # rename to Pop_source
  
  # join with exporter country population
  left_join(pop_df_agg, by = c("exporter_iso3c" = "Country_iso"), relationship = "many-to-many") %>%
  rename(Pop_exporter = Pop_avg) %>%  # rename to Pop_exporter
  
  # join with consumer country population
  left_join(pop_df_agg, by = c("consumer_iso3c" = "Country_iso"), relationship = "many-to-many") %>%
  rename(Pop_consumer = Pop_avg) %>%  # rename to Pop_exporter
  
  ## drop NA for now, since only PICTs are included and it's leaving lots of NA's until other demographic data collected
  filter(
    (is.na(source_country_iso3c) | source_country_iso3c %in% valid_countries),
    (is.na(exporter_iso3c) | exporter_iso3c %in% valid_countries),
    (is.na(consumer_iso3c) | consumer_iso3c %in% valid_countries)
  ) %>%
  
  # relocate columns
  relocate(Pop_source, Pop_exporter, Pop_consumer, .before = 4)


#### [2] calculate net flows #### 
#### [step 1] from source to consumer - FOREIGN TRADE ####
# grab foreign trade
foreign_flow <- percap_df %>% filter(consumption_source=="foreign")
# grab source (-) and nutrients; make flow negative
out_flow <- foreign_flow %>%
  select(Country=source_country_iso3c,contains("total_")) %>%
  mutate(Role="Source") %>% relocate(Role,.before=2) %>%
  group_by(Country) %>% summarize(across(contains("total_"),sum))
# grab consumer (+) and nutrients; keep flow positive
in_flow <- foreign_flow %>%
  select(Country=consumer_iso3c,contains("total_")) %>%
  mutate(Role="Consumer") %>% relocate(Role,.before=2) %>%
  group_by(Country) %>% summarize(across(contains("total_"),sum))
# calculate net
foreign_net_flow <- out_flow %>%
  full_join(in_flow, by = "Country", suffix = c("_out", "_in")) %>%
  # replace NA with 0
  replace_na(list(
    protein_total_g_in = 0,
    fattyacids_total_g_in = 0,
    calcium_total_mg_in = 0,
    zinc_total_mg_in = 0,
    iron_total_mg_in = 0,
    vitamina_total_mcg_in = 0,
    vitaminb12_total_mcg_in = 0
  )) %>%
  mutate(
    protein_net_flow = protein_total_g_in - protein_total_g_out,
    fattyacids_net_flow = fattyacids_total_g_in - fattyacids_total_g_out,
    calcium_net_flow = calcium_total_mg_in - calcium_total_mg_out,
    zinc_net_flow = zinc_total_mg_in - zinc_total_mg_out,
    iron_net_flow = iron_total_mg_in - iron_total_mg_out,
    vitamina_net_flow = vitamina_total_mcg_in - vitamina_total_mcg_out,
    vitaminb12_net_flow = vitaminb12_total_mcg_in - vitaminb12_total_mcg_out
  ) %>%
  select(Country, contains("net_flow")) 




#### [step 2] calculate DOMESTIC CONSUMPTION (source=consumer) ####
# grab domestic flow
domestic_flow <- percap_df %>% filter(consumption_source=="domestic")
# calculate total nutrients from domestic consumption
dom_consump <- domestic_flow %>%
  select(Country = source_country_iso3c, contains("total_")) %>%
  mutate(Role = "Source") %>%
  relocate(Role, .before = 2) %>%
  group_by(Country) %>%
  summarize(across(contains("total_"), sum)) %>%
  rename_with(~ gsub("_total.*", "_net_flow", .), contains("total_"))


#### [2] combine foreign + domestic flows and convert to per capita ####
#### [step 1] calculate foreign and domestic net flows ####
# foreign flow (calculate net flows)
foreign_flow <- foreign_net_flow %>%
  left_join(pop_df_agg, by = c("Country" = "Country_iso")) %>%
  mutate(Source = "Foreign")

# domestic consumption flow (calculate net flows)
dom_consump <- dom_consump %>%
  left_join(pop_df_agg, by = c("Country" = "Country_iso")) %>%
  mutate(Source = "Domestic")

#### [step 2] combine the flows ####
# combine the domestic and foreign flows
combined_df <- bind_rows(dom_consump, foreign_flow)

#### [step 3] calculate net flows ####
# summarize net flows across domestic and foreign for each country
net_df <- combined_df %>%
  group_by(Country) %>%
  summarize(across(contains("_net_flow"), sum, na.rm = TRUE),
            Pop_avg = first(Pop_avg)) %>%  # keep population constant
  mutate(Source = "Net")  # assign "Net" as the source

#### [step 4] calculate per capita flows ####
# calculate per capita flows for foreign, domestic, and net flows
final_pc <- combined_df %>%
  bind_rows(net_df) %>%  # combine foreign, domestic, and net flows
  mutate(across(contains("_net_flow"), 
                .names = "{.col}_pc",  # create new columns for per capita flows
                ~ . / Pop_avg))  # divide net flows by population


#### [step 5] adjust nutrient flows to DAILY values (right now aggregated 2007-2020) #### 
# calculate number of days between 2007 and 2020
total_days <- (2020 - 2007 + 1) * 365 + 3  # +3 for leap years: 2008, 2012, 2016

# Adjust net flows per capita to daily values
adjusted_net_flows <- final_pc %>%
  mutate(across(contains("_net_flow_pc"), ~ . / total_days))


#### [3] compare to RNI and calculate nutrient metrics ####
#### [step 1] calculate the nutrient fulfillment ratios ####
# filter for source = "net" and reshape the data for nutrient comparison
net_flow_df <- adjusted_net_flows %>%
  filter(Source == "Net") %>%
  select(Country, contains("_net_flow_pc"))  # select only the per capita net flow columns

# pivot the dataframe to long format
net_flow_long <- net_flow_df %>%
  pivot_longer(cols = contains("_net_flow_pc"), 
               names_to = "category", 
               values_to = "net_flow_pc") %>%
  mutate(category = case_when(
    category == "protein_net_flow_pc" ~ "protein",
    category == "fattyacids_net_flow_pc" ~ "fattyacids",
    category == "calcium_net_flow_pc" ~ "calcium",
    category == "zinc_net_flow_pc" ~ "zinc",
    category == "iron_net_flow_pc" ~ "iron",
    category == "vitamina_net_flow_pc" ~ "vitamina",
    category == "vitaminb12_net_flow_pc" ~ "vitaminb12",
    TRUE ~ category
  ))

# join the net flows with the rni_og dataframe to calculate fulfillment ratios
comparison_df <- net_flow_long %>%
  left_join(rni_og, by = "category") %>%
  mutate(rni_ratio = net_flow_pc / rec_intake_adults)  %>% # calculate the ratio of net flow to rni
  group_by(category) %>%
  mutate(rni_ratio_zscore = (rni_ratio - mean(rni_ratio, na.rm = TRUE)) / sd(rni_ratio, na.rm = TRUE)) %>%
  ungroup()

# calculate the best nutrients
nutrient_stats <- comparison_df %>%
  group_by(category) %>%
  summarize(Nutrient_fulfillment=mean(rni_ratio,na.rm=TRUE))

# calculate average rni z-score
rni_zscore_df <- group_by(comparison_df,Country) %>% 
  summarise(rni_avg_zscore = mean(rni_ratio_zscore))

#### [step 2] calculate the average nutrient fulfillment ratio ####
# calculate the average fulfillment ratio for each country
avg_fulfillment <- comparison_df %>%
  group_by(Country) %>%
  summarize(average_fulfillment_ratio = mean(rni_ratio, na.rm = TRUE))

#### [step 3] calculate the minimum fulfillment ratio (nutrient shortfall metric) ####
# identify the minimum fulfillment ratio (biggest nutritional gap) for each country
min_fulfillment <- comparison_df %>%
  group_by(Country) %>%
  summarize(min_fulfillment_ratio = min(rni_ratio, na.rm = TRUE))

#### [step 4] calculate the nutrient deficiency score ####
# define the threshold (e.g., 50% of the rni)
threshold <- 0.5  # you can change this to 0.75 for a different threshold

# count how many nutrients are below the threshold for each country
deficiency_score <- comparison_df %>%
  group_by(Country) %>%
  summarize(nutrient_deficiency_count = sum(rni_ratio < threshold, na.rm = TRUE))

#### [step 5] combine all results ####
# combine the results into one dataframe
final_summary <- avg_fulfillment %>%
  left_join(min_fulfillment, by = "Country") %>%
  left_join(deficiency_score, by = "Country") %>%
  left_join(rni_zscore_df, by = "Country")

#### [4] calculate % of country's national RNI needs fulfilled ####
# [step 1] join the net flows with rni values
net_df_long <- net_df %>%
  pivot_longer(cols = ends_with("net_flow"), names_to = "category", values_to = "net_flow_value") %>% 
  mutate(category = str_remove(category, "_net_flow"),
         net_flow_value_daily = net_flow_value / total_days) %>% 
  left_join(rni_og, by = "category")

# [step 2] calculate total rni's from net flows, then percent of total population fulfilled
net_rni_perc_df <- net_df_long %>%
  mutate(net_flow_rnis_total = net_flow_value / rec_intake_adults,
         rni_perc_pop_total = net_flow_rnis_total / Pop_avg,
         net_flow_rnis_daily = net_flow_value_daily / rec_intake_adults,
         rni_perc_pop_daily = net_flow_rnis_daily / Pop_avg)

## save summary df's
# write.csv(nutrient_stats,"output/nutrient_stats_all.csv",row.names = FALSE)
# write.csv(final_summary,"output/nutrient_metrics_all.csv",row.names = FALSE)
# write.csv(comparison_df,"output/nutrient_percapita_all.csv",row.names = FALSE)
# write.csv(net_rni_perc_df,"output/nutrient_rni_percpop_global.csv",row.names = FALSE)
## note: _all file ending is picts-centric, _global file ending is all data





## next: move to 5a_plot_nutrients_percapita.R

