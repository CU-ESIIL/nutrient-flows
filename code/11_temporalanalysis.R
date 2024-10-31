#### goal: assess relationships between network merics and relevant variables ####

#### [0] prep ####
library(tidyverse)
library(countrycode)

# load network data
net_met_df <- read_csv("output/networkmetrics_annual_pics.csv")
node_met_df <- read_csv("output/nodemetrics_annual_pics.csv")

# create variables
country_consumption <- read_csv("output/edge_consumption_countries_nutrients.csv")
country_list <- unique(c(country_consumption$SourceCountry,country_consumption$ExporterCountry,country_consumption$SourceCountry))
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

# load variable data - oni, subsidies, sdg's
oni_og <- read_csv("data/oni.csv")
subsidies_og <- read_csv("data/fish_subsidies/Sumaila_dataset.csv")
sdg2_og <- read_csv("data/Goal2.csv")
sdg3_og <- read_csv("data/Goal3.csv")
treaties_og <- readxl::read_xlsx("data/fapda_details-202410301647.xlsx") # from fao https://fapda.apps.fao.org/fapda/#main.html
psma_og <- read_csv("data/psma_parties.csv") # from fao https://www.fao.org/port-state-measures/background/parties-psma/en/


#### [1] process external variables ####
## align at timesteps, calculate oni summary stats
oni_annual <- oni_og %>% 
  rowwise() %>% 
  mutate(Mean_oni = mean(c_across(DJF:NDJ), na.rm = TRUE),
         Var_oni = var(c_across(DJF:NDJ), na.rm=TRUE),
         Median_oni = median(c_across(DJF:NDJ), na.rm=TRUE),
         Min_oni = min(c_across(DJF:NDJ), na.rm = TRUE),
         Max_oni = max(c_across(DJF:NDJ), na.rm = TRUE),
         Range_oni = Max_oni - Min_oni) %>%
  filter(Year >= min(net_met_df$Year) & Year <= max(net_met_df$Year)) %>% select(-c(DJF:NDJ))

## process SDG's
# sdg 2
sdg2.1 <- sdg2_og %>%
  filter(SeriesDescription == "Prevalence of undernourishment (%)" & TimePeriod >= 2000 & TimePeriod <= 2020) %>%
  select(Goal, Target, Indicator, SeriesDescription, GeoAreaName, TimePeriod, Value) %>%
  mutate(COUNTRY = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
         Value = as.numeric(Value)) %>%
  filter(COUNTRY %in% country_list) %>%
  filter(!is.na(Value)) %>%
  mutate(Value = ifelse(Value < 1, 1, ifelse(Value < 2.5, 2.5, Value)),
         Unit="Percentage",SeriesDescription="Prevalence of undernourishment (percentage)") %>% ungroup() %>%
  group_by(COUNTRY,TimePeriod) %>%
  mutate(Continent = ifelse(COUNTRY %in% picts, "PICs", countrycode(COUNTRY, "iso3c", "continent"))) %>% ungroup() %>%
  arrange(COUNTRY, TimePeriod) %>%
  group_by(Continent) %>%
  mutate(Value_continent=mean(Value)) %>% ungroup() %>%
  group_by(Continent,TimePeriod) %>%
  mutate(Value_continent_annual_sdg2=mean(Value)) %>% ungroup() %>%
  rename(Year=TimePeriod,Node=Continent) %>%
  select(Node,Year,Value_continent_annual_sdg2)

# sdg 3
sdg3.1 <- sdg3_og %>% 
  filter(SeriesDescription == "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)", 
         TimePeriod >= 2000 & TimePeriod <= 2020) %>%
  select(Goal, Target, Indicator, SeriesDescription, GeoAreaName, TimePeriod, Value) %>%
  mutate(COUNTRY = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
         Value = as.numeric(Value)) %>%
  filter(COUNTRY %in% country_list) %>%
  filter(!is.na(Value)) %>%
  mutate(Value = ifelse(Value < 1, 1, ifelse(Value < 2.5, 2.5, Value))) %>%
  arrange(COUNTRY, TimePeriod)  %>% # Ensure the data is ordered by COUNTRY and TimePeriod
  group_by(COUNTRY,TimePeriod) %>%
  mutate(Value = mean(Value),
         Unit="Probability",
         SeriesDescription="Non-communicable disease mortality (probability)") %>% distinct() %>%
  mutate(Continent = ifelse(COUNTRY %in% picts, "PICs", countrycode(COUNTRY, "iso3c", "continent"))) %>% ungroup() %>%
  arrange(COUNTRY, TimePeriod) %>%
  group_by(Continent) %>%
  mutate(Value_continent=mean(Value)) %>% ungroup() %>%
  group_by(Continent,TimePeriod) %>%
  mutate(Value_continent_annual_sdg3=mean(Value)) %>% ungroup() %>%
  rename(Year=TimePeriod,Node=Continent) %>%
  select(Node,Year,Value_continent_annual_sdg3) %>% distinct()

# combine all SDGs
sdg_full <- left_join(sdg2.1,sdg3.1,relationship = "many-to-many") %>% distinct()





#### combine all data ####
merged_df_net <- left_join(net_met_df,oni_annual,by="Year")
merged_df_node <- left_join(node_met_df,oni_annual,by="Year") %>%
  left_join(sdg_full,by=c("Year","Node"))


#### [3] analysis time - network-wide metrics vs. other variables ####
## 'splorin
ggplot(merged_df_net,aes(Year,Mean_oni)) + geom_line()
ggplot(merged_df_net,aes(Year,modularity)) + geom_line()


## correlations
cor(net_met_df$modularity,oni_annual$Var)

## linear regression
model <- lm(modularity ~ Mean_oni, data = merged_df_net)
summary(model)

## multiple linear regressions
model <- lm(modularity ~ Mean_oni + Max_oni + Var_oni, data = merged_df_net)
summary(model)



#### [4] analysis time - node-specific metrics vs. other variables ####
## correlation
cor(merged_df_node$strength_diff,merged_df_node$Value_continent_annual_sdg2,use="complete.obs")


## mixed effects## strength_diffmixed effects
library(lme4)
model_mixed <- lmer(strength_diff ~ Mean_oni + (1 | Node) + (1 | Year), data = merged_df_node)
summary(model_mixed)

## fixed effects
library(plm)
model_fixed <- plm(strength_diff ~ Mean_oni, data = merged_df_node, index = c("Node", "Year"), model = "within")
summary(model_fixed)

## random effects
model_random <- plm(strength_diff ~ Value_continent_annual_sdg2, 
                    data = merged_df_node, 
                    index = c("Node", "Year"), model = "random")
summary(model_random)

## GAMs
library(mgcv)
model_gam <- gam(Value_continent_annual_sdg2 ~ s(strength_diff), 
                 data = merged_df_node)
summary(model_gam)
plot(model_gam)
# gam w/ nodes and year
model_gam2 <- gam(Value_continent_annual_sdg2 ~ s(strength_diff) + s(Year) + Node, 
                 data = merged_df_node)
summary(model_gam2)
plot(model_gam2)

model_gam3 <- gam(Value_continent_annual_sdg2 ~ s(strength_diff,Year) + s(Year) + Node, 
                  data = merged_df_node)
summary(model_gam3)
plot(model_gam3)

# Plot the smooth term for strength_diff and Year interaction
plot(model_gam3, select = 1, rug = TRUE, shade = TRUE, main = "Interaction: strength_diff and Year")

# Plot the smooth term for Year
plot(model_gam3, select = 2, rug = TRUE, shade = TRUE, main = "Smooth Effect: Year")

# Plot the smooth term for each Node (if needed)
plot(model_gam3, select = 3, rug = TRUE, shade = TRUE, main = "Node Effects")




