##### summary: get PICT population data (PDH), other country data (source) 
#### [0] prep ####
library(rsdmx) # for pacific island countries
library(countrycode)
library(tidyverse)
library(WDI) # for world bank

# load nutrient data for a full country list
combined_data <- read_csv("output/consumption_annual_global_HS07_nutrients.csv")
full_country_list <- unique(c(combined_data$source_country_iso3c,combined_data$exporter_iso3c,combined_data$consumer_iso3c))

#### [1] pull data from Pacific Data Hub API ####
url <- "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A..MIDYEARPOPEST._T._T?startPeriod=1990&endPeriod=2027"

picts_pop_df <- readSDMX(url)
picts_df <- as.data.frame(picts_pop_df)

#### [2] clean Pacific Data Hub data ####
picts_clean <- picts_df %>% 
  select(Country_iso=GEO_PICT,Year=obsTime,Pop=obsValue) %>%
  mutate(Country_iso=countrycode(Country_iso,origin="iso2c",destination="iso3c"),
         Country=countrycode(Country_iso,origin="iso3c",destination="country.name")) %>%
  filter(!is.na(Country)) %>% arrange(Country,Year) %>%
  mutate(Year=as.numeric(Year))
picts_names <- unique(picts_clean$Country_iso)

#### [3] pull all other data from world bank ####
wb_df <- WDI(
  country = "all",               
  indicator = "SP.POP.TOTL",     
  start = 1990,                  
  end = 2023                     
)


#### [4] clean world bank data ####
wb_clean <- wb_df %>% 
  filter(!iso3c %in% picts_names) %>% # remove the PICTs (already grabbed from Pacific Data Hub)
  filter(iso3c %in% full_country_list) %>% # keep the rest from the nutrient list
  select(Country=country,Country_iso=iso3c,Year=year,Pop=SP.POP.TOTL)


#### [5] combine all population data ####
pop_df <- bind_rows(picts_clean,wb_clean)

# check the country inclusion - what's missing from the final country list
country_check <- unique(pop_df$Country_iso)
a<-setdiff(full_country_list,country_check)

### note: missing Taiwan - might want to check other data source to fill that in ##


#### [6] save output ####
# write.csv(picts_clean,"output/pop_picts.csv",row.names = FALSE)
# write.csv(pop_df, "output/pop_full.csv",row.names=FALSE)

## next: head to 'prep_consumption.R' for per capita nutrient calculation
  
  
  
