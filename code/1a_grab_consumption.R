#### [0] prep ####
library(tidyverse)
library(countrycode)

# define the PICTs to filter for
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

#### [1] pull consumption data ####
# define the path to your data folder and the file pattern to match
data_path <- "data/consumption/"
file_pattern <- "consumption_midpoint_HS07_\\d{4}\\.csv"  

# list all files in the folder that match the pattern for all years
file_list <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)


# define a function to read and filter each file
load_and_filter <- function(file) {
  read_csv(file) %>%
    filter(
      # consumer_iso3c %in% picts |
      #   exporter_iso3c %in% picts |
      #   source_country_iso3c %in% picts,
      habitat == "marine",
      method == "capture"
    ) %>%
    mutate(
      source_country_short = if_else(
        str_length(countrycode(source_country_iso3c, "iso3c", "country.name")) > 6, 
        source_country_iso3c, 
        countrycode(source_country_iso3c, "iso3c", "country.name")
      ),
      exporter_country_short = if_else(
        str_length(countrycode(exporter_iso3c, "iso3c", "country.name")) > 6, 
        exporter_iso3c, 
        countrycode(exporter_iso3c, "iso3c", "country.name")
      ),
      consumer_country_short = if_else(
        str_length(countrycode(consumer_iso3c, "iso3c", "country.name")) > 6, 
        consumer_iso3c, 
        countrycode(consumer_iso3c, "iso3c", "country.name")
      ),
      source_country = countrycode(source_country_iso3c, "iso3c", "country.name")
      ,
      exporter_country = countrycode(exporter_iso3c, "iso3c", "country.name")
      ,
      consumer_country = countrycode(consumer_iso3c, "iso3c", "country.name")

    )
}

# combine into a single dataframe
combined_data <- bind_rows(lapply(file_list, load_and_filter))

## save it - annual
# write.csv(combined_data,paste0("output/consumption_annual_",unique(combined_data$hs_version), ".csv"),row.names = FALSE)

## save it - years aggregated
consumption_allyears <- group_by(combined_data,
                                 source_country_iso3c,exporter_iso3c,consumer_iso3c) %>%
  summarise(total_weight = sum(consumption_live_t))
# write.csv(consumption_allyears,paste0("output/consumption_allyears_",unique(combined_data$hs_version),".csv"),row.names = FALSE)

