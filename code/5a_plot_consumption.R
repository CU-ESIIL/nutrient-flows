#### [0] prep ####
library(tidyverse)
library(rfishbase)
library(RColorBrewer)

## load data
# consumption_og <- read_csv("~/data-store/nutrient-flows/output/consumption_annual_HS07.csv")
# nutrients_cons_og <- read_csv("~/data-store/nutrient-flows/output/consumption_annual_HS07_nutrients.csv")
consumption_og <- read_csv("output/consumption_annual_global_HS07.csv")
nutrients_cons_og <- read_csv("output/consumption_annual_global_HS07_nutrients.csv")

picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

#### [1] explore ####
colSums(is.na(nutrients_cons_og)) # check where NA's are at
# why does the exporter iso3c have so many NA's?

table(nutrients_cons_og$consumption_source)
table(nutrients_cons_og$dom_source)
# Q: waht is dom source vs. consumption source?

## seems like - when consumption source = domestic, source = consumer; aka no exporting & local consumption
## and when - when dom source = domestic, source = exporter; aka no local production & foreign consumption (also consumption source = foreign, so confirmed)

#### [2] plot ####
## what are the common names of species?
scientific_names <- sort(unique(nutrients_cons_og$sciname))
scientific_names <- str_to_sentence(scientific_names)
species_info <- species(scientific_names,fields = c("SpecCode", "Genus", "Species", "FBname")) # use fishbase for common names

## what is the most common species?
# summarize and count the occurrences/tonnes of each species in sciname
species_counts <- nutrients_cons_og %>%
  group_by(sciname) %>%
  summarize(count = n(), consumption_live_t=sum(consumption_live_t),
            .groups = 'drop') %>%
  arrange(desc(count)) 

# prepare species_info with lowercase sciname for joining
species_info <- species(str_to_sentence(unique(nutrients_cons_og$sciname)), fields = c("Genus", "Species", "FBname")) %>%
  mutate(sciname = str_to_lower(paste(Species))) %>%  # create lowercase sciname
  select(sciname, CommonName = FBname)  # select relevant columns

# join species counts with common names
species_combined <- species_counts %>%
  mutate(sciname = str_to_lower(sciname)) %>%  # ensure sciname is lowercase
  left_join(species_info, by = "sciname") %>%
  slice_max(order_by = count, n = 20) %>%
  mutate(label_name = ifelse(!is.na(CommonName), paste(str_to_sentence(sciname), " / ", CommonName), str_to_sentence(sciname)))

#### plot the bar chart for the top species - counts ####
ggplot(species_combined, aes(x = reorder(label_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip coordinates for better readability
  labs(title = "Most Traded Species (instances)",
       x = "Species",
       y = "Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/species_counts.jpg")

#### plot the bar chart for the top species - tons ####
ggplot(species_combined, aes(x = reorder(label_name, consumption_live_t), y = consumption_live_t)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip coordinates for better readability
  labs(title = "Most Traded Species (volume)",
       x = "Species",
       y = "Tonnes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/species_weight.jpg")

#### what roles are countries playing? ####
# pivot longer to get roles in one column
country_role_counts <- nutrients_cons_og %>%
  pivot_longer(cols = c(source_country_short, exporter_country_short, consumer_country_short),
               names_to = "role", values_to = "country") %>%
  filter(!is.na(country)) %>%
  group_by(country, role) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(role) %>%
  mutate(role = recode(role,
                       "source_country_iso3c" = "Source","source_country_short" = "Source",
                       "exporter_iso3c" = "Exporter","exporter_country_short" = "Exporter",
                       "consumer_iso3c" = "Consumer","consumer_country_short" = "Consumer"),
         role = factor(role, levels = c("Source", "Exporter", "Consumer")))

#### plot the facet-wrapped bar plot ####
# first line picks top 30 countries
ggplot(country_role_counts %>% slice_max(order_by = count, n = 30)
       ,aes(x = tidytext::reorder_within(country, count, role), y = count, fill = role)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  coord_flip() +  # flip coordinates for better readability
  facet_wrap(~ role, scales = "free") +  # facet by role
  tidytext::scale_x_reordered() +  # adjust scale_x to reorder within each facet
  labs(title = "Country Roles",
       x = "Country",
       y = "Counts",
       fill = "Role") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/country_roles.jpg")


#### what's domestic consumption look like? ####
domestic_cons <- nutrients_cons_og %>%
  filter(consumption_source == "domestic") %>%
  select(Country = source_country_iso3c, Species = sciname, Quantity_species = consumption_live_t) %>%
  group_by(Country, Species) %>%
  summarize(Quantity_species = sum(Quantity_species), .groups = "drop") %>% # sum per species
  group_by(Country) %>%
  mutate(Quantity_country = sum(Quantity_species), Counts=n(),.groups = "drop") %>% # total sum per country
  ungroup()

# plot counts
ggplot(domestic_cons #%>% filter(!Country=="PNG")
       ,aes(x = reorder(Country, -Counts), y = Counts
            # , fill = Species
            )) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  theme_minimal() +
  labs(y = "Counts", x = NULL,
       fill = "Species", title = "Domestic Consumption") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))

# plot quantity
ggplot(domestic_cons #%>% filter(!Country=="PNG")
       ,aes(x = reorder(Country, -Quantity_country),
            y = Quantity_species, fill = Species)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  theme_minimal() +
  labs(y = "Quantity (tonnes)", x = NULL,
       fill = "Species", title = "Domestic Consumption") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
