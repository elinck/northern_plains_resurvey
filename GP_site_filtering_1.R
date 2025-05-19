library(tidyverse) # load tidyverse data science libraries

# read in csv of GBIF occurences
data <- read_csv("data/occurences.csv")

# look at "head" of data
head(data)

# identify dimensions: first column is number of rows, second column is number of columns
dim(data)

# another way to look at attributes ("$" shows you what fields you can access)
str(data)

# we can subset columns we are interested in with "select":
data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year)

# however, it's always good to check what values we actually have:
data %>%
  select(stateProvi) %>%
  unique() %>%
  print(n=100) # displays more rows than default

# filtering by year is similar: 
data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year) %>%
  filter(stateProvi %in% c("Montana", "North Dakota", "South Dakota")) %>%
  filter(year < 1950)

# we can save a subset dataframe after applying these filters using "<-" (note different name)
subset_data <- data %>% 
  select(class, species, stateProvi, decimalLat, decimalLon, year) %>%
  filter(stateProvi %in% c("Montana", "North Dakota", "South Dakota")) %>%
  filter(year < 1950)
subset_data

# let's count the number of records (observations) in each taxonomic class class
subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n())

# we can do the same for species with "n_distinct(column)"
subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n_distinct(species))

# let's save this as its own object so we can export it
table <- subset_data %>%
  group_by(class, stateProvi) %>%
  summarise(n = n_distinct(species))
write_csv(table, "data/distinct_species_by_state.csv")

# let's also revisit filtering to produce a csv with only Montana records for plotting in a separate script
# I've updated this to include locality info (05/02/25)
# Only Montana records, pre-1950
montana_data_1950 <- data %>%
  select(class, species, stateProvi, locality, decimalLat, decimalLon, year, institutio) %>%
  filter(stateProvi == "Montana") %>%
  filter(year < 1950)  # <-- Add this line to filter years
write_csv(montana_data_1950, "data/montana_1950_records.csv")


# to select sites, we may want to organize localities by the number of records in each class...
montana_data_1950 %>%
  group_by(class, locality) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# ...or species in each class *******************
montana_data_1950 %>%
  group_by(class, locality) %>%
  summarise(n = n_distinct(species)) %>%
  arrange(desc(n))

#mt aves 
bird_count_by_site <- montana_data_1950 %>%
  filter(class == "Aves") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(bird_count_by_site, "data/bird_count_by_site_mt.csv")


bird_species_by_site <- montana_data_1930 %>%
  filter(class == "Aves") %>%  # change for mammals, amphibians, herps, etc
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))  # sort by descending species count
write_csv(bird_species_by_site, "data/bird_species_by_site_mt.csv")


# For mammals: record count by site mt
mammal_count_by_site <- montana_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammal_count_by_site, "data/mammal_count_by_site_mt.csv")

# For mammals: species richness by site mt
mammal_species_by_site <- montana_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammal_species_by_site, "data/mammal_species_by_site_mt.csv")



# Squamata: record count by site mt
squamata_count_by_site <- montana_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_count_by_site, "data/squamata_count_by_site_mt.csv")

# Squamata: species richness by site mt
squamata_species_by_site <- montana_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_species_by_site, "data/squamata_species_by_site_mt.csv")


# Amphibia: record count by site mt
amphibia_count_by_site <- montana_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_count_by_site, "data/amphibia_count_by_site_mt.csv")

# Amphibia: species richness by site mt
amphibia_species_by_site <- montana_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_species_by_site, "data/amphibia_species_by_site_mt.csv")


# Testudines: record count by site
testudines_count_by_site <- montana_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_count_by_site, "data/testudines_count_by_site_mt.csv")

# Testudines: species richness by site
testudines_species_by_site <- montana_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_species_by_site, "data/testudines_species_by_site_mt.csv")


#reapeated process for north dakota class's

# Only North Dakota records, pre-1950
north_dakota_data_1950 <- data %>%
  select(class, species, stateProvi, locality, decimalLat, decimalLon, year, institutio) %>%
  filter(stateProvi == "North Dakota") %>%
  filter(year < 1950)  # <-- Add this line to filter years
write_csv(north_dakota_data_1950, "data/north_dakota_1950_records.csv")



# Aves: record count by site in North Dakota
aves_count_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Aves") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(aves_count_by_site_nd, "data/aves_count_by_site_nd.csv")

# Aves: species richness by site in North Dakota
aves_species_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Aves") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(aves_species_by_site_nd, "data/aves_species_by_site_nd.csv")

# Squamata: record count by site in North Dakota
squamata_count_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_count_by_site_nd, "data/squamata_count_by_site_nd.csv")

# Squamata: species richness by site in North Dakota
squamata_species_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_species_by_site_nd, "data/squamata_species_by_site_nd.csv")

# Amphibia: record count by site in North Dakota
amphibia_count_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_count_by_site_nd, "data/amphibia_count_by_site_nd.csv")

# Amphibia: species richness by site in North Dakota
amphibia_species_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_species_by_site_nd, "data/amphibia_species_by_site_nd.csv")

# Testudines: record count by site in North Dakota
testudines_count_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_count_by_site_nd, "data/testudines_count_by_site_nd.csv")

# Testudines: species richness by site in North Dakota
testudines_species_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_species_by_site_nd, "data/testudines_species_by_site_nd.csv")

# Mammalia: record count by site in North Dakota
mammalia_count_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammalia_count_by_site_nd, "data/mammalia_count_by_site_nd.csv")

# Mammalia: species richness by site in North Dakota
mammalia_species_by_site_nd <- north_dakota_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammalia_species_by_site_nd, "data/mammalia_species_by_site_nd.csv")


library(tidyverse) # load tidyverse data science libraries

# Only South Dakota records, pre-1950
south_dakota_data_1950 <- data %>%
  select(class, species, stateProvi, locality, decimalLat, decimalLon, year, institutio) %>%
  filter(stateProvi == "South Dakota") %>%
  filter(year < 1950)  # <-- Add this line to filter years
write_csv(south_dakota_data_1930, "data/south_dakota_1950_records.csv")

# Aves: record count by site in South Dakota
aves_count_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Aves") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(aves_count_by_site_sd, "data/aves_count_by_site_sd.csv")

# Aves: species richness by site in South Dakota
aves_species_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Aves") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(aves_species_by_site_sd, "data/aves_species_by_site_sd.csv")

# Squamata: record count by site in South Dakota
squamata_count_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_count_by_site_sd, "data/squamata_count_by_site_sd.csv")

# Squamata: species richness by site in South Dakota
squamata_species_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Squamata") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(squamata_species_by_site_sd, "data/squamata_species_by_site_sd.csv")

# Amphibia: record count by site in South Dakota
amphibia_count_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_count_by_site_sd, "data/amphibia_count_by_site_sd.csv")

# Amphibia: species richness by site in South Dakota
amphibia_species_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Amphibia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(amphibia_species_by_site_sd, "data/amphibia_species_by_site_sd.csv")

# Testudines: record count by site in South Dakota
testudines_count_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_count_by_site_sd, "data/testudines_count_by_site_sd.csv")

# Testudines: species richness by site in South Dakota
testudines_species_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Testudines") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(testudines_species_by_site_sd, "data/testudines_species_by_site_sd.csv")

# Mammalia: record count by site in South Dakota
mammalia_count_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammalia_count_by_site_sd, "data/mammalia_count_by_site_sd.csv")

# Mammalia: species richness by site in South Dakota
mammalia_species_by_site_sd <- south_dakota_data_1950 %>%
  filter(class == "Mammalia") %>%
  group_by(locality, decimalLat, decimalLon, institutio, year) %>%
  summarise(n = n_distinct(species), .groups = "drop") %>%
  arrange(desc(n))
write_csv(mammalia_species_by_site_sd, "data/mammalia_species_by_site_sd.csv")






