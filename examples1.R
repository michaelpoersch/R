library("eurostat")
library("ggplot2")
library("tidyverse")
library("bpa")
library("here")

# Liste aller Datensätze
# toc <- get_eurostat_toc()

#
# Geburtenrate
#
fertility <- get_eurostat("demo_r_frate3") %>% filter(time == "2014-01-01") %>% mutate(cat = cut_to_classes(values, n = 7, decimals = 1))
mapdata <- merge_eurostat_geodata(fertility, resolution = "20")

ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cat), color = "grey", size = .1) + 
  scale_fill_brewer(palette = "RdYlBu") + 
  labs(title = "Fertility rate, ny NUTS-3 regions, 2014",
    subtitle = "Avg. number of live births per woman",
    fill = "Total fertility rate(%)") +
  theme_light() +
  coord_map(xlim = c(-12,44), ylim = c(35,67))

#
# Bevölkerung
#
population <- get_eurostat("tps00010")

# der Datnsatz enth?lt auch Gruppierungen und L?nder, die nicht in der EU sind
# nur EU L?nder sollen ausgew?hlt werden
sel <- eu_countries %>% select(code) 
population_sel <- population %>% filter(geo %in% c(t(sel)))

# rename, damit join funktioniert
country <- eu_countries %>% rename(geo = code, geo_name = name)
population_gg <- population_sel %>% left_join(country)

# Darstellung pro Land (28)
ggplot(population_gg, aes(x = time, y = values, fill = indic_de)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ geo_name)

# Darstellung pro Jahr (12; 2005-2016)
ggplot(population_gg, aes(x = geo_name, y = values, fill = indic_de)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ time) + 
  coord_flip()

#
# Urban: große Städte
#

# urban_cities %>% basic_pattern_analysis() %>% head(10)
# bpa(urban_cities, unique_only = TRUE)

urban_cities <- get_eurostat("urb_cpop1")
# pop_structure <- get_eurostat("urb_cpopstr")

cities <- read_csv("cities.csv") %>% select(Label, Notation) %>% rename(cities = Notation, city_label = Label)
indic <- read_csv("indic_ur.csv") %>% select(Label, Notation) %>% rename(indic_ur = Notation, indic_label = Label) %>% filter(grepl("^DE", indic_ur))
urban_cities <- urban_cities %>% left_join(cities)


used_indic = urban_cities %>% distinct(indic_ur) %>% left_join(indic)
