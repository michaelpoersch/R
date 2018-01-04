library("eurostat")
library("tidyverse")

#query <- search_eurostat("road", type = "table")
# select "tsdtr420" -> People killed in road accidents

# dat <- get_eurostat(id = "tsdtr420", time_format = "num", filters = list(geo = c("DE")))
dat <- get_eurostat(id = "tsdtr420", time_format = "num")

myacc <- dat %>% rename(values_acc = values)

# query <- search_eurostat("population", type = "table")

population <- get_eurostat(id = "demo_pjan", time_format = "num")
mypop <- population %>% filter(age == "TOTAL" & time >= 1999 & sex == "T") %>% rename(values_pop = values) %>% select(-age)

country <- eu_countries %>% rename(geo = code, geo_name = name)
#iso_alpha2 <- ISO_3166_1 %>% select(Alpha_2, Name) %>% rename(geo = Alpha_2)

result <- left_join(myacc, mypop) %>% mutate(values_rel = values_acc / values_pop * 1000) %>% left_join(country)

country <- c("DE", "UK", "FR", "IT", "AT", "ES")
# alle Länder damit filter funktioniert ;-) 
country <- unname(unlist(distinct(myacc, geo)))
result_plot <- result %>% filter(geo %in% country$geo)

ggplot( result_plot, aes(x = time, y = values_rel)) + 
  geom_point(size = 2) +
  geom_line() + 
  facet_wrap(~ geo_name, nrow = 6) + 
  labs(title = "Road accidents", x = "Year", y = "Vicitms")

# zuviele Informationen; 
ggplot( result_plot, aes(x = time, y = values_rel, color = geo, group = geo, shape = geo)) + 
  geom_point(size = 2) +
  geom_line() + 
  theme_bw() + 
  labs(title = "Road accidents", x = "Year", y = "Vicitms")
