library(tidyr)

test<- Timelinechart %>% spread(Status, Affected  ) %>%
  mutate (per = death/confirmed)

library(openintro)
library(highcharter)
library(countrycode)

new_data_country <- full_table %>% filter(Date == latest_date)%>% group_by(Country) %>%
  summarise(confirmed = sum(confirmed),
            death = sum(death),
            recovered = sum(recovered)) %>% mutate(iso3 = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

top_10 <- new_data_country %>% arrange(desc(confirmed)) %>% head(10) %>% select(c(1,2,4,3)) %>% rename(Confirmed = confirmed, Recovered = recovered, Death = death)

highchart() %>%
  hc_add_series_map(worldgeojson,new_data_country,
                    name = "Country",
                    value = "confirmed",
                    joinBy = "iso3") %>%
  hc_mapNavigation(enabled = T)  %>%
  hc_colorAxis(stops = color_stops())

w <-worldgeojson


tests <- full_table %>% filter(Date == latest_date)%>% group_by(Country) %>%
  summarise(confirmed = sum(confirmed),
            death = sum(death),
            recovered = sum(recovered))


