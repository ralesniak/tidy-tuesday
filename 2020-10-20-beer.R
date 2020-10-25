library(tidyverse)
library(rgeocodio)
library(sf)
library(tigris)
library(rmapshaper)
library(showtext)
library(ggtext)
library(ggrepel)


# Parameters and Fonts ----------------------------------------------------

dmv <- c("DC", "VA", "MD")
wgs <- 4326
dmv_projection <- 3968 #NAD83 / Virginia Lambert

# map fonts

font_add_google(name = "Spartan", family = "spartan")
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

# map parameters

water_color <- "#C9ECF7"
high <- "#F50000"
low <- "#FFD6D6"
low_text <- "#FFADAD"

# Load and filter data ----------------------------------------------------

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

beer_awards_dmv <- beer_awards %>%
  filter(state %in% dmv)


# Geocode and join --------------------------------------------------------

#get unique "address" in City, ST format for geocoding

beer_awards_geo <- beer_awards_dmv %>%
  mutate(address = glue::glue("{city}, {state}")) %>%
  distinct(address) %>%
  #this one wouldn't geocode with just the name
  mutate(address = recode(address, 
                          `Vint Hill, VA` = "7134 Farm Station Rd, Vint Hill Farms, VA 20187"))

#geocode with geocodio
geocoded <- gio_batch_geocode(beer_awards_geo$address)

geocoded_clean <- geocoded %>%
  select(query, response_results) %>%
  unnest(response_results) %>%
  select(query, location.lat, location.lng) %>%
  #just get the first one - doesn't matter which one
  distinct(query, .keep_all = TRUE) %>%
  mutate(query = recode(query, 
                          `7134 Farm Station Rd, Vint Hill Farms, VA 20187` = "Vint Hill, VA")) %>%
  rename(address = query)

#join in lat/lon
beer_awards_dmv_geo <- beer_awards_dmv %>%
  mutate(address = glue::glue("{city}, {state}")) %>%
  left_join(geocoded_clean, by = "address")

# Water -------------------------------------------------------------------

#base map of all states and filter to dmv
states <- states() %>%
  filter(STUSPS %in% dmv) %>%
  st_transform(crs = dmv_projection)

#use a list of counties to get all of the area water files
md_counties <- counties("MD") %>%
  st_set_geometry(NULL) %>%
  pull(NAMELSAD)

md_water <- area_water("MD", md_counties)
  
md_water_2 <- md_water %>%
  #get rid of small stuff
  filter(AWATER > 1000000 | FULLNAME == "Chesapeake Bay" | FULLNAME == "Tangier Sound") %>%
  st_transform(crs = dmv_projection) %>%
  #make it smaller to avoid long output
  summarize() %>%
  ms_simplify(., keep = 0.01)

#use a list of counties to get all of the area water files - repeat for VA
va_counties <- counties("VA") %>%
  st_set_geometry(NULL) %>%
  pull(NAMELSAD)

va_water <- area_water("VA", va_counties)

va_water_2 <- va_water %>%
  filter(AWATER > 1000000 | FULLNAME == "Chesapeake Bay" | FULLNAME == "Tangier Sound") %>%
  st_transform(crs = dmv_projection) %>%
  summarize() %>%
  ms_simplify(., keep = 0.01)


# Map ---------------------------------------------------------------------

beer_awards_sf <- beer_awards_dmv_geo %>% 
  #count by city - summarizing high level for mapping
  count(address, location.lat, location.lng) %>%
  #Sterling is covering up Ashburn even with st_jitter
  filter(address != "Sterling, VA") %>%
  #make shapefile
  st_as_sf(crs = wgs, coords = c("location.lng", "location.lat"), remove = FALSE) %>%
  st_transform(crs = dmv_projection)

#adding in text labels for chosen interesting points
annotation_data <- beer_awards_sf %>%
  filter(address %in% c("Roseland, VA", "Baltimore, MD", "Richmond, VA", "Ashburn, VA")) %>%
  arrange(address) %>%
  add_column(
    label = c(
      "Lost Rhino in Ashburn won a medal 4 out of the last 5 years.",
      "Clipper City in Baltimore won 7 medals in the mid-00s but hasn't won since 2014",
      "Richmond has had 3 winners in the past 5 years.",
      "Devils Backbone in Roseland won 20 medals over 10 years."
    )
  ) %>%
  mutate(label = str_wrap(label, 25))

beer_dmv_chart <- ggplot() +
  #basemap
  geom_sf(data = states, aes(fill = STUSPS), color = NA) +
  scale_fill_manual(values = c("gray95", "gray85", "gray75")) +
  #water
  geom_sf(data = md_water_2, fill = water_color, color = NA) +
  geom_sf(data = va_water_2, fill = water_color, color = NA) +
  #cities
  geom_sf(data = beer_awards_sf, aes(color = n), size = 5) +
  #confirm map is printing in dmv_projection
  coord_sf(crs = st_crs(dmv_projection)) +
  #labels - lots of manual stuff here
  geom_text_repel(
    data = annotation_data,
    aes(label = label,
        x = location.lng,
        y = location.lat),
    size = 7,
    hjust = c(0, 1, 0, 0),
    nudge_x = c(-1, 1.2, 0.5, -1),
    nudge_y = c(-0.5, 0, 0.2, -0.3),
    box.padding = 1.5,
    segment.curvature = -0.2,
    segment.ncp = 3,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  #give a little more room on the right for labels
  scale_x_continuous(expand = expansion(mult = c(0, .1))) +
  #uses parameters from intro
  scale_color_steps(low = low, high = high) +
  #labels, also uses params
  labs(title = "The DMV region has a history of award-winning beer",
       subtitle = glue::glue("Count of Great American Beer Festival medals by city in DC, MD, or VA,",
       " n = <b style = 'color:{low_text}'>{min(beer_awards_sf$n)}</b>", 
       " - <b style = 'color:{high}'>{max(beer_awards_sf$n)}</b>"),
       caption = "Source: Great American Beer Festival, 1987 - 2020\nMap: Rachel Lesniak @ralesniak") +
  #theme - mostly space and fonts
  theme_void(base_size = 25) +
  theme(panel.background = element_rect(fill = "gray95",
                                        size = 0),
        plot.background = element_rect(fill = "gray95"),
        plot.margin = unit(c(10, 12, 10, 12), "points"),
        legend.position = "none",
        text = element_text(family = "roboto"),
        plot.title = element_text(family = "spartan", face = "bold"),
        plot.subtitle = element_markdown(family = "roboto"))

png(filename = "beer_dmv_chart.png",
    width = 1200,
    height = 670)
beer_dmv_chart
dev.off()