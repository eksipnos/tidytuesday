
library(tidyverse)
library(viridis)
library(showtext)
library(colorspace)
library(ggtext)


font_add_google("Noto Serif Display", "Serif") 
showtext_auto()


theme_custom <- theme(
  text = element_text(family = "Serif"),
  plot.title = element_text(
    face = "bold", size = 16), 
  plot.margin = margin(0.5,0.5,0.5,0.5, "cm"), 
  plot.caption = element_markdown(size = 8, 
                                  vjust = 1,
                                  hjust = 0.5,
                                  margin = margin(t = 20)), 
  plot.background = element_rect(fill = "white", colour = "white")
)

theme_set(theme_void() + theme_custom)

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

US_States <- tibble(
  fullname = state.name, 
  STATE = state.abb
)

electricstations <- stations %>% 
  filter(FUEL_TYPE_CODE == "ELEC") %>% 
  group_by(STATE) %>% 
  count() %>% 
  left_join(US_States, by = "STATE") %>% 
  rename(region = fullname) %>% 
  mutate(region = str_to_lower(region)) %>% 
  na.omit(region)

state <- map_data("state")

test <- electricstations %>% 
  full_join(state) 

ggplot(test) +
  geom_polygon(aes(x = long, 
                   y= lat, 
                   group =  group, 
                   fill = n)) +
  scale_fill_continuous_sequential(palette = "Purples") +
  labs(title = "Electric Stations in the USA", 
       caption = "<br>  @eksipnos") + 
  coord_map(projection = "mercator") +
  guides(fill = guide_colourbar(
                        title = "Number of stations",
                             title.position = "top",
                               nbins = 10)) 

ggsave("Electric_stations_US.png")



