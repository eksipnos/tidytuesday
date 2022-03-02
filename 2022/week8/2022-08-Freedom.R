

# fonts and colors and alpha
# guides()
# gghighlight
# annotate


# year from 1995-2020
# CL = Civil Liberties
# PR = Political rights

library(gghighlight)
library(tidyverse)
library(showtext)

font_add_google("Source Sans Pro", "SourceSansPro")   
showtext_auto()


theme_custom <- theme(
  text = element_text(family = "SourceSansPro", size = 16),
  plot.title = element_text(
    face = "bold"
  )
)

theme_set(theme_minimal() + theme_customs)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>% 
  janitor::clean_names()

freedom %>% 
  group_by(region_name, year) %>% 
  mutate(cl_mean = mean(cl)) %>% 
 ggplot(aes(year, cl_mean,
            group = region_name,
            color = region_name)) +
  geom_line() +
  scale_color_viridis_d(guide = "none") +
  gghighlight(use_direct_label = F) +
  facet_wrap(~region_name,
             nrow = 1) +
    labs(title = "Civil Liberties in different continents", 
       subtitle = "Development from 1995-2020") +
  guides(x = guide_axis(n.dodge = 1, angle = 90)) +
  theme(
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.title = element_blank(), 
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(1,1,1,1, "cm")
  )


ggsave("Freedom.png")
