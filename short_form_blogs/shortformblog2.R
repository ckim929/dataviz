library(tidyverse)
library(dplyr)
#install.packages("devtools")
#install.packages("Rtools")
#devtools::install_github("vdeminstitute/vdemdata", force = TRUE)
library(vdemdata)
library(ggplot2)
library(plotly)
#devtools::install_github("andybega/states")
library(states)
library(data.table)
library(reshape2)
library(rworldmap)
library(patchwork)

vdem.raw <- data.table::setDT(vdemdata::vdem)
vdem.codebook <- data.table::setDT(vdemdata::codebook)

# Practice
#vdem %>%
  #filter(year == 2020, !is.na(v2x_regime), !is.na(e_v2x_gender_4C)) %>%
  #group_by(v2x_regime, e_v2x_gender_4C) %>%
  #count() %>%
  #ggplot(aes(x = v2x_regime, y = , e_v2x_gender_4C, fill = n)) +
  #geom_tile() -> gender_regime

#ggplotly(gender_regime) -> gender_plotly

#gender_plotly


# Determining variables of interest
#metrics <- c('v2x_regime','e_v2x_gender_4C')
#vdem.codebook[tag %in% metrics, .(name, vartype, tag, question, scale)]


# Variable 1: Academic Freedom (v2xca_academ) 	v2xpe_exlgender
# Variable 2: Exclusion by Socio-Economic Group (v2xpe_exlecon)

# Election Turnout = v3eltrnout


metrics <- c('v2xpe_exlgender','v2xpe_exlecon')
id.vars <- c('country_name','COWcode', 'histname', 'codingstart_contemp', 'codingend_contemp', 'year', 'e_regionpol')
vars <- c(id.vars, metrics)

# subset the raw data
vdem <- vdem.raw[, ..vars]
rm(vdem.raw)

vdem_from_1990 <- vdem[year > 1990]

vdem_avg <- vdem_from_1990 %>%
  group_by(year) %>%
  summarise_at(vars(v2xpe_exlgender, v2xpe_exlecon), list(name = mean))

colors <- c("Exclusion by Gender" = "#B81C0C",
            "Exclusion by Socio-Economic Group" = "#2051B9")

### VISUALIZATION 1 (geom_line()) ###
ggplot(data = vdem_avg,
       aes(x = year)) +
  geom_line(aes(y = v2xpe_exlgender_name, color = "Exclusion by Gender"),
            size = 1.5) +
  geom_line(aes(y = v2xpe_exlecon_name, color = "Exclusion by Socio-Economic Group"), size = 1.5) +
  ggtitle('Average Index (0-1) of Gender and Socioeconomic exclusion') +
  labs(x = "Year",
       y = "Value",
       color = "Legend") +
  ylim(0.3,0.5) +
  xlim(1990, 2020) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))


### VISUALIZATION 2 (geom_point()) ###

vdem_from_2020 <- vdem[year > 2020]

vdem_avg_country <- vdem_from_2020 %>%
  group_by(country_name, e_regionpol) %>%
  select(country_name, e_regionpol, v2xpe_exlgender, v2xpe_exlecon) %>%
  summarise_at(vars(v2xpe_exlgender, v2xpe_exlecon), list(name = mean))

# Group by region

vdem_avg_country$e_regionpol <- factor(vdem_avg_country$e_regionpol, labels = c("Eastern Europe", "Latin America", "North Africa and Middle East",
                                                                                "Sub-Saharan Africa", "Western Europe and North America", "Eastern Asia",
                                                                                "South-Eastern Asia", "Southern Asia", "The Pacific", "The Caribbean"))

plot_2 <- ggplot(data = vdem_avg_country, aes(x = v2xpe_exlecon_name, y = v2xpe_exlgender_name, color = as_factor(e_regionpol))) +
  geom_point(size = 3, alpha = 0.75) +
  ggtitle("Correlation Between Political Exclusion by Gender and Socio-Economic Status") +
  labs(x = "Exclusion by Socio-Economic Status Index",
       y = "Exclusion by Gender Index") +
  scale_color_discrete(name = "Regions", labels = c("Eastern Europe", "Latin America", "North Africa and Middle East", 
                                                   "Sub-Saharan Africa", "Western Europe and North America", "Eastern Asia", 
                                                   "South-Eastern Asia", "Southern Asia", "The Pacific", "The Caribbean")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"))


#plot_ly(data = vdem_avg_country,
        #x = ~v2xpe_exlecon_name,
        #y = ~v2xpe_exlgender_name,
        #color = ~as_factor(e_regionpol),
        #hoverinfo = 'text',
        #text = ~country_names)

plot_2


ggplotly(plot_2)


### VISUALIZATION 3 (geom_polygon()) ###

vdem_map <- vdem_avg_country %>%
  rename(region = country_name, gender_index = v2xpe_exlgender_name, socioeconomic_index = v2xpe_exlecon_name)

world_map <- map_data("world")
mm_map <- left_join(vdem_map, world_map, by = "region")

map_theme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust=0.5, face = "bold"),
  legend.position="bottom",
)

map1 <- ggplot(data = mm_map, mapping = aes(long, lat, group = group)) +
  geom_polygon(aes(fill = as.numeric(gender_index))) +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  ggtitle("Gender Index in 2020") +
  labs(fill = "Gender Index") +
  map_theme 

ggplotly(map1)

map2 <- ggplot(data = mm_map, mapping = aes(long, lat, group = group)) +
  geom_polygon(aes(fill = as.numeric(socioeconomic_index))) +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  ggtitle("Socio-Economic Index in 2020") +
  labs(fill = "Socio_Economic Index") +
  map_theme

ggplotly(map2)



### VISUALIZATION 4 ###
# Plotting categorical variables with year ranges

vdem_avg_range <- vdem_from_1990 %>%
  mutate(ranges = cut(year,
                      seq(1990, 2020, 5))) %>%
  group_by(ranges) %>%
  dplyr::summarize(gender_avg = mean(v2xpe_exlgender), socio_avg = mean(v2xpe_exlecon)) %>%
  as.data.frame()

vdem_avg_range_2 <- melt(vdem_avg_range, id.vars='ranges')
vdem_avg_range_3 <- na.omit(vdem_avg_range_2)

#vdem_avg_range2 <- vdem_avg_range %>%
  #group_by(ranges) %>%
  #summarise_at(vars(v2xpe_exlgender, v2xpe_exlecon), list(name = mean))



  
ggplotly(viz4)



############################################################################################3333






