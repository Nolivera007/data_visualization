# 2021-01-21
# Noé Olivera

# Cargar librerias --------------------------------------------------------

library(tidyverse)
library(sf)
library(ggtext)
library(extrafont)
library(Cairo)
library(here)

# Cargar datos del mapa ------------------------------------------------------------
# los datos se descargaron de: https://data.humdata.org/dataset/honduras-admin-level-1-boundaries

hn_map <- st_read("hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm1_sinit_20161005.shp")

# Gráfico 1 ------------------------------------------------

hn_map %>%
  ggplot() +
  geom_sf()


# Cargar los datos del covid-19 ---------------------------------------------------
# los datos se descargaron de: https://covid19honduras.org/

covid <- read_csv("covid.csv")

# Unir las dos bases de datos ---------------------------------------------

covid %>%
  mutate(
    Departamentos = str_to_title(Departamentos)
  ) -> covid

hn_map %>% 
  mutate(
    ADM1_ES = str_to_title(ADM1_ES)
  ) -> hn_map

hn_map_covid <- hn_map %>% 
  left_join(covid, by = c("ADM1_ES" = "Departamentos"))

# Gráfico 2 ---------------------------------------------------------------

hn_map_covid %>%
  ggplot(aes(fill = Infectados)) +
  geom_sf()

# Gráfico 3 ---------------------------------------------------------------

theme_set(
  theme_void() +
  theme(
    plot.title = element_markdown(family = "Times New Roman",size = 35, hjust = .5),
    plot.subtitle = element_text(family = "Times New Roman", size = 18, hjust = .5),
    legend.position = "bottom",
    legend.title = element_text(hjust = .5, family = "Times New Roman"),
    legend.text = element_text(angle = 0),
    plot.caption = element_markdown(family = "Times New Roman", size = 8, hjust = .5, linewidth = 1.5),
    plot.background = element_rect(color = "white"),
    plot.margin = margin(1,1,1,1, unit = "cm")
  )
)

hn_map_covid %>%
  ggplot() +
  geom_sf(aes(fill = Infectados), color = "white", size = .5) +
  scale_fill_gradient("Casos Covid-19",
                      low = "#154360",
                      high = "#7FB3D5",
                      breaks = c(seq(0, 40000, 5000))) +
  labs(
    title = "Casos Covid-19 Honduras 2021",
    subtitle = "Casos por Departamento",
    caption =
    "<br>\nGráfico: <img src='img/twitter.png' width='10'/> **nolivera007**<br>
    <br>\nDatos: {covid19honduras.org}<br>
    <br>\nDatos al 2021-01-21<br>"
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 20,
      barheight = 0.5,
      title.position = "top",
      title.jhust = .5
    )
  ) +
  ggsave(
    path = paste(here()), 
    filename = "map_hn.png",
    width = 20,
    height = 28,
    units = "cm",
    dpi = 300,
    type = "cairo-png"
  )
  