# 2021-01-23
# Noé Olivera

# Cargar kibrerias --------------------------------------------------------
library(tidyverse)
library(coronavirus)
library(ggrepel)
library(ggthemes)
library(ggtext)
library(gganimate)
library(extrafont)
library(Cairo)

rm(list = rm())

# Actualizar datos del paquete coronavirus --------------------------------

update_dataset()

data(coronavirus)
head(coronavirus)
view(coronavirus)

# Seleccionar datos -------------------------------------------

ca <- c("Costa Rica", "El Salvador", "Guatemala",
        "Belice", "Honduras", "Nicaragua", "Panama")

covid_ca <- coronavirus %>% 
  select(date, country, type, cases) %>% 
  filter(type == "confirmed" & country %in% ca) %>% 
  group_by(country) %>% 
  mutate(
    dias_acumulados = as.numeric(date - min(date)),
    casos_acumulados = cumsum(cases),
    country = ifelse(country == "Panama", "Panamá", country)
  ) %>% 
  ungroup()

head(covid_ca)
view(covid_ca)  

ultimo_dato <- covid_ca %>% 
  filter(date == max(date))

# fecha donde se reportaron mas de 100 casos acumulados
covid_ca %>% 
  filter(casos_acumulados > 100) %>% 
  select(date, country, casos_acumulados)

min <- as.Date("2020-05-01")
max <- max(covid_ca$date)
lim <- as.Date(c(min, max))

# Gráfico base ---------------------------------------------------------------
  
covid_ca %>% 
  ggplot(aes(date, casos_acumulados, color = country)) +
  geom_line()

# Gráfico estatico --------------------------------------------------------

covid_ca %>% 
  ggplot(aes(date, casos_acumulados, color = country)) +
  geom_line(size = 1.2) +
  geom_point(data = ultimo_dato, size = 3) +
  geom_text(
    data = ultimo_dato,
    aes(label = country, color = country),
    size = 5,
    segment.color = "grey20",
    force = 1,
    family = "Times New Roman",
    hjust = 1,
    vjust = 0,
    nudge_x = -3
  ) + 
  scale_x_date(limits = lim, date_labels = "%b %y",
               breaks = "month") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, max(covid_ca$casos_acumulados), 50000)) +
  labs(
    x = NULL,
    y = NULL,
    title = "**Covid-19 en Centroamérica**",
    subtitle = "Evolución de casos confirmados",
    caption =
      "<br>\nGráfico: <img src='img/twitter.png' width='10'/>
      <span style = 'color:#EFC242;'>**nolivera007**</span><br>
    <br>\nDatos: {RamiKrispin/coronavirus} al 22-01-2021<br>"
  ) +
  guides(color = FALSE) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "#2E4053"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 33, hjust = .5,
                                  color = "white",
                                  family = "Times New Roman"),
    plot.subtitle = element_text(size = 16, hjust = .5,
                                 color = "white", lineheight = 1),
    plot.caption = element_markdown(size = 8, hjust = .5, linewidth = 1.5,
                                    color = "white"),
    plot.margin = margin(2, 3, 2, 3, unit = "cm"),
    axis.text = element_text(colour = "white")
  ) +
  ggsave(
    path = paste(here::here()), 
    filename = "line_sta.png",
    width = 10,
    height = 8,
    units = "in",
    dpi = 300,
    type = "cairo-png"
  )
