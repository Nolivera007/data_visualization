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

covid_ca %>% 
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

# Gráfico dinamico --------------------------------------------------------

plot_din <- covid_ca %>% 
  ggplot(aes(date, casos_acumulados, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_text(
    aes(x = date, label = country),
    size = 5,
    family = "Times New Roman",
    hjust = 0,
    vjust = 0,
    nudge_x = -2
  ) + 
  geom_point(aes(x = date), size = 2) +
  coord_cartesian(clip = 'off') +
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
      <br>\nDatos: RamiKrispin/coronavirus al 22-01-2021<br>
      "
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.background = element_rect(fill = "#2E4053"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 24, hjust = .5,
                                  color = "white",
                                  family = "Times New Roman"),
    plot.subtitle = element_text(size = 12, hjust = .5,
                                 color = "white", lineheight = 1),
    axis.text = element_text(color = "white"),
    plot.caption = element_markdown(size = 8, hjust = .5, linewidth = 1.5,
                                    color = "white"),
    plot.margin = margin(30, 60, 30, 50)
  ) +
  guides(color = FALSE) +
  transition_reveal(date) +
  ease_aes("linear")  

animate(plot_din, nframes = 60, width = 600, height = 450, units = "px")
anim_save("line_din.gif")