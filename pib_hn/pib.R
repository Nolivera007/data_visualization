library(dplyr)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)
library(Cairo)

pib <- readr::read_csv("data/crecimiento_economico.csv")
head(pib)

min(pib$crecimiento_pib, na.rm = TRUE)
summary(pib)

pib %>% 
  filter(!is.na(crecimiento_pib), anio > 2001) %>% 
  ggplot(aes(anio, crecimiento_pib)) +
  geom_line(color = "grey", lwd = .9) +
  geom_point(aes(anio, punto),
             size = 3,
             color = "white",
             fill = "#173045",
             shape = 21) +
  geom_text_repel(aes(label = round(punto, 1)),
                  size = 4,
                  color = "white",
                  family="Segoe Print"
  ) +
  scale_x_continuous(breaks = seq(2001, 2020, 2)) +
  scale_y_continuous(breaks = seq(-10, 12, 3), labels = paste0(seq(-10, 12, 3), "%")) +
  labs(
    title = "Crecimiento del PIB: Honduras 2000-2020*",
    subtitle = "(variación % anual)",
    caption = "<br>\nGráfico: <span style = 'color:#EFC242;'>**nolivera007**</span><br>
    <br>Datos: Banco Mundial<br>
    <br>*Estimaciones preliminares<br>"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#173045",color = NA),
    plot.background = element_rect(fill = "#173045",color = NA),
    plot.title = element_text(family="Segoe Print", face = "bold",color="white", size=15, hjust = .5),
    plot.subtitle = element_text(color = "white", size=10, family="Segoe Print", hjust = .5),
    plot.caption = element_markdown(color="white", vjust = -8, family="Segoe Print", size = 6,  hjust = .5),
    plot.margin=unit(c(1,1,1,1),"cm"),
    axis.title.x=element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(vjust =0.5, color="white", family="Segoe Print"),
    axis.text.y = element_text(color="white", family="Segoe Print"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
    ) +
  ggsave("pib.png",
         width = 16,
         height = 20,
         units = c("cm"),
         type = "cairo-png",
         path = paste(here::here())
  )
