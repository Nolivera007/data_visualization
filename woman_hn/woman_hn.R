library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggimage)
library(ggtext)
library(showtext)
library(here)
library(extrafont)

nodes <- read_csv("data/nodos.csv")
edges <- read_csv("data/edges.csv")

nodes %>% 
  mutate(
    dominio = as_factor(dominio)
  ) -> nodes

graph <- tbl_graph(nodes, edges)

img <- here("img/woman.png")
showtext_auto()

ggraph(graph, layout = "partition") +
  geom_edge_diagonal(aes(color = color), alpha = 0.5) +
  geom_node_text(aes(x = x, y = y, label = dominio, filter = levels == 3, color = categoria), size = 10, family = "Atkinson Hyperlegible", hjust = 1, vjust = 0.5, lineheight = 0.9) +
  geom_node_text(aes(label = node, filter = levels == 2, color = node), size = 13, family = "Atkinson Hyperlegible", vjust = 0.5, fontface = "bold") +
  geom_node_point(aes(filter = levels == 2, color = node), size = 110, alpha = 0.40) +
  geom_node_point(aes(filter = levels == 2, color = node), size = 120, shape = 1) +
  geom_node_range(aes(y = y + 0.02, yend = y + 1.5 * numero/max(nodes$numero, na.rm = TRUE), x = x, xend = x, filter = levels == 3, color = categoria), size = 12) +
  geom_node_text(aes(x = x, y = y + 1.5 * numero/max(nodes$numero, na.rm = TRUE), label = numero, filter = levels == 3, color = categoria), nudge_y = 0.025, size = 10, family = "Atkinson Hyperlegible", fontface = "bold", hjust = 0, vjust = 0.5) +
  geom_image(data = filter(nodes, levels == 1), aes(x = 10.5, y = 1.35, image = img), size = 0.09, asp = 1.8) + 
  scale_color_manual(values = c("Actividad" = "#6C3483", "Participación" = "#1A5276", "Estudios" = "#117864", "Población" = "#A04000")) +
  scale_edge_color_identity() +
  labs(
    title = "<b>Características de las <span style = 'color:#DE3163;'>mujeres</span><br> en Honduras</b>",
    subtitle = "Actividad laboral, tasa de participación, años de estudios promedio y porcentaje de población femenina.",
    caption = "<br>\nGráfico: <span style = 'color:#EFC242;'>**nolivera007**</span><br>
    <br>\nDatos: **INE (EPHPM) Jun 2019** <br>"
  ) +
  coord_flip() +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    plot.title = element_markdown(family = "Times New Roman", size = 100, color = "grey40", hjust = 0.5),
    plot.subtitle = element_text(family = "Times New Roman", size = 35, margin = margin(t = 20), hjust = 0.5, color = "grey40"),
    plot.caption = element_markdown(family = "Times New Roman", size = 25, color = "grey40"),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.position = "none"
  ) +
  ggsave(here("woman.pdf"), height = 22, width = 22*1.8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here("woman.pdf"),
  filenames = here("woman.png"),
  dpi = 72
)
