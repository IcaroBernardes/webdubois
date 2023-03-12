# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(geobr)
library(ggbeeswarm)
library(ggplot2)
library(ggforce)
library(ggtext)
library(ggview)
library(junebug)
library(purrr)
library(readxl)
library(rmapshaper)
library(santoku)
library(scales)
library(systemfonts)
library(tidyr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
red <- palette[6]
blue <- palette[8]

## Makes special styled fonts available to R (e.g.: Medium, Solid, etc)
### Lists fonts visible to {systemfonts}
fonts_list <- systemfonts::system_fonts()

### Takes all font styles that share that exact family name and
### registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

### Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the fonts
font_brands_glyphs <- "Font Awesome 6 Brands Regular"

## Loads the data. Data downloaded and picked from Table 1.1 of the
## "Introdução" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
pop <- readxl::read_xlsx("2023/week04/data.xlsx", sheet = "pop")

## Loads the data. Data downloaded and picked from the 2021 directory of this IBGE page:
## https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/15761-areas-dos-municipios.html?=&t=downloads
area <- readxl::read_xlsx("2023/week04/data.xlsx", sheet = "area")

## Gets the shapes of the Brazilian states
shapes <- geobr::read_state(year = 2020)

# 1. Data handling ##########
## Joins the two databases together and keeps only info on Black people
df <- dplyr::left_join(pop, area) |> 
  dplyr::filter(race == "black")

## Calculates the absolute size of the Black population for each state
df <- df |> 
  dplyr::mutate(pop = pop_total_per1k*1000*(pop_pct/100))

## Calculates the population density for each state (person per sq. km)
df <- df |> 
  dplyr::mutate(dens = pop/area) |> 
  dplyr::select(UF, abbrev, dens)

## Plots the density as a jitter plot
jitter <- df |> 
  ggplot() +
  ggbeeswarm::geom_beeswarm(
    aes(x = dens, y = "UFs", group = abbrev), method = "center", cex = 3, side = 1L
  ) + 
  scale_x_log10()
print(jitter)

## Adds vertical lines that indicate the
## limits of the categories of density
categ_lim <- c(3, 10, 30, 100)
jitter <- jitter + 
  geom_vline(xintercept = categ_lim, color = "red", linetype = "dashed")
print(jitter)

## Divides the densities in five groups of equal widths
df <- df |> 
  dplyr::mutate(category = santoku::chop(
    dens, breaks = categ_lim,
    labels = santoku::lbl_glue(
      label = "MORE THAN {l}<br>{r} OR LESS",
      first = "{r} OR<br>LESS",
      last = "MORE<br>THAN {l}",
      fmt = scales::label_number()
    ),
    extend = TRUE, left = FALSE
  ))

## Simplifies the shapes of the states to the point of eliminating islands,
## but keeping the general form of the land
shapes <- rmapshaper::ms_simplify(shapes, keep = 0.005)

## Creates a contour of the country by uniting the states
## and dissolving their internal borders
br <- rmapshaper::ms_dissolve(shapes)

## Selects and renames columns of the shapes
shapes <- shapes |> 
  dplyr::select("abbrev" = abbrev_state, geom)

## Joins together data and shapes
df <- dplyr::left_join(shapes, df)

## Defines coordinates of the legend
legend <- dplyr::tibble(
  x = seq(-72, -38, length.out = 5),
  y = 17.5,
  breaks = levels(df$category),
  values = c(gold, blue, red, brown, black)
)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:140px;'>DISTRIBUTION OF BLACKS IN BRAZIL (2021).</span>
<br><br>
<span style='font-size:80px;'>DISTRIBUTION DES NOIRS AU BRÉSIL (2021).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the plot
p <- df |> 
  ggplot() +
  
  ### Places the shapes
  geom_sf(aes(fill = category), color = black,
          linewidth = 0.8, linetype = "twodash") +
  
  ### Places the contour of the country
  geom_sf(fill = NA, color = black, linewidth = 2, data = br) +
  
  ### Places the legend points
  geom_point(aes(x = x, y = y, fill = breaks), shape = 21, color = black, 
             size = 30, stroke = 1.5, data = legend) +
  
  ### Places the legend labels
  ggtext::geom_richtext(
    aes(x = x, y = y, label = breaks), family = "Teko", nudge_y = -3.5,
    size = 12.5, lineheight = 1, fill = NA, label.colour = NA, data = legend
  ) +
  
  ### Places a decoration line behind the title of the legend (by hand)
  geom_segment(x = -75, xend = -35, y = 10, yend = 10,
               linewidth = 6, lineend = "round") +
  
  ### Places the title of the legend (by hand)
  ggtext::geom_richtext(
    aes(x = -55, y = 10, label = "BLACK PEOPLE PER SQUARE KILOMETER."),
    family = "Teko", size = 20, fill = tan, label.colour = NA,
    label.padding = unit(c(0, 0.02, 0, 0.02), "npc")
  ) +
  
  ### Defines the colors of the shapes
  scale_fill_manual(
    values = legend$values,
    breaks = legend$breaks
  ) +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 80, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(60, 0, 0, 0),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week04/distribution.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
