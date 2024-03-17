# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(readxl)
library(scales)
library(systemfonts)
library(tidyr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]

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

## Loads the data. Data downloaded and picked from
## Tables 136 and 9605 of SIDRA: https://sidra.ibge.gov.br
rawData <- readxl::read_xlsx("2024/week06/data.xlsx")

# 1. Data handling ##########
## Converts the race to factor
workData <- rawData |> 
  dplyr::mutate(race = factor(race, levels = c("pretos", "pardos", "brancos")))

## Defines the year labels
yearLabels <- rawData |> 
  dplyr::summarise(
    total = sum(population),
    .by = year
  ) |> 
  dplyr::arrange(year) |> 
  dplyr::mutate(x = total + 9000000,
                y = year)

## Defines the y-axis guide lines
guideY <- rawData |> 
  dplyr::summarise(
    xmax = sum(population),
    .by = year
  ) |> 
  dplyr::slice(-c(1,n()))

## Define the race labels
raceLabels <- workData |> 
  dplyr::arrange(desc(race)) |> 
  dplyr::slice(n()-1, n(), .by = race) |> 
  dplyr::mutate(
    population = lag(cumsum(population), default = 0) + population/2,
    .by = year
  ) |> 
  dplyr::summarise(
    a = (population[2]-population[1])/(year[2]-year[1]),
    b = population[1] - a*year[1],
    .by = race
  ) |> 
  dplyr::mutate(
    y = 2019,
    x = a*y + b,
    race = toupper(race)
  ) |> 
  dplyr::mutate(color = c(black, tan, tan))

## Defines the population labels
popLabels <- workData |> 
  dplyr::arrange(desc(race)) |> 
  dplyr::slice(n()-1, n(), .by = race) |> 
  dplyr::mutate(
    x = lag(cumsum(population), default = 0) + population/2,
    .by = year
  ) |> 
  dplyr::mutate(
    pop = scales::label_number(big.mark = ".", decimal.mark = ",")(population),
    pct = scales::label_number(accuracy = 1, suffix = "%")(pct)
  ) |> 
  tidyr::pivot_longer(cols = c(pop, pct)) |> 
  dplyr::mutate(
    color = case_when(race == "brancos" ~ black,
                      year == 2022 & name == "pct" ~ black,
                      TRUE ~ tan),
    size = scales::rescale(population, to = c(9, 25)),
    vjust = ifelse(name == "pct", 1.3, -0.3)
  )

# 2. Plot production ##########
## Creates the title
title <- glue("
<span style='font-size:110px;'>THE BLACK AND WHITE ELEMENTS<br>OF THE BRAZILIAN POPULATION.</span>
<br>{glue::glue_collapse(rep('\U2582', 20))}<br><br>
<span style='font-size:70px;'>LES ÉLÉMENTS NOIR ET BLANC DE LA POPULATION BRÉSILIENNE.</span>
<br>{glue::glue_collapse(rep('\U2582', 20))}<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 \uf16d \uf08c </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes 
</span>
")

## Creates the plot
p <- workData |> 
  ggplot() +
  
  ### Places the area geometries
  geom_area(
    aes(x = population, y = year, fill = race),
    linewidth = 1, color = black,
    outline.type = "upper", orientation = "y"
  ) +
  
  ### Places the year labels
  geom_text(
    aes(x = x, y = y, label = year),
    family = "Teko", size = 20, hjust = 1,
    data = yearLabels
  ) +
  
  ### Places the race labels
  geom_text(
    aes(x = x, y = y, label = race, color = I(color)),
    family = "Teko", size = 15,
    data = raceLabels
  ) +
  
  ### Places the population labels
  geom_text(
    aes(x = x, y = year, label = value,
        color = I(color), size = I(size), vjust = vjust),
    family = "Teko", data = popLabels
  ) +
  
  ### Places the y-axis guide lines
  geom_linerange(aes(xmin = 0, xmax = xmax, y = year),
                 color = tan, linewidth = 2, data = guideY) +
  
  ### Places the title
  labs(title = title) +
  
  ### Maps the color races
  scale_fill_manual(
    values = c("pretos" = black, "pardos" = brown, "brancos" = gold)
  ) +
  
  ### Reverses the scales
  scale_y_reverse(expand = expansion(add = c(2,1))) +
  scale_x_reverse(expand = expansion(mult = c(0.1,0))) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 15, lineheight = 2.5,
      margin = margin(0, 0, 40, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(60, 60, 60, 60),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2024/week06/week06.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
