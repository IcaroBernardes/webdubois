# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(readr)
library(stringr)
library(systemfonts)
library(tidyr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
red <- palette[6]
green <- palette[7]

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

## Loads the data. Data taken from the 
## "Secretaria Nacional de Políticas Penais" section of Brazilian govt. portal:
## https://www.gov.br/depen/pt-br/servicos/sisdepen/relatorios-e-manuais/bases-de-dados
df <- readr::read_csv2("2023/week06/data.csv")

## Filters only prisons that are able to get race data in some way.
## Keeps only variables that show the amount of prisioners by race
races <- df |> 
  dplyr::filter(`5.2 Quantidade de pessoas privadas de liberdade por cor de pele/raça/etnia | O estabelecimento tem condições de obter estas informações em seus registros?` != "Não") |> 
  dplyr::select(matches("cor de pele(.+)Total"), -matches("Masculino|Feminino"))

## Simplifies the variables names
races <- races |>  
  dplyr::rename_with(.fn = ~stringr::str_extract(., "(?<=etnia \\| )[:alpha:]+")) |> 
  dplyr::rename_with(.fn = stringr::str_trim)

## Coalesces and pivots the data
races <- races |>  
  dplyr::filter(!is.na(Total)) |> 
  dplyr::select(-Total) |> 
  dplyr::summarise(across(.fns = sum, na.rm = TRUE)) |> 
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "race_pt",
    values_to = "people"
  )

## Translates names from Portuguese to English and orders the categories
translate <- dplyr::tibble(
  race_pt = c("Indígena", "Preta", "Parda", "Branca", "Amarela", "Não"),
  race = c("Natives", "Blacks", "Blacks", "Whites", "Asians", "Unknown")
) |> 
  dplyr::mutate(race = factor(race, unique(race)))
races <- races |> 
  dplyr::left_join(translate) |> 
  dplyr::select(-race_pt) |> 
  dplyr::group_by(race) |> 
  dplyr::summarise(people = sum(people)) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(race)

## Defines some layout constants
a <- 0.5
e <- 1.5E+05
phi <- pi/8

## Defines coordinates for the corners of the bars
coord0 <- races |> 
  dplyr::mutate(x = NA, y = NA, end_x = NA, end_y = NA)
coord1 <- races |> 
  dplyr::mutate(x = NA, y = NA, end_x = NA, end_y = NA)

for (i in 1:nrow(races)) {
  
  L = races$people[i]
  
  x0 = lag(coord0$end_x, default = 0)[i]
  y0 = lag(coord0$end_y, default = 0)[i]
  x1 = lag(coord1$end_x, default = x0 - e*cos(phi))[i]
  y1 = lag(coord1$end_y, default = y0 + e*sin(phi))[i]
  
  A0 = (1 + a^2)
  B0 = -2*(x0 + a*y0)
  C0 = (x0^2 + y0^2 - L^2)
  
  del0 = B0^2 - 4*A0*C0
  
  xend0 = (-B0 + sqrt(del0))/(2*A0)
  yend0 = a*xend0
  xend1 = xend0 - e*cos(phi)
  yend1 = yend0 + e*sin(phi)
  
  coord0$end_x[i] = xend0
  coord0$end_y[i] = yend0
  coord0$x[i] = x0
  coord0$y[i] = y0
  coord1$end_x[i] = xend1
  coord1$end_y[i] = yend1
  coord1$x[i] = x1
  coord1$y[i] = y1
  
}

## Stacks the coordinates columns
coord0 <- coord0 |> 
  tidyr::pivot_longer(cols = ends_with(c("x", "y")),
                      names_prefix = "end_",
                      names_to = ".value")
coord1 <- coord1 |> 
  tidyr::pivot_longer(cols = ends_with(c("x", "y")),
                      names_prefix = "end_",
                      names_to = ".value")

## Inverts the order of coordinates of the top side of the bars
coord1 <- coord1 |> 
  dplyr::group_by(race) |> 
  dplyr::slice(n():1) |> 
  dplyr::ungroup()

## Joins the coordinates tibbles
coords_joined <- dplyr::bind_rows(coord0, coord1) |> 
  dplyr::arrange(race)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:110px;'>INCARCERATED PEOPLE BY RACE IN BRAZIL (2ND HALF OF 2021).</span>
<br><br>
<span style='font-size:70px;'>PERSONNES INCARCÉRÉES PAR RACE AU BRÉSIL (2ÈME SEMESTRE 2021).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: SENAPPEN AND IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the message
message <- "
<b style='font-size:130px;'>3664</b><br><br>
<span style='font-size:60px;'>
PRISONERS<br>
PER MILLION<br>
OF BLACK<br>
POPULATION.
</span>
"

## Lists colors in order
colors <- c(green, black, brown, gold, red)

## Creates the plot
p <- coords_joined |> 
  ggplot() +
  
  ### Places the bars with borders
  geom_polygon(aes(x = x, y = y, fill = race),
               color = "black", linewidth = 1, key_glyph = "point") +
  
  ggtext::geom_richtext(
    aes(x = 10000, y = 373000, label = message), vjust = 1, family = "Teko",
    size = 6.5, lineheight = 1.5, fill = NA, label.colour = NA
  ) +
  
  ### Places the title
  labs(title = title) +
  
  ### Defines the colors of the lines
  scale_fill_discrete(type = colors, labels = toupper) +
  
  ### Customizes the legend keys and labels
  guides(fill = guide_legend(
    label.position = "left", reverse = TRUE,
    override.aes = list(size = 36, shape = 21, stroke = 3, color = "black")
  )) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 80, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(100, 20, 100, 20),
    
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(0.06, "npc"),
    legend.key.width = unit(0.15, "npc"),
    legend.title = element_blank(),
    legend.text = element_text(size = 50, hjust = 1),
    legend.position = c(0.85, 0.21),
    
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week06/incarcerated.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)