# 0. Initial setup ##########
## Loads packages
library(colorspace)
library(dplyr)
library(ggborderline)
library(ggplot2)
library(ggtext)
library(ggview)
library(junebug)
library(readr)
library(scales)
library(systemfonts)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
tan <- palette[3]
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

## Loads the data. Data scrapped from the INEP page:
## https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior
## Scrapping code is in 2023/week08/scrapping.R
df <- readr::read_csv("2023/week08/data.csv")

# 1. Data handling ##########
## Keeps only data on the number on
## Black and White Graduates on a given year
df <- df |> 
  dplyr::rename("year" = "NU_ANO_CENSO",
                "race" = "RAÇA",
                "conc" = "CONC") |> 
  dplyr::filter(race %in% c("BRANCA", "PRETA", "PARDA")) |> 
  dplyr::select(year, race, conc)

## Joins and translates races names
df <- df |> 
  dplyr::mutate(race = case_when(race == "BRANCA" ~ "WHITE",
                                 TRUE ~ "BLACK")) |> 
  dplyr::group_by(year, race) |> 
  dplyr::summarise(conc = sum(conc)) |> 
  dplyr::ungroup()

## Calculates the growth rate in percents
rate <- df |> 
  dplyr::group_by(race) |> 
  dplyr::mutate(rate = round((conc - lag(conc))/lag(conc), 3),
                label = scales::label_percent()(rate))

## Defines coordinates for the rates
rate <- rate |> 
  dplyr::mutate(year = year - 0.5,
                mid = (conc + lag(conc))/2) |> 
  dplyr::slice(-1L) |> 
  dplyr::ungroup()

## Defines coordinates for the events notes
notes <- dplyr::tibble(
  x = c(2012, 2020),
  y = c(390, 350)*1e+03,
  label = c(
    "HALF OF ADMISSIONS RESERVED<br>TO MINORITIES (BY RACE AND INCOME)",
    "COVID-19 PANDEMIC"
  )
)

## Defines coordinates for the legend texts
leg_text <- dplyr::tibble(
  x = c(2010.8, 2010.5, 2010.0, 2010.25),
  y = c(665, 635, 584, 545)*1e+03,
  size = c(13, 13, 13, 8),
  label = c(
    "WHITE - BLANCHE - ",
    "BLACK - NOIRE -",
    "SMALL FIGURES = PERCENTAGE OF<br>\U2800INCREASE PER YEAR",
    "LES PETITS CHIFFRES INDIQUENT L´ACCROISSEMENT<br>PROPORTIONAL PAR ANS"
  )
)

## Defines coordinates for the legend keys and background
leg_lines <- dplyr::tibble(
  xmin = c(2009.8, 2012.9, 2012.3),
  xmax = c(2013.9, 2013.3, 2012.7),
  y = c(596, 665, 635)*1e+03,
  color = c("bg", "WHITE", "BLACK"),
  linewidth = c(150, 13, 13)
)

# 2. Plot production ##########
## Creates the title
## Creates the title
title <- "
<span style='font-size:120px;'>GRADUATES BY RACE IN BRAZIL (2009-2021).</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:80px;'>DIPLÔMÉS PAR RACE AU BRÉSIL (2009-2021).</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: INEP | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates a function for labeling the y-axis
labeller <- function(breaks) {
  ### Takes the brakes and puts them at the millions scale
  str = scales::label_comma(scale = 1e-03)(breaks)
  
  ### Makes the first and last values empty
  str[c(1, length(str))] = ""
  
  ### Returns the whole vector of strings
  return(str)
}

## Creates the plot
p <- df |> 
  ggplot() +
  
  ### Places the lines
  ggborderline::geom_borderline(
    aes(x = year, y = conc, color = race,
        bordercolour = ggplot2::after_scale(colorspace::darken(color, 0.3))),
    linewidth = 10, borderwidth = 2
  ) +
  
  ### Places the rates
  geom_text(aes(x = year, y = mid, label = label),
            nudge_y = 2e+04, family = "Teko", size = 7, data = rate) +
  
  ### Places the notes
  ggtext::geom_richtext(
    aes(x = x, y = y, label = label), size = 12, angle = -90, 
    fill = tan, label.colour = NA, family = "Teko",
    label.padding = unit(c(0.75, 0.75, 1.75, 0.75), "lines"),
    data = notes
  ) +
  
  ### Places the legend keys and background
  geom_linerange(aes(xmin = xmin, xmax = xmax, y = y, color = color,
                     linewidth = I(linewidth)), data = leg_lines) +
  
  ### Places the legend texts
  ggtext::geom_richtext(
    aes(x = x, y = y, label = label, size = I(size)),
    fill = NA, label.colour = NA, family = "Teko", hjust = 0,
    data = leg_text
  ) +
  
  ### Defines the colors of the lines
  scale_color_discrete(
    type = c("BLACK" = red, "WHITE" = green, "bg" = tan)
  ) +
  
  ### Defines breaks of the scales
  scale_x_continuous(breaks = unique(df$year), expand = expansion()) +
  scale_y_continuous(
    name = "<span style='font-size:80px;'>P</span><span style='font-size:60px;'>ER 1000</span>",
    n.breaks = 8, limits = c(0, 7e+05),
    expand = expansion(), labels = labeller
  ) +
  
  ### Places the title
  labs(title = title, x = NULL) +
  
  ### Eliminates and customizes plot elements
  theme_minimal() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 200, 100, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(80, 80, 80, 80),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "black", linewidth = 2),
    
    legend.position = "none",
    text = element_text(family = "Teko"),
    
    axis.line = element_line(color = "black", linewidth = 2),
    axis.text = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(size = 60, margin = margin(0, 30, 0, 0)),
    axis.text.x = element_text(size = 40, margin = margin(10, 0, 0, 0)),
    axis.title.y = ggtext::element_markdown(
      face = "bold", color = "black", angle = 0, margin = margin(0, -90, 0, 0)
    )
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week08/graduates.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
