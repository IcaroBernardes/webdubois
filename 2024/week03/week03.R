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

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
pink <- palette[5]
red <- palette[6]
green <- palette[7]
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

## Loads the data. Data downloaded and picked from Table 2.17 of the
## "Padrão de vida e distribuição de rendimentos" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/9221-sintese-de-indicadores-sociais.html?=&t=resultados
rawData <- readxl::read_xlsx("2024/week03/data.xlsx")

# 1. Data handling ##########
## Keeps only data on extreme poverty among Blacks
workData <- rawData |> 
  dplyr::filter(race == "blacks",
                vulnerability == "extreme poverty")

## Converts the years to factor and reverses the order
workData <- workData |> 
  dplyr::mutate(year = factor(year),
                year = forcats::fct_rev(year))

## Highlights the first and last values of the series
highlightData <- workData |> 
  dplyr::slice(c(1, n()))

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:120px;'>PERCENTAGE OF BLACKS IN EXTREME POVERTY.</span>
<br><br>
<span style='font-size:70px;'>VALUES ARE CONVERTED FROM BRL TO USD PPP (PURCHASING POWER PARITY) AS OF 2017.</span><br>
<span style='font-size:70px;'>DATA SHOWS THE PERCENTAGE OF PEOPLE WITH INCOME PER CAPITA BELOW USD 2.15 PPP 2017.</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 \uf16d \uf08c </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes 
</span>
"

## Creates the message
maxWht <- rawData |> 
  dplyr::filter(race == "whites",
                vulnerability == "extreme poverty") |> 
  dplyr::slice_max(pct) |> 
  dplyr::mutate(pct = scales::label_percent(accuracy = 0.01, scale = 1)(pct))
message <- glue::glue("
<span style='font-size:230px;'>{maxWht$pct}</span>
<span style='font-size:80px;'>OF THE WHITES WERE IN EXTREME POVERTY IN {maxWht$year}.</span>
")

## Creates the plot
p <- workData |> 
  ggplot() +
  
  ### Places the bars
  geom_col(aes(x = pct, y = year),
           fill = red, color = black, width = 0.5) +
  
  ### Places the highlighted labels
  geom_text(
    aes(
      x = pct/2, y = year,
      label = scales::label_percent(accuracy = 0.01, scale = 1)(pct)
    ),
    family = "Teko", fontface = "bold",
    size = 70, size.unit = "pt", data = highlightData
  ) +
  
  ### Annotates the message
  annotate(
    "TextBox", x = I(1), y = I(7.8), label = message,
    hjust = 1, vjust = 0, width = unit(0.27, "npc"), size = 15,
     family = "Teko", fill = NA, box.colour = NA, color = brown
  ) +
  
  ### Controls x-axis expansion beyond limits
  scale_x_continuous(expand = expansion(0,0)) +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 120, 40, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(80, 60, 60, 60),
    
    legend.position = "none",
    text = element_text(family = "Teko"),
    
    axis.text.y = element_text(size = 70, margin = margin(0, 20, 0, 0))
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2024/week03/week03.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
