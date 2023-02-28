# 0. Initial setup ##########
## Loads packages
library(dplyr)
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
red <- palette[6]
blue <- palette[8]
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

## Loads the data. Data downloaded and picked from Table 631 of SIDRA (IBGE).
## It can be downloaded from this IBGE page:
## https://sidra.ibge.gov.br/Tabela/631
df <- readxl::read_xlsx("2023/week10/data.xlsx")


