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
brown <- palette[2]
tan <- palette[3]
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

## Translates names from Portuguese to English
translate <- dplyr::tibble(
  race_pt = c("Branca", "Preta", "Parda", "Amarela", "Indígena", "Não"),
  race = c("Whites", "Blacks", "Blacks", "Asians", "Natives", "Unknown")
)
races <- races |> 
  dplyr::left_join(translate) |> 
  dplyr::select(-race_pt) |> 
  dplyr::group_by(race) |> 
  dplyr::summarise(people = sum(people)) |> 
  dplyr::ungroup()




# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:140px;'>DISTRIBUTION OF BLACKS IN BRAZIL (2021).</span>
<br><br>
<span style='font-size:80px;'>DISTRIBUTION DES NOIRS AU BRÉSIL (2021).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: SENAPPEN AND IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"