# 0. Initial setup ##########
## Loads packages
library(colorspace)
library(dplyr)
library(forcats)
library(ggborderline)
library(ggplot2)
library(ggtext)
library(ggview)
library(junebug)
library(readxl)
library(scales)
library(stringr)
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

## Loads the data. Data from IBGE and downloaded from this page:
## https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&t=downloads
## Data picked from:
## Pesquisa_de_Orcamentos_Familiares_2017_2018 > Primeiros_resultados > tabelas_despesas > tabelas_despesas_xls_20191108.zip
df_exp <- readxl::read_xlsx("2023/week01/data.xlsx", sheet = "expenses")

## Loads the data. Data downloaded and picked from Table 2.10 of the
## "Mercado de trabalho e distribuição de renda" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df_inc <- readxl::read_xlsx("2023/week01/data.xlsx", sheet = "income")

## Gets the average percentage of each type of expenditure across income classes
top <- df_exp |> 
  dplyr::group_by(income_BRL) |> 
  dplyr::summarise(pct = 100*expenditure_BRL/sum(expenditure_BRL),
                   type_en = type_en) |> 
  dplyr::group_by(type_en) |> 
  dplyr::summarise(avg = round(mean(pct))) |> 
  dplyr::arrange(desc(avg))

## Lumps categories together
df_exp <- df_exp |> 
  dplyr::mutate(type = factor(type_en)) |> 
  dplyr::mutate(type = forcats::fct_recode(
    type,
    "Clothing, health and wellness" = "Clothing",
    "Clothing, health and wellness" = "Hygene and self-care",
    "Clothing, health and wellness" = "Health",
    "Clothing, health and wellness" = "Personal services",
    "Other expenses and savings" = "Education",
    "Other expenses and savings" = "Leisure", 
    "Other expenses and savings" = "Smoking",
    "Other expenses and savings" = "Other occasional expenses",
    "Other expenses and savings" = "Investments",
    "Taxes and debts" = "Taxes and other recurrent expenses",
    "Taxes and debts" = "Debt payment"
  ))

## Converts income and expenditure from BRL to USD.
## Mean exchange rates from 2017 and 2018 taken from https://www.exchangerates.org.uk
exc_rt <- (0.3134 + 0.2755)/2






## Creates a function to relabel the income classes of the families
exchanger <- function(string, unit) {
  
  ### Gets the limits of a class
  limits_brl = stringr::str_split_fixed(string, "-", 2) |> 
    stringr::str_extract("[:digit:]+") |> 
    na.exclude()
  
  ### Converts the limits to numeric, applies the exchange rate
  ### (if unit is "USD") and rounds the number
  if (unit == "USD") {
    rate = exc_rt
  } else {
    rate = 1
  }
  limits_unit = as.numeric(limits_brl)
  limits_unit = round(limits_unit*rate)
  
  ### Creates a named character vector to replace the limits
  limits_unit = as.character(limits_unit)
  names(limits_unit) = limits_brl
  
  ### Replaces the limits
  string = stringr::str_replace_all(string, limits_unit)
  
  ###
  
  ### Replaces the symbols with text
  string = stringr::str_replace_all(
    string,
    c("\\(" = "More than ",
      "\\]" = " or less",
      "-" = ", ")
  )
  
}

df_exp <- df_exp |> 
  dplyr::mutate(exp = exc_rt*expenditure_BRL,
                income_USD = exchanger(income_BRL, "USD"),
                income_BRL = exchanger(income_BRL, "BRL"))



