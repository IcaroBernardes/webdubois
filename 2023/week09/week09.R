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
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
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

## Loads the data. Data downloaded and picked from Table 6.14 of the
## "Participação e gestão" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df <- readxl::read_xlsx("2023/week09/data.xlsx")

# 1. Data handling ##########
## Keeps only data on Black people
df <- df |> dplyr::filter(race == "blacks")

## Calculates the percentage of each range on their group
df <- df |> 
  dplyr::group_by(sex) |> 
  dplyr::mutate(pct = 100*candidates/sum(candidates)) |> 
  dplyr::ungroup()

## Converts the variable "sex" to factor and changes its values
df <- df |> 
  dplyr::mutate(
    sex_fct = factor(sex),
    sex_fct = forcats::fct_relevel(sex_fct, "female", after = Inf),
    sex_fct = forcats::fct_recode(
      sex_fct,
      "<strong style='font-size:55px;'>BOTH SEXES.</strong><br><span style='font-size:50px;'>HOMMES ET FEMMES.</span><br>" = "both",
      "<strong style='font-size:55px;'>MALES.</strong><br><span style='font-size:50px;'>HOMMES.</span><br>" = "male",
      "<strong style='font-size:55px;'>FEMALES.</strong><br><span style='font-size:50px;'>FEMMES.</span><br>" = "female"
    ))

## Exchange rate from 2020 taken from https://www.exchangerates.org.uk
exc_rt <- 0.1958

## Creates a function to relabel the levels of campaign financing
exchanger <- function(brl_string, idiom) {
  
  ### Gets the limits of a class
  limits = stringr::str_split_fixed(brl_string, "-", 2) |> 
    stringr::str_extract("[:digit:]+") |> 
    na.exclude()
  
  ### Converts the limits to numeric, applies the exchange rate and rounds the number
  limits_brl = as.numeric(limits)
  limits_unit = round(limits_brl*exc_rt)
  
  ### Applies scales to the numbers
  limits_brl = scales::label_number()(limits_brl)
  limits_unit = scales::label_number()(limits_unit)
  
  ### Creates a named character vector to replace the limits
  ### Uses \\b to specify entire words limited by boundaries
  names(limits_unit) = glue::glue("\\b{limits}\\b")
  names(limits_brl) = glue::glue("\\b{limits}\\b")
  
  ### Replaces the converted limits
  usd_string = stringr::str_replace_all(brl_string, limits_unit)
  
  ### Binds together the strings with each unit
  string = glue::glue("USD: {usd_string}<br>BRL: {brl_string}")
  
  ### Replaces the original limits
  string = stringr::str_replace_all(string, limits_brl)
  
  ### Replaces the symbols with text in English or French
  if (idiom == "en") {
    replace = c(
      "\\(" = "More than ",
      "\\]" = " or less",
      "-Inf\\)" = "",
      "\\[0-" = "",
      "-" = ", "
    )
  } else {
    replace = c(
      "\\(" = "Plus que ",
      "\\]" = " ou moins",
      "-Inf\\)" = "",
      "\\[0-" = "",
      "-" = ", "
    )
  }
  string = stringr::str_replace_all(string, replace)
  
}

## Converts capital from BRL to USD
df <- df |> 
  dplyr::mutate(
    capital = factor(capital, levels = unique(capital)),
    capital_en = forcats::fct_relabel(capital, exchanger, idiom = "en"),
    capital_fr = forcats::fct_relabel(capital, exchanger, idiom = "fr")
  )

## Defines coordinates for the text legends (English and French)
legend_en <- df |> 
  dplyr::filter(sex == "both") |> 
  dplyr::arrange(desc(capital_en)) |> 
  dplyr::mutate(y = cumsum(pct)-(pct/2)) |> 
  dplyr::select(capital = "capital_en", y) |> 
  dplyr::mutate(x = 0, hjust = 0)
legend_fr <- df |> 
  dplyr::filter(sex == "female") |> 
  dplyr::arrange(desc(capital_fr)) |> 
  dplyr::mutate(y = cumsum(pct)-(pct/2)) |> 
  dplyr::select(capital = "capital_fr", y) |> 
  dplyr::mutate(x = 4, hjust = 1)
legend <- dplyr::bind_rows(legend_en, legend_fr)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:90px;'>FINANCES OF BLACK CANDIDATURES FOR MAYOR BY SEX IN BRAZIL (2020).</span>
<br><span style='font-size:50px;'>VALUES CONVERTED TO US DOLLARS USING THE AVERAGE EXCHANGE RATE OF 2020: 0.1958 USD = 1 BRL.</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:70px;'>FINANCES DES CANDIDATURES NOIRES À LA MAIRE PAR SEXE AU BRÉSIL (2020).</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the plot
p <- df |> 
  ggplot() +
  
  ### Places the bars
  geom_col(aes(x = sex_fct, y = pct, fill = capital),
           color = "black", linewidth = 2, width = 0.4) +
  
  ### Places the text legends
  ggtext::geom_richtext(
    aes(x = x, y = y, label = capital, hjust = hjust), family = "Teko",
    fill = NA, label.colour = NA, size = 7, data = legend
  ) +
  
  ### Defines the colors
  scale_fill_discrete(
    type = c(red, blue, gold, brown, green)
  ) +
  
  ### Puts the x-axis text at the top
  scale_x_discrete(position = "top") +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 150, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(100, 100, 100, 100),
    
    axis.text.x = ggtext::element_markdown(size = 40, lineheight = 1.5),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week09/mayors.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
