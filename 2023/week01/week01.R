# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(ggview)
library(junebug)
library(metR)
library(readxl)
library(scales)
library(shadowtext)
library(stringr)
library(systemfonts)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
pink <- palette[5]
red <- palette[6]
blue <- palette[8]
purple <- palette[9]

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
## Pesquisa_de_Orcamentos_Familiares_2017_2018 > Primeiros_resultados > tabelas_despesas > tabelas_despesas_xls_20191108.zip > Tab 1.1.1 até 1.1.6.xls.xlsx
df_exp <- readxl::read_xlsx("2023/week01/data.xlsx", sheet = "expenses")

## Loads the data. Data downloaded and picked from Table 2.10 of the
## "Mercado de trabalho e distribuição de renda" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df_inc <- readxl::read_xlsx("2023/week01/data.xlsx", sheet = "income")

# 1. Data handling ##########
## Average exchange rates from 2017 and 2018 taken from https://www.exchangerates.org.uk
exc_rt <- (0.3134 + 0.2755)/2

## Creates a function to relabel and convert (or not) the levels of capital
exchanger <- function(brl_string, convert) {
  
  ### Gets the limits of a class
  limits = stringr::str_split_fixed(brl_string, "-", 2) |> 
    stringr::str_extract("[:digit:]+") |> 
    na.exclude()
  
  ### Converts the limits to numeric,
  ### applies the exchange rate (if needed) and rounds the number
  limits_unit = as.numeric(limits)
  if (convert) {limits_unit = round(limits_unit*exc_rt)}
  
  ### Applies scales to the numbers
  limits_unit = scales::label_number()(limits_unit)
  
  ### Creates a named character vector to replace the limits
  ### Uses \\b to specify entire words limited by boundaries
  names(limits_unit) = glue::glue("\\b{limits}\\b")
  
  ### Replaces the converted limits
  string = stringr::str_replace_all(brl_string, limits_unit)
  
  ### Replaces the symbols with text
  replace = c(
    "\\(" = "More than ",
    "\\]" = " or less",
    "-Inf\\)" = "",
    "\\[0-" = "",
    "-" = "<br>"
  )
  string = stringr::str_replace_all(string, replace)
  
}

## Converts income and expenditure from BRL to USD
df_exp <- df_exp |> 
  dplyr::mutate(expenditure = exc_rt*expenditure_BRL,
                income_USD = exchanger(income_BRL, convert = TRUE),
                income_BRL = exchanger(income_BRL, convert = FALSE),
                income_BRL = factor(income_BRL, levels = rev(unique(income_BRL))))

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
  )) |> 
  dplyr::group_by(type, income_BRL, income_USD) |> 
  dplyr::summarise(expenditure = sum(expenditure)) |> 
  dplyr::ungroup()

## Reorders the categories
df_exp <- df_exp |> 
  dplyr::mutate(
    type = forcats::fct_relevel(
      type,
      c("Other expenses and savings", "Taxes and debts",
        "Clothing, health and wellness", "Alimentation",
        "Housing", "Transportation")
    )
  )

## Calculates the percent of expenditure for each category
df_exp <- df_exp |>
  dplyr::group_by(income_BRL) |> 
  dplyr::mutate(pct = 100*expenditure/sum(expenditure)) |> 
  dplyr::ungroup()

## Defines the coordinates of the expenditure labels
label_exp <- df_exp |>
  dplyr::group_by(income_BRL) |> 
  dplyr::arrange(desc(type)) |> 
  dplyr::mutate(x = cumsum(pct)-(pct/2),
                label = scales::label_number(accuracy = 1, suffix = "%")(pct)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(color = ifelse(type %in% c("Transportation", "Housing"),
                               tan, black)) |> 
  dplyr::select(x, income_BRL, label, color)

## Defines coordinates of the "income table"
table_inc <- dplyr::tibble(
  xmin = -40,
  xmid = -22,
  xmax = -4,
  ymin = c(1:7 - 0.5, 7.5),
  ymax = c(1:7 + 0.5, 7.8)
)

## Defines coordinates of the "expenditure table"
table_exp <- dplyr::tibble(
  ymin = 8.2,
  ymid = 8.4,
  ymax = 9.3,
  xmin = seq(0, 100, length.out = 7)[-7],
  xmax = seq(0, 100, length.out = 7)[-1],
  type = rev(levels(df_exp$type))
) |> 
  dplyr::mutate(label = toupper(type),
                label = stringr::str_wrap(label, width = 14))

## Defines coordinates of the bars of distribution across income classes
inc_prop <- dplyr::tibble(
  race = c("blacks", "whites"),
  vjust = c(0, 1),
  sign = c(1, -1),
  color = c(red, gold),
  text = c("white","black")
)
df_inc <- df_inc |> 
  dplyr::left_join(inc_prop) |> 
  dplyr::mutate(income_class = factor(income_class, levels = unique(income_class)),
                x = as.numeric(income_class),
                x = scales::rescale(x, to = c(-35, 95)),
                ymin = -4,
                ymax = ymin + sign*pct_pop/6,
                label = scales::label_number(accuracy = 0.01, suffix = "%")(pct_pop))

# 2. Plot production ##########
## Creates the title and subtitle
title <- "
<span style='font-size:100px;'>INCOME AND EXPENDITURE OF BRAZILIAN FAMILIES (2017-2018).</span>
<br><br>
<span style='font-size:40px;'>ALL VALUES CONVERTED FROM BRAZIL REAL TO US DOLLARS USING THE AVERAGE EXCHANGE RATE OF 2017 (0.3134 USD) AND 2018 (0.2755 USD).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

subtitle <- "
<span style='font-size:70px;'>POPULATION DISTRIBUTION ACROSS INCOME CLASSES BY RACE (2018).</span>
"

## Creates the plot
p <- df_exp |> 
  ggplot() +
  
  ### Places the expenditure bars elements
  #### Places the bars
  geom_col(aes(x = pct, y = income_BRL, fill = type), color = "black", width = 0.45) +
  
  #### Places the expenditure labels
  geom_text(aes(x = x, y = income_BRL, label = label, color = I(color)),
            size = 12, family = "Teko", data = label_exp) +
  
  ### Places the "income table" elements
  #### Places the "income table cells"
  geom_rect(aes(xmin = xmin, xmax = xmid, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", data = table_inc) +
  geom_rect(aes(xmin = xmid, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", data = table_inc) +
  
  #### Places the "income arrows"
  metR::geom_arrow(aes(x = xmax, y = ymin, dx = 2, dy = 0), pivot = 0,
                   arrow.type = "open", arrow.length = 0.8, data = table_inc) +
  
  #### Places the income labels
  ggtext::geom_richtext(
    aes(x = -39, y = income_BRL, label = toupper(income_BRL)), size = 9, family = "Teko",
    hjust = 0, fill = NA, label.colour = NA
  ) +
  ggtext::geom_richtext(
    aes(x = -21, y = income_BRL, label = toupper(income_USD)), size = 9, family = "Teko",
    hjust = 0, fill = NA, label.colour = NA
  ) +
  
  #### Places the income currency
  annotate("text", x = -31, y = 7.65, label = "BRL",
           size = 10, family = "Teko") +
  annotate("text", x = -13, y = 7.65, label = "USD",
           size = 10, family = "Teko") +
  
  #### Places the "income table title"
  annotate("rect", xmin = -40, xmax = -4, ymin = 7.8, ymax = 8.2,
           fill = NA, color = "black") +
  annotate("text", x = -22, y = 8.0, label = "MONTHLY TOTAL INCOME",
           size = 14, family = "Teko") +
  
  ### Places the "expenditure table" elements
  #### Places the "expenditure table cells"
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymid, fill = type),
            color = "black", data = table_exp) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymid, ymax = ymax),
            fill = NA, color = "black", data = table_exp) +
  
  #### Places the labels for the expenditure types
  geom_text(aes(x = (xmin + xmax)/2, y = (ymid + ymax)/2, label = label),
            size = 9, family = "Teko", lineheight = 0.8, data = table_exp) +
  
  #### Places the "expenditure table title"
  annotate("rect", xmin = 0, xmax = 100, ymin = 9.3, ymax = 9.7,
           fill = NA, color = "black") +
  annotate("text", x = 50, y = 9.5, label = "MONTHLY EXPENDITURE FOR",
           size = 14, family = "Teko") +
  
  ### Places the "further statistics" label and line decoration
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.1,
           fill = "#c7a77a", color = NA) +
  annotate("segment", x = -Inf, xend = Inf, y = 0.1, yend = 0.1, linetype = "dashed") +
  annotate("text", x = 40, y = 0.2, size = 12, family = "Teko", vjust = 0,
           label = "FOR FURTHER STATISTICS RAISE THIS FRAME.") +
  
  ### Places the subtitle
  ggtext::geom_richtext(
    aes(x = 30, y = -0.6, label = subtitle),
    family = "Teko", fill = NA, label.colour = NA
  ) +
  
  ### Places the income classes "bars"
  geom_linerange(aes(x = x, ymin = ymin, ymax = ymax, color = I(color)),
                 linewidth = 45, lineend = "round", data = df_inc) +
  
  ### Places the income classes "labels"
  geom_point(aes(x = x, y = ymin), size = 45, data = df_inc) +
  geom_text(aes(x = x, y = ymin, label = income_class), color = "white",
            size = 7, family = "Teko", data = df_inc) +
  
  ### Places the income classes distribution
  geom_text(aes(x = x, y = ymax, label = label, vjust = vjust, color = I(text)),
            size = 9, family = "Teko", data = df_inc) +
  
  ### Places the races labels of the bars
  shadowtext::geom_shadowtext(
    aes(x = -38, y = -6.8, label = "WHITES."), hjust = 0,
    color = gold, size = 20, family = "Teko", bg.colour = "black"
  ) +
  shadowtext::geom_shadowtext(
    aes(x = 98, y = -1.6, label = "BLACKS."), hjust = 1,
    color = red, size = 20, family = "Teko", bg.colour = "white"
  ) +
  
  ### Defines the colors
  scale_fill_discrete(
    type = c(tan, blue, pink, purple, black, brown)
  ) +
  
  ### Expands the y-axis slightly
  scale_y_discrete(expand = expansion(mult = c(1.5,0))) +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 50, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(70, 0, 0, 0),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
# ggview::ggview(p, device = "png", dpi = 320,
#                units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week01/expenses.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
