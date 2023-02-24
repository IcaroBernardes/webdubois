# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(ggplot2)
library(ggforce)
library(ggtext)
library(ggview)
library(junebug)
library(purrr)
library(readxl)
library(scales)
library(systemfonts)
library(tidyr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]

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

## Loads the data. Data downloaded and picked from Table 2.5 of the
## "Mercado de trabalho e distribuição de renda" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df <- readxl::read_xlsx("2023/week02/data.xlsx") |> 
  dplyr::filter(year >= 2017)

# 1. Data handling ##########
## Scales the income values to represent circles' radius
df <- df |> 
  dplyr::mutate(r = scales::rescale(real_income, to = c(6, 10)))

## Converts the income values to currency labels
df <- df |> 
  dplyr::mutate(real_income = scales::label_dollar(
    prefix = "R$ ",
    big.mark = " ",
  )(real_income))

## Defines coordinates that will aid the plot construction
df <- df |> 
  dplyr::mutate(y_inf = 0.1*r,
                y_sup = 0.3*r,
                y_t1 = 0.3*r,
                y_t2 = 1*r)

## Defines coordinates of the trapezes
dx1 <- 0.25
dx2 <- 0.7
trap <- df |> 
  dplyr::group_by(year) |> 
  tidyr::nest() |> 
  dplyr::mutate(data = purrr::map(data, function(subdf) {
    r = subdf$r
    y_sup = subdf$y_sup
    y_t1 = subdf$y_t1
    y_t2 = subdf$y_t2
    cx = sqrt(2*r*y_sup - y_sup^2)
    
    dplyr::tibble(
      x1 = c(-cx, cx, dx1*cx, -dx1*cx),
      y1 = c(rep(r-y_sup,2), rep(r+y_t1,2)),
      x2 = c(dx1*cx, -dx1*cx, -dx2*cx, dx2*cx),
      y2 = c(rep(r+y_t1,2), rep(r+y_t2,2))
    )
  })) |> 
  tidyr::unnest(cols = data)

## Defines coordinates of the knots
df <- df |> 
  dplyr::mutate(xk = dx1*sqrt(2*r*y_sup - y_sup^2),
                yk = r+y_t1)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:140px;'>REAL AVERAGE INCOME OF BLACK BRAZILIANS.</span>
<br><br>
<span style='font-size:80px;'>USUAL INCOME FROM MAIN WORK OF THOSE WHO ARE 14 OR OLDER.<br>
VALUES IN AVERAGE 2021 REAIS (BRL).</span>
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
  
  ### Places the circular part of the coin sack
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r), fill = brown, linewidth = 0) +
  
  ### Places rectangles to hide top and bottom of the circle
  geom_rect(aes(xmin = -r, xmax = r, ymin = -r, ymax = -r+y_inf), fill = tan, linewidth = 0) +
  geom_rect(aes(xmin = -r, xmax = r, ymin = r, ymax = r-y_sup), fill = tan, linewidth = 0) +
  
  ### Places pairs of trapezes to create the mouth of the coin sack
  geom_polygon(aes(x = x1, y = y1), fill = brown, linewidth = 0, data = trap) +
  geom_polygon(aes(x = x2, y = y2), fill = brown, linewidth = 0, data = trap) +
  
  ### Places a line to represent a knot on the coin sack
  geom_segment(aes(x = xk, xend = -xk, y = yk, yend = yk), linewidth = 3, lineend = "round") +
  
  ### Places the year and income
  geom_text(aes(x = 0, y = -r, label = year), family = "Teko", size = 20, nudge_y = -3) +
  geom_text(aes(x = 0, y = 0, label = real_income), family = "Teko", size = 13) +
  
  ### Places the title
  labs(title = title) +
  
  ### Expands the y-axis slightly
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  
  ### Makes a facet column by year
  ggforce::facet_col(~year) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, vjust = 0, size = 20, lineheight = 3),
    plot.background = element_rect(fill = tan, color = NA),
    plot.margin = margin(60, 40, 100, 40),
    
    aspect.ratio = 1,
    text = element_text(family = "Teko"),
    strip.text = element_blank()
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week02/income.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
