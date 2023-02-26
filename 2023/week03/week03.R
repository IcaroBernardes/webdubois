# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(geomtextpath)
library(ggborderline)
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

## Loads the data. Data downloaded and picked from Table 5.5 of the
## "Violência" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df <- readxl::read_xlsx("2023/week03/data.xlsx")

# 1. Data handling ##########
## Collapses the data by race, sex and year
df <- df |> 
  dplyr::group_by(race, sex, year) |> 
  dplyr::summarise(victims = sum(victims)) |> 
  dplyr::ungroup()

## Rearranges the order of the data to make
## the shape of plot similar to the original
df <- df |> dplyr::slice(c(2, 4, 3, 1))

## Defines constants for building the spiral and segments
step <- 0.03
a <- 0
b <- 0.1
start_phi <- 0.7*pi
end_phi <- 3.8*pi

## Calculates the spiral coordinates
df_spiral <- dplyr::tibble(
  phi = seq(start_phi,end_phi, by = step),
  r = a+b*phi,
  x = r*sinpi(phi/pi),
  y = r*cospi(phi/pi)
)

## Calculates the coefficients of the line extension of the spiral
coefs <- df_spiral |> 
  dplyr::slice(n())
coefA <- (b - (a + b*coefs$phi)*tanpi(coefs$phi/pi))/(b*tanpi(coefs$phi/pi) + (a + b*coefs$phi))
coefB <- coefs$y - coefA*coefs$x

## Calculates the coordinates of the line extension of the spiral
df_lines <- dplyr::tibble(
  x = seq(coefs$x, max(df_spiral$x), step),
  y = coefA*x + coefB
)

## Binds together the spiral and its extension
df_spiralize <- df_spiral |> 
  dplyr::select(x, y) |> 
  dplyr::bind_rows(df_lines)

## Calculates the length of spiral and its extension
spiral_extension <- df_lines |> dplyr::slice(c(1,n()))
l_line <- sqrt((spiral_extension$x[2] - spiral_extension$x[1])^2 + (spiral_extension$y[2] - spiral_extension$y[1])^2)
l_spiral <- (b/2)*(end_phi*sqrt(1+end_phi^2) + log(end_phi + sqrt(1+end_phi^2)))
l_spiralize <- l_spiral + l_line

## Rescales the "victims" column to a new range
df <- df |> 
  dplyr::mutate(
    rescaled = scales::rescale(victims, to = c(0.2*l_line, l_spiralize))
  )

## Calculates the coordinates of the other three segments.
## Based on the rescaled "victims" column
### Gets the base of the line - Line #1
x_base <- spiral_extension$x[2]
y_base <- spiral_extension$y[2]
df_line1 <- dplyr::tibble(
  x = x_base,
  y = y_base,
)

### Loops to find the coordinates that make the segment long enough - Line #1
len <- 0
x_temp <- x_base
while (len < df$rescaled[2]) {
  x_temp <- x_temp - step
  b_temp <- y_base + (1/coefA)*x_base
  y_temp <- -(1/coefA)*x_temp + b_temp
  len <- sqrt((x_temp - x_base)^2 + (y_temp - y_base)^2)
}

### Adds the coordinates - Line #1
df_line1 <- df_line1 |> 
  dplyr::add_row(x = x_temp, y = y_temp)

### Gets the base of the line - Line #2
x_base <- df_line1$x[2]
y_base <- df_line1$y[2]
df_line2 <- dplyr::tibble(
  x = x_base,
  y = y_base,
)

### Loops to find the coordinates that make the segment long enough - Line #2
len <- 0
x_temp <- x_base
while (len < df$rescaled[3]) {
  x_temp <- x_temp + step
  b_temp <- y_base - coefA*x_base
  y_temp <- coefA*x_temp + b_temp
  len <- sqrt((x_temp - x_base)^2 + (y_temp - y_base)^2)
}

### Adds the coordinates - Line #2
df_line2 <- df_line2 |> 
  dplyr::add_row(x = x_temp, y = y_temp)

### Gets the base of the line - Line #3
x_base <- df_line2$x[2]
y_base <- df_line2$y[2]
df_line3 <- dplyr::tibble(
  x = x_base,
  y = y_base,
)

### Defines coordinates for the horizontal segment - Line #3
x_temp <- x_base - df$rescaled[4]
y_temp <- y_base

### Adds the coordinates - Line #3
df_line3 <- df_line3 |> 
  dplyr::add_row(x = x_temp, y = y_temp)

## Defines the coordinates of the labels (by hand, unfortunately)
labels <- df |> 
  dplyr::slice(-1L) |> 
  dplyr::select(sex, race, victims) |> 
  dplyr::mutate(
    sex = ifelse(sex == "male", "MEN", "WOMEN"),
    color = ifelse(race == "black", brown, "white"),
    race = toupper(race),
    victims = scales::label_number()(victims)
  ) |> 
  dplyr::mutate(
    x = c(-0.3, -0.4, -1.5),
    y = c(3.0, 4.15, 4.6),
    hjust = c(1, 0, 1),
    vjust = c(0.5, 0.5, 1),
    label = glue::glue(c("<span style='font-size:100px;'>{victims}</span>
                         <br><span style='font-size:50px;'>
                         <span style='color:{color};'>{race} {sex}</span>
                         WERE<br>VICTIMS OF HOMICIDE</span>"))
  )

## Defines the coordinates of the curved labels (kind of by hand)
label_spiral1 <- dplyr::tibble(
  phi = seq(3.11*pi,3.4*pi, by = step),
  r = a+b*phi,
  x = r*sinpi(phi/pi),
  y = r*cospi(phi/pi),
  label = "BLACK MEN"
)
label_spiral2 <- dplyr::tibble(
  phi = seq(2.5*pi,3.15*pi, by = step),
  r = a+b*phi,
  x = r*sinpi(phi/pi),
  y = r*cospi(phi/pi),
  label = "WERE VICTIMS OF HOMICIDE"
)

## Defines the coordinates of the highlighted label (by hand)
highlight_label <- dplyr::tibble(
  x = 0.1,
  y = 0.14,
  label = scales::label_number()(df$victims[1])
)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:140px;'>HOMICIDE VICTIMS BY RACE AND SEX.</span>
<br><br>
<span style='font-size:80px;'>DEATHS IN BRAZIL IN 2020.</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the plot
p <- NULL |> 
  ggplot(aes(x = x, y = y)) + 
  
  ### Places the lines that compose the plot
  ggborderline::geom_borderpath(
    linewidth = 10, lineend = "round", bordercolour = black,
    borderwidth = 2, color = red, data = df_spiralize
  ) +
  ggborderline::geom_borderpath(
    linewidth = 10, lineend = "round", bordercolour = black,
    borderwidth = 2, color = gold, data = df_line1
  ) +
  ggborderline::geom_borderpath(
    linewidth = 10, lineend = "round", bordercolour = black,
    borderwidth = 2, color = blue, data = df_line2
  ) +
  ggborderline::geom_borderpath(
    linewidth = 10, lineend = "round", bordercolour = black,
    borderwidth = 2, color = green, data = df_line3
  ) +
  
  ### Places the labels
  ggtext::geom_richtext(
    aes(label = label, hjust = hjust, vjust = vjust),
    fill = NA, label.colour = NA, family = "Teko", data = labels
  ) +
  
  ### Places the curved labels
  geomtextpath::geom_textpath(
    aes(label = label), size = 15, family = "Teko", text_only = TRUE,
    vjust = -0.5, hjust = 0, color = brown, data = label_spiral1
  ) +
  geomtextpath::geom_textpath(
    aes(label = label), size = 15, family = "Teko", text_only = TRUE,
    vjust = -0.5, hjust = 1, data = label_spiral2
  ) +
  
  ### Places the highlighted label
  geom_text(aes(label = label), size = 35,
            family = "Teko", data = highlight_label) +
  
  ### Expands the x-axis to accommodate the labels
  scale_x_continuous(expand = expansion(mult = 0.6)) +
  
  ### Guarantees that forms won't be distorted
  coord_equal() +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 40, 0)
    ),
    plot.background = element_rect(fill = tan, color = NA),
    plot.margin = margin(60, 40, 100, 40),
    
    text = element_text(family = "Teko")
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week03/victims.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
