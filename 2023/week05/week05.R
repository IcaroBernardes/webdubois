# 0. Initial setup ##########
## Loads packages
library(colorspace)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(readxl)
library(scales)
library(stringr)
library(systemfonts)

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
## "Estatísticas" section of the "Galeria do Samba" portal:
## https://galeriadosamba.com.br/V50/CA_Estatisticas.asp?ano=2022&grupo=00
## Udpated to 2023
df <- readxl::read_xlsx("2023/week05/data.xlsx")

## Defines coordinates of the squares (by hand)
nudge <- 5
df <- df |> 
  dplyr::mutate(
    x = c(
      89, #01
      90.5, #02
      57, #03
      54.5, #04
      70, #05
      54, #06
      97, #07
      52, #08
      77, #09
      99, #10
      68, #11
      72, #12
      50.5, #13
      62, #14
      50.5 #15
    ),
    y = c(
      89, #01
      54.5, #02
      52, #03
      95.5, #04
      95.5, #05
      66, #06
      73.5, #07
      86, #08
      46.5, #09
      67, #10
      45.5, #11
      45.5, #12
      75, #13
      99.5, #14
      80 #15
    ),
    xnudge = c(
      +1, #01
      +1, #02
      -1, #03
      -1, #04
      0, #05
      -1, #06
      +1, #07
      -1, #08
      0, #09
      +1, #10
      0, #11
      0, #12
      -1, #13
      0, #14
      -1 #15
    ),
    ynudge = c(
      0, #01
      0, #02
      0, #03
      0, #04
      +1, #05
      0, #06
      0, #07
      0, #08
      -1, #09
      0, #10
      -1, #11
      -1, #12
      0, #13
      +1, #14
      0 #15
    )
  )

## Defines coordinates of the lines (by hand)
df_lines <- df |> 
  dplyr::mutate(
    xend = xnudge*(nudge + championships/2) + x,
    yend = ynudge*(nudge + championships/2) + y,
    hjust = scales::rescale(xnudge, to = c(1,0)),
    vjust = scales::rescale(ynudge, to = c(1,0))
  ) |> 
  dplyr::select(school, x, y, xend, yend, hjust, vjust) |> 
  dplyr::add_row(
    .before = 1L,
    school = c(
      "Unidos da Capela",
      "Império Serrano",
      "Acadêmicos do Grande Rio",
      "Estácio de Sá",
      "Unidos de Vila Isabel",
      "Unidos do Viradouro",
      "Unidos do Viradouro"
    ),
    x = c(
      62,
      70,
      68,
      72,
      77,
      105,
      105
    ),
    y = c(
      105,
      105,
      40,
      40,
      40,
      65,
      67
    ),
    xend = c(
      57,
      75,
      63,
      72,
      82,
      107,
      105
    ),
    yend = c(
      105,
      105,
      40,
      38,
      40,
      65,
      65
    ),
    hjust = c(
      1,
      0,
      1,
      0.5,
      0,
      0,
      0
    ),
    vjust = c(
      0.5,
      0.5,
      0.5,
      1,
      0.5,
      0.5,
      0.5
    )
  )

## Joins data to help building labels
df <- df_lines |>
  dplyr::distinct(school, .keep_all = TRUE) |> 
  dplyr::select(-x, -y) |> 
  dplyr::right_join(df)

## Adds colors to the tibbles
color_table <- dplyr::tibble(
  school = c("Portela", "Estação Primeira de Mangueira",
             "Beija-Flor de Nilópolis", "Acadêmicos do Salgueiro",
             "Império Serrano", "Imperatriz Leopoldinense",
             "Mocidade Independente de Padre Miguel",
             "Unidos da Tijuca", "Unidos de Vila Isabel",
             "Unidos do Viradouro", "Acadêmicos do Grande Rio",
             "Estácio de Sá", "Recreio de Ramos",
             "Unidos da Capela", "Vizinha Faladeira"),
  color = c(blue, pink, blue, red, green, green, green,
            blue, blue, red, green, red, red, blue, blue)
)
df <- df |> dplyr::left_join(color_table)
df_lines <- df_lines |> dplyr::left_join(color_table)

## Adds breaks to the schools names
df <- df |> 
  dplyr::mutate(school = stringr::str_wrap(school, width = 18),
                school = stringr::str_replace_all(school, "\n", "<br>"))

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:130px;'>RIO SAMBA SCHOOLS PARADE WINNERS (1932-2023).</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:80px;'>GAGNANTS DU DÉFILÉ DES ÉCOLES DE SAMBA DE RIO (1932-2023).</span>
<br>\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582\U2582<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: GALERIA DO SAMBA | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the message
message <- "
<span style='font-size:270px;'>15</span><br>
<span style='font-size:85px;'>
\U2800 SAMBA SCHOOLS WON<br>
\U2800\U2800 THE MAIN PARADE<br>
\U2800\U2800\U2800 COMPETITION IN RIO.
</span>
"

## Creates the plot
p <- df |> 
  ggplot() +
  
  ### Places the segments
  geom_segment(aes(
    x = x, xend = xend, y = y, yend = yend, fill = I(color),
    color = ggplot2::after_scale(colorspace::darken(col = fill, amount = 0.2))
  ), linewidth = 2, data = df_lines) +
  
  ### Places the squares
  geom_tile(aes(
    x = x, y = y, width = championships, height = championships, fill = I(color),
    color = ggplot2::after_scale(colorspace::darken(col = fill, amount = 0.2))
  ), linewidth = 2) +
  
  ### Places the labels
  ggtext::geom_richtext(
    aes(x = xend, y = yend, label = school, hjust = hjust, vjust = vjust),
    family = "Teko", fill = NA, label.colour = NA, size = 17, lineheight = 0.9,
    label.margin = unit(c(0.01, 0.01, 0.01, 0.01), "npc")
  ) +
  
  ### Places the message
  ggtext::geom_richtext(
    aes(x = 58, y = 75.5, label = message), family = "Teko",
    hjust = 0, fill = NA, label.colour = NA, color = brown, size = 17
  ) +
  
  ### Defines axes limits and makes their proportions equal
  coord_equal(xlim = c(30, 120), ylim = c(35, 110)) +
  
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
    plot.margin = margin(0, 0, 0, 0),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week05/samba_champions.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
