# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(ggforce)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(purrr)
library(readxl)
library(scales)
library(stringr)
library(systemfonts)
library(tidyr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
red <- palette[6]
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

## Loads the data. Data downloaded and picked from Tables 4.7 and 4.8 of the
## "Educação" section of this IBGE page:
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
df <- readxl::read_xlsx("2023/week07/data.xlsx")

# 1. Data handling ##########
## Sums all enrollments from each course field
tops <- df |> 
  dplyr::group_by(course) |> 
  dplyr::summarise(enrollments = sum(enrollments)) |> 
  dplyr::arrange(desc(enrollments))

## Lists new categories to simplify the data
categories <- dplyr::tibble(
  course = c(
    "Engineering",
    "Agriculture, forestry and fishing",
    "Computer sciences",
    "Math and Natural sciences",
    "Social sciences",
    "Arts and humanities",
    "Law and Business",
    "Health and wellness",
    "Education",
    "Services",
    "Basic programs"
  ),
  broad_field = c(
    "Engineering & production, computer and natural sciences",
    "Engineering & production, computer and natural sciences",
    "Engineering & production, computer and natural sciences",
    "Engineering & production, computer and natural sciences",
    "Arts, social and human sciences",
    "Arts, social and human sciences",
    "Law and Business",
    "Health and wellness",
    "Education and others",
    "Education and others",
    "Education and others"
  )
)

## Aggregates data by broad field and race
df <- df |> 
  dplyr::left_join(categories) |> 
  dplyr::group_by(race, broad_field) |> 
  dplyr::summarise(enrollments = sum(enrollments)) |> 
  dplyr::ungroup()

## Arranges the fields by total enrollments
df <- df |> 
  dplyr::group_by(broad_field) |> 
  dplyr::mutate(total_enroll = sum(enrollments)) |> 
  dplyr::arrange(desc(enrollments)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(broad_field = factor(broad_field, levels = unique(broad_field)))

## Calculates percentages and cumulative percentages of enrollment by race
df <- df |> 
  dplyr::arrange(race, broad_field) |> 
  dplyr::group_by(race) |> 
  dplyr::mutate(pct = enrollments/sum(enrollments),
                cumpct = cumsum(enrollments)/sum(enrollments)) |> 
  dplyr::ungroup()

## Defines the angles that limit the arc sections
arc <- pi/3
A_min_top <- 0 - arc
A_max_top <- 0 + arc
A_min_bot <- pi - arc
A_max_bot <- pi + arc

## Adds the angles limits to the data
angular <- dplyr::tibble(
  race = c("blacks", "whites"),
  A_min = c(A_min_top, A_min_bot),
  A_max = c(A_max_top, A_max_bot)
)
df <- df |> dplyr::left_join(angular)

## Defines limits for each arc section
## that represents each course enrollment by race
df <- df |> 
  dplyr::group_by(race) |> 
  dplyr::mutate(start = A_min + (A_max-A_min)*lag(cumpct, default = 0),
                end = A_min + (A_max-A_min)*cumpct) |> 
  dplyr::ungroup()

## Defines the radius of the arcs and the radius of the percentage labels
R <- 10
r <- 9

## Defines coordinates for the percentages
df <- df |> 
  dplyr::mutate(mid_angle = (start + end)/2,
                x_pct = r*sin(mid_angle),
                y_pct = r*cos(mid_angle),
                label = scales::label_percent(accuracy = 1)(pct),
                color = rep(c("white","black","white","black","white"), 2))

## Defines coordinates for the legend (by hand)
legend <- dplyr::tibble(
  broad_field = c(
    "Law and Business",
    "Education and others",
    "Engineering & production, computer and natural sciences",
    "Arts, social and human sciences",
    "Health and wellness"
  ),
  x = c(rep(-9.2, 2), rep(9.2, 3)),
  y = c(1, -1, 2, 0, -2),
  hjust = c(0, 0, 1, 1, 1)
) |> 
  dplyr::mutate(
    label = toupper(broad_field),
    label = stringr::str_wrap(label, width = 18),
    label = stringr::str_replace_all(label, "\n", "<br>"),
    label = glue::glue("{label}.")
  )

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:120px;'>HIGHER EDUCATION ENROLLMENTS BY RACE AND FIELD.</span>
<br><br>
<span style='font-size:80px;'>ENROLLMENTS ON REMOTE AND IN PERSON COURSES IN BRAZIL (2020).</span>
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
  
  ### Places the arcs
  ggforce::geom_arc_bar(
    aes(x0 = 0, y0 = 0, r0 = 0, r = R,
        start = start, end = end, fill = broad_field), linewidth = 1.5
  ) +
  
  ### Places the percentage labels
  geom_text(aes(x = x_pct, y = y_pct, label = label, color = I(color)),
            family = "Teko", size = 15) +
  
  ### Places the races names
  annotate("richtext", x = c(0,0), y = c(10.3,-10.3),
           label = c("BLACKS.","WHITES."), vjust = c(0,1),
           fill = NA, label.colour = NA, family = "Teko", size = 22) +
  
  ### Places the legend keys and labels
  geom_point(aes(x = x, y = y, fill = broad_field),
             shape = 21, stroke = 1.5, size = 30, data = legend) +
  ggtext::geom_richtext(
    aes(x = x, y = y, label = label, hjust = hjust), family = "Teko",
    size = 12, lineheight = 1, fill = NA, label.colour = NA, 
    label.margin = unit(c(0, 0.04, 0, 0.04), "npc"), data = legend
  ) +
  
  ### Defines the colors
  scale_fill_discrete(
    type = c(red, gold, blue, tan, brown)
  ) +
  
  ### Places the title
  labs(title = title) +
  
  ### Makes the axes have the same scale in order to not distort the arcs
  coord_equal() +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 100, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(100, 0, 100, 0),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week07/enrollments.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
