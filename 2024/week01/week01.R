# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(geobr)
library(ggbeeswarm)
library(ggplot2)
library(ggtext)
library(ggview)
library(junebug)
library(patchwork)
library(purrr)
library(readxl)
library(rmapshaper)
library(santoku)
library(scales)
library(systemfonts)
library(tidyr)

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

## Loads the data. Data downloaded from SIDRA:
## https://sidra.ibge.gov.br/tabela/9605
rawData <- readxl::read_xlsx("2024/week01/data.xlsx", sheet = "Tabela", skip = 4)

## Gets the shapes of Bahia immediate regions
shapes <- geobr::read_immediate_region(year = 2020, code_immediate = "BA")

# 1. Data handling ##########
## Renames the columns directly
colnames(rawData) <- c("region", "pretos_2010", "pardos_2010",
                       "pretos_2022", "pardos_2022")

## Removes footnotes
workData <- rawData |> 
  dplyr::filter(!is.na(pretos_2010))

## Stacks the percentages and separates years and colors
workData <- workData |> 
  tidyr::pivot_longer(cols = -region,
                      names_sep = "_",
                      names_to = c("color", "year"),
                      values_to = "pct")

## Sums the groups that comprise the Black Brazilians
workData <- workData |> 
  dplyr::summarise(
    pct = sum(pct),
    .by = c(region, year)
  )

## Creates a key from the regions' names in both datasets
workData <- workData |> 
  dplyr::mutate(key = stringi::stri_trans_general(region, "latin-ascii;lower"),
                key = stringr::str_remove_all(key, "[:space:]|[:punct:]"))
shapes <- shapes |> 
  dplyr::transmute(key = stringi::stri_trans_general(name_immediate, "latin-ascii;lower"),
                   key = stringr::str_remove_all(key, "[:space:]|[:punct:]"))

## Plots the percentages as a jitter plot
jitter <- workData |> 
  ggplot() +
  ggbeeswarm::geom_beeswarm(
    aes(x = pct, y = "region", group = region), method = "center", cex = 3, side = 1L
  )
print(jitter)

## Adds vertical lines that indicate the
## limits of the percentage ranges
categ_lim <- seq(60, 85, 5)
jitter <- jitter + 
  geom_vline(xintercept = categ_lim, color = "red", linetype = "dashed")
print(jitter)

## Divides the percentages into groups of equal widths
workData <- workData |> 
  dplyr::mutate(category = santoku::chop(
    pct, breaks = categ_lim,
    labels = santoku::lbl_glue(
      label = "MORE THAN {l}%, {r}% OR LESS",
      first = "{r}% OR LESS",
      last = "MORE THAN {l}%",
      fmt = scales::label_number()
    ),
    extend = TRUE, left = FALSE
  ))

## Simplifies the shapes
shapes <- rmapshaper::ms_simplify(shapes, keep = 0.05)

## Creates a contour of the state by uniting the lands
## and dissolving their internal borders
bahia <- rmapshaper::ms_dissolve(shapes)

## Merges the datasets
workData <- dplyr::left_join(shapes, workData)

## Defines the coordinates of the legend
legend <- dplyr::tibble(
  x = c(rep(0.05, 4), rep(0.55, 3)),
  y = c(seq(0.1, 0.4, by = 0.1),
        seq(0.72, 0.92, by = 0.1)),
  breaks = levels(workData$category),
  values = c(green, gold, pink, red, tan, brown, blue)
)

# 2. Plot production ##########
## Creates a function that generates the insets
insetMaker <- function(value) {
  
  p <- workData |> 
    dplyr::filter(year == value) |> 
    ggplot() +
    
    ### Places the contour of Bahia
    geom_sf(fill = NA, color = black, linewidth = 2, data = bahia) +
    
    ### Places the shapes
    geom_sf(aes(fill = category), color = black,
            linewidth = 0.7, linetype = "twodash") +
    
    ### Defines the colors of the shapes
    scale_fill_manual(
      values = legend$values,
      breaks = legend$breaks,
      guide = "none"
    ) +
    
    ### Strips of all elements
    theme_void()
  
  ### Save on the global envir.
  assign(glue::glue("inset{value}"), p, envir = .GlobalEnv)
  
}

## Uses purrr to create the plots
purrr::walk(c(2010,2022), insetMaker)

## Creates the title
title <- "
<span style='font-size:95px;'>RATIO OF BLACK POPULATION IN BAHIA BY IMMEDIATE REGION.</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 \uf16d \uf08c </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes 
</span>
"

## Creates the plot
p <- NULL |> 
  ggplot() +
  
  ### Defines the plot limits
  xlim(0,1) +
  ylim(0,1) +
  
  ### Places the legend points
  geom_point(aes(x = x, y = y, fill = I(values)), shape = 21, color = black,
             size = 30, stroke = 1.5, data = legend) +
  
  ### Places the legend labels
  geom_text(
    aes(x = x, y = y, label = breaks), family = "Teko",
    size = 15, nudge_x = 0.05, hjust = 0, data = legend
  ) +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 40, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(60, 10, 10, 10),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) +
  
  ### Places the years labels
  annotate(
    "text", x = c(0.2, 0.75), y = c(0.99, 0.5),
    label = c(2010,2022), family = "Teko", size = 25,
    vjust = 1
  ) +
  
  ### Places the inset plots
  patchwork::inset_element(
    inset2010,
    0, 0.5, 0.5, 0.95
  ) +
  patchwork::inset_element(
    inset2022,
    0.5, 0.05, 1, 0.5
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2024/week01/week01.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
