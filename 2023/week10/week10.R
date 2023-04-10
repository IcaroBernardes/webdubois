# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(geobr)
library(geogrid)
library(ggnewscale)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(metR)
library(purrr)
library(readxl)
library(santoku)
library(scales)
library(sf)
library(stringi)
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

## Downloads the shapes of the Brazilian states
state <- geobr::read_state()

## Loads the data. Data downloaded and picked from Table 631 of SIDRA (IBGE).
## It can be downloaded from this IBGE page:
## https://sidra.ibge.gov.br/Tabela/631
df <- readxl::read_xlsx("2023/week10/data.xlsx")

# 1. Data handling ##########
## Calculates percentages of population in each home state
df <- df |> 
  dplyr::group_by(home) |> 
  dplyr::mutate(pct = 100*people/sum(people)) |> 
  dplyr::ungroup()

## Keeps only data on migration (to and from) Bahia state
df <- df |> 
  dplyr::filter(home == "Bahia" | born == "Bahia") |> 
  dplyr::filter(!(home == "Bahia" & born == "Bahia"))

## Creates a variable to represent if data
## is about people who live or were born in Bahia
df <- df |> 
  dplyr::mutate(status = ifelse(home == "Bahia", "home", "born"))

## Checks the "pretty breaks" suggested by {santoku}
aux <- df |> dplyr::filter(status == "home")
santoku::chop_pretty(aux$pct, n = 6, drop = FALSE)
aux <- df |> dplyr::filter(status == "born")
santoku::chop_pretty(aux$pct, n = 6, drop = FALSE)

## Creates custom breaks from the ones suggested
home_breaks <- c(seq(0, 1, by = 0.2), Inf)
born_breaks <- c(seq(0, 5, by = 1), Inf)

## Breaks the data into ranges
df <- df |> 
  dplyr::group_by(status) |>
  dplyr::arrange(status) |> 
  tidyr::nest() |> 
  dplyr::ungroup() |> 
  dplyr::mutate(breaks = list(born_breaks, home_breaks)) |> 
  dplyr::mutate(
    data = purrr::map2(
      data, breaks,
      ~.x |> 
        dplyr::mutate(fill = santoku::chop(
          pct, breaks = .y, drop = FALSE, extend = FALSE, close_end = FALSE,
          labels = santoku::lbl_glue(label = "{l}% OR MORE,<br>LESS THAN {r}%", last = "{l}% OR MORE")
        ))
    )
  ) |> 
  dplyr::select(-breaks)

## Separates the data
df_born <- df$data[[1]] ### Born in Bahia
df_home <- df$data[[2]] ### Home in Bahia

## Simplifies the data to only show the other states
df_home <- df_home |> 
  dplyr::select(-home) |> 
  dplyr::rename("name_state" = "born")
df_born <- df_born |> 
  dplyr::select(-born) |> 
  dplyr::rename("name_state" = "home")

## Explore possible hex grids. Makes hex cells based on some seeds
par(mfrow = c(4, 4), mar = c(1, 1, 2, 1))
for (i in 1:16) {
  new_cells <- geogrid::calculate_grid(shape = state, grid_type = "hexagonal", seed = i, learning_rate = 0.5)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

## Generates hex grids based on the chosen seed
new_cells_hex <- geogrid::calculate_grid(shape = state, grid_type = "hexagonal", seed = 13, learning_rate = 0.5)
state_hex <- geogrid::assign_polygons(shape = state, new_polygons = new_cells_hex)

## Keeps only the relevant columns on the shape file
home_hex <- state_hex |> 
  dplyr::select(name_state, abbrev_state, V1, V2, geometry) |> 
  dplyr::rename(ctx = V1, cty = V2)

## Extracts the sfc geometry from the grid and translates it slightly to the north
translate <- 60
born_hex <- home_hex |> 
  sf::st_geometry() |> 
  purrr::map(function(x) {
    x[[1]][,2] = x[[1]][,2] + translate
    return(x)
  })

## Converts the list to an sfc geometry again
born_hex <- sf::st_as_sfc(born_hex)

## Creates another version of the hex grid
born_hex <- sf::st_set_geometry(home_hex, born_hex)
born_hex <- born_hex |> dplyr::mutate(cty = cty + translate)

## Applies the same CRS to the translated grid
crs_grid <- sf::st_crs(home_hex)
sf::st_crs(born_hex) <- crs_grid

## Makes the states names lowercase and without accentuation for all objects
home_hex <- home_hex |> dplyr::mutate(name_state = stringi::stri_trans_general(name_state, "lower; latin-ascii"))
born_hex <- born_hex |> dplyr::mutate(name_state = stringi::stri_trans_general(name_state, "lower; latin-ascii"))
df_home <- df_home |> dplyr::mutate(name_state = stringi::stri_trans_general(name_state, "lower; latin-ascii"))
df_born <- df_born |> dplyr::mutate(name_state = stringi::stri_trans_general(name_state, "lower; latin-ascii"))

## Adds the datasets to the shapes
home_hex <- dplyr::left_join(home_hex, df_home)
born_hex <- dplyr::left_join(born_hex, df_born)

## Gets the coordinates of the center of Bahia's "hex cell"
home_cnt <- home_hex |> dplyr::filter(abbrev_state == "BA")
home_cnt <- c(ctx = home_cnt$ctx, cty = home_cnt$cty)
born_cnt <- born_hex |> dplyr::filter(abbrev_state == "BA")
born_cnt <- c(ctx = born_cnt$ctx, cty = born_cnt$cty)

## Creates labels for the states
home_labels <- home_hex |> 
  dplyr::mutate(label = glue::glue("{abbrev_state}: {scales::label_number(accuracy = 0.01)(pct)}%"),
                label = ifelse(abbrev_state == "BA", "BA", label),
                lbly = case_when(abbrev_state == "BA" ~ 0,
                                 cty >= home_cnt["cty"] ~ 1,
                                 TRUE ~ -1),
                color = ifelse(as.numeric(fill) >= 3 | is.na(pct), "white", "black")) |> 
  dplyr::select(ctx, cty, label, lbly, color)
sf::st_geometry(home_labels) <- NULL
born_labels <- born_hex |> 
  dplyr::mutate(label = glue::glue("{abbrev_state}: {scales::label_number(accuracy = 0.01)(pct)}%"),
                label = ifelse(abbrev_state == "BA", "BA", label),
                lbly = case_when(abbrev_state == "BA" ~ 0,
                                 cty >= born_cnt["cty"] ~ 1,
                                 TRUE ~ -1),
                color = ifelse(as.numeric(fill) >= 3 | is.na(pct), "white", "black")) |> 
  dplyr::select(ctx, cty, label, lbly, color)
sf::st_geometry(born_labels) <- NULL

## Defines the angle of the arrows
home_arw <- home_labels |> 
  dplyr::mutate(d = sqrt((cty - home_cnt["cty"])^2 + (ctx - home_cnt["ctx"])^2),
                sin = (home_cnt["cty"] - cty)/d,
                sign = sign(home_cnt["ctx"] - ctx),
                angle = 180*asin(sin)/pi,
                angle = sign*(angle+90)) |> 
  dplyr::select(ctx, cty, angle, color)
born_arw <- born_labels |> 
  dplyr::mutate(d = sqrt((cty - born_cnt["cty"])^2 + (ctx - born_cnt["ctx"])^2),
                sin = (born_cnt["cty"] - cty)/d,
                sign = sign(born_cnt["ctx"] - ctx),
                angle = 180*asin(sin)/pi,
                angle = sign*(angle+90)) |> 
  dplyr::select(ctx, cty, angle, color)

## Defines the coordinates for the legend
legend <- dplyr::tibble(
  x = seq(-90, -17, length.out = 6),
  y = 14,
  born = levels(df_born$fill),
  home = levels(df_home$fill)
)

# 2. Plot production ##########
## Creates the title
title <- "
<span style='font-size:120px;'>MIGRATION IN THE BRAZILIAN STATES.</span>
<br><br>
<span style='font-size:80px;'>PERCENTAGE OF POPULATION FROM STATES BORN OR LIVING IN BAHIA (2010).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf08c </span>@icarobsc
</span>
"

## Creates the plot
p <- ggplot() +
  
  ### Places elements of the upper map (born in Bahia)
  #### Places the hex grid
  geom_sf(aes(fill = fill), color = black, linewidth = 1.5, data = born_hex) +
  
  #### Defines the fill scale
  scale_fill_discrete(
    type = c(gold, pink, red, brown,green, blue), na.value = black
  ) +
  
  #### Places the states labels
  geom_text(aes(x = ctx, y = cty + lbly, label = label, color = I(color)),
            family = "Teko", size = 6, data = born_labels) +
  
  #### Places the states arrows
  metR::geom_arrow(aes(x = ctx, y = cty, angle = angle, color = I(color), mag = I(1)),
                   pivot = 1, start = 90, data = born_arw) +
  
  #### Places the title of the hex grid
  annotate(
    "text", x = -53.5, y = 68, family = "Teko", size = 15, lineheight = 0.7,
    label = "PERCENTAGE OF POPULATION OF EACH\nSTATE THAT WAS BORN IN BAHIA."
  ) +
  
  ### Allows the application of a new fill scale
  ggnewscale::new_scale_fill() +
  
  ### Places elements of the lower map (home in Bahia)
  #### Places the hex grid
  geom_sf(aes(fill = fill), color = black, linewidth = 1.5, data = home_hex) +
  
  #### Defines the fill scale
  scale_fill_discrete(
    type = c(gold, pink, red, brown,green, blue), na.value = black
  ) +
  
  #### Places the states labels
  geom_text(aes(x = ctx, y = cty + lbly, label = label, color = I(color)),
            family = "Teko", size = 6, data = home_labels) +
  
  #### Places the states arrows
  metR::geom_arrow(aes(x = ctx, y = cty, angle = angle, color = I(color), mag = I(1)),
                   pivot = 0, start = -90, data = home_arw) +
  
  #### Places the title of the hex grid
  annotate(
    "text", x = -53.5, y = -40, family = "Teko", size = 15, lineheight = 0.7,
    label = "PERCENTAGE OF POPULATION LIVING IN\nBAHIA THAT WAS BORN IN EACH STATE."
  ) +
  
  ### Places the legend points
  geom_point(aes(x = x, y = y, fill = home), color = black,
             stroke = 1.5, shape = 21, size = 20, data = legend) +
  
  ### Places the legend labels
  ggtext::geom_richtext(
    aes(x = x, y = y, label = home), fill = NA, label.colour = NA,
    family = "Teko", size = 10, lineheight = 1,
    vjust = 1, nudge_y = -2.5, data = legend
  ) +
  ggtext::geom_richtext(
    aes(x = x, y = y, label = born), fill = NA, label.colour = NA,
    family = "Teko", size = 10, lineheight = 1,
    vjust = 0, nudge_y = 2.5, data = legend
  ) +
  
  ### Defines limits for the x-axis
  coord_sf(xlim = c(-92, -15)) +
  
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
    plot.margin = margin(70, 50, 50, 50),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  ) 

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2023/week10/migration.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
