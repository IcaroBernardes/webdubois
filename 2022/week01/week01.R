# 0. Library and fonts management
library(tidyverse)
library(readxl)
library(scales)
library(showtext)
library(geomtextpath)
library(ggtext)
library(ggbump)
library(ggfx)
library(sf)
library(rnaturalearth)
library(rmapshaper)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"
sysfonts::font_add_google(name = "Noto Sans Symbols 2", family = "Noto Sans Symbols 2") ### Symbols
symbols <- "Noto Sans Symbols 2"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 1.1 ### To set the lineheight
bgcolor <- "#d2b48c"
r <- 45 ### Radius of the mask circles
x0_br <- -73 ### Long. of the center of BR circle
y0_br <- 0 ### Lat. of the center of BR circle
x0_af <- 17 ### Long. of the center of AF circle
y0_af <- 0 ### Lat. of the center of AF circle

# 1. Data download, load and handling
## Data on enslaved Blacks comes from the Slave Voyages Project.
## Query made in https://www.slavevoyages.org/voyage/database filtering only
## travels with purchases in Africa and landings in Brazil
rawdata <- readxl::read_xlsx("2022/week01/data.xlsx", sheet = "Data")
br_regions <- readxl::read_xlsx("2022/week01/data.xlsx", sheet = "Brazil")
af_regions <- readxl::read_xlsx("2022/week01/data.xlsx", sheet = "Africa")

## Adds geographic coordinates to the African regions
## (based on the maps of the Slave Voyages Project)
af_coord <- tibble(
  region = unique(af_regions$region_AF),
  name = c("Gold Coast", "Other Africa", "West Central Africa",
           "Bight of Benin", "Bight of Biafra", "Senegambia",
           "Windward Coast", "Southeast Africa", "Sierra Leone"),
  y = c(3.60, 13.14, -7.78, 5.88, 3.93, 14.36, 4.16, -17.26, 8.19),
  x = c(-1.52, 24.27, 11.99, 3.45, 8.60, -17.61, -8.21, 39.08, -13.32)
)

## Adds geographic coordinates to the Brazilian regions
## (based on the maps of the Slave Voyages Project)
br_coord <- tibble(
  region = unique(br_regions$region_BR),
  name = region,
  y = c(-1.86, -12.60, -8.06, -23.03, -10.56),
  x = c(-43.49, -40.50, -36.00, -41.92, -51.17)
)

## Binds the coordinates tibbles
coord <- rbind(af_coord,br_coord)

## Unites the African id info. and the enslaved people data.
## Keeps columns only ofn the number of embarked
df <- rawdata %>% 
  dplyr::left_join(af_regions) %>% 
  dplyr::select(-c("id_AF","Embarked_Total","port_AF"), -starts_with("Disembarked"))

## Sums the data for each African region
df <- df %>% 
  dplyr::group_by(region_AF) %>% 
  dplyr::summarise(across(.fns = sum)) %>% 
  dplyr::ungroup() 

## Arranges all embarked people data in a few columns
df <- df %>% 
  tidyr::pivot_longer(cols = starts_with("Embarked"),
                      names_prefix = "Embarked_",
                      names_to = "id_BR",
                      values_to = "people") %>% 
  dplyr::mutate(id_BR = as.numeric(id_BR))

## Unites the Brazilian id info. and the enslaved data
df <- df %>% 
  dplyr::left_join(br_regions) %>% 
  dplyr::select(-c("id_BR","port_BR"))

## Sums the data for each Brazilian region
df <- df %>% 
  dplyr::group_by(region_AF, region_BR) %>% 
  dplyr::summarise(people = sum(people)) %>% 
  dplyr::ungroup() 

## Eliminates lines with zeros
df <- df %>% dplyr::filter(people != 0)

## Puts all regions in one column and sums the
## total embarked people coming from some and destined to others
df <- df %>% 
  tidyr::pivot_longer(cols = starts_with("region"),
                      names_to = "stop",
                      values_to = "region") %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(people = sum(people))

## Creates size for the connections proportional to the number of people,
## creates an id variable fo geom_sigmoid and joins the coordinates data
df <- df %>% 
  dplyr::mutate(size = scales::rescale(people, to = c(0.5, 3)),
                id = 1:n()) %>% 
  dplyr::left_join(coord)

## Gets the shapes of the countries of the world, simplifies them and
## highlights Brazil and the African continent with a dark fill
world <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  dplyr::mutate(fill = ifelse(name == "Brazil" | continent == "Africa",
                              "#664936",
                              "#e6b270"))
geom <- sf::st_geometry(world) %>% 
  rmapshaper::ms_simplify(keep = 0.5, keep_shapes = TRUE)
sf::st_geometry(world) <- geom

## Creates the circles to mask the map
circle_br <- tibble(
  x = seq(x0_br-r, x0_br+r, length.out = 300),
  y = sqrt((r^2) - (x-x0_br)^2) + y0_br
)
circle_br <- rbind(
  circle_br,
  circle_br %>%
    dplyr::arrange(desc(x)) %>%
    dplyr::slice(-1L,-n()) %>%
    dplyr::mutate(y = -y + 2*y0_br)
)
circle_af <- tibble(
  x = seq(x0_af-r, x0_af+r, length.out = 300),
  y = sqrt((r^2) - (x-x0_af)^2) + y0_af
)
circle_af <- rbind(
  circle_af,
  circle_af %>%
    dplyr::arrange(desc(x)) %>%
    dplyr::slice(-1L,-n()) %>%
    dplyr::mutate(y = -y + 2*y0_af)
)

## Defines coordinates for the curved texts
top_br <- which.max(circle_br$y)
curvtxt_br <- circle_br %>% 
  dplyr::slice((top_br+30):(top_br+150)) %>% 
  dplyr::mutate(label = "DIASPORA OF")
top_af <- which.max(circle_af$y)
curvtxt_af <- circle_af %>% 
  dplyr::slice((top_af-150):(top_af-30)) %>% 
  dplyr::mutate(label = "THE BLACK PEOPLE.")

## Defines coordinates for the titles
titles <- tibble(
  x = -28,
  y = c(105, 80, -77),
  size = c(55, 14, 10),
  label = c(
    "THE AFRO-BRAZILIANS.",
    
    "POSTERS INSPIRED BY W.E.B. DU BOIS WITH DATA FROM IBGE AND THE SLAVE VOYAGES PROJECT.<br>
    GRAPHICS MADE BY ÍCARO BERNARDES (@IcaroBSC).",
    
    'THIS MAP REPRESENTS THE DIASPORIC MOVEMENT TOWARDS BRAZIL FORCED UPON ENSLAVED AFRICANS BETWEEN 1551 AND 1875.<br>
    FOR THESE 3 CENTURIES, MORE THAN **3.5 MILLION PEOPLE** WERE PACKED INTO TINY SPACES TO THE "NEW WORLD".<br>
    THE DATA COMES FROM EXTENSIVE RECORDS THROUGHLY ANALYSED BY THE SLAVE VOYAGES PROJECT TEAM.<br>
    THE POINTS ON THIS MAP REPRESENT PORTS OF ENSLAVED AFRICANS COMMERCE.<br>
    THE THICKNESS OF THE LINES IS PROPORTIONAL TO THE NUMBER OF ENSLAVED AFRICANS THAT WERE EMBARKED TOWARDS BRAZIL.'
  )
)

## Creates the message
message <- c(
  "UNFORTUNATELY THE FORCED DIASPORA OF AFRICANS DID NOT SUBSIDE.<br>
  TODAY MANY FLEE THEIR COUNTRIES RUNNING AWAY FROM POVERTY, WAR AND ENVIRONMENTAL DISASTERS.<br>
  THE FUTURE OF AFRICA STILL IS BEING EXPORTED TO THE WORLD."
  )

## Extracts the coordinates of the connection lines and uses them to make a path
## for the curved text identifying the regions
feat_connec <- tibble(
  name = df$name,
  side = c(rep(1,2),rep(-1,4),rep(1,2),rep(-1,3),1,rep(-1,2))
)
connec <- df %>% 
  ggplot(aes(x = x, y = y)) +
  ggbump::geom_sigmoid(aes(xend = -28.5, yend = -9,
                           group = id, size = I(size)),
                       color = "#dc143c", alpha = 0.5,
                       smooth = 5)
connec <- connec %>% 
  ggplot2::layer_data()
connec <- df %>% 
  dplyr::select(id, name) %>% 
  dplyr::left_join(connec, by = c("id" = "group")) %>% 
  dplyr::select(x, y, id, name) %>% 
  dplyr::left_join(feat_connec)
connec <- connec %>% 
  dplyr::group_by(id) %>%
  dplyr::arrange(id, side*x) %>%
  dplyr::filter(abs(x-x[1])/abs(x[1]) <= 0.7) %>%
  dplyr::ungroup()

# 2. Generates the plot
## Creates the main plot
p <- ggplot() +
  
  ### Defines the circles as references for masking later
  ggfx::as_reference(
    geom_polygon(aes(x = x, y = y), data = circle_br),
    id = 'circle_br'
  ) +
  ggfx::as_reference(
    geom_polygon(aes(x = x, y = y), data = circle_af),
    id = 'circle_af'
  ) +
  
  ### Places the circles as background
  geom_polygon(aes(x = x, y = y), fill = "#d9b99e",
               color = "black", data = circle_br) +
  geom_polygon(aes(x = x, y = y), fill = "#d9b99e",
               color = "black", data = circle_af) +
  
  ### Masks the shapes and connection lines
  ggfx::with_mask(
    geom_sf(aes(fill = I(fill)), color = "black", data = world),
    mask = ggfx::ch_alpha('circle_br')
  ) +
  ggfx::with_mask(
    geom_sf(aes(fill = I(fill)), color = "black", data = world),
    mask = ggfx::ch_alpha('circle_af')
  ) +
  ggfx::with_mask(
    ggbump::geom_sigmoid(aes(x = x, y = y, xend = -28.5, yend = -9,
                             group = id, size = I(size)),
                         color = "#dc143c", alpha = 0.5,
                         smooth = 5, data = df),
    mask = ggfx::ch_alpha('circle_br')
  ) +
  ggfx::with_mask(
    ggbump::geom_sigmoid(aes(x = x, y = y, xend = -28.5, yend = -9,
                             group = id, size = I(size)),
                         color = "#dc143c", alpha = 0.5,
                         smooth = 5, data = df),
    mask = ggfx::ch_alpha('circle_af')
  ) +
  
  ### Places the points to represent the regions
  geom_point(aes(x = x, y = y), shape = 21, size = 3,
             fill = "black", color = "#d9b99e", stroke = 1,  data = df) +
  
  ### Places a star in the State of Bahia
  annotate("text", x = -40.5, y = -12.6, label = "\U2605",
           size = 9, family = symbols, color = bgcolor) +
  
  ### Places the curved texts to label the regions
  geomtextpath::geom_textpath(aes(x = x, y = y, label = name, group = id),
                              size = 3, family = sans, text_only = TRUE,
                              vjust = -0.3, hjust = 0.15, data = connec) +
  
  ### Places the curved texts on the top
  geomtextpath::geom_textpath(aes(x = x, y = y, label = label),
                              size = 11, family = sans, text_only = TRUE,
                              hjust = 0, offset = unit(0.08, "in"),
                              data = curvtxt_br) +
  geomtextpath::geom_textpath(aes(x = x, y = y, label = label),
                              size = 11, family = sans, text_only = TRUE,
                              hjust = 1, offset = unit(0.08, "in"),
                              data = curvtxt_af) +
  
  ### Places the legend texts and keys
  annotate("text", x = -80, y = -52.5, label = "\U1D363", size = 17, family = symbols) +
  annotate("text", x = -75, y = -53, size = 15, family = sans, hjust = 0,
           label = "ROUTES OF THE AFRICAN SLAVE TRADE.") +
  annotate("point", x = -80, y = -63, size = 17) +
  annotate("text", x = -80, y = -63, label = "\U2605",
           size = 10, family = symbols, color = bgcolor) +
  annotate("text", x = -75, y = -63, size = 15, family = sans, hjust = 0,
           label = "THE STATE OF BAHIA.") +
  
  ### Places the titles
  ggtext::geom_richtext(aes(x = x, y = y, label = label, size = I(size)),
                        lineheight = lnhgt, vjust = 1, family = sans,
                        label.color = NA, fill = NA, data = titles) +
  
  ### Places the message
  ggtext::geom_textbox(aes(x = -28, y = -120, label = message), 
                       lineheight = lnhgt, family = sans, size = 15,
                       box.color = NA, fill = "#654321", color = bgcolor,
                       halign = 0.5, valign = 0.5, box.r = unit(25, "pt"),
                       width = unit(0.9, "npc"), height = unit(0.1, "npc")) +
  
  ### Defines limits for the plot
  coord_sf(xlim = c(-120, 64), ylim = c(-129, 105)) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.background = element_rect(fill = bgcolor, color = NA)
  )

## Saves the plot
ggsave("2022/week01/enslaved.png", plot = p, dpi = "retina",
       width = 22, height = 28)


