# 0. Library and fonts management
library(tidyverse)
library(ggplot2)
library(showtext)
library(scales)
library(glue)
library(ggtext)
library(ggforce)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.7 ### To set the lineheight
bgcolor <- "#d2b48c"
scale_lims <- c(0.1,2.9) ### Limits of the rescale
r <- 0.1 ### Exterior radius of the arc sections
r0 <- 0.05 ### Interior radius of the arc sections
e <- r-r0 ### Thickness of the bars
R <- r + (e/2) ### Radius of the center of arc sections
H <- 1-2*r ### Max length of the horizontal sections
D <- pi*(r+r0) ### Max length of the arc sections
dh <- 2*r-e ### Distance between consecutive horizontal sections
dd <- r0+r ### Distance between consecutive arc centers

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("2022/week09/data.csv")

## Gets only data of blacks
df <- rawdata %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::select(value = busy_per_1000,
                ed = ed_level)

## Rescales the values to new limits
df <- df %>% 
  dplyr::mutate(scaled = scales::rescale(value, to = scale_lims))

## Gets values and categories into a single variable
df <- df %>% 
  dplyr::mutate(ed = glue::glue("{toupper(ed)}\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0{value}"))

## Converts the education levels to factor with 
## levels ordered as they appear in the data
df <- df %>% 
  dplyr::mutate(ed = factor(ed, levels = unique(ed)))

## Creates a function to get the scaled data and
## generate coordinates for the horizontal and arc sections
curver <- function(x) {
  
  ### Gets the scaled value
  scaled = x$scaled
  
  ### Calculates how many full horizontal and arc sections it spans
  full_pair = scaled %/% (H+D)
  
  ### Generates the coordinates for the full sections (if there are any)
  if (full_pair > 0) {
    
    data = tibble::tibble(
      xmin = rep(r, full_pair),
      xmax = rep(1-r, full_pair),
      ymin = seq(0, -(full_pair-1)*dh, by = -dh) - e/2,
      ymax = seq(0, -(full_pair-1)*dh, by = -dh) + e/2,
      x0 = case_when(1:full_pair %% 2 == 0 ~ r,
                     TRUE ~ r+H),
      y0 = seq(0, -(full_pair-1)*dd, by = -dd) - (r0 + e/2),
      start = 0,
      end = case_when(1:full_pair %% 2 == 0 ~ -pi,
                      TRUE ~ pi)
    )
    
  } else {
    
    data = tibble::tibble(
      xmin = NA,
      xmax = NA,
      ymin = NA,
      ymax = NA,
      x0 = NA,
      y0 = NA,
      start = NA,
      end = NA
    )
    
  }
  
  ### Calculates the length of an incomplete pair of sections
  remains = scaled %% (H+D)
  
  ### Investigates if the remainder spans a full horizontal section
  rem_h = remains %/% H
  
  ### Defines the coordinates of the last horizontal and arc sections
  if (rem_h == 1) {
    
    #### Calculates the perimeter of the last arc section
    P = remains %% H
    
    data = data %>% 
      tibble::add_row(
        xmin = r,
        xmax = 1-r,
        ymin = -full_pair*dh - e/2,
        ymax = -full_pair*dh + e/2,
        x0 = ifelse(full_pair %% 2 == 0, r+H, r),
        y0 = -full_pair*dd - (r0 + e/2),
        start = 0,
        end = ifelse(full_pair %% 2 == 0, P/R, -P/R)
      )
    
  } else {
    
    data = data %>% 
      tibble::add_row(
        xmin = ifelse(full_pair %% 2 == 0, r, 1-r-remains),
        xmax = ifelse(full_pair %% 2 == 0, r+remains, 1-r),
        ymin = -full_pair*dh - e/2,
        ymax = -full_pair*dh + e/2
      )
    
  }
  
}

## Applies the 'curver' function to the data
coords <- df %>% 
  dplyr::group_by(ed) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = data %>% purrr::map(curver)) %>% 
  tidyr::unnest(cols = data) %>% 
  dplyr::ungroup()

## Defines the titles and message
title <- "
OCCUPIED BLACKS PER 1000 PEOPLE 
BY EDUCATIONAL LEVEL IN BRAZIL.
"
subtitle <- "
ARE CONSIDERED OCCUPIED: EMPLOYERS, EMPLOYEES, SELF-EMPLOYED, DOMESTIC WORKERS AND NON-PAID FAMILY AIDES. DATA FROM 2018.
INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)
"
message <- "
THE NUMBER OF OCCUPIED BLACKS WITH HIGHER EDUCATION (AND USUALLY HIGHER INCOMES)<br>
IS LITTLE MORE THAN A <span style = color:'white';font-size:80px;>HALF</span> OF OCCUPIED WHITES WITH HIGHER EDUCATION.
"

# 2. Generates the plot
## Creates the main plot
p <- coords %>% 
  ggplot() +
  
  ### Places the horizontal sections
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black") +
  
  ### Places the arc sections
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = start,
                            end = end), fill = "black") +
  
  ### Places the categories names
  geom_text(aes(x = 0, y = 0, label = ed), size = 14, family = sans,
            hjust = 1, lineheight = lnhgt, data = df) +
  
  ### Places the titles
  labs(title = title, subtitle = subtitle, caption = message) +
  
  ### Controls the proportions
  coord_equal(xlim = c(-1.3,1), ylim = c(NA,0.03)) +
  
  ### Divides the categories using facets
  facet_wrap(~ed, ncol = 1, strip.position = "left") +
  
  ### Eliminates most theme elements and customizes the rest
  theme_void() +
  theme(
    text = element_text(family = sans),
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = element_text(size = 100, hjust = 0.5, lineheight = lnhgt,
                              margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
    plot.subtitle = element_text(size = 35, hjust = 0.5, lineheight = 0.9,
                                 margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
    plot.caption = ggtext::element_textbox(
      size = 45, halign = 0.5, valign = 1, lineheight = 1.1, fill = "#654321",
      color = bgcolor, box.color = NA, width = 3, hjust = 0.5,
      margin = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt"),
      padding = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt")
    ),
    plot.margin = margin(t = 0, r = 5, b = 10, l = 5, unit = "pt"),
    panel.spacing = unit(80, "pt"),
    strip.text = element_blank()
  )

## Saves the plot
ggsave("2022/week09/job.png", plot = p, dpi = "retina",
       width = 22, height = 28)

