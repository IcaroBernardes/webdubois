# 0. Library and fonts management
library(tidyverse)
library(showtext)
library(glue)
library(ggtext)

## Adding Google Fonts
sysfonts::font_add_google(name = "Teko", family = "teko") ### Sans Serif
sans <- "teko"

## Allows the use of the downloaded Google Font
## To see the results updated, it's needed to call windows() or save the image
showtext::showtext_opts(dpi = 320)
showtext::showtext_auto()

## Defines some layout constants
lnhgt <- 0.9 ### To set the lineheight
bgcolor <- "#d2b48c"

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("2022/week10/data.csv")

## Selects the variables to be used
df <- rawdata %>% 
  dplyr::select(race, year, candidates, elected)

## Rearranges the data
df <- df %>% 
  tidyr::pivot_longer(cols = c(candidates, elected)) %>% 
  dplyr::mutate(cat = glue::glue("{year}\n{toupper(name)}"))

## Converts the categories to factor
df <- df %>% 
  dplyr::mutate(cat = factor(cat, levels = c(
    "2014\nELECTED","2018\nELECTED",
    "2014\nCANDIDATES","2018\nCANDIDATES"
  )))

## Converts the race to factor and associates with the colors
df <- df %>% 
  dplyr::mutate(race = factor(race, levels = c("non-black","black")),
                fill = ifelse(race == "black", "#dc143c", "black"))

## Calculates the percentages and coordinates for their labels
df <- df %>% 
  dplyr::group_by(cat) %>% 
  dplyr::mutate(pct = round(100*value/sum(value), 2),
                pct = glue::glue("{pct}%")) %>% 
  dplyr::arrange(cat, desc(race)) %>% 
  dplyr::mutate(y = lag(value, default = 0) + value/2) %>% 
  dplyr::mutate(size = ifelse(name == "elected", 7, 18)) %>% 
  dplyr::ungroup()

## Defines coordinates for the legend
leg <- tibble(
  x = 1,
  y = c(1500,2200),
  fill = c("#dc143c","black"),
  right = c("BLACKS<br>NOIRS",
            "NON-BLACKS<span style='font-size:20px;'> (MOSTLY WHITES)</span><br>
            NON-NOIRS<span style='font-size:20px;'> (PRINCIPALEMENT DES BLANCS)</span>"),
  left = "PROPORTION OF<br>PROPORTION DE"
)

## Defines the title and subtitle
title <- c(
  "PROPORTION OF BLACKS AND NON-BLACKS CANDIDATES AND ELECTED
  FOR THE LOWER HOUSE OF THE BRAZILIAN CONGRESS."
)

subtitle <- c(
  "DATA FROM THE 2014 AND 2018 ELECTIONS. MOST OF THE NON-BLACKS ARE WHITES.
  INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)"
)

## Calculates some values of interest
pop <- rawdata %>% 
  dplyr::filter(year == 2018) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(pct = round(100*population/sum(population)),
                   race = unique(race)) %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::pull(pct)
reprs <- df %>% 
  dplyr::filter(cat == "2018\nELECTED") %>% 
  dplyr::mutate(rate = round(100*value/sum(value))) %>% 
  dplyr::filter(race == "black") %>% 
  dplyr::pull(rate)
rate <- rawdata %>% 
  dplyr::filter(year == 2018, race == "black") %>% 
  dplyr::mutate(rate = round(candidates/elected)) %>% 
  dplyr::pull(rate)
cap <- rawdata %>% 
  dplyr::filter(year == 2018, race == "black") %>%
  dplyr::mutate(cap = round(incomes_cand/(candidates*1000))) %>% 
  dplyr::pull(cap)
tot_elec <- rawdata %>% 
  dplyr::filter(year == 2018) %>% 
  dplyr::summarise(tot = sum(elected)) %>% 
  dplyr::pull(tot)
elec <- round(tot_elec*pop/100)
cand <- elec*rate
mon <- round(cand*cap/1000)

## Defines coordinates for the message
message <- tibble(
  x = c(rep(1.8,7), rep(1.82,7)),
  y = c(seq(5200, 8500, length.out = 7),
        seq(5200, 8500, length.out = 7)+35),
  hjust = c(rep(1,7), rep(0,7)),
  size = c(rep(10,7), rep(24,7)),
  label = c(
    "IN 2018, BLACKS ACCOUNTED FOR<br>
    OF THE BRAZILIAN POPULATION.",
    
    "HOWEVER ONLY<br>
    OF THE ELECTED WERE BLACK.",
    
    "1 IN EVERY<br>
    BLACK CANDIDATES GOT ELECTED THIS YEAR.",
    
    "THEIR CAMPAIGNS AMASSED<br>
    PER CANDIDATE.",
    
    "THE BLACK PEOPLE WOULD NEED TO ELECT<br>
    PEOPLE TO REACH PROPORTIONAL REPRESENTATION.",
    
    "THAT MEANS HAVING AS CANDIDATES<br>
    PEOPLE. A TWO-FOLD INCREASE.",
    
    "AND ALSO MEANS INCREASING THE TOTAL FINANCING TO<br>
    WHICH IS A THIRD OF WHAT WHITES RECEIVED IN 2018.",
    
    glue::glue("**{pop}%**"),
    glue::glue("**{reprs}%**"),
    glue::glue("**{rate}**"),
    glue::glue("**<span style='font-size:60px;'>R$ </span>{cap}<span style='font-size:60px;'> K</span>**"),
    glue::glue("**{elec}**"),
    glue::glue("**{cand}**"),
    glue::glue("**<span style='font-size:60px;'>R$ </span>{mon}<span style='font-size:60px;'> M</span>**")
  )
)

# 2. Generates the plot
## Creates the plot
p <- df %>% 
  ggplot() +
  
  ### Places the bars
  geom_col(aes(x = cat, y = value, fill = I(fill), group = race), width = 0.5) +
  
  ### Places the percentage labels
  geom_text(aes(x = cat, y = y, label = pct, size = I(size)),
            family = sans, fontface = "bold") +
  
  ### Places the legend keys
  geom_tile(aes(x = x, y = y, fill = I(fill)),
            width = 0.18, height = 400, data = leg) +
  
  ### Places the legend texts
  ggtext::geom_richtext(aes(x = x, y = y, label = left), size = 10,
                        label.color = NA, fill = NA, family = sans,
                        hjust = 1, nudge_x = -0.1, nudge_y = -50, data = leg) +
  ggtext::geom_richtext(aes(x = x, y = y, label = right), size = 10,
                        label.color = NA, fill = NA, family = sans,
                        hjust = 0, nudge_x = 0.1, nudge_y = -50, data = leg) +
  
  ### Places the message
  ggtext::geom_richtext(aes(x = x, y = y, label = label,
                            hjust = hjust, size = I(size)),
                        label.color = NA, fill = NA, color = "#654321",
                        lineheight = lnhgt, family = sans,
                        vjust = 0, data = message) +
  
  ### Places the title and subtitle
  labs(title = title, subtitle = subtitle) +
  
  ### Reverses the y-axis and eliminates extra space
  scale_y_reverse(expand = expansion(0,0)) +
  
  ### Places the categories of the x-axis on top
  scale_x_discrete(position = "top") +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    text = element_text(family = sans),
    
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = element_text(size = 75, hjust = 0.5, lineheight = lnhgt,
                              margin = margin(t = 120, r = 0, b = 0, l = 0, unit = "pt")),
    plot.subtitle = element_text(size = 30, hjust = 0.5, lineheight = 0.9,
                                 margin = margin(t = 40, r = 0, b = 50, l = 0, unit = "pt")),
    plot.margin = margin(t = 0, r = 20, b = 150, l = 20, unit = "pt"),
    
    axis.text.x = element_text(size = 40, lineheight = lnhgt, face = "bold",
                               margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt"))
  )

## Saves the plot
ggsave("2022/week10/elections.png", plot = p, dpi = "retina",
       width = 22, height = 28)


