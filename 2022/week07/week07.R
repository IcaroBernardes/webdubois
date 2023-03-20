# 0. Library and fonts management
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(showtext)
library(glue)
library(seriation)
library(ggtext)
library(sysfonts)

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
idiom <- "en" ### Português (pt) or English (en)

# 1. Data download, load and handling
## Data on inequality comes from IBGE
## https://www.ibge.gov.br/estatisticas/sociais/populacao/25844-desigualdades-sociais-por-cor-ou-raca.html?=&t=resultados
rawdata <- read.csv2("2022/week07/data.csv")

## Converts the data into a matrix in which each row has all six values for each state
mat <- rawdata %>% 
  tidyr::pivot_wider(
    names_from = c("race", "capital"),
    values_from = "percent"
  )
states <- mat$state
mat <- as.matrix(mat[,-1])
rownames(mat) <- states

## Uses a BEA_TSP algorithm to determine the order of rows
## that minimizes difference between adjacent rows
set.seed(42)
order <- seriation::seriate(mat, method = "BEA_TSP")
order <- seriation::get_order(order, 1)
order <- states[order]

## Applies the obtained order to the data and create a numeric variable
df <- rawdata %>% 
  dplyr::mutate(state = factor(state, levels = order),
                y = as.numeric(state))

## Defines the order of the capital variable
if (idiom == "en") {
  df <- df %>% 
    dplyr::mutate(capital = factor(capital,
                                   levels = c("more than 1M", "100k to 1M", "less than 100k")))
} else {
  df <- df %>% 
    dplyr::mutate(capital = case_when(capital == "more than 1M" ~ "mais de 1M",
                                      capital == "100k to 1M" ~ "de 100k a 1M",
                                      capital == "less than 100k" ~ "menos de 100k",
                                      TRUE ~ ""),
                  capital = factor(capital,
                                   levels = c("mais de 1M", "de 100k a 1M", "menos de 100k")))
}

## Inverts the values of percentage for black so the data is on the left
df <- df %>% 
  dplyr::mutate(percent = ifelse(race == "black", -percent, percent))

## Orders the categories
if (idiom == "en") {
  df <- df %>% 
    dplyr::mutate(race = factor(race, levels = c("black","white")))
} else {
  df <- df %>% 
    dplyr::mutate(race = ifelse(race == "black", "negro", "branco"),
                  race = factor(race, levels = c("negro","branco")))
}

## Defines coordinates for the categories labels
if (idiom == "en") {
  cat_race <- c(rep("white",3),rep("black",3))
} else {
  cat_race <- c(rep("branco",3),rep("negro",3))
}

categories <- tibble(
  race = cat_race,
  y = rep(c(6,16,26),2)
) %>% 
  dplyr::mutate(x = c(40, 80, 88, -40, -80, -88),
                angle = c(rep(-45,3),rep(45,3)),
                label = rep(unique(df$capital),2)) %>% 
  dplyr::mutate(label = toupper(label),
                label = stringr::str_wrap(label, width = 8))

if (idiom == "en") {
  categories <- categories %>% 
    dplyr::mutate(race = factor(race, levels = c("black","white")))
} else {
  categories <- categories %>% 
    dplyr::mutate(race = factor(race, levels = c("negro","branco")))
}

## Defines the breaks for the x-axis
x_breaks <- c(
  seq(-100, -10, by = 10),
  seq(10, 100, by = 10)
)

## Defines the title of the x-axis
if (idiom == "en") {
  name_x <- "PER CENTS."
} else {
  name_x <- "POR CENTOS."
}

## Defines the titles and message
if (idiom == "en") {
  title <- "CAPITAL OF CANDIDATURES FOR THE LOWER HOUSE\nOF THE BRAZILIAN CONGRESS BY RACE AND STATE."
  subtitle <- "
  THIS DATA IS FROM 2018 AND THE CURRENCY IS BRAZILIAN REAIS (BRL). FOR COMPARISSON, ONE US DOLLAR THIS YEAR WAS WORTH 3.65 BRL IN AVERAGE.
  STATES ARE ORDERED BY SIMILARITY OF THE PERCENTAGES (USING BOND ENERGY AND TRAVELING SALESPERSON ALGORITHM).
  INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ÍCARO BERNARDES (@IcaroBSC)
  "
  message <- "
  ALL WHITE CANDIDATURES TOGETHER RECEIVED 283M USD WHILE BLACK ONES RECEIVED 85M USD.<br>
  THIS IS AROUND <span style = color:'white';font-size:80px;>A THIRD</span> OF WHAT WHITES OBTAINED.
  "
} else {
  title <- "CAPITAL DAS CANDIDATURAS A DEPUTADO\nFEDERAL POR RAÇA E UF."
  subtitle <- "
  DADOS DE 2018 E MOEDA EM REAIS (BRL).
  AS UF'S SÃO ORDENADAS POR SIMILARIDADE DAS PORCENTAGENS (USANDO ALGORITMO DE BOND ENERGY E TRAVELING SALESPERSON).
  INSPIRADO POR: W.E.B. DU BOIS | DADOS DE: IBGE | GRÁFICO POR: ÍCARO BERNARDES (@IcaroBSC)
  "
  message <- "
  CANDIDATURAS BRANCAS RECEBERAM JUNTAS 1B BRL ENQUANTO CANDIDATURAS NEGRAS RECEBERAM 311M BRL.<br>
  TAL VALOR É PRÓXIMO A <span style = color:'white';font-size:80px;>UM TERÇO</span> DO QUE OS BRANCOS OBTIVERAM.
  "
}

# 2. Generates the plot
## Creates the main plot
p <- df %>% 
  ggplot() +
  
  ### Places the bars 
  geom_col(aes(x = percent, y = y, fill = capital), color = "black",
           size = 0.2, width = 1, orientation = "y") +
  
  ### Places the labels of the categories
  geom_text(aes(x = x, y = y, label = label, angle = angle),
            family = sans, size = 16, lineheight = lnhgt, data = categories) +
  
  ### Places a central line that highlights the zero
  geom_vline(xintercept = 0, size = 0.5) +
  
  ### Facets the plot between the two groups
  facet_grid(.~race, scales = "free_x", labeller = labeller(.cols = ~toupper(glue::glue("{.}S.")))) +
  
  ### Places the titles and message
  labs(title = title, subtitle = subtitle, caption = message) +
  
  ### Eliminates excess of elements
  theme_void() +
  
  ### Customizes the plot
  theme(
    text = element_text(family = sans),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 28,
                                margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
    axis.text.x = element_text(size = 28,
                               margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt")),
    axis.text.y = element_text(size = 32),
    axis.text.y.left = element_text(hjust = 1,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
    axis.text.y.right = element_text(hjust = 0,
                                     margin = margin(t = 0, r = 0, b = 0, l = 20, unit = "pt")),
    plot.margin = margin(t = 80, r = 70, b = 20, l = 70, unit = "pt"),
    plot.background = element_rect(fill = bgcolor, color = NA),
    plot.title = element_text(size = 90, hjust = 0.5, vjust = 1, lineheight = lnhgt,
                              margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
    plot.subtitle = element_text(size = 30, hjust = 0.5, vjust = 1, lineheight = 0.9,
                                 margin = margin(t = 10, r = 0, b = 50, l = 0, unit = "pt")),
    plot.caption = ggtext::element_textbox(
      size = 45, halign = 0.5, valign = 1, lineheight = 1.1, fill = "#654321",
      color = bgcolor, box.color = NA, width = 2, hjust = 0.5,
      margin = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt"),
      padding = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt")
    ),
    strip.text = element_text(size = 32,
                              margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
    panel.ontop = TRUE,
    panel.spacing = unit(0, "lines"),
    panel.grid.minor.x = element_line(color = "black", size = 0.1),
    panel.grid.major.x = element_line(color = "black", size = 0.2)
  ) +
  
  ### Defines the axes scales
  scale_x_continuous(name = name_x, breaks = x_breaks, minor_breaks = seq(-100,100,2),
                     labels = abs, expand = expansion(0,0)) +
  scale_y_continuous(name = NULL, breaks = 1:n_distinct(df$state),
                     labels = toupper(order), expand = expansion(0,0), sec.axis = dup_axis()) + 
  
  ### Defines the colors of the categories
  scale_fill_discrete(type = c("#00aa00","#dc143c","#4682b4"), guide = "none")

## Saves the plot
ggsave("2022/week07/congress.png", plot = p, dpi = "retina",
       width = 22, height = 28)

