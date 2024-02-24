# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggview)
library(junebug)
library(systemfonts)
library(stringr)
library(readxl)
library(glue)

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

## Loads the data
rawData <- readxl::read_xlsx("2024/week03/data.xlsx")

# 1. Data handling ##########
## Selects and renames variables
workData <- rawData |>
  dplyr::rename(year = ano, lyrics = letra) |> 
  dplyr::select(year, lyrics)

## Eliminates duplicates, keeps data between 1993 and 2018
## and lowers the case of text
workData <- workData |> 
  dplyr::distinct() |> 
  dplyr::filter(between(year, 1993, 2018)) |> 
  dplyr::mutate(lyrics = tolower(lyrics))

## Lists the search terms
listTerms <- c(
  ### Relevant lands for Black Brazilians
  "áfrica", "african[ao](s?)", "palmares",
  "bahia", "baian[ao](s?)", "benin",
  "angola", "cabo verde", "guiné", "moçambique",
  "são tomé e príncipe", "daomé", "benguela",
  
  ### Black heroes
  "zumbi", "dandara", "pelé",
  "xica", "mahim", "henrique dias",
  "maria felipa", "dragão do mar",
  "francisco josé do nascimento",
  "almirante negro", "joão cândido",
  "jovita feitosa", "tobias barreto",
  "joão pedro teixeira", "andré rebouças",
  "josé do patrocínio", "antonieta de barros",
  "abdias nascimento",
  
  ### Samba heroes
  "clementina", "cartola", "martinho",
  "bezerra da silva", "pixinguinha",
  "ciata", "paulinho da viola",
  "nelson cavaquinho", "noel rosa",
  "ary barroso", "elton medeiros",
  "monarco", "candeia", "silas de oliveira",
  "ivone lara", "wilson moreira", "lupicínio",
  "riachão", "nelson sargento", "ataulfo alves",
  "almir guineto", "jorge aragão", "arlindo cruz",
  "beth carvalho", "zeca pagodinho",
  "jackson do pandeiro", "beth carvalho",
  
  ### Afro-Brazilian Gods (Orixás)
  "oxalá", "iemanjá", "oxum", "oxumarê",
  "oxóssi", "xangô", "obá", "ogum", "iansã",
  "obaluaiê", "omulu", "exu", "nanã",
  
  ### Ways to greet the Gods
  "epá babá", "exê babá", "odoyá", "odocyabá",
  "ora ie iê ô", "arroboboi", "okê arô",
  "kaô kabecilê", "akiro obá yê",
  "patakori ogum", "ogum yê", "eparrei",
  "atotô", "laroiê", "mojubá", "saluba nanã",
  "saravá",
  
  ### Ways to refer to Black Brazilians
  "negr[ao](s?)", "pret[ao](s?)"
) |> 
  glue::glue_collapse(sep = "\\b|\\b")
listTerms <- glue::glue("\\b{listTerms}\\b")

## Counts the occurrence of the terms
workData <- workData |> 
  dplyr::mutate(terms = stringr::str_count(lyrics, listTerms))

## Summarizes by year
workData <- workData |> 
  dplyr::summarise(
    terms = sum(terms),
    .by = year
  ) |> 
  dplyr::arrange(desc(year)) |> 
  dplyr::mutate(year = factor(year, level = unique(year))) 

# 2. Plot production ##########


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
p <- workData |> 
  ggplot() +
  
  geom_col(aes(x = terms, y = year))



## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2024/week01/week01.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
