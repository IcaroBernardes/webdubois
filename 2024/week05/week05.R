# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggview)
library(glue)
library(junebug)
library(patchwork)
library(systemfonts)
library(stringr)

## Defines colors
palette <- c("#000000", "#654321", "#d2b48c", "#ffd700", "#ffc0cb", "#dc143c", "#00aa00", "#4682b4", "#7e6583")
black <- palette[1]
brown <- palette[2]
tan <- palette[3]
gold <- palette[4]
red <- palette[6]

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
rawData <- read.csv("2024/week05/data.csv")

## Loads the terms (NER extraction)
NERterms <- read.csv("2024/week05/terms.csv")

# 1. Data handling ##########
## Keeps only Afro-Brazilian related terms
## and eliminates duplicates
NERterms <- NERterms |> 
  dplyr::slice(-c(1,4,7,14,17,22,24,25,27,30,34,35,36,39,
                  42,43,45:49,52:56,58:62,65:72,75,76,91))

## Creates a lists of search terms based on web searches
listTerms <- dplyr::tibble(
  term = c(
    ### Relevant lands for Black Brazilians
    "áfrica", "palmares", "bahia",  "benin",
    "angola", "cabo verde", "guiné", "moçambique",
    "são tomé e príncipe", "daomé", "benguela",
    "senegâmbia", "congo", "costa da mina",
    
    ### Black heroes
    "zumbi", "dandara", "pelé",
    "mahim", "henrique dias",
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
    "nelson cavaquinho",
    "ary barroso", "elton medeiros",
    "monarco", "candeia", "silas de oliveira",
    "ivone lara", "wilson moreira", "lupicínio",
    "riachão", "nelson sargento", "ataulfo alves",
    "almir guineto", "jorge aragão", "arlindo cruz",
    "beth carvalho", "zeca pagodinho",
    "jackson do pandeiro",
    
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
    "saravá"
  ),
  type = c(
    rep("places", 14),
    rep("people", 42),
    rep("expressions", 30)
  )
)

## Merges the lists, makes all terms lowercase and 
## eliminates duplicates
listTerms <- listTerms |> 
  dplyr::bind_rows(NERterms) |> 
  dplyr::mutate(term = tolower(term)) |> 
  dplyr::distinct()

## Creates a pattern search with all terms.
## Each term is searched as a string,
## not a substring (note the use of \\b)
pattern <- listTerms$term |> 
  glue::glue_collapse(sep = "\\b|\\b")
pattern <- glue::glue("\\b{pattern}\\b")

## Counts the occurrence of the terms on each samba
workData <- rawData |> 
  dplyr::mutate(lyrics = tolower(lyrics),
                terms = stringr::str_count(lyrics, pattern))

## Counts the number of sambas in which each term appears
termsData <- listTerms |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    pattern = glue::glue("\\b{term}\\b"),
    sambas = workData$lyrics |> 
      stringr::str_detect(pattern) |> 
      sum()
  ) |> 
  dplyr::ungroup()

## Lists the top5 terms of each type and their counts
termsData <- termsData |> 
  dplyr::slice_max(order_by = sambas, by = type, n = 5) |> 
  dplyr::mutate(body = glue::glue("{toupper(term)}({sambas})")) |> 
  dplyr::summarise(
    body = glue::glue_collapse(body, sep = ", "),
    .by = type
  ) |> 
  dplyr::mutate(body = glue::glue("{body} AND MORE."))

## Counts the number of sambas in which each type appears
plotData <- listTerms |> 
  dplyr::summarise(
    pattern = glue::glue_collapse(term, sep = "\\b|\\b"),
    .by = type
  ) |> 
  dplyr::mutate(pattern = glue::glue("\\b{pattern}\\b")) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    sambas = workData$lyrics |> 
      stringr::str_detect(pattern) |> 
      sum()
  ) |> 
  dplyr::ungroup()
  
## Creates labels for the types
plotData <- plotData |> 
  dplyr::mutate(
    title = glue::glue("<strong>{toupper(type)}.</strong>")
  ) |> 
  dplyr::left_join(termsData)

## Orders the types and
## creates the percentage labels 
plotData <- plotData |> 
  dplyr::mutate(
    type = factor(type, levels = c("places", "expressions", "people"))
  ) |> 
  dplyr::arrange(desc(type)) |> 
  dplyr::mutate(Ytxt = cumsum(sambas)-30,
                Yval = lag(cumsum(sambas), default = 0) + sambas/2,
                colVal = c(black, tan, tan))

# 2. Plot production ##########
## Creates the title
title <- "
<strong style='font-size:90px;'>SAMBAS IN WHICH OF AFRO-BRAZILIAN TERMS APPEAR.</strong>
<br>
<span style='font-size:75px;'>TERMS EXTRACTED WITH NER FROM 480 LYRICS OF RIO SAMBA SCHOOLS (1985-2018).</span>
<br><br>
<span style='font-size:60px;'>INSPIRED BY: W.E.B. DU BOIS | DATA FROM: GALERIA DO SAMBA | GRAPHIC BY: ÍCARO BERNARDES<br>
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf099 \uf16d \uf08c </span>@IcaroBSC | 
<span style='font-family:\"Font Awesome 6 Brands Regular\";font-size:40px;'>\uf09b </span>@IcaroBernardes 
</span>
"

## Creates the plot
p <- plotData |> 
  ggplot() +
  
  ### Places the stacked columns
  geom_col(aes(x = 0, y = sambas, fill = type), color = black) +
  
  ### Places the labels of the columns
  geom_text(
    aes(x = 0, y = Yval, label = sambas, color = I(colVal)),
    size = 20, family = "Teko", fontface = "bold"
  ) +
  
  ### Places the title of the labels (type)
  ggtext::geom_richtext(
    aes(x = -1.8, y = Ytxt, label = title),
    hjust = 0, vjust = 0, size = 25, family = "Teko",
    fill = NA, label.colour = NA, fontface = "bold"
  ) +
  
  ### Places the body of the labels (type)
  ggtext::geom_textbox(
    aes(x = -1.6, y = Ytxt, label = body),
    hjust = 0, vjust = 1, size = 12, family = "Teko",
    fill = NA, box.colour = NA,
    width = unit(0.37, "npc")
  ) +
  
  ### Defines the color scheme
  scale_fill_manual(values = c("places" = black,
                               "expressions" = brown,
                               "people" = gold)) +
  
  ### Controls x-axis expansion beyond limits
  scale_x_continuous(
    limits = c(-2,1),
    expand = expansion(0,add = c(0.05,0))
  ) +
  
  ### Places the title
  labs(title = title) +
  
  ### Eliminates and customizes plot elements
  theme_void() +
  theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5, vjust = 0, size = 20, lineheight = 2.5,
      margin = margin(0, 0, 0, 0)
    ),
    plot.background = element_rect(fill = tan, color = tan),
    plot.margin = margin(60, 40, 0, 40),
    
    legend.position = "none",
    text = element_text(family = "Teko")
  )

## Shows an accurate preview of the plot
ggview::ggview(p, device = "png", dpi = 320,
               units = "in", width = 22, height = 28)

## Saves the plot
ggsave("2024/week05/week05.png", plot = p, device = "png", dpi = 320,
       units = "in", width = 22, height = 28)
