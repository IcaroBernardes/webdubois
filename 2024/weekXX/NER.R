# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(huggingfaceR)
library(purrr)
library(glue)
library(stringr)
library(cli)
library(carnaval)
library(readxl)
library(tidyr)
library(tidytext)
library(tm)
library(stringi)

## Loads the data
rawData <- readxl::read_xlsx("2024/week03/data.xlsx") |> 
  dplyr::distinct() |> 
  dplyr::filter(between(ano, 1993, 2018))

## Gets the scores on the Sambas' lyric and melody
sambaScore <- get_scores(1993:2018, criterions = "SAMBA DE ENREDO")

## Downloads the pipeline from
## https://huggingface.co/FacebookAI/xlm-roberta-large-finetuned-conll03-english
modelRoberta <- huggingfaceR::hf_load_pipeline(
  model_id = "FacebookAI/xlm-roberta-large-finetuned-conll03-english", 
  task = "token-classification"
)

# 1. Data cleaning ##########
## Filters out schools that weren't evaluated
workData <- rawData |> 
  dplyr::filter(stringr::str_detect(escola, "não incluída no julgamento", negate = TRUE))

## Gets a list of all schools by year
paradeList <- sambaScore |> 
  dplyr::summarise(
    names = glue::glue_collapse(school, sep = "|"),
    .by = year
  )

## Separates the name of the school from the "Enredo" (theme of the parade)
workData <- workData |> 
  dplyr::rename(year = ano,
                lyrics = letra) |> 
  dplyr::left_join(paradeList) |> 
  dplyr::mutate(school = stringr::str_extract(escola, names),
                theme = stringr::str_remove(escola, school),
                theme = stringr::str_remove(theme, "(( L)?( A)?( S)?)$"),
                theme = stringr::str_trim(theme)) |> 
  dplyr::select(year, school, theme, lyrics)

# 2. NER process ##########
## Adds periods to the end of the sentences
workData <- workData |> 
  dplyr::mutate(lyrics = str_replace_all(lyrics, "\r\n", " \\.\r\n"))

## Creates a function that submits the lyrics
## through the pipeline and returns a tibble
extractor <- function(lyric) {
  lyric |> 
    modelRoberta() |> 
    purrr::compact() |> 
    purrr::map(dplyr::as_tibble) |> 
    purrr::list_rbind()
}

## Creates a function that extracts the entities
## with score above 0.7. Also binds together entities
## that are next to each other on the lyrics
binder <- function(tbl) {
  
  ### Verifies which rows have their end close to start of the next row
  tbl <- tbl |> 
    dplyr::mutate(together = (lead(start, default = 10000) - end <= 1),
                  row = 1:n())
  
  ### Lists the row number of lines that break/end the sequence of closeness
  vec <- which(!tbl$together)
  
  ### Groups the rows and collapses them
  tbl <- tbl |> 
    dplyr::mutate(
      together = santoku::chop(row, vec, seq_along(vec),
                               left = FALSE, close_end = FALSE)
    ) |> 
    dplyr::summarise(
      word = glue::glue_collapse(word),
      .by = c(together, entity)
    )
  
  ### Cleans the entities
  tbl |> 
    dplyr::mutate(word = stringr::str_replace_all(word, "▁", " "),
                  word = stringr::str_trim(word)) |> 
    dplyr::select(-together)
  
}

## Wraps all in a single function
## with a failsafe in case of error
safePipe <- purrr::possibly(
  \(el) {
    el |> 
      extractor() |> 
      binder()
  }
)

## Process all data through the pipeline
result <- workData$lyrics |> 
  purrr::imap(\(el, row, total = nrow(workData)) {
    
    ### Comunicates progress
    cli::cli_inform("Processing row {row}/{total}...")
    
    ### Operates the pipeline
    safePipe(el)
    
  })

## Merges the data
workData <- workData |> 
  dplyr::mutate(entities = result) |> 
  tidyr::unnest(cols = entities)

## Gets the year and lyrics to count
## the frequency of the terms (without NER)
textData <- workData |> 
  dplyr::select(year, lyrics)

## Keeps entities that have three or more characters
workData <- workData |> 
  dplyr::filter(stringr::str_length(word) >= 3)

## Counts the occurrence by type
countData <- workData |> 
  dplyr::count(entity, word, sort = TRUE)

## Separates the data by type
countData <- countData |> 
  tidyr::nest(.by = entity)

## Shows the most frequent entities
### Location
countData |> 
  dplyr::filter(entity == "I-LOC") |> 
  dplyr::pull(data) |> 
  purrr::pluck(1) |> 
  print(n = 15)

### Person
countData |> 
  dplyr::filter(entity == "I-PER") |> 
  dplyr::pull(data) |> 
  purrr::pluck(1) |> 
  print(n = 15)

### Organization
countData |> 
  dplyr::filter(entity == "I-ORG") |> 
  dplyr::pull(data) |> 
  purrr::pluck(1) |> 
  print(n = 15)

### Miscellanea
countData |> 
  dplyr::filter(entity == "I-MISC") |> 
  dplyr::pull(data) |> 
  purrr::pluck(1) |> 
  print(n = 15)

# 3. Counting terms frequency ##########
## Standardizes the lyrics 
textData <- textData |> 
  dplyr::mutate(lyrics = stringi::stri_trans_general(lyrics, "lower; latin-ascii"))

## Filters out stopwords
stops <- tm::stopwords(kind = "portuguese")
stops <- c(stops, "vem", "pra", "vou", "nao", "bis", "vai", "faz", "la")
textData <- textData |> 
  dplyr::mutate(lyrics = removeWords(lyrics, stops))
  
## Counting terms
textData <- textData |> 
  tidytext::unnest_tokens(output = "word", input = "lyrics") |> 
  dplyr::count(word, sort = TRUE)
