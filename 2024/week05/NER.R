# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(cli)
library(carnaval)
library(readxl)
library(tidyr)
library(stringi)
library(GenAI)
library(santoku)

## Loads the data
rawData <- readxl::read_xlsx("2024/week05/rawData.xlsx") |> 
  dplyr::distinct() |> 
  dplyr::filter(between(ano, 1985, 2018))

## Gets the scores on the Sambas' lyric and melody
sambaScore <- get_scores(1985:2018, criterions = "SAMBA DE ENREDO")

## Creates a Google Generative AI object
google = genai.google(
  api = Sys.getenv("GOOGLE_GENERATIVE_AI"),
  model = "gemini-pro",
  version = "v1",
  proxy = FALSE
)

## Configures the prompt
config <- list(
  harm.category.dangerous.content = 5,
  harm.category.harassment = 5,
  harm.category.hate.speech = 5,
  harm.category.sexually.explicit = 5,
  temperature = 0
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

## Calculates the average score for each school at each year
sambaScore <- sambaScore |> 
  dplyr::summarise(
    score = mean(score, na.rm = TRUE),
    .by = c(school, year)
  )

## Splits the scores into two groups each year: highest and lowest
sambaScore <- sambaScore |> 
  dplyr::select(school, year, score) |> 
  dplyr::mutate(
    score = santoku::chop_evenly(score, 2, labels = c("lowest", "highest")),
    .by = year
  )

## Merges the data
workData <- dplyr::left_join(workData, sambaScore)

## Saves the data
workData |> 
  write.csv("2024/week05/data.csv", row.names = FALSE)

# 2. NER process ##########
## Adds an ID to the data
workData <- workData |> 
  dplyr::mutate(id = 1:n())

## Creates a function for extracting the entities
extractor <- function(samba, iter, total) {
  
  ### Comunicates progress
  cli::cli_inform("Processing lyrics {.strong [{iter}/{total}]}")
  
  ### Clears the chat history
  GenAI::chat.history.reset(google)
  
  ### Composes the prompt
  prompt = glue::glue(
    "Leia a letra deste samba:\n{samba}\n\n
    Agora, busque termos e entes no texto (NER).
    Crie três listas: locais, pessoas e expressões africanas.
    Retorne apenas a lista como uma string em formato JSON"
  )
  
  ## Composes the prompt
  res = google |> 
    GenAI::txt(prompt, config = config)
  
  ## Reads the JSON
  res = res |> 
    stringr::str_remove_all("`|json") |> 
    jsonlite::fromJSON()
  
  ## Collapses the lists
  dplyr::tibble(
    id = iter,
    places = glue::glue_collapse(res$locais, sep = "|"),
    people = glue::glue_collapse(res$pessoas, sep = "|"),
    expressions = glue::glue_collapse(res$expressoes_africanas, sep = "|")
  )
  
}

## Creates a version that skips errors
extractor <- purrr::possibly(extractor)

## Applies the function
NER <- workData$lyrics |> 
  purrr::imap(\(x, y) extractor(samba = x, iter = y, total = nrow(workData)))

## Eliminates errors and converts the list to a tibble
NER <- NER |> 
  purrr::compact() |> 
  purrr::list_rbind()

## Merges the data
plotData <- dplyr::left_join(NER, workData) |> 
  dplyr::select(-id)

## Converts empty string to NA
plotData <- plotData |> 
  dplyr::mutate(across(
    .cols = c(places, people, expressions),
    .fns = \(x) ifelse(nchar(x) == 0, NA, x)
  ))

## Creates a function that ranks the most frequent entities
ranker <- function(entities) {
  
  ### Lists the entities
  term = entities |> 
    na.exclude() |> 
    glue::glue_collapse(sep = "|") |> 
    stringr::str_split(pattern = "\\|") |> 
    unlist()
  
  ### Filters the top terms
  term = dplyr::tibble(
    term = term
  ) |> 
    dplyr::count(term, sort = TRUE) |> 
    dplyr::mutate(cum = cumsum(n),
                  pct = cum/sum(n)) |> 
    dplyr::filter(lead(pct) < 0.30)
  
}

## Applies the function
places <- ranker(plotData$places)
people <- ranker(plotData$people)
expressions <- ranker(plotData$expressions)

## Saves a tibble with all entities and their types
places <- places |> dplyr::mutate(type = "places")
people <- people |> dplyr::mutate(type = "people")
expressions <- expressions |> dplyr::mutate(type = "expressions")
places |> 
  dplyr::bind_rows(people) |> 
  dplyr::bind_rows(expressions) |> 
  dplyr::select(term, type) |> 
  write.csv("2024/week05/terms.csv", row.names = FALSE)
