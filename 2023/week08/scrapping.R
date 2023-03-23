# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(glue)
library(purrr)
library(readr)
library(stringi)
library(stringr)
library(tidyr)

# 1. Helpful tips!  ##########
## Data comes from the INEP portal:
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior

## To find a suitable encoding for the filename use:
# alternatives = purrr::map_dfr(iconvlist(), ~tibble(
#   file = as.character(
#     try(iconv(file_list, from = ., to = ""))
#   ),
#   encoding = .
# ))

## To find a suitable encoding for the file use:
# possibilities = stringi::stri_enc_detect(file_list)

# 2. Data handling ##########
## Lists years
years <- 2009:2021

## Downloads and binds the data together
df <- years |> 
  purrr::map_dfr(function(year) {
    ### Defines the url
    url = glue::glue("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_{year}.zip")
    
    ### Downloads the zip file
    zip_file = tempfile()
    download.file(url, zip_file)
    
    ### Lists the files and gets the path to the data inside the zip
    file_list = unzip(zip_file, list = TRUE)
    file_list = file_list |>
      dplyr::filter(stringr::str_detect(Name, stringr::regex("csv$", ignore_case = TRUE)),
                    stringr::str_detect(Name, "MICRODADOS_CADASTRO_CURSOS")) |> 
      dplyr::pull(Name)
    
    ### Reads the file and filters it
    data_file <- readr::read_delim(unz(zip_file, file_list), delim = ";", locale = readr::locale(encoding = "windows-1252")) |> 
      dplyr::select(NU_ANO_CENSO, ends_with(c("BRANCA", "PRETA", "PARDA", "AMARELA", "INDIGENA", "CORND")))
  })

## Sums all the data
df <- downdf |> 
  dplyr::group_by(NU_ANO_CENSO) |> 
  dplyr::summarise(across(.fns = sum, na.rm = TRUE)) |> 
  dplyr::ungroup()

## Rearranges the data
df <- df |> 
  tidyr::pivot_longer(cols = -NU_ANO_CENSO,
                      names_prefix = "QT_",
                      names_sep = "_",
                      names_to = c(".value","RAÇA"))

## Saves the data
write.csv(df, "2023/week08/data.csv", row.names	= FALSE)
