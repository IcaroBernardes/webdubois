############################## Shapes the images ##############################
# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(glue)
library(magick)
library(purrr)
library(stringr)

## Lists all original images
original_images <- list.files(path = "originals", pattern = ".png", full.names = TRUE)

## Lists all new images
new_images <- list.files(recursive = TRUE, pattern = ".png", full.names = TRUE)
new_images <- new_images |> 
  stringr::str_subset("week[:digit:]{2}") |> 
  stringr::str_subset("_pt.png|week00", negate = TRUE)

# 1. Images handling ##########
## Defines the final dimensions of thumbnails
dim_num <- 200
dim_str <- as.character(dim_num)

## Creates a new image with white background and a black circle
mask <- magick::image_draw(magick::image_blank(dim_num, dim_num))
symbols(dim_num/2, dim_num/2, circles=(dim_num/2)-3, bg="black", inches=FALSE, add=TRUE)
dev.off()

## Creates a function that adjusts the images
adjuster <- function(img, final_path, thumb) {
  
  ### Gets image dimensions
  info = magick::image_info(img)
  w = info$width
  h = info$height
  
  ### Defines final dimensions
  if (thumb) {
    h_std = dim_num
    w_std = dim_num
  } else {
    h_std = 700
    w_std = 550
  }
  
  ### Defines the scale geometry
  if (thumb) {
    geom_scales = glue::glue("{w_std}x")
  } else {
    if (h/w > 1.272727) {
      geom_scales = glue::glue("{w_std}x")
    } else {
      geom_scales = glue::glue("x{h_std}")
    }
  }
  
  ### Manipulates the image
  img = img |> 
    magick::image_scale(geometry = geom_scales) |> 
    magick::image_crop(geometry = glue::glue("{w_std}x{h_std}"),
                       gravity = "Center")
  
  ### Creates circular versions for the thumbnails
  if (thumb) {
    #### Creates an image composite using the image and the circular mask
    img = magick::image_composite(img, mask, operator = "copyopacity")
    
    #### Sets the background as transparent
    img = magick::image_background(img, "transparent")
  }
  
  ### Saves the image
  magick::image_write(img, final_path)
  
  ### Forces the memory cleaning process
  gc(verbose = FALSE)
  
}

## Scales and crops the original images
original_images |> 
  purrr::walk(function (path) {
    
    ### Reads the image
    img = magick::image_read(path)
    
    ### Creates the path to the final image
    path = stringr::str_remove(path, "originals/")
    final_path = glue::glue("www/dubois/{path}")
    
    ### Adjusts the image
    adjuster(img, final_path, FALSE)
    adjuster(img, glue::glue("www/dubois/thumb_{path}"), TRUE)
    
  })

## Scales and crops the new images
new_images |> 
  purrr::walk(function (path) {
    
    ### Reads the image
    img = magick::image_read(path)
    
    ### Creates the path to the final image
    year = stringr::str_extract(path, "/[:digit:]{4}/") |> 
      stringr::str_remove_all("/") |> 
      stringr::str_sub(3, 4)
    week = stringr::str_extract(path, "/week[:digit:]{2}/") |> 
      stringr::str_remove_all("/|week")
    path = glue::glue("y{year}wk{week}.png")
    final_path = glue::glue("www/new/{path}")
    
    ### Adjusts the image
    adjuster(img, final_path, FALSE)
    adjuster(img, glue::glue("www/new/thumb_{path}"), TRUE)
    
  })

########################## Creates the African symbol ##########################
# 0. Initial setup ##########
## Loads packages
library(rnaturalearth)
library(rmapshaper)
library(ggplot2)

## Loads the shapes of the African countries
africa <- rnaturalearth::ne_countries(
  continent = "Africa",
  returnclass = "sf"
)

# 1. Data handling ##########
## Dissolves the internal boundaries and simplifies the shape
africa <- africa |> 
  rmapshaper::ms_dissolve() |> 
  rmapshaper::ms_simplify(keep = 0.4)

## Plots the shape
africa |> 
  ggplot() +
  geom_sf(color = NA, fill = "#ffedd6") +
  coord_sf(xlim = c(-18, 52), ylim = c(-36,38), expand = FALSE) +
  theme_void()

## Saves the plot
w <- 40
ggsave("www/africa.png", width = w, height = w*(104/70), units = "px")

########## Creates an JS object that has the database for the images ##########
# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(jsonlite)
library(tidyr)

# 1. Data handling ##########
## Creates a tibble with the data
df <- dplyr::tibble(
  ids = c(
    "y22wk01", "y22wk02", "y22wk03", "y22wk04", "y22wk05",
    "y22wk06", "y22wk07", "y22wk08", "y22wk09", "y22wk10",
    "y23wk02", "y23wk03", "y23wk04", "y23wk05", "y23wk06",
    "y23wk07"
  ),
  `original-title` = c(
    "THE GEORGIA NEGRO",
    "ASSESSED VALUATION OF ALL TAXABLE PROPERTY OWNED BY GEORGIA NEGROES",
    "RELATIVE NEGRO POPULATION OF THE STATES OF THE UNITED STATES",
    "VALUATION OF TOWN AND CITY PROPERTY OWNED BY GEORGIA NEGROES",
    "SLAVES AND FREE NEGROES",
    "ILLITERACY",
    "CONJUGAL CONDITION OF AMERICAN NEGROES ACCORDING TO AGE PERIODS",
    "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE OWNED BY GEORGIA NEGROES",
    "NUMBER OF NEGRO STUDENTS TAKING THE VARIOUS COURSES OF STUDY OFFERED IN GEORGIA SCHOOLS",
    "PROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL AGE WHO ARE ENROLLED IN THE PUBLIC SCHOOLS",
    "VALUE OF LAND OWNED BY GEORGIA NEGROES",
    "CITY AND RURAL POPULATION 1890",
    "DISTRIBUTION OF NEGROES IN THE UNITED STATES",
    "NEGRO BUSINESS MEN IN THE UNITED STATES",
    "CRIME AMONG AMERICAN NEGROES",
    "OCCUPATIONS OF NEGROES AND WHITES IN GEORGIA"
  ),
  `new-title` = c(
    "THE AFRO-BRAZILIANS",
    "REAL MONTHLY INCOME PER CAPITA OF BLACK HOUSEHOLDS IN BRAZIL",
    "ACCESS OF BLACK PEOPLE TO A PERSONAL MOBILE PHONE AND INTERNET IN THE STATES OF BRAZIL",
    "REAL INCOME OF BLACK PEOPLE WHOSE MAIN WORK IS INFORMAL",
    "PARTICIPATION IN MANAGERIAL POSITIONS BY RACE IN BRAZIL",
    "ILLITERACY RATE OF BLACK PEOPLE IN BRAZIL",
    "CAPITAL OF CANDIDATURES FOR THE LOWER HOUSE OF THE BRAZILIAN CONGRESS BY RACE AND STATE",
    "RATE OF HOMICIDES OF BLACKS BY PER 100K PEOPLE",
    "OCCUPIED BLACKS PER 1000 PEOPLE BY EDUCATIONAL LEVEL IN BRAZIL",
    "PROPORTION OF BLACKS AND NON-BLACKS CANDIDATES AND ELECTED FOR THE LOWER HOUSE OF THE BRAZILIAN CONGRESS",
    "REAL AVERAGE INCOME OF BLACK BRAZILIANS",
    "HOMICIDE VICTIMS BY RACE AND SEX",
    "DISTRIBUTION OF BLACKS IN BRAZIL",
    "RIO SAMBA SCHOOLS PARADE WINNERS",
    "INCARCERATED PEOPLE BY RACE IN BRAZIL",
    "HIGHER EDUCATION ENROLLMENTS BY RACE AND FIELD"
  )
)

## Creates a list-column for each image id
df <- df |> 
  dplyr::group_by(ids) |> 
  tidyr::nest()

## Creates a named list with the original data
listed <- df |> dplyr::pull(data)
names(listed) <- df |> dplyr::pull(ids)

## Creates the JS object
listed |> jsonlite::toJSON()
