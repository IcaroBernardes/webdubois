# 0. Initial setup ##########
## Loads packages
library(dplyr)
library(glue)
library(magick)
library(purrr)
library(stringr)

# 0. Images adjustment ##########
## Lists all original images
original_images <- list.files(path = "originals", pattern = ".jpg", full.names = TRUE)

## Lists all new images
new_images <- list.files(recursive = TRUE, pattern = ".png", full.names = TRUE)
new_images <- new_images |> 
  stringr::str_subset("week[:digit:]{2}") |> 
  stringr::str_subset("_pt.png|week00", negate = TRUE)

## Creates a function that adjusts the images
adjuster <- function(img, final_path, thumb) {
  
  ### Gets image dimensions
  info = magick::image_info(img)
  w = info$width
  h = info$height
  
  ### Defines final dimensions
  if (thumb) {
    h_std = 128
    w_std = 100
  } else {
    h_std = 700
    w_std = 550
  }
  
  ### Defines the scale geometry
  if (h/w > 1.272727) {
    geom_scales = glue::glue("{w_std}x")
  } else {
    geom_scales = glue::glue("x{h_std}")
  }
  
  ### Manipulates the image and saves
  img |> 
    magick::image_scale(geometry = geom_scales) |> 
    magick::image_crop(geometry = glue::glue("{w_std}x{h_std}"),
                       gravity = "Center") |> 
    magick::image_write(final_path)
  
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
