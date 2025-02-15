set.seed(2024)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2/3,
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  #cli.num_colors = 0,
  #cli.hyperlink = FALSE,
  #pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

ggplot2::theme_set(ggplot2::theme_light())


# Custom frequency table function
table_frq <- function(data,var,digits=2){
  
  headertxt <- c("N", "Raw %", "Valid %", "Cum %")
  
  # calculate frequencies 
  tblx <- data |> 
    group_by({{var}}) |>
    summarise(n = n(), 
              n_miss = sum(is.na({{ var }})), 
              n_valid = n - n_miss) |>
    mutate(raw = n / sum(n), 
           valid = n_valid / sum(n_valid), 
           cum = cumsum(valid))
  
  # remove tmp calculations
  # round entries
  tblx <- tblx |>
    dplyr::select(-c(n_miss, n_valid)) |>
    dplyr::rename_with(~ headertxt, 2:5) |>
    dplyr::mutate(across(where(is.numeric), round, digits))
  

  tblx
  
}



