set.seed(2024)

library(flextable)

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
  dplyr.summarise.inform = FALSE,
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



#' Custom functions 
#' 



#' @title Check if variable name exists in data frame. 
#' 
#' @description Useful to avoid error messages before generating figures and tables. Performs exact match.  
#' 
#' @param needle String of one or more column names.  
#' @param stack String of all column names
#' 
#' @return logit 
#' 
col_exists <- function(data, needle){
  
  # get all column names
  stack <- names(data)
  
  # if column name is an object, conver to string
  needle <- deparse(substitute(needle))
  
  # if column name was in string, remove double quotes
  needle <- str_remove_all(needle, "\"")
  
  # check if in data frame
  exists <- all(needle %in% stack)
  
  exists
}



#' Custom function to calculate cramer's v on the fly from gtsummary calls
#' 
#' @param data data frame object, passed implicitly
#' @param var variable name to use for frequency table
#' @param by second variable for crosstab
#'
get_cramer_v <- function(data, var, by, ...) {
  table(data[[var]], data[[by]]) |>
    rstatix::cramer_v()
}



#' Custom frequency table function
#' 
#' @param data data frame object, passed implicitly
#' @param var variable name to use for frequency table
#' @param digits numeric round up to number of digits
#' @param fsize numeric font size for word tables
#' 
table_frq <- function(data, var, digits=2, fsize=11){

  # if (! data |> col_exists({{var}})) {
  #   msg <- paste0("Variable does not exist in data frame - skipping frequency table")
  #   return(msg)
  # }
  
  headertxt <- c("N", "Raw%", "Valid%", "Cum%")
  
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
    dplyr::mutate(across(where(is.numeric), \(x) round(x, digits=digits)))
  
  
  # convert variable object to string
  label_str <-deparse(substitute(var))

  # get label of the variable, i.e. the question wording
  var_label <- attr(data[,label_str], which="label")  
  

  # word document
  if (!knitr::is_html_output()) {

    caption_txt <- var_label 
    # caption_auto <- officer::run_autonum(seq_id = "tab", 
    #                                      pre_label = paste0("Table", " " ))
    
    tnum_rows <- nrow(tblx)
    missing_str <- "(Missing)"
    
    tblx <- flextable(tblx) |> 
      flextable::font(fontname="Roboto", part="all") |> 
      flextable::fontsize(size=fsize, part = "all") |>
      flextable::bold(bold=T, part = "header") |> 
      flextable::width(j=c(1:5), width=c(3,1,.6,1,1)) |>
      flextable::compose(i = tnum_rows, j = 1, as_paragraph(as_chunk(missing_str)))
  }
  
  tblx
}


#' Generic table print function to switch correctly to flextable for 
#' word documents. 
#' 
table_df <- function(tblx, digits=2, fsize=11){
  

  # word document
  if (!knitr::is_html_output()) {
    
    tblx <- flextable(tblx) |> 
      flextable::font(fontname="Roboto", part="all") |> 
      flextable::fontsize(size=fsize, part = "all") |>
      flextable::bold(bold=T, part = "header") |>
      flextable::autofit()
  }
  
  
  tblx
  
}


#' Crosstab function to retrieve different statistical tests for contingency table
#'
#' This function creates a contingency table from two categorical variables and calculates statistical tests such as Chi-squared, Cramér’s V, and Fisher’s exact test.
#'
#' @param data A data frame containing the variables.
#' @param var The first categorical variable (to appear in rows).
#' @param by The second categorical variable (to appear in columns).
#' 
#' @return A string of test statistics.
#' 
crosstab_stats <- function(data, var, by, ...){
  
  # Create contingency table (absolute frequencies)
  contingency_table <- table(data[[var]], data[[by]])
  
  # Ensure the table has at least two rows and two columns
  if (nrow(contingency_table) < 2 | ncol(contingency_table) < 2) {
    stop("Insufficient data to generate a contingency table.")
  }
  
  # Perform statistical tests
  chi_test <- chisq.test(contingency_table)
  #cramers_v <- sjstats::cramer(contingency_table)
  cramers_v <- rstatix::cramer_v(contingency_table)
  fisher_test <- fisher.test(contingency_table)
  
  # put it all in one string
  source_note = paste0(
    "Chi-squared = ", round(chi_test$statistic, 3), 
    " | p-value = ", signif(chi_test$p.value, 3), 
    " | df = ", chi_test$parameter,
    " | Cramér's V = ", round(cramers_v, 3),
    " | Fisher’s p = ", signif(fisher_test$p.value, 3)
  )
  
  statest <- list(source_note, chi_test, cramers_v, fisher_test)

  return(statest)  
}



#' Crosstab function to plot stacked barchart of two variables. 
#'
#'
#' @param data A data frame containing the variables.
#' @param var The first categorical variable (y-axis).
#' @param by The second categorical variable (x-axis).
#' 
#' @return A ggplot2 objects
#' 
crosstab_plot <- function(data, var, by, xlab="", ylab="Proportion", fillab="", ...) {
  
  cpal <- RColorBrewer::brewer.pal(6, "Set1")
  
  #  Calculate gender proportions
  df.prop <- data |> 
    filter(!is.na({{by}})) |> 
    group_by({{by}}, {{var}}) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by({{by}}) |> 
    mutate(
      prop = n / sum(n),
      label = ifelse(prop > 0.03, paste0(n, " (", scales::percent(prop, accuracy = 1), ")"), "")
    )
  
  # Stacked bar chart
  ggplot(df.prop, aes(x = {{by}}, y = prop, fill = {{var}})) +
    geom_col(width = 0.8, position = "stack") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3.5, lineheight = 0.9) +
    scale_fill_manual(values = cpal) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = xlab, y = ylab, fill = fillab) +
    theme_light()
  
}


