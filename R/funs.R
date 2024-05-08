#' Prepare Structure Expected For File Containing Prefixes
#'
#' @param path path where save the structure (with file name and extension)
#'
#' @return
#' Side effect- saves file.
#' @noRd
prepare_structure <- function(path) {
  writexl::write_xlsx(data.frame(prefix = NA,
                                 operator = NA),
                      path = path,
                      format_headers = FALSE)
}

#' Get Uploaded Prefixes And Transform Into Named Vector
#'
#' @param path path to where is file to download (with file name and extension)
#'
#' @return
#' Named vector of type `integer`. The same length as number of rows in uploaded prefixes file.
#' Operators will become names and prefixes the values of vector.
#' @noRd
get_prefixes <- function(path) {
  prefixes_operators <- readxl::read_xlsx(path, col_types = c("numeric", "text"))
  prefixes <- prefixes_operators$prefix
  names(prefixes) <- prefixes_operators$operator
  prefixes # we need named vector
}

#' Generate Numbers Based On Data From Prefixes File
#'
#' @param prefix named vector of type integer.
#'
#' @return
#' Named integer vector.
#' @details 
#' `seq.int` is not vectorized, so we need to use `lapply` (this is used in `draw_sample` fun).
#' @noRd
generate_numbers <- function(prefix) {
  seq.int(from = fill_number(0, prefix), to = fill_number(9, prefix), by = 1L)
}

#' Fill Prefix Number Using Zero
#'
#' @param num_part number to fill in for the first generated number.
#' @param num_to_fill number to fill in for the last generated number.
#'
#' @return
#' Integer vector length 1 with added zeroes or nines (depends on `num_to_fill` arg) - as much as needed to have exactly 9-digit number.
#' 9-digit, because all phone numbers in Poland have this number of digits.
#' @noRd
fill_number <- function(num_to_fill, num_part) {
  paste0(num_part, paste0(rep(num_to_fill, 9 - nchar(num_part)),
                          collapse = ""),
         collapse = "") |> 
    as.integer()
}

#' Draw Sample of Generated Numbers
#'
#' @param size how many numbers to draw?
#' @param prefixes named vector of prefixes from which to generate numbers and then draw sample.
#'
#' @return
#' Integer vector of length specified in `size` parameter.
#' @noRd
draw_sample <- function(size, prefixes) {
  numbers <- lapply(prefixes, generate_numbers) |> 
    AnnotationDbi::unlist2(use.names = TRUE)
  if (size > length(numbers)) {
    size <- length(numbers)
  }
    sample(numbers, size, replace = FALSE)
}

#' Transform Named Vector To `data.frame` Object
#'
#' @param data_to_transform named vector to transform.
#'
#' @return
#' `data.frame` of the same length as vector passed in.
#' @noRd
transform_to_df <- function(data_to_transform) {
  data.frame(number = data_to_transform,
             operator = names(data_to_transform))
}

#' Save Data As `.xlsx` File
#'
#' @param sample_to_save data to save.
#' @param path path to where save file (with file name and extension)
#'
#' @return
#' Side effect - saves file on disk.
#' @noRd
save_sample <- function(sample_to_save, path) {
  writexl::write_xlsx(sample_to_save, path, format_headers = FALSE)
}

#' Show Operators Distribution On Plot
#'
#' @param sampled_data data to plot. This will be generated phone numbers after sampling.
#'
#' @return
#' `ggplot2` object.
#' @noRd
show_operators_distribution <- function(sampled_data) {
  sampled_data |> 
    dplyr::count(operator) |> 
    dplyr::mutate(percent = round(n / sum(n), 4)) |> 
    ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(operator, percent), percent, label = scales::percent(percent, accuracy = 0.01, decimal.mark = ","))) +
    ggplot2::geom_col(fill = "#DE6FA1") +
    ggplot2::geom_label() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(face = "bold", colour = "#200712", size = 10),
                   axis.text.x = ggplot2::element_text(color = "darkgrey"))
}
