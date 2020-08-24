#' ADA's standard label length check
#'
#' Checks a character vector for any rows that contain >= 255 characters.
#'
#' @param label character vector; a vector containing all variable labels from a dataset. Can be retrieved using \link[sjlabelled]{get_label}
#'
#' @return Returns a filtered tibble with strings over the 255 word limit, and their length count.
#'
#'
#' @export

check_labellength <- function(label) {
  # Before processing, check if passing a character vector
  if (!is_character(label)) {
    print("Error: 'label' is not a character vector.")
  } else { # Check char
    check <- tibble(label) %>%
      rowwise() %>%
      mutate(length = nchar(label)) %>%
      filter(length >= 255)

    print(check$label)
  }
}
