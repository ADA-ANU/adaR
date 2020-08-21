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
