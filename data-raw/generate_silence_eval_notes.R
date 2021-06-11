# thank you
# https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r
get_missing_global_variables <- function(wd = getwd()) {

  # Run devtools::check() and reprex the results
  check_output <- reprex::reprex(input = sprintf("devtools::check(pkg = '%s', vignettes = FALSE)\n", wd),
                                 comment = "")

  # Get the lines which are notes about missing global variables, extract the variables and
  # construct a vector as a string
  missing_global_vars <- check_output %>%
    stringr::str_squish() %>%
    paste(collapse = " ") %>%
    stringr::str_extract_all("no visible binding for global variable '[^']+'") %>%
    `[[`(1) %>%
    stringr::str_extract("'.+'$") %>%
    stringr::str_remove("^'") %>%
    stringr::str_remove("'$") %>%
    unique() %>%
    sort()

  # Get a vector to paste into `globalVariables()`
  to_print <- if (length(missing_global_vars) == 0) {
    "None"
  } else {
    missing_global_vars %>%
      paste0('"', ., '"', collapse = ", \n  ") %>%
      paste0("c(", ., ")")
  }

  # Put the global variables in the console
  cat("Missing global variables:\n", to_print)

  # Return the results of the check
  invisible(missing_global_vars)

}

# get the names
v <- get_missing_global_variables()

# write to file
sink("R/silence_tidy_eval_notes.R")

for (i in 1:length(v)) {
  message(glue::glue("i {i} length {length(v)}"))

  if (i != length(v)) {
    cat(glue::glue("{v[i]} <-"))
    cat("\n")
  } else {
    cat(glue::glue("{v[i]} <- NULL"))
  }


}

sink()

