# Some utility functions for checking the package
# (in the alpha phase)

test_fn <- function(bad_strings = c("func")) {
  functions <- list.files("R") %>%
  	 stringr::str_replace_all(pattern = "\\.R",
      replacement = "")

  # This is bad programming. Given the intention of
  # the functions it should be totally fine.
  ids <- unique(unlist(purrr::map(bad_strings, grep,
    x = functions)))
  if (length(ids > 0)) {
    warning(paste("Please fix the following function names:",
      paste(functions[ids], collapse = ", "),
      sep = "\n"))
  } else {
    message("Function names are fine!")
  }
}


get_all_prm <- function() {
  prm_files <- list.files("R", full.names = T)

  # Exclude due to non existent parameters. Somewhat
  # hard-coded.
  exclude <- purrr::map(c("data.R", "utils.R", "pipe.R"),
    grep, x = prm_files) %>% purrr::flatten() %>%
    unlist()
  prm_names <- prm_files[-exclude] %>% purrr::map(.,
    readLines)

  ids <- purrr::map(prm_names, grep, pattern = "@param")
  prm_names <- purrr::map2(prm_names, ids, ~.x[.y])

  # Extract parameters from vector of strings.
  get_prm_name <- function(string) {
    result <- stringr::str_split_fixed(string,
      pattern = "@param ", n = 2)[, 2]
    stringr::str_split_fixed(result, pattern = " ",
      n = 2)[, 1]
  }

  # Apply to all prm_names
  out <- tibble::tibble(file = prm_files[-exclude],
    prms = purrr::map(prm_names, get_prm_name))
  return(out)
}

check_tbl <- get_all_prm()
unlist(check_tbl$prms)

check_tbl$prms[[2]]

# Update vector of accepted names from time to time.
# Might be useful lateron to check if new flawed names are introduced.
accepted_names <- sort(unique(unlist(check_tbl$prms)))

get_flawed_function <- function(pot_flawed = c("gam_list",
  "index_current", "index_ref", "input_tibble", "model.gamm",
  "varname"), name_tbl = get_all_prm()) {
  result <- vector(mode = "list", length = length(pot_flawed))
  for (i in seq_along(result)) {
    result[[i]] <- tibble::tibble(prmn = pot_flawed[i],
      fn = name_tbl$file[purrr::map_lgl(name_tbl$prms,
        ~any(. %in% pot_flawed[i]))])
  }
  tibble::as_tibble(do.call(rbind, result))
}

# Check if functions are executed within their
# respective examples
check_example <- function() {
  prm_files <- list.files("R", full.names = T)

  # Exclude due to non existent parameters. Somewhat
  # hard-coded.
  exclude <- purrr::map_int(c("data.R", "utils.R",
    "pipe.R"), grep, x = prm_files)
  prm_raw <- prm_files[-exclude] %>% purrr::map(.,
    readLines)

  # Extract example lines
  prm_exp <- purrr::map(prm_raw, ~.[grep(pattern = "#'",
    x = .)])
  prm_exp <- purrr::map(prm_exp, ~.[(grep(pattern = "@examples",
    x = .) + 1):length(.)])
  dont_run <- grepl(pattern = "dontrun\\{", x = prm_exp)
  prm_exp[dont_run] <- purrr::map(prm_exp[dont_run],
    ~.[-(grep(pattern = "dontrun\\{", x = .):grep(pattern = "\\}",
      x = .))])
  prm_exp <- purrr::map(prm_exp, ~.[!grepl(pattern = "#' #",
    x = .)])

  # Let´s see if the functions are called within the
  # example
  fn <- gsub(pattern = ".R", replacement = "", x = basename(prm_files[-exclude]))
  func_call <- purrr::map2_lgl(.x = prm_exp, .y = fn,
    ~any(grepl(pattern = .y, x = .x)))
  tibble::tibble(functionname = fn, call_in_example = func_call)
}

# check_this <- check_example()
# check_this[check_this$call_in_example == FALSE, ]
