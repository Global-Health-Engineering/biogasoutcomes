# description -------------------------------------------------------------

# This data processing script was written for qualitative data. The data are
# interviews which were originally stored as DOCX files. In each DOCX file,
# the Interviewer and Interviewee are identified using these two labels. After
# initial conversion from DOCX to md format, this script converts the interviews
# into a table format.

# libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# functions ---------------------------------------------------------------

# taken from: https://community.rstudio.com/t/unlist-columns-with-lists-of-different-length/80044/4?u=larnsce

rectangularize <- function(df) {
  max_len <- df %>%
    map(unlist) %>%
    lengths() %>%
    max()

  df %>%
    map(unlist) %>%
    map(~ `length<-`(., max_len)) %>%
    bind_cols()
}

# prompt 3 -----------------------------------------------------------------

# https://www.perplexity.ai/search/cab027ef-a5ef-4b2c-9201-0c990a9dc953?s=c

files <- list.files(path = "data-raw", pattern = "*.md", full.names = TRUE)

markdown_text <- list()

# Initialize an empty list for the text string vectors
text_strings_list <- list()

# Initialize a variable to store the current text string
current_string <- ""

for (i in seq_along(files)) {
  text_strings <- c()
  current_string <- ""
  markdown_text[[i]] <- readLines(files[[i]])
  # Iterate through the lines of the markdown file
  for (line in markdown_text[[i]]) {
    # If the line is not empty, add it to the current text string
    if (line != "") {
      current_string <- paste(current_string, line)
    } else {
      # If the line is empty, add the current text string to the vector
      text_strings <- c(text_strings, current_string)
      # Reset the current text string
      current_string <- ""
    }
  }
  text_strings_list[[i]] <- text_strings
}

text_tibble_list <- map(text_strings_list, ~tibble(text = .))

# Create a one-column tibble with the text strings

identify_person <- function(x) {

  x |>
    mutate(interviewer = case_when(
      str_detect(text, "Interviewer") == TRUE ~ TRUE
    )) |>
    mutate(interviewee = case_when(
      str_detect(text, "Interviewee") == TRUE ~ TRUE
    )) |>
    filter(!(is.na(interviewer) & is.na(interviewee)))
}

text_tibble_list_person <- map(text_tibble_list, identify_person)

check_errors <- function(x) {
  index_interviewer <- which(lag(x$interviewer) == TRUE & x$interviewer == TRUE)

  if (length(index_interviewer) > 0) {
    print(paste("Issue at lines", index_interviewer))
    stop("Check input file to see if value for interviewer was repeated")
  }

  # Identify the row where TRUE follows TRUE in the interviewer variable
  index_interviewee <- which(lag(x$interviewee) == TRUE & x$interviewee == TRUE)

  if (length(index_interviewee) > 0) {
    stop("Check input file to see if value for interviewee was repeated")
  }
}

map(text_tibble_list_person, check_errors)

check_errors(text_tibble_list_person[[6]])

text_tibble_list_person[[6]] |> View()
# -------------------------------------------------------------------------

text_tibble_list_person

tidy_text <- function(x) {
  x |>
    pivot_longer(cols = !text,
                 names_to = "person",
                 values_to = "value") |>
    filter(value == TRUE) |>
    select(-value) |>
    mutate(text = str_remove(text, fixed(" **Interviewer:** "))) |>
    mutate(text = str_remove(text, fixed(" **Interviewee:** ")))
}

text_tibble_list_person_tidy <- map(text_tibble_list_person, tidy_text)

# -------------------------------------------------------------------------

add_missing_values <- function(x) {
  x |>
    pivot_wider(names_from = person,
                values_from = text,
                values_fn = list) |>
    rectangularize()
}

text_tibble_list_person_tidy_complete <- map(text_tibble_list_person_tidy, add_missing_values)

# -------------------------------------------------------------------------

output_list <- list()

for (i in seq_along(files)) {
  output_list[[i]] <- text_tibble_list_person_tidy_complete[[i]] |>
    mutate(interview_date = lubridate::ymd(
      str_extract(files[[i]], "\\d{4}-\\d{2}-\\d{2}")
    ),
    interview_id = str_extract(files[[i]], "(?<=data-raw/)\\d{2}"),
    question_id = seq(1:n()))
}


# -------------------------------------------------------------------------

biogasoutcomes <- output_list |>
  bind_rows() |>
  select(interview_id, interview_date, question_id, interviewer, interviewee)


usethis::use_data(biogasoutcomes, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
readr::write_csv(biogasoutcomes, here::here("inst", "extdata", "biogasoutcomes.csv"))
openxlsx::write.xlsx(biogasoutcomes, here::here("inst", "extdata", "biogasoutcomes.xlsx"))


