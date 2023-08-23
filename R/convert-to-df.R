

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

# Read the markdown file
markdown_text <- readLines("data/01_2021-05-31.md")

# Initialize an empty vector for the text strings
text_strings <- c()

# Initialize a variable to store the current text string
current_string <- ""

# Iterate through the lines of the markdown file
for (line in markdown_text) {
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

# Create a one-column tibble with the text strings

text_tibble <- tibble::tibble(text = text_strings)

library(dplyr)
library(tidyr)
library(purrr)

tib <- text_tibble |> 
    mutate(interviewer = case_when(
        str_detect(text, "Interviewer") == TRUE ~ TRUE
    )) |> 
    mutate(interviewee = case_when(
        str_detect(text, "Interviewee") == TRUE ~ TRUE
    )) |> 
    filter(!(is.na(interviewer) & is.na(interviewee))) 


# Identify the row where TRUE follows TRUE in the interviewer variable
index_interviewer <- which(lag(tib$interviewer) == TRUE & tib$interviewer == TRUE)

if (length(index_interviewer) > 0) {
    stop("Check input file to see if value for interviewer was repeated")
}

# Identify the row where TRUE follows TRUE in the interviewer variable
index_interviewee <- which(lag(tib$interviewee) == TRUE & tib$interviewee == TRUE)

if (length(index_interviewee) > 0) {
    stop("Check input file to see if value for interviewee was repeated")
}


# -------------------------------------------------------------------------


tib2 <- tib |> 
    pivot_longer(cols = !text,
                 names_to = "person",
                 values_to = "value") |> 
    filter(value == TRUE) |> 
    select(-value) |> 
    #mutate(id = seq(1:n())) |>
    mutate(text = str_remove(text, fixed(" **Interviewer:** "))) |> 
    mutate(text = str_remove(text, fixed(" **Interviewee:** ")))


# -------------------------------------------------------------------------



tib2 |> 
    pivot_wider(names_from = person,
                values_from = text, 
                values_fn = list) |> 
    rectangularize() 


# -------------------------------------------------------------------------

tib2 |> 
    mutate(interview_date = lubridate::ymd("2021-05-31"),
           interview_id = 1,
           question_id = seq(1:n()))
