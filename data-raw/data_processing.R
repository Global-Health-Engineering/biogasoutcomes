






## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
write_csv(DATASET, here::here("inst", "extdata", "DATASET.csv"))
openxlsx::write.xlsx(DATASET, here::here("inst", "extdata", "DATASET.xlsx"))


