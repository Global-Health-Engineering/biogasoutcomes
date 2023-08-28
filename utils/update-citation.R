#install.packages("cffr")
library(cffr)

packageVersion("cffr")

# Hard code doi
doi <- "10.5281/zenodo.8289621"

# creates CFF with all author roles
mod_cff <- cff_create("DESCRIPTION",
                      dependencies = FALSE,
                      keys = list("doi" = doi,
                                  "date-released" = Sys.Date()))

# writes the CFF file
cff_write(mod_cff)

# Now write a CITATION file from the CITATION.cff file
# Use inst/CITATION instead (the default if not provided)
path_cit <- file.path("inst/CITATION")

write_citation("CITATION.cff", file = path_cit)

# By last, read the citation
cat(readLines(path_cit), sep = "\n")

# Check by reading, use meta object
meta <- packageDescription("biogasoutcomesmalawi")
meta$Encoding <- "UTF-8"

utils::readCitationFile(path_cit, meta)

# Start of inst/CITATION
utils::readCitationFile("inst/CITATION")
