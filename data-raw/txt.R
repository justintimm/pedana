# Prepare ----------------------------------------------------------------------

txt <- read.csv2("data-raw/txt.csv")[, c("var", "en", "de")]

# Save -------------------------------------------------------------------------

usethis::use_data(txt, internal = TRUE, overwrite = TRUE)
rm(txt)

# Check data -------------------------------------------------------------------

tools::checkRdaFiles("R/sysdata.rda")
