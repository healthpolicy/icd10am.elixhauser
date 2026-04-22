# data-raw/build_data.R
# Build internal lookup data for icd10am.elixhauser.
# Run this script interactively from the package root to regenerate R/sysdata.rda.
#
# Source files (edit these when ICD-10-AM editions are updated):
#   elixhauser_icd10am_codes.xlsx  — ICD-10-AM to Elixhauser category mapping
#   elixhauser_weights.xlsx        — van Walraven weights per category (elixhauser sheet)

library(readxl)

char_elix_codes <- as.data.frame(
  readxl::read_excel("data-raw/elixhauser_icd10am_codes.xlsx",
                     sheet = 1,
                     col_types = c("text", "text", "text", "numeric", "numeric",
                                   "text", "text", "text", "text", "text", "text"))
)

elix_weights <- as.data.frame(
  readxl::read_excel("data-raw/elixhauser_weights.xlsx",
                     sheet = "elixhauser",
                     col_types = c("text", "text", "numeric"))
)

usethis::use_data(char_elix_codes, elix_weights, internal = TRUE, overwrite = TRUE)
