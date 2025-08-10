# ------------------------------------------------------------------------------
# IV Falsification Tests: Survey Summary Generator
# 
# This script processes a machine-readable survey of instrumental variable (IV) 
# papers in economics to generate summary statistics and LaTeX files used in the paper.
#
# INPUT:
# - "NC_literature_survey.csv": Coded dataset of 140 IV papers (RA-coded).
#
# OUTPUT:
# - "survey_summary.csv": Clean summary statistics for each journal and overall.
# - "survey_summary.tex": LaTeX-formatted summary table (Table 2).
# - "stats.tex": Inline LaTeX commands with key statistics for use in the manuscript.
# ------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(xtable)

# --- Load and clean data -------------------------------------

references <- read_csv("Literature_survey/NC_literature_survey.csv") %>% clean_names() %>%
  mutate(papers = TRUE) %>% # abbreviate journal names using recode_factor:
  mutate(journal = recode_factor(journal, "Quarterly" = "QJE", "Review of Economic Studies" = "REStud", "Econometrica" = "ECMA")) %>%
  dplyr::select(journal, papers, is_falsification, contains("falsification"), contains("test_type"), iv_control_in_pi_test, nc_count) %>%
  mutate(test_type_replace_pi = test_type_pi  & !(iv_control_in_pi_test)) %>%
  dplyr::select(-test_type_pi)

# --- Compute falsification summary table ---------------------
falsification_any <- references %>% 
  group_by(journal) %>% filter(!is.na(is_falsification)) %>%
  dplyr::select(-iv_control_in_pi_test) %>%
  summarize_if(is.logical, .funs = sum, na.rm = T) %>%
  mutate(falsification_share = is_falsification/papers)

falsification_any_total <- references %>%
  filter(!is.na(is_falsification)) %>%
  dplyr::select(-iv_control_in_pi_test) %>%
  summarize_if(is.logical, .funs = sum, na.rm = T) %>%
  mutate(falsification_share = is_falsification/papers)

falsification_types <- references %>%
  filter(is_falsification) %>%
  dplyr::select(-iv_control_in_pi_test) %>%
  dplyr::select(-papers) %>%
  group_by(journal) %>%
  summarise_if(is.logical, .funs = sum, na.rm = T) %>%
  mutate_if(is.integer, .funs = function(x) x/.$is_falsification)

falsification_types_total <- references %>%
  filter(is_falsification) %>%
  dplyr::select(-iv_control_in_pi_test) %>%
  dplyr::select(-papers) %>%
  summarise_if(is.logical, .funs = sum, na.rm = T) %>%
  mutate_if(is.integer, .funs = function(x) x/.$is_falsification)

nc_count_median <- references %>%
  mutate(nc_count = as.integer(nc_count)) %>%
  dplyr::select(journal, nc_count) %>%
  group_by(journal) %>%
  summarise(median_nc = median(nc_count, na.rm = T))

nc_count_median_total <- references %>%
  mutate(nc_count = as.integer(nc_count)) %>%
  dplyr::select(journal, nc_count) %>%
  summarise(median_nc = median(nc_count, na.rm = T))

iv_control_in_pi_test <- references %>%
  filter(is_falsification) %>%
  mutate(iv_control_in_pi_test = ifelse(is.na(iv_control_in_pi_test), 0, iv_control_in_pi_test)) %>%
  group_by(journal) %>%
  summarise(iv_control_in_pi_test = mean(iv_control_in_pi_test, na.rm = T))

iv_control_in_pi_test_total <- references %>%
  filter(is_falsification) %>%
  mutate(iv_control_in_pi_test = ifelse(is.na(iv_control_in_pi_test), 0, iv_control_in_pi_test)) %>%
  summarise(iv_control_in_pi_test = mean(iv_control_in_pi_test, na.rm = T))

survey_summary <- falsification_any %>%
  dplyr::select(journal, papers, falsification_share) %>%
  left_join(falsification_types, by = "journal") %>%
  left_join(iv_control_in_pi_test, by = "journal") %>%
  left_join(nc_count_median, by = "journal") %>%
  dplyr::select(-is_falsification) %>% arrange(-papers)

survey_summary_total <- falsification_any_total %>%
  dplyr::select(papers, falsification_share) %>%
  bind_cols(falsification_types_total) %>%
  bind_cols(iv_control_in_pi_test_total) %>%
  bind_cols(nc_count_median_total) %>%
  dplyr::select(-is_falsification) %>%
  mutate(journal = "All")

# format all columns except for papers and median_nc as two decimal digits
summary_table_final <- bind_rows(survey_summary_total, survey_summary) %>% 
  mutate(across(.cols = -c(papers, median_nc, journal), ~sprintf("%.2f", round(., 2)))) %>%
  dplyr::select(journal, everything())

write_csv(summary_table_final, file = "Literature_survey/survey_summary.csv")

# --- Compute inline LaTeX stats ------------------------------

# delete stats.tex if it exists:
if (file.exists("Literature_survey/stats.tex")) {
  file.remove("Literature_survey/stats.tex")
}

write_stat <- function(stat, commandName) {
  # save stats as custom Latex command name
  # change %.2f to integer formatting:
  cat(sprintf("\\newcommand{\\%s}{%.0f\\%%}\n", commandName, stat*100),
      file = "Literature_survey/stats.tex", append = TRUE)
}

# Falsification share of all papers
falsification_share <- references %>%
  summarise(falsification_share_total = mean(is_falsification, na.rm = T))
write_stat(falsification_share, "falsificationShare")

# NCO share of all falsification:
nco_share_of_falsification <- references %>%
  filter(is_falsification == T) %>%
  summarise(nco_share = mean(falsification_type_nco, na.rm = T))
write_stat(nco_share_of_falsification, "ncoShareOfFalsification")

# NCI share of all falsification:
nci_share_of_falsification <- references %>%
  filter(is_falsification == T) %>%
  summarise(nci_share = mean(falsification_type_nci, na.rm = T))
write_stat(nci_share_of_falsification, "nciShareOfFalsification")

# Other share of all falsification:
other_share_of_falsification <- references %>%
  filter(is_falsification == T) %>%
  summarise(other_share = mean(falsification_type_other, na.rm = T))
write_stat(other_share_of_falsification, "otherShareOfFalsification")


# Pseudo Outcome share of NCO tests:
po_share_of_nco <- references %>%
  filter(falsification_type_nco == T) %>%
  summarise(po_share = mean(test_type_po, na.rm = T))
write_stat(po_share_of_nco, "poShareOfNco")

# Share of NCI tests that replace the IV:
replace_pi_share_of_nci <- references %>%
  filter(falsification_type_nci == T) %>%
  summarise(pi_share = mean(test_type_replace_pi, na.rm = T))
write_stat(replace_pi_share_of_nci, "replaceIVShareOfNci")

# Pseudo IV share of NCI tests:
replace_pi_share_of_nci <- references %>%
  filter(falsification_type_nci == T) %>%
  summarise(pi_share = mean(test_type_replace_pi | iv_control_in_pi_test, na.rm = T))
write_stat(replace_pi_share_of_nci, "replaceOrAddShareOfNCI")

# Share of all Pseudo IVs that control for IV:
iv_control_in_pi_test <- references %>%
  filter(test_type_replace_pi | iv_control_in_pi_test) %>%
  summarise(iv_control_in_pi_test = mean(iv_control_in_pi_test, na.rm = T))
write_stat(iv_control_in_pi_test, "ivControlInPiTest")

# fraction of papers with nc_count == 1:
nc_count_1 <- references %>%
  filter(is_falsification == T) %>%
  mutate(nc_count_1 = ifelse(nc_count == 1, 1, 0)) %>%
  summarise(nc_count_1 = mean(nc_count_1, na.rm = T))
# Write to a .tex file
write_stat(nc_count_1, "ncCountOne")

# Convert data frame to an xtable object
xtable_obj <- xtable(summary_table_final)

# Customize the column names 
colnames(xtable_obj) <- c("", "Papers Reviewed", "Share with Falsification Tests", "Negative Control Outcome", "Negative Control Instrument", "Other", "Replace Outcome with NCO", "Replace IV with NCI", "Add NCI to Reduced-Form", "# Negative Controls Used (median)")

# LaTeX export
print(
  xtable_obj,
  include.rownames = FALSE,
  add.to.row = list(pos = list(1),
                    command = c("\\textit{by Journal:}\\\\")),
  hline.after = c(-1, 0, 6),
  file = "Literature_survey/survey_summary.tex"
)


