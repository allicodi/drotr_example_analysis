# ------------------------------------------------------------------------------
# Example script to run analysis across seeds for single rule
# -------------------------------------------------------------------------------

# Directory for personal R package library
.libPaths("~/Rlibs")

here::i_am("run_analysis.R")

# make sure you have pre-installed drotr package from github
# devtools::install_github("allicodi/drotr")

library(drotr)

# --------------------- Load data and wrappers -------------------------------

source(here::here("R/00_load_and_prep_data.R"))
source(here::here("R/01_wrappers.R"))

# ------------------------------  Set seed ---------------------------------

cargs <- commandArgs(TRUE)
seed <- cargs[[1]]
fit_nuisance <- cargs[[2]]

set.seed(seed)

# Alternative:
# seed <- Sys.getenv("SLURM_ARRAY_TASK_ID")
# set.seed(seed)

# ---------------- Model Specification -------------------

# Nuisance Model Specification

# Simple models for sake of running example quickly
sl.library.outcome <- c("SL.glm", "SL.outcome.1", "SL.outcome.2")

sl.library.treatment <- c("SL.mean", "SL.treatment")

sl.library.missingness <- c("SL.mean", "SL.missing.1", "SL.missing.2")

W_list <- c("rotavirus_new", "rotavirus_bin", "norovirus_new", "norovirus_bin", "adenovirus_new",
            "adenovirus_bin", "sapovirus_new","sapovirus_bin", "astrovirus_new", "astrovirus_bin",
            "st_etec_new", "st_etec_bin", "shigella_new", "shigella_bin", "campylobacter_new",
            "campylobacter_bin", "tepec_new", "tepec_bin", "v_cholerae_new", "v_cholerae_bin",
            "salmonella_new", "salmonella_bin", "cryptosporidium_new", "cryptosporidium_bin",
            "dy1_scrn_vomitall", "dy1_scrn_lstools", "dy1_scrn_sstools", "dy1_scrn_diardays",
            "dy1_scrn_dehydr", "avemuac", "wfazscore", "lfazscore", "wflzscore", "site",
            "dy1_ant_sex", "agemchild", "an_ses_quintile", "an_tothhlt5", "month_en", "rotaseason")

# -------------------  CATE Model Specification ---------------------

Z_list_host <- c("avemuac", "wfazscore", "wflzscore", 
                   "lfazscore", "dy1_ant_sex", 
                   "agemchild", "an_ses_quintile", "an_tothhlt5")

# Simple models for sake of running example quickly
CATE_library_host <- c("SL.ranger",
                       "SL.mean",
                       "SL.cate.host")

threshold_list <- c(0.06, 0.08, 0.10)

# ------------------------- Fit nuisance model and save or load pre-fit nuisance -------------------------

if(fit_nuisance){
    nuisance_output <- learn_nuisance(df = abcd_data,
                                    id_name = "pid",
                                    Y_name = "lazd90",
                                    A_name = "an_grp_01",
                                    W_list = W_list,
                                    sl.library.outcome = sl.library.outcome,
                                    sl.library.treatment = sl.library.treatment,
                                    sl.library.missingness = sl.library.missingness,
                                    outcome_type = "gaussian",
                                    k_folds = 10,
                                    ps_trunc_level = 0.01)

    saveRDS(nuisance_output, file = paste0("/projects/dbenkes/allison/drotr_example_analysis/nuisance/nuisance_seed_", seed, ".Rds"))

} else{
    nuisance_output <- readRDS(file = paste0("/projects/dbenkes/allison/drotr_example_analysis/nuisance/nuisance_seed_", seed, ".Rds"))
}

nuisance_models <- nuisance_output$nuisance_models
k_fold_assign_and_CATE <- nuisance_output$k_fold_assign_and_CATE
validRows <- nuisance_output$validRows

# ------------------------- Rule based on host ----------------------------

results_host <- estimate_OTR(df = abcd_data,
                            Y_name = "lazd90",
                            A_name = "an_grp_01",
                            Z_list = Z_list_host,
                            W_list= W_list,
                            id_name = "pid",
                            sl.library.CATE = CATE_library_host,
                            nuisance_models = nuisance_models,
                            k_fold_assign_and_CATE = k_fold_assign_and_CATE,
                            validRows = validRows,
                            threshold = threshold_list,
                            k_folds = 10,
                            ps_trunc_level = 0.01,
                            outcome_type = "gaussian")

print(results_host)

# Save only otr_results portion of object (excluding model fits) in the project space
saveRDS(results_host$results, file = paste0("/projects/dbenkes/allison/drotr_example_analysis/results/results_host_seed_", seed, ".Rds"))

# Save everything (including models)
# saveRDS(results_host, file = paste0("/projects/dbenkes/allison/drotr_sim/journal/results_real_data/results/host/results_host_seed_", seed, ".Rds"))