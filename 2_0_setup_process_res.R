# get directories and classifications of variables
dir_code <- "/n/dominici_nsaph_l3/Lab/projects/vwang_causal_airpol_pd/code/"
source(paste0(dir_code, "constants.R"))

# initiate table for regression results (coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "Coefficient", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/estimates_same.txt"),
    append = TRUE)

# initiate table for m-out-of-n bootstrap standard errors (of coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "BootSE", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_same.txt"),
    append = TRUE)
# initiate table for regression results (coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "Coefficient", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/estimates_prior.txt"),
    append = TRUE)

# initiate table for m-out-of-n bootstrap standard errors (of coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "BootSE", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "naive_approach/boot_SE_prior.txt"),
    append = TRUE)

# initiate table for regression results (coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "Coefficient", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "mutually_adj_approach/estimates_same.txt"),
    append = TRUE)

# initiate table for m-out-of-n bootstrap standard errors (of coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "BootSE", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "mutually_adj_approach/boot_SE_same.txt"),
    append = TRUE)
# initiate table for regression results (coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "Coefficient", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "mutually_adj_approach/estimates_prior.txt"),
    append = TRUE)

# initiate table for m-out-of-n bootstrap standard errors (of coefficient for exposure)
cat(paste("Exposure", "Method", "Trim", "BootSE", sep = ","),
    sep = "\n",
    file = paste0(dir_results, "mutually_adj_approach/boot_SE_prior.txt"),
    append = TRUE)