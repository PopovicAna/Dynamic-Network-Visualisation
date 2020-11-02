# Optimisation of the linked and unlinked specimen populations: -----------

# Reading in several functions needed for subsequent analysis
 source("Files/DataOptFunctions.R", local = T)

#### GCMS
# Applying pre-treatments
GCMS_TV_N4R <- (GCMS_T/rowSums(GCMS_T))^0.25

# Applying the optimnal population rules and comparison metrics to the data
OPT_GCMS_PT_CM_R <- R4(PCC(L(GCMS_TV_N4R)),PCC(as.matrix(GCMS_TV_N4R)))

# Defining the optimal comparison metric and population rule based on the ROC AUC
OPT_GCMS_PT <- "N4R"
OPT_GCMS_CM <- "PCC"
OPT_GCMS_PR <- "R4"