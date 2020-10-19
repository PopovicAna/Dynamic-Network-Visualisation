# Optimisation of the linked and unlinked specimen populations: -----------

# Reading in several functions needed for subsequent analysis
 source("Files/DataOptFunctions.R", local = T)

#### GCMS
# Applying pre-treatments
GCMS_TV_N <- GCMS_T/rowSums(GCMS_T)
GCMS_TV_N2R <- GCMS_TV_N^0.5
GCMS_TV_N4R <- GCMS_TV_N^0.25

# Applying population rules and comparison metrics to the data
GCMS_PT_CM_R <-  
  lapply(  
    list(N=GCMS_TV_N,N2R=GCMS_TV_N2R,N4R=GCMS_TV_N4R), 
    function(X){
      lapply(
        list(CAN=CAN,EUC=EUC,MAN=MAN,MCF=MCF,PCC=PCC),
        function(CM,PT){
          list(
            R1 = R1(CM(as.matrix(PT))),
            R2 = data.frame(R2(CM(L(PT)),CM(U(PT)))),
            R3 = R3(CM(as.matrix(PT))),
            R4 = data.frame(R4(CM(L(PT)),CM(as.matrix(PT)))))
        },
        PT=X)
    })

# Adding in comparison metric and pupulation rule info
for(i in 1:length(GCMS_PT_CM_R)){
  for(j in 1:length(GCMS_PT_CM_R[[1]])){
    for(k in 1:length(GCMS_PT_CM_R[[1]][[1]])){
      GCMS_PT_CM_R[[i]][[j]][[k]]$Freq = (GCMS_PT_CM_R[[i]][[j]][[k]]$Freq/max(GCMS_PT_CM_R[[i]][[j]][[k]]$Freq))*100
      GCMS_PT_CM_R[[i]][[j]][[k]]$PT = rep(c("N","N2R","N4R")[i],nrow(GCMS_PT_CM_R[[i]][[j]][[k]]))
      GCMS_PT_CM_R[[i]][[j]][[k]]$CM = rep(c("CAN","EUC","MAN","MCF","PCC")[j],nrow(GCMS_PT_CM_R[[i]][[j]][[k]]))
      GCMS_PT_CM_R[[i]][[j]][[k]]$R = rep((paste0("R",k)),nrow(GCMS_PT_CM_R[[i]][[j]][[k]])) 
    }
  }
}

# Extracting the area under curve (AUC) of each reciever operating characteristic (ROC) calculation
GCMS_PT_CM_R <- do.call(rbind,do.call(rbind,do.call(rbind,GCMS_PT_CM_R)))

GCMS_AUC <- GCMS_PT_CM_R %>% 
  group_by(PT,CM,R) %>% 
  group_map(~{roc(.x$label, .x$Freq, levels = c("Inter", "Intra"), direction = ">")$auc})

GCMS_ROC <- data.frame(AUC = unlist(GCMS_AUC)) %>% bind_cols(GCMS_PT_CM_R %>% distinct(PT,CM,R))

# Defining the optimal comparison metric and population rule based on the ROC AUC
OPT_GCMS_AUC <- max(GCMS_ROC$AUC)
OPT_GCMS_PT <- GCMS_ROC[which.max(GCMS_ROC$AUC),"PT"]
OPT_GCMS_CM <- GCMS_ROC[which.max(GCMS_ROC$AUC),"CM"]
OPT_GCMS_PR <- GCMS_ROC[which.max(GCMS_ROC$AUC),"R"]

OPT_GCMS_PT_CM_R <- GCMS_PT_CM_R %>%
  filter(PT == OPT_GCMS_PT,
         CM == OPT_GCMS_CM,
         R == OPT_GCMS_PR)
