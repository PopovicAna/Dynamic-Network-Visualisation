# Reading in the data: ----------------------------------

#Importing the lookup table
Lookup <-  read.xlsx("Data/Profiles.xlsx", sheet = "Lookup", rowNames = F, colNames = T, detectDates = T)

#Importing Gas Chromatpgraphy Mass Spectometry data
GCMS <-  as.data.frame(read.xlsx("Data/Profiles.xlsx", sheet = "GCMS", rowNames = T, colNames = T))

#Importing Isotopic Ratio Mass Spectrometry data
IRMS <-  as.data.frame(read.xlsx("Data/Profiles.xlsx", sheet = "IRMS", rowNames = T, colNames = T))

#Importing Capillary Electrophoresis data
CE <- as.data.frame(read.xlsx("Data/Profiles.xlsx", sheet = "CE", rowNames = T, colNames = T))



# Choosing target variables - GCMS profiles only: -------------------------

# (1) Presence of variables in all specimens
Count <- data.frame(Variables = colnames(GCMS),
                    Percentage = round((colSums(GCMS !=0)/nrow(GCMS)), digits = 5),
                    row.names = NULL)

Bar <- ggplot(Count, aes(Variables, Percentage*100)) +
  geom_bar(stat = "identity") +
  theme_light()+
  theme(axis.title.x = element_text(face="bold",size = 12,margin=margin(10,0,0,0)),
        axis.title.y = element_text(face="bold",size = 12,margin=margin(0,10,0,0)),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, vjust = 0.5),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA),
        legend.position = "none")+
  scale_y_continuous("Percentage of specimens (%)")  +
  coord_cartesian(ylim = c(0,100))

# (2) Intra-varaibility and inter-variability of variables
Specimen_List <- data.frame(row.names = rownames(GCMS))
Specimen_List$Group <- Lookup$Group[match(row.names(Specimen_List),Lookup$Specimen)]
Specimen_List$Count <- plyr::count(Specimen_List,"Group")$freq[match(Specimen_List$Group,plyr::count(Specimen_List,"Group")$Group)]

Lkd <- lapply(c("G_158","G_030","G_162","G_285"),function(SG){
  X = stack(subset(GCMS, row.names(GCMS) %in% row.names(Specimen_List[Specimen_List$Group==SG,])))
  X$Group = rep(SG,nrow(X))
  return(X)
})
ULkd <-  stack(subset(GCMS, row.names(GCMS) %in% row.names(Specimen_List[Specimen_List$Count==1,])))
ULkd$Group <- rep("Multiple",nrow(ULkd))
Group.labs <- c("Inter-variability",
                "Intra-variability of Group A",
                "Intra-variability of Group B",
                "Intra-variability of Group C",
                "Intra-variability of Group D")
names(Group.labs) <- c("Multiple","G_158","G_030","G_162","G_285")

BoxP <- rbind(do.call(rbind,Lkd),ULkd)

psplit <- ggplot(BoxP, aes(x = ind,y = values)) + 
  geom_boxplot(outlier.shape = 1,outlier.size = 1,lwd = 0.3) +
  facet_wrap(~ Group, scale= "fixed", ncol=1, labeller = labeller(Group = Group.labs)) +
  labs(x="Target variables",y="Pre-treated variable intensity") +
  theme_light()+
  theme(axis.title.x = element_text(face="bold", size = 12, margin = margin(10,0,0,0)),
        axis.title.y = element_text(face="bold", size = 12, margin = margin(0,10,0,0)),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# (3) Correlation of variables
M = cor(GCMS, method = "spearman")
corrplot <- ggcorrplot(M, method = "square", type = "upper", outline.col = "white", lab = TRUE, 
                       p.mat = ggcorrplot::cor_pmat(M, method = "spearman"), insig = "blank",
                       colors = c("#6D9EC1", "white", "#E46726")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 0, vjust = 0.5),
        panel.grid.minor = element_blank())