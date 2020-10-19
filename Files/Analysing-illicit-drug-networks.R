# Creating clusters or Chemical Classes (CC) for specimens ----------------

# Calculating similarity between specimens based on the optimal comparison metric (CM)
Scores_GCMS <- as.dist(get(OPT_GCMS_CM)(as.matrix(GCMS)), diag = FALSE, upper = FALSE)

# Creating a correlation matrix for the viewing in the app
CCM <- as.matrix(Scores_GCMS)
CCM <- round(CCM,2)
CCM[upper.tri(CCM)] <- ""
diag(CCM) <- ""
CCM <<- as.data.frame(CCM)

# Defining the Optimum linkage method for clustering analysis
OPT_LM <- rownames(as.matrix(which.max(mapply(
  function(LM){cor(Scores_GCMS, cophenetic(hclust(d = Scores_GCMS, method = LM)))},
  c("ward.D","ward.D2","single","complete","average","mcquitty")))))

# Defining chemical classes using hierarchical clustering analysis and the THV (cutoff value for clustering)
CC_GCMS <- hclust(Scores_GCMS, method = OPT_LM)
clusters <- data.frame(CC = dendextend::cutree(CC_GCMS, h = input$THV, 
                                               order_clusters_as_data = F))

# Adding specimen-CC codes to Lookup
Lookup$cluster <<- clusters$CC[match(Lookup$Specimen, rownames(clusters))]



# Creating network plots between CCs and respective Groups ----------------
#Note: Trying to plot the relationship betweeen CC and specimen results in a messy graph.
#A cleaner alternative is plotting the relationships between CC and specimen groups. 

# Extracting date information (onset and terminus) for each CC
Net_CC <- do.call(data.frame, aggregate(Date~cluster,Lookup, function(x) c(min(x),max(x))))
Net_CC <- setNames(Net_CC,c("vertex.id","onset","terminus"))
Net_CC$terminus <- Net_CC$terminus+1
Net_CC$type <- "CC"
Net_CC$name <- paste0("CC_",Net_CC$vertex.id)

# Extracting date information (onset and terminus) for each specimen group (SG)
Net_SG <- Lookup[,c("Group","cluster")]
names(Net_SG)[1] <- "name"
Net_SG$terminus <- Net_CC$terminus[match(Net_SG$cluster,Net_CC$vertex.id)]
Net_SG <- aggregate(terminus~name,Net_SG,max)
Net_SG$onset <- as.numeric(Lookup$Date[match(Net_SG$name,Lookup$Group)])
Net_SG$type <- "SG"
Net_SG$vertex.id <- Net_SG$name

# Creating a dataframe of unique nodes
Nodes <- rbind(Net_CC,Net_SG)
Nodes$onset.censored <- F
Nodes$terminus.censored <- F
Nodes <- Nodes[c("name","onset","terminus","vertex.id","onset.censored","terminus.censored","type")]

# Creating a dataframe of unique links
Links <- data.frame(from=Lookup$cluster,to=Lookup$Group, stringsAsFactors = F)
Links$onset <- Nodes$onset[match(Links$to,Nodes$vertex.id)]
Links$terminus <- Nodes$terminus[match(Links$from,Nodes$vertex.id)]
Links$onset.censored <- F
Links$terminus.censored <- F
Links$from <- paste0("CC_",Links$from)