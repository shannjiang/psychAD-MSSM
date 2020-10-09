library(plyr)
library(synapser)
synLogin('username','password')

#load HBCC atacseq and chipseq data
HBCC_atacseq_data = read.csv(synGet(entity='syn23006232')$path, header = T)
HBCC_chipseq_data = read.csv(synGet(entity='syn23006230')$path, header = T)
colnames(HBCC_chipseq_data)[1] = "Projects"

#remove duplicated samples
HBCC_atacseq_data = unique(HBCC_atacseq_data, by = colnames(HBCC_atacseq_data))
HBCC_chipseq_data = unique(HBCC_chipseq_data, by = colnames(HBCC_chipseq_data))

#Combine HBCC_atacseq and chipseq
HBCC_chipseq_data = HBCC_chipseq_data[,colnames(HBCC_atacseq_data)]
HBCC_assay_data = rbind(HBCC_atacseq_data, HBCC_chipseq_data)

#load CMC Human clinical metadata
CMC_clinical_data = read.csv(synGet(entity='syn2279441')$path, header = T)
CMC_clinical_data$CMC_Brain_ID = paste("CMC",CMC_clinical_data$Brain.ID,sep = "_")
CMC_clinical_data[CMC_clinical_data == "CMC_NA"] = NA

#Compare to CMC clinical data
HBCC_assay_data_matched_with_CMC = join(HBCC_assay_data, CMC_clinical_data, by = "Brain.ID")

#Remove columns with all NA values
HBCC_assay_data_matched_with_CMC[HBCC_assay_data_matched_with_CMC=="N/A"] = NA
HBCC_assay_data_matched_with_CMC_v2 = HBCC_assay_data_matched_with_CMC[,colSums(is.na(HBCC_assay_data_matched_with_CMC)) < nrow(HBCC_assay_data_matched_with_CMC)]

#Match with CMC ChIPseq dissection
CMC_chipseq_meta = read.csv(synGet(entity='syn20549418')$path, header = T)
CMC_chipseq4mapping = subset(CMC_chipseq_meta, select = c(Brain_ID, Institution_Dissection_ID))
CMC_chipseq4mapping_unique = unique(CMC_chipseq4mapping, by = colnames(CMC_chipseq4mapping))
colnames(CMC_chipseq4mapping_unique) = c("Brain.ID","CMC_ChIPseq_dissection_ID")
HBCC_assay_data_matched_with_CMC_v3 = join(HBCC_assay_data_matched_with_CMC_v2, CMC_chipseq4mapping_unique, by = c("Brain.ID"))
HBCC_assay_data_matched_with_CMC_v3$CMC_ChIPseq_dissection_ID[1:285] = NA
HBCC_assay_data_matched_with_CMC_v3$CMC_ChIPseq_dissection_ID = gsub("HBCC_PFC_","",HBCC_assay_data_matched_with_CMC_v3$CMC_ChIPseq_dissection_ID)
HBCC_assay_data_matched_with_CMC_v3$Dissection_ID_equal <- HBCC_assay_data_matched_with_CMC_v3$Dissection.ID == HBCC_assay_data_matched_with_CMC_v3$CMC_ChIPseq_dissection_ID

#Match with RIN
CMC_rnaseq_meta = read.csv(synGet(entity='syn16816488')$path, header = T)
CMC_HBCC_rnaseq_meta = CMC_rnaseq_meta[CMC_rnaseq_meta$Brain_ID %in% HBCC_assay_data_matched_with_CMC_v3$Brain.ID,]
CMC_HBCC_rnaseq_meta = CMC_HBCC_rnaseq_meta[grep("HBCC_PFC", CMC_HBCC_rnaseq_meta$Institution_Dissection_ID),]
CMC_HBCC_rnaseq_meta$Dissection.ID = gsub("HBCC_PFC_","",CMC_HBCC_rnaseq_meta$Institution_Dissection_ID)
CMC_HBCC_rnaseq_meta4join = subset(CMC_HBCC_rnaseq_meta, select = c(Dissection.ID, rnaSeq_isolation.RIN, rnaSeq_report.Exclude_Reason))
HBCC_assay_data_matched_with_CMC_v4 = join(HBCC_assay_data_matched_with_CMC_v3, CMC_HBCC_rnaseq_meta4join, by = "Dissection.ID")
HBCC_assay_data_matched_with_CMC_v5 = unique(HBCC_assay_data_matched_with_CMC_v4, by = colnames(HBCC_assay_data_matched_with_CMC_v4))

write.csv(HBCC_assay_data_matched_with_CMC_v5, file = "HBCC_assay_data_matched_with_CMC_09282020.csv")

## upload on synapse
file <- synStore(File(path = "HBCC_assay_data_matched_with_CMC_09282020.csv", parent = "syn22399913"),
               used = list(list(name = "HBCC_atacseq_data", url = "https://www.synapse.org/#!Synapse:syn23006232", wasExecuted = FALSE),
                           list(name = "HBCC_chipseq_data", url = "https://www.synapse.org/#!Synapse:syn23006230", wasExecuted = FALSE),
                           list(name = "CMC_clinical_data", url = "https://www.synapse.org/#!Synapse:syn2279441", wasExecuted = FALSE),
						   list(name = "CMC_rnaseq_meta", url = "https://www.synapse.org/#!Synapse:syn16816488", wasExecuted = FALSE),
                           list(name = "HBCC atacseq chipseq merged data", url = "https://github.com/shannjiang/psychAD-MSSM/blob/master/VA_Clinical_Brain_region_merging_synapse_v2.r", wasExecuted = TRUE)),
               activityName = "Merge HBCC atacseq chipseq merged data",
               activityDescription = "Create the merged HBCC atacseq and chipseq data")

