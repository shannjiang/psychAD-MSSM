library(data.table)
library(plyr)
library(synapser)
synLogin('username','password') 

# load VA Brain region Batch 1 sheet
VA_BR_B1_data = read.csv(synGet(entity='syn22801019')$path, header = T)

#load VA Brain region Batch 2 sheet
VA_BR_B2_data = read.csv(synGet(entity='syn22801018')$path, header = T)

#load VA Clinical data
VA_Clinical_data = read.csv(synGet(entity='syn22801020')$path, header = T)

#VA Brain region Batch 1 duplication checking and output
VA_BR_B1_data_DT = data.table(VA_BR_B1_data)
VA_BR_B1_data_DT_duplication_boolean_code = duplicated(VA_BR_B1_data_DT)

if ('TRUE' %in% as.data.frame(table(VA_BR_B1_data_DT_duplication_boolean_code))$VA_BR_B1_data_DT_duplication_boolean_code) {
VA_BR_B1_data_with_duplication_code = data.frame(VA_BR_B1_data, VA_BR_B1_data_DT_duplication_boolean_code)
VA_BR_B1_data_duplications = VA_BR_B1_data_with_duplication_code[VA_BR_B1_data_with_duplication_code$VA_BR_B1_data_DT_duplication_boolean_code %in% 'TRUE',]
write.csv(VA_BR_B1_data_duplications, file="/path/to/the/destination/folder/VA_BR_B1_data_duplications.csv")
}

VA_BR_B1_data_unique = unique(VA_BR_B1_data, by = colnames(VA_BR_B1_data))

#VA Brain region Batch 2 duplication checking and output
VA_BR_B2_data_DT = data.table(VA_BR_B2_data)
VA_BR_B2_data_DT_duplication_boolean_code = duplicated(VA_BR_B2_data_DT)

if ('TRUE' %in% as.data.frame(table(VA_BR_B2_data_DT_duplication_boolean_code))$VA_BR_B2_data_DT_duplication_boolean_code) {
VA_BR_B2_data_with_duplication_code = data.frame(VA_BR_B2_data, VA_BR_B2_data_DT_duplication_boolean_code)
VA_BR_B2_data_duplications = VA_BR_B2_data_with_duplication_code[VA_BR_B2_data_with_duplication_code$VA_BR_B2_data_DT_duplication_boolean_code %in% 'TRUE',]
write.csv(VA_BR_B2_data_duplications, file="/path/to/the/destination/folder/VA_BR_B2_data_duplications.csv")
}

VA_BR_B2_data_unique = unique(VA_BR_B2_data, by = colnames(VA_BR_B2_data))

#remove duplication in VA Clinical data
VA_Clinical_data_unique = unique(VA_Clinical_data, by = colnames(VA_Clinical_data))

# harmanize column names between Batch 1 and 2
colnames(VA_BR_B1_data_unique)[5] = "Brain.Region"
colnames(VA_BR_B1_data_unique)[9:10] = c("Sex","Race")
colnames(VA_BR_B2_data_unique)[9:11] = c("Loc","Shell","Box")
# remove Note column in Batch 2 because of no content
VA_BR_B2_data_unique = VA_BR_B2_data_unique[,-12]

# move BB and SubNum ahead
VA_BR_B1_data_unique = VA_BR_B1_data_unique[,c(colnames(VA_BR_B1_data_unique)[2:3],colnames(VA_BR_B1_data_unique)[1],colnames(VA_BR_B1_data_unique)[4:11])]
VA_BR_B2_data_unique = VA_BR_B2_data_unique[,c(colnames(VA_BR_B2_data_unique)[2:3],colnames(VA_BR_B2_data_unique)[1],colnames(VA_BR_B2_data_unique)[4:11])]

# partition VA_BR_B1/2 into shared and unique sets
VA_BR_B1_share = VA_BR_B1_data_unique[VA_BR_B1_data_unique$BB %in% VA_BR_B2_data_unique$BB,]
VA_BR_B1_unique = VA_BR_B1_data_unique[!(VA_BR_B1_data_unique$BB %in% VA_BR_B2_data_unique$BB),]

VA_BR_B2_share = VA_BR_B2_data_unique[VA_BR_B2_data_unique$BB %in% VA_BR_B1_data_unique$BB,]
VA_BR_B2_unique = VA_BR_B2_data_unique[!(VA_BR_B2_data_unique$BB %in% VA_BR_B1_data_unique$BB),]

#Horizontally combine VA_BR_B1/2_share
colnames(VA_BR_B1_share)[3:11] = paste("Batch1",colnames(VA_BR_B1_share)[3:11],sep = "_")
colnames(VA_BR_B2_share)[3:11] = paste("Batch2",colnames(VA_BR_B2_share)[3:11],sep = "_")
VA_BR_B1_B2_share = join(VA_BR_B1_share, VA_BR_B2_share, by = c("BB","SubNum"))

#Harmanize VA_BR_B1/2_unique to combine with shared parts
colnames(VA_BR_B1_unique)[3:11] = paste("Batch1",colnames(VA_BR_B1_unique)[3:11],sep = "_")
colnames(VA_BR_B2_unique)[3:11] = paste("Batch2",colnames(VA_BR_B2_unique)[3:11],sep = "_")
share_lookup4B1_unique = VA_BR_B1_B2_share[,-c(3:11)]
VA_BR_B1_unique2 = join(VA_BR_B1_unique,share_lookup4B1_unique,by = c("BB","SubNum"))
share_lookup4B2_unique = VA_BR_B1_B2_share[,-c(12:20)]
VA_BR_B2_unique2 = join(VA_BR_B2_unique,share_lookup4B2_unique,by = c("BB","SubNum"))
VA_BR_B2_unique2 = VA_BR_B2_unique2[,colnames(VA_BR_B1_B2_share)]
VA_BR_combo = rbind(VA_BR_B1_B2_share, VA_BR_B1_unique2, VA_BR_B2_unique2)

#Partition VA BR/Clinical data into shared and unique sets
VA_BR_share = VA_BR_combo[VA_BR_combo$BB %in% VA_Clinical_data_unique$BB,]
VA_BR_unique = VA_BR_combo[!(VA_BR_combo$BB %in% VA_Clinical_data_unique$BB),]
VA_Clinical_share = VA_Clinical_data_unique[VA_Clinical_data_unique$BB %in% VA_BR_combo$BB,]
VA_Clinical_unique = VA_Clinical_data_unique[!(VA_Clinical_data_unique$BB %in% VA_BR_combo$BB),]

#Horizontally combine VA BR and Clinical share
VA_BR_Clinical_share = join(VA_BR_share,VA_Clinical_share,by = c("BB","SubNum"))

#Harmanize VA BR/Clinical unique to combine with shared parts
share_lookup4BR_unique = VA_BR_Clinical_share[,-c(3:20)]
VA_BR_unique2 = join(VA_BR_unique, share_lookup4BR_unique, by = c("BB","SubNum"))
VA_BR_unique2$Age = VA_BR_unique2$Batch1_Age
VA_BR_unique2$Sex = VA_BR_unique2$Batch1_Sex
VA_BR_unique2$Race = VA_BR_unique2$Batch1_Race
VA_BR_unique2$PMI = VA_BR_unique2$Batch1_PMI
share_lookup4Clinical_unique = VA_BR_Clinical_share[,-c(21:101)]
VA_Clinical_unique2 = join(VA_Clinical_unique, share_lookup4Clinical_unique, by = c("BB","SubNum"))
VA_Clinical_unique2 = VA_Clinical_unique2[,colnames(VA_BR_Clinical_share)]
VA_BR_Clinical_combo = rbind(VA_BR_Clinical_share, VA_BR_unique2, VA_Clinical_unique2)
VA_BR_Clinical_combo = subset(VA_BR_Clinical_combo, select = -c(Batch1_Age,Batch1_Sex,Batch1_Race,Batch1_PMI))

#Compare with CMC
BB2CMC_mapping_data = read.csv(synGet(entity='syn22801015')$path, header = T)
oldBB2CMC_mapping_data = subset(BB2CMC_mapping_data, select = colnames(BB2CMC_mapping_data)[c(1,3)])
colnames(oldBB2CMC_mapping_data) = c("BB","CMC_individual_ID")
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo, oldBB2CMC_mapping_data, by = c("BB"))
CMC_atacseq_meta = read.csv(synGet(entity='syn17094130')$path, header = T)
CMC_chipseq_meta = read.csv(synGet(entity='syn20549418')$path, header = T)
CMC_microarray_meta = read.csv(synGet(entity='syn16816489')$path, header = T)
CMC_proteomics_meta = read.csv(synGet(entity='syn17101049')$path, header = T)
CMC_rnaseq_meta = read.csv(synGet(entity='syn16816488')$path, header = T)
CMC_snp_meta = read.csv(synGet(entity='syn16816490')$path, header = T)
CMC_WGS_meta = read.csv(synGet(entity='syn16816491')$path, header = T)
CMC_atacseq_mapping = as.data.frame(cbind(CMC_atacseq_meta$Individual_ID, CMC_atacseq_meta$Individual_ID))
colnames(CMC_atacseq_mapping) = c("CMC_individual_ID","CMC_ATACseq")
CMC_atacseq_mapping = unique(CMC_atacseq_mapping, by = colnames(CMC_atacseq_mapping))
CMC_chipseq_mapping = as.data.frame(cbind(CMC_chipseq_meta$Individual_ID, CMC_chipseq_meta$Individual_ID))
colnames(CMC_chipseq_mapping) = c("CMC_individual_ID","CMC_ChIPSeq")
CMC_chipseq_mapping = unique(CMC_chipseq_mapping, by = colnames(CMC_chipseq_mapping))
CMC_microarray_mapping = as.data.frame(cbind(CMC_microarray_meta$Individual_ID, CMC_microarray_meta$Individual_ID))
colnames(CMC_microarray_mapping) = c("CMC_individual_ID","CMC_microarray")
CMC_microarray_mapping = unique(CMC_microarray_mapping, by = colnames(CMC_microarray_mapping))
CMC_proteomics_mapping = as.data.frame(cbind(CMC_proteomics_meta$Individual_ID, CMC_proteomics_meta$Individual_ID))
colnames(CMC_proteomics_mapping) = c("CMC_individual_ID","CMC_proteomics")
CMC_proteomics_mapping = unique(CMC_proteomics_mapping, by = colnames(CMC_proteomics_mapping))
CMC_rnaseq_mapping = as.data.frame(cbind(CMC_rnaseq_meta$Individual_ID, CMC_rnaseq_meta$Individual_ID))
colnames(CMC_rnaseq_mapping) = c("CMC_individual_ID","CMC_rnaSeq")
CMC_rnaseq_mapping = unique(CMC_rnaseq_mapping, by = colnames(CMC_rnaseq_mapping))
CMC_snp_mapping = as.data.frame(cbind(CMC_snp_meta$Individual_ID, CMC_snp_meta$Individual_ID))
colnames(CMC_snp_mapping) = c("CMC_individual_ID","CMC_SNP")
CMC_snp_mapping = unique(CMC_snp_mapping, by = colnames(CMC_snp_mapping))
CMC_WGS_mapping = as.data.frame(cbind(CMC_WGS_meta$Individual_ID, CMC_WGS_meta$Individual_ID))
colnames(CMC_WGS_mapping) = c("CMC_individual_ID","CMC_WGS")
CMC_WGS_mapping = unique(CMC_WGS_mapping, by = colnames(CMC_WGS_mapping))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_atacseq_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_chipseq_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_microarray_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_proteomics_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_rnaseq_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_snp_mapping, by = c("CMC_individual_ID"))
VA_BR_Clinical_combo2 = join(VA_BR_Clinical_combo2, CMC_WGS_mapping, by = c("CMC_individual_ID"))

#Compare with AMP-AD
msbb_individual_metadata = read.csv(synGet(entity='syn21996431')$path, header = T)
msbb_individualID_split = as.data.frame(t(as.data.frame(strsplit(msbb_individual_metadata$individualID,"_"))))
colnames(msbb_individualID_split) = c("AMPAD","MSSM","SubNum")
msbb_individualID_split2 = cbind(msbb_individualID_split, msbb_individual_metadata$individualID)
colnames(msbb_individualID_split2)[4] = "AMPAD_msbb_individualID"
msbb_individualID_split2$SubNum = as.integer(msbb_individualID_split2$SubNum)
msbb_lookup4VA = subset(msbb_individualID_split2, select = colnames(msbb_individualID_split2)[c(3:4)])
VA_BR_Clinical_combo_matched_CMC_AMPAD = join(VA_BR_Clinical_combo2, msbb_lookup4VA, by = "SubNum")

#mark received barcodes
#VA_brain_tissue_received_wb = loadWorkbook("Tasker 1390 cataloged.xlsx",create = TRUE)
#VA_brain_tissue_received_data = readWorksheet(VA_brain_tissue_received_wb, sheet = "Sheet1", startRow = 0, endRow = 0, startCol = 0, endCol = 0)
VA_brain_tissue_received_data = read.csv(synGet(entity='syn23003001')$path, header = T)
VA_Barcode_received = data.frame(VA_brain_tissue_received_data$Barcode, VA_brain_tissue_received_data$Barcode)
colnames(VA_Barcode_received) = c("Batch2_Barcode","Barcode_received")
VA_BR_Clinical_combo_matched_CMC_AMPAD_v2 = join(VA_BR_Clinical_combo_matched_CMC_AMPAD, VA_Barcode_received, by = "Batch2_Barcode")
write.csv(VA_BR_Clinical_combo_matched_CMC_AMPAD_v2, file = "VA_BR_Clinical_combo_matched_CMC_AMPAD_10082020.csv")


CMC_atacseq_meta = read.csv(synGet(entity='syn17094130')$path, header = T)
CMC_chipseq_meta = read.csv(synGet(entity='syn20549418')$path, header = T)
CMC_microarray_meta = read.csv(synGet(entity='syn16816489')$path, header = T)
CMC_proteomics_meta = read.csv(synGet(entity='syn17101049')$path, header = T)
CMC_rnaseq_meta = read.csv(synGet(entity='syn16816488')$path, header = T)
CMC_snp_meta = read.csv(synGet(entity='syn16816490')$path, header = T)
CMC_WGS_meta = read.csv(synGet(entity='syn16816491')$path, header = T)
## upload on synapse
file <- synStore(File(path = "VA_BR_Clinical_combo_matched_CMC_AMPAD_10082020.csv", parent = "syn22399913"),
               used = list(list(name = "VA_BR_B1_data", url = "https://www.synapse.org/#!Synapse:syn22801019", wasExecuted = FALSE),
                           list(name = "VA_BR_B2_data", url = "https://www.synapse.org/#!Synapse:syn22801018", wasExecuted = FALSE),
                           list(name = "VA_Clinical_data", url = "https://www.synapse.org/#!Synapse:syn22801020", wasExecuted = FALSE),
						   list(name = "CMC_atacseq_meta", url = "https://www.synapse.org/#!Synapse:syn17094130", wasExecuted = FALSE),
						   list(name = "CMC_chipseq_meta", url = "https://www.synapse.org/#!Synapse:syn20549418", wasExecuted = FALSE),
						   list(name = "CMC_microarray_meta", url = "https://www.synapse.org/#!Synapse:syn16816489", wasExecuted = FALSE),
						   list(name = "CMC_proteomics_meta", url = "https://www.synapse.org/#!Synapse:syn17101049", wasExecuted = FALSE),
						   list(name = "CMC_rnaseq_meta", url = "https://www.synapse.org/#!Synapse:syn16816488", wasExecuted = FALSE),
						   list(name = "CMC_snp_meta", url = "https://www.synapse.org/#!Synapse:syn16816490", wasExecuted = FALSE),
						   list(name = "CMC_WGS_meta", url = "https://www.synapse.org/#!Synapse:syn16816491", wasExecuted = FALSE),
						   list(name = "VA_brain_tissue_received_data", url = "https://www.synapse.org/#!Synapse:syn23003001", wasExecuted = FALSE),
                           list(name = "Merged VA final data", url = "https://github.com/roussosp/psychAD-MSSM/blob/master/Analysis_Randomize_samples.R", wasExecuted = TRUE)),
               activityName = "Merge VA clinical and brain tissue data",
               activityDescription = "Create the final merged VA clinical and brain tissue data")


