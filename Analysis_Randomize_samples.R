library(synapser) 
synLogin() 

# Obtain a pointer and download the data 
mssm = read.csv(synGet(entity='syn22800932')$path)
hbcc = read.csv(synGet(entity='syn22977958')$path)
rush = read.csv(synGet(entity='syn22981925')$path)

hbcc_filtered = hbcc[hbcc$Mass..mg.>=10 & 
                       !is.na(hbcc$Sex) & 
                       !is.na(hbcc$ageOfDeath) & 
                       hbcc$PMI..in.hours.<=48 & 
                       hbcc$rnaSeq_isolation.RIN>=6 & 
                       !is.na(hbcc$CMC_Brain_ID) & 
                       hbcc$Dx!="undetermined",]
hbcc_filtered = hbcc_filtered[order(hbcc_filtered$Brain.ID, -abs(hbcc_filtered$Mass..mg.) ), ] 
hbcc_filtered = hbcc_filtered[ !duplicated(hbcc_filtered$Brain.ID), ]     
hbcc_filtered = data.frame(SubID = hbcc_filtered$Brain.ID, Institution = "HBCC", BrainRegion = hbcc_filtered$Brain.Region,
                           tissueWeight = hbcc_filtered$Mass..mg., BoxLocation = hbcc_filtered$Box.Location,
                           AliquotBox = hbcc_filtered$Aliquot.Box., SampleLocation = hbcc_filtered$Sample.Location)
#dim(hbcc_filtered)
#cor(hbcc_filtered$rnaSeq_isolation.RIN, hbcc_filtered$pH, use="pairwise.complete.obs",m="s")


mssm_filtered = mssm[mssm$PMI<=48*60 &
                       !is.na(mssm$Batch1_Barcode),]
mssm_filtered = mssm_filtered[ !duplicated(mssm_filtered$SubNum), ]     

mssm_filtered = data.frame(SubID = mssm_filtered$SubNum, Institution = "MSSM", BrainRegion = mssm_filtered$Batch1_Brain.Region,
                           tissueWeight = mssm_filtered$Batch1_Size, BoxLocation = "",
                           AliquotBox = "", SampleLocation = "")


rush_filtered = data.frame(SubID = rush$Projid, Institution = "RUSH", BrainRegion = rush$Region,
                           tissueWeight = "", BoxLocation = rush$Freezer,
                           AliquotBox = rush$Shelf, SampleLocation = rush$Intra.box)

merged = rbind(hbcc_filtered, mssm_filtered, rush_filtered)
set.seed(nrow(merged))
merged$randnum = runif(nrow(merged))
merged = merged[order(merged$randnum),]
