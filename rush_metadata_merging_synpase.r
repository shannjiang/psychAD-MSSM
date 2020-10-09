#library(XLConnect)
library(data.table)
library(plyr)
library(synpaser)
synLogin('username','password')

#load clinical data
rush_basic_data = read.csv(synGet(entity='syn23015568')$path, header = T)
rush_corecases_data = read.csv(synGet(entity='syn23015567')$path, header = T)

#combine
rush_corecases4basic = as.data.frame(rush_corecases_data[,!(colnames(rush_corecases_data) %in% colnames(rush_basic_data))])
colnames(rush_corecases4basic) = 'race'
rush_corecases4basic = cbind(rush_corecases_data$projid, rush_corecases4basic)
colnames(rush_corecases4basic)[1] = "projid"
rush_basic4corecases = rush_basic_data[,!(colnames(rush_basic_data) %in% colnames(rush_corecases_data))]
rush_basic4corecases = cbind(rush_basic_data$projid, rush_basic4corecases)
colnames(rush_basic4corecases)[1] = "projid"
rush_corecases_data2 = join(rush_corecases_data, rush_basic4corecases, by = "projid")
rush_basic_data2 = join(rush_basic_data, rush_corecases4basic, by = "projid")
rush_corecases_data2 = rush_corecases_data2[, colnames(rush_basic_data2)]
rush_basic_corecases_combo = rbind(rush_basic_data2, rush_corecases_data2)
#write.csv(rush_basic_corecases_combo, file = "rush_basic_corecases_combo.csv")

#load Rush brain tissue data
rush_brain_set1_data = read.csv(synGet(entity='syn22981924')$path, header = T)
rush_brain_set2_data = read.csv(synGet(entity='syn22981925')$path, header = T)

#partition rush_basic_corecases_combo, rush_brain_set1_data and rush_brain_set2_data into shared and unique
colnames(rush_brain_set1_data) = paste0("set1-",colnames(rush_brain_set1_data),sep = "")
colnames(rush_brain_set2_data) = paste0("set2-",colnames(rush_brain_set2_data),sep = "")
colnames(rush_brain_set1_data)[3] = "projid"
colnames(rush_brain_set2_data)[3] = "projid"
rush_brain_data = join(rush_brain_set1_data, rush_brain_set2_data, by = "projid")
rush_basic_corecases_combo_shared = rush_basic_corecases_combo[rush_basic_corecases_combo$projid %in% rush_brain_data$projid,]
rush_basic_corecases_combo_unique = rush_basic_corecases_combo[!(rush_basic_corecases_combo$projid %in% rush_brain_data$projid),]
rush_brain_data_shared = rush_brain_data[rush_brain_data$projid %in% rush_basic_corecases_combo$projid,]
rush_brain_data_unique = rush_brain_data[!(rush_brain_data$projid %in% rush_basic_corecases_combo$projid),]
rush_basic_corecases_brain_data_shared = join(rush_basic_corecases_combo_shared, rush_brain_data_shared, by = "projid")
rush_basic_corecases_unique2 = join(rush_basic_corecases_combo_unique, rush_brain_data, by = "projid")
rush_brain_data_unique2 = join(rush_brain_data_unique, rush_basic_corecases_combo, by = "projid")
rush_basic_corecases_unique2 = rush_basic_corecases_unique2[,colnames(rush_basic_corecases_brain_data_shared)]
rush_brain_data_unique2 = rush_brain_data_unique2[,colnames(rush_basic_corecases_brain_data_shared)]
rush_basic_corecases_brain_data_combo = rbind(rush_basic_corecases_brain_data_shared, rush_basic_corecases_unique2, rush_brain_data_unique2)
write.csv(rush_basic_corecases_brain_data_combo, file = "rush_basic_corecases_brain_data_combo.csv")

## upload on synapse
file <- synStore(File(path = "rush_basic_corecases_brain_data_combo.csv", parent = "syn22399913"),
               used = list(list(name = "rush_basic_data", url = "https://www.synapse.org/#!Synapse:syn23015568", wasExecuted = FALSE),
                           list(name = "rush_corecases_data", url = "https://www.synapse.org/#!Synapse:syn23015567", wasExecuted = FALSE),
                           list(name = "rush_brain_set1_data", url = "https://www.synapse.org/#!Synapse:syn22981924", wasExecuted = FALSE),
						   list(name = "rush_brain_set2_data", url = "https://www.synapse.org/#!Synapse:syn22981925", wasExecuted = FALSE),
                           list(name = "Merged Rush final data", url = "https://github.com/shannjiang/psychAD-MSSM/blob/master/VA_Clinical_Brain_region_merging_synapse_v2.r", wasExecuted = TRUE)),
               activityName = "Merge Rush basic, corecases and brain tissue data",
               activityDescription = "Create the final merged Rush clinical and brain tissue data")

