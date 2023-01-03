# 
# Obiba's Opal - Upload Testing Datasets
#

library(DSOpal)
library(opalr)
library(readr)
library(tibble)

upload_testing_dataset_table <- function(opal, project_name, table_name, col_types, local_file_path) {
    if (! opal.project_exists(opal, project_name))
        opal.project_create(opal, project_name, database = "mongodb")
  
    dataset <- readr::read_csv(local_file_path, col_types = col_types)
    data    <- as_tibble(dataset, rownames = '_row_id_')
  
    opal.table_save(opal, data, project_name, table_name, id.name = "_row_id_", force = TRUE)
}

opal <- opal.login('administrator','datashield_test&', url='https://192.168.56.100:8443/', opts = list(ssl_verifyhost=0, ssl_verifypeer=0))

upload_testing_dataset_table(opal, 'MEDIATION', 'UPBdata1', "_nnfnfffii",       'MEDIATION/UPBdata1.csv')
upload_testing_dataset_table(opal, 'MEDIATION', 'UPBdata2', "_nnfnfffii",       'MEDIATION/UPBdata2.csv')
upload_testing_dataset_table(opal, 'MEDIATION', 'UPBdata3', "_nnfnfffii",       'MEDIATION/UPBdata3.csv')
upload_testing_dataset_table(opal, 'MEDIATION', 'framing',  "_ffiffinniinfiii", 'MEDIATION/framing.csv')
upload_testing_dataset_table(opal, 'MEDIATION', 'student',  "_fififfffffffi",   'MEDIATION/student.csv')
upload_testing_dataset_table(opal, 'MEDIATION', 'vv2015',   "_nnnnnnf",         'MEDIATION/vv2015.csv')

opal.logout(opal)
