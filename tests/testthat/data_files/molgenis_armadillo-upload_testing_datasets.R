# 
# Molgenis' Armadillo - Upload Testing Datasets
#

library(MolgenisArmadillo)
library(readr)
library(tibble)

upload_testing_dataset_table <- function(project_name, folder_name, table_name, col_types = col_types, local_file_path) {
    dataset        <- readr::read_csv(local_file_path, col_types = col_types)
    data           <- as_tibble(dataset, rownames = '_row_id_')
    names(data)[1] <- 'id';

    MolgenisArmadillo::armadillo.upload_table(project_name, folder_name, dataset, table_name)
}

MolgenisArmadillo::armadillo.set_credentials(server = 'http://127.0.0.1:9000', access_key = "molgenis", secret_key = "molgenis")

if (! 'datashield' %in% MolgenisArmadillo::armadillo.list_projects())
    MolgenisArmadillo::armadillo.create_project('datashield')

upload_testing_dataset_table('datashield', 'MEDIATION', 'UPBdata1', "_nnfnfffii",       'MEDIATION/UPBdata1.csv')
upload_testing_dataset_table('datashield', 'MEDIATION', 'UPBdata2', "_nnfnfffii",       'MEDIATION/UPBdata2.csv')
upload_testing_dataset_table('datashield', 'MEDIATION', 'UPBdata3', "_nnfnfffii",       'MEDIATION/UPBdata3.csv')
upload_testing_dataset_table('datashield', 'MEDIATION', 'framing',  "_ffiffinniinfiii", 'MEDIATION/framing.csv')
upload_testing_dataset_table('datashield', 'MEDIATION', 'student',  "_fififfffffffi",   'MEDIATION/student.csv')
upload_testing_dataset_table('datashield', 'MEDIATION', 'vv2015',   "_nnnnnnf",         'MEDIATION/vv2015.csv')

print(MolgenisArmadillo::armadillo.list_tables('datashield'))
