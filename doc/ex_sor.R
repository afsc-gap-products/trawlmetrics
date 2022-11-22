library(trawlmetrics)

cruise = 202202 # cruise number
cruise_idnum = 758 # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
vessel = 162
region = "EBS"
survey = "NBS_2022"
width_range = c(10, 22)

sor_setup_directory(cruise = cruise, # cruise number
                    cruise_idnum = cruise_idnum, # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
                    vessel = vessel,
                    region = "EBS",
                    survey = "NBS_2022",
                    width_range = c(10, 22),
                    convert_marport_to_netmind = FALSE)

sor_run(cruise = cruise, # cruise number
        vessel = vessel,
        region = "EBS",
        survey = "NBS_2022")


sor_plot_results(cruise = cruise, # cruise number
                 vessel = vessel,
                 region = "EBS",
                 survey = "NBS_2022")

sor_fill_missing(height_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/HEIGHT_EBS_202202_162.rds",
                 spread_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/SPREAD_AFTER_SOR_EBS_202202_162.rds",
                 rds_dir = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022")

ex_files <- list.files("C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022", pattern = "_final.rds", full.names = TRUE)

for(ii in 1:length(ex_files)) {
  test <- readRDS(ex_files[ii])
  print(test)
  print(ex_files[ii])
  readline(prompt = "hi")
}


readRDS(file = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/SPREAD_AFTER_SOR_EBS_202202_162.rds")

test <- readRDS(file = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022/202202_162_1_sor.rds")
is.null(test[["sor_results"]])


readRDS(file = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/HEIGHT_EBS_202202_162.rds")
