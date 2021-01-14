# filename: survey_analysis.R
# Kenya DHS survey analysis for vaccine coverage and equity impact

# run program from source file location/folder
#-------------------------------------------------------------------------------
# load libraries
library (rdhs)
library (data.table)
library (ggplot2)
library (survey)
library (haven)
library (foreign)

# clear workspace
rm (list = ls ()) 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# download DHS data set
# get_dhs_data <- function () 
# {
#   # (one-time) set up your credentials (run it first time)
#   set_rdhs_config (email = "kaja.abbas@lshtm.ac.uk",
#                    project = "Health impact of public health interventions",
#                    data_frame = "data.table::as.data.table")
#   
#   # 2014 Kenya (Standard DHS)
# }
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# process DHS data set

# read stata data set
mydata <- read.dta ("KEKR72FL.dta")
dt <- setDT (mydata)

mydata_eth <- read.dta ("ETKR71FL.dta")
dt_eth <- setDT (mydata_eth)



#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# main program - start
#-------------------------------------------------------------------------------
# start time
print (Sys.time ())  

# end time
print (Sys.time ())  
#-------------------------------------------------------------------------------
# main program - end
#-------------------------------------------------------------------------------