###############################################################################
#File      : performance.R
#Author    : Karles Pescoran Heredia

#An algorithm to create calculate objective functions from SWAT/SWATGL outputs.

#Version History
#02-06-2025
###############################################################################

start <- Sys.time()

requiredPackages <- c('dplyr',
                      'lubridate',
                      'openxlsx')

invisible(suppressMessages(suppressWarnings(lapply(requiredPackages, library, character.only = TRUE))))

###############################################################################
# SET FILE PATHS (USER)
###############################################################################
# Full paths
#Workdir              <- "C:/PROYECTOS/OSTRICH_cleaning/PRUEBA01"
source_path          <- "C:/OSTRICH_SWAT/executed/EX_1/source"
targetVariables_path <- "C:/OSTRICH_SWAT/executed/EX_1/variables.xlsx"
obsVariables_path    <- "C:/OSTRICH_SWAT/data/obsm"
# Just the name
TxtInOut_name        <- "TxtInOut"


###############################################################################
###############################################################################
# DO NOT CHANGE ANYTHING BELOW THIS
###############################################################################
###############################################################################


###############################################################################
# Checking files existence
###############################################################################
#setwd(Workdir)
User_output_path     <- paste0(source_path,"/User_output_read.R")
User_objective_path  <- paste0(source_path,"/User_objective_function.R")
save_output_name     <- "sim"

if(!file.exists(targetVariables_path) |
   !dir.exists(TxtInOut_name) |
   !dir.exists(obsVariables_path)){
  stop("Files or Dirs do not exist")
}
if(nchar(targetVariables_path) == 0|
   nchar(TxtInOut_name) == 0|
   nchar(obsVariables_path) == 0|
   nchar(save_output_name) == 0){
  stop("Files or Dirs do not exist")
}

source(User_output_path)
source(User_objective_path)

################################################################################
### Importing dates & time_step ################################################
################################################################################
file_cio <- readLines(paste0(TxtInOut_name,"/file.cio"))

skip_years <- as.numeric(substr(file_cio[60],12,17))

start_date <- as.Date(paste0(as.numeric(substr(file_cio[9],12,17)) +
                               skip_years, "0101"), "%Y%m%d")
start_date <- start_date + as.numeric(substr(file_cio[10],12,17)) - 1

end_date <- as.Date(paste0(as.numeric(substr(file_cio[9],12,17)) +
                             as.numeric(substr(file_cio[8],12,17)) - 1, "0101"), "%Y%m%d")
end_date <- end_date + as.numeric(substr(file_cio[11],12,17)) - 1

time_step <- as.numeric(substr(file_cio[59],12,17))

start_month <- as.numeric(format(start_date, "%m"))
end_month <- as.numeric(format(end_date, "%m"))
start_year <- as.numeric(format(start_date, "%Y"))
end_year <- as.numeric(format(end_date, "%Y"))


################################################################################
### Importing N of Subbasin and HRU ############################################
################################################################################
sub_files <- list.files(TxtInOut_name, pattern = "\\.sub", recursive = TRUE, full.names = TRUE)
sub_files <- sub_files[!grepl("output",sub_files)]
N_SUB <- length(sub_files)
N_HRU <- 0
for(i in sub_files){
  N_HRU <- N_HRU + as.numeric(substr(readLines(i)[53],14,17))
}


################################################################################
### Functions ##################################################################
################################################################################

### Read SWAT file ouputs ######################################################
read_output_SWAT <- function(output_type, component = 1, column){
  #component = targetVariables$Component[i]
  #column = targetVariables$Column[i]
  output <- read.table(paste0(TxtInOut_name,"/",output_type$output),
                       header = FALSE, sep = "",
                       skip = output_type$inicio)

  if(output_type$timestep == 0){
    remover <- 13*(1:(end_year - start_year))*output_type$Ncomponent                    #monthly summarys, except for last year
    remover <- unlist(lapply(remover, function(x){(x - output_type$Ncomponent + 1):x})) #for each SUB or HRU
    remover <- remover - (start_month - 1) * output_type$Ncomponent                     #moving to initial month
    output <- output[-remover,] # removing summarys
    output <- output[-c((nrow(output) - 2 * output_type$Ncomponent + 1):nrow(output)),] #removing summary for last year and overall summary

  }else if(output_type$timestep == 2){
    output <- output[-c((nrow(output) - output_type$Ncomponent + 1):nrow(output)),]     #removing overall summary
  }

  return(output[seq(from = component,
                    to = nrow(output),
                    by = output_type$Ncomponent),
                column]
  )
}

### General objective functions ################################################
objective_function <- function(obs,sim){
  missing_values <- is.na(obs)
  if(sum(missing_values) > 0){
    obs <- obs[-missing_values]
    sim <- sim[-missing_values]
  }

  mObs <- mean(obs)
  mSim <- mean(sim)
  obs_mObs <- obs - mObs
  sim_mSim <- sim - mSim
  sim_obs <-  sim - obs
  sumSim <- sum(sim)
  sumObs <- sum(obs)
  pearson <- cor(obs, sim)
  sdObs <- sd(obs)
  sdSim <- sd(sim)

  NSE <- 1 - sum(sim_obs**2)/sum(obs_mObs**2)
  R2 <- pearson ** 2
  PBIAS <- (sumObs - sumSim)/sumObs*100
  KGE <- 1 - sqrt((pearson - 1)**2  + (sdSim/sdObs - 1)**2 + (mSim/mObs - 1)**2)
  RSR <- sqrt(sum( (obs-sim)^2 ))/sqrt(sum( (obs-mObs)^2 ))
  RMSE <- sqrt(mean(sim_obs**2))
  NRMSE <- RMSE/sdObs

  return(data.frame(NSE,R2,PBIAS,KGE,RSR,RMSE,NRMSE))
}


################################################################################
### Reading output files #######################################################
################################################################################
targetVariables <- read.xlsx(targetVariables_path)
names(targetVariables) <- c("File",	"Component", "Column")
targetVariables <- data.frame(N = 1:nrow(targetVariables), targetVariables)

if(sum(targetVariables$File == "User_output_read.R") > 1){
  stop(paste0(targetVariables_path, " can not contain more than 1 User_output_read.R"))
}

obsVariables <- list.files(obsVariables_path, full.names = TRUE)
obsVariables <- grep("UserObs_[0-9]+\\.txt$", obsVariables, value = TRUE)
obsVariables_target <- as.numeric(gsub("UserObs_|.txt","",basename(obsVariables)))
obsVariables <- lapply(obsVariables, function(x){read.table(x, header = TRUE)})

output_list <- data.frame(
  output = c("watout.dat", "output.hru", "output.rch", "output.sub",
             "output.rsv", "output.sed", "output.snw", "output.swr"),
  Ncomponent = c(1, N_HRU, N_SUB, N_SUB,
                 N_SUB, N_HRU, N_HRU, N_HRU),
  timestep = c(1, time_step, time_step, time_step,
               time_step, time_step, 1, 1),
  inicio = c(6,9,9,9,
             9,1,2,3)
)

series <- list()
current_sim = 1
for(i in seq_along(targetVariables$N)){
  if(targetVariables$File[i] %in% output_list$output){
    output_type <- output_list[which(output_list$output == targetVariables$File[i]),]
    if(targetVariables$File[i] == "watout.dat"){
      targetVariables$Component[i] <- 1
    }else if(targetVariables$Component[i] > output_type$Ncomponent){
      stop("HRU or SUB higher than avaliable ones (Check Component)")
    }

    series[[current_sim]] <- read_output_SWAT(output_type, targetVariables$Component[i], targetVariables$Column[i])
    current_sim <- length(series) + 1

  }else if(targetVariables$File[i] == "User_output_read.R"){

    tmp <- User_output_read(TxtInOut_name)

    if(length(tmp) == 0){stop("User_output_read.R did not return any output")}

    for(j in 1:length(tmp)){
      series[[current_sim + j - 1]] <- tmp[[j]]
    }

    current_sim <- length(series) + 1
  }else{
    stop(paste0(targetVariables$File[i], " does not exist"))
  }
}

if(sum(obsVariables_target %in% 1:length(series)) != length(series)){
  stop(paste0("Wrong number of observed variables Obs = ",
              length(obsVariables),
              ", Sim = ", length(series)))
}


################################################################################
### Saving output files ########################################################
################################################################################
if(!file.exists(paste0(save_output_name, "/Nsim.txt"))){
  Nsim <- 1
  dir.create(save_output_name)
}else{
  Nsim <- as.numeric(readLines(paste0(save_output_name, "/Nsim.txt"))) + 1
}
for(i in 1:length(series)){
  tmp <- paste0(save_output_name, "/sim_",i,".txt")
  if(!file.exists(tmp)){
    write(series[[i]], file = tmp, append = TRUE, ncolumns = 1)
    write(paste0("\"", 1, "\""), file = tmp, append = TRUE, ncolumns = 1)
    write(1, file = paste0(save_output_name, "/Nsim.txt"))
  }else{
    if(!file.exists(paste0(save_output_name, "/Nsim.txt"))){
      stop("There is no record for simulation number (Nsim.txt file)")
    }
    write(series[[i]], file = tmp, append = TRUE, ncolumns = 1)
    write(paste0("\"", Nsim, "\""), file = tmp, append = TRUE, ncolumns = 1)
  }
}
write(Nsim, file = paste0(save_output_name, "/Nsim.txt"))


################################################################################
### Calculating & Saving objective function ####################################
################################################################################
i = 4
ObjFunc_values <- data.frame()
for(i in 1:length(series)){
  obs_tmp <- obsVariables[[obsVariables_target[obsVariables_target == i]]]
  if(nrow(obs_tmp) != length(series[[i]])){
    stop(paste0("N of obs =", nrow(obs_tmp)," from Userobs_",i,
                " does not match N of sim = ", length(series[[i]])))
  }else{
    target <- obs_tmp$Type == "C"
    if(sum(target) >0){
      obs_ <- obs_tmp$Series[target]
      sim_ <- series[[i]][target]
      OF <- objective_function(obs_,sim_)
      UOF <- User_objective_function(obs_,sim_)
      if(length(UOF) > 0){
        UOF <- data.frame(t(unlist(UOF)))
        names(UOF) <- paste0("UObjFunc_",1:ncol(UOF))
        tmp <- data.frame(V = paste0("V",i), type = "C", OF, UOF)
      }else{
        tmp <- data.frame(V = paste0("V",i), type = "C", OF)
      }
      ObjFunc_values <- bind_rows(ObjFunc_values,tmp)

    }else{
      stop(paste0("No calibration values for series ",i))
    }

    target <- obs_tmp$Type == "V"
    if(sum(target) >0){
      obs_ <- obs_tmp$Series[target]
      sim_ <- series[[i]][target]
      OF <- objective_function(obs_,sim_)
      UOF <- User_objective_function(obs_,sim_)
      if(length(UOF) > 0){
        UOF <- data.frame(t(unlist(UOF)))
        names(UOF) <- paste0("UObjFunc_",1:ncol(UOF))
        tmp <- data.frame(V = paste0("V",i), type = "V", OF, UOF)
      }else{
        tmp <- data.frame(V = paste0("V",i), type = "V", OF)
      }
      ObjFunc_values <- bind_rows(ObjFunc_values,tmp)

    }else{
      ObjFunc_values[nrow(ObjFunc_values) + 1, ] <- NA
    }

    target <- obs_tmp$Type == "C" | obs_tmp$Type == "V"
    if(sum(target) >0){
      obs_ <- obs_tmp$Series[target]
      sim_ <- series[[i]][target]
      OF <- objective_function(obs_,sim_)
      UOF <- User_objective_function(obs_,sim_)
      if(length(UOF) > 0){
        UOF <- data.frame(t(unlist(UOF)))
        names(UOF) <- paste0("UObjFunc_",1:ncol(UOF))
        tmp <- data.frame(V = paste0("V",i), type = "ALL", OF, UOF)
      }else{
        tmp <- data.frame(V = paste0("V",i), type = "ALL", OF)
      }
      ObjFunc_values <- bind_rows(ObjFunc_values,tmp)

    }else{
      ObjFunc_values[nrow(ObjFunc_values) + 1, ] <- NA
    }
  }
}
ObjFunc_values <- ObjFunc_values %>% mutate(Nsim, .before = 1)

write.table(ObjFunc_values,paste0(TxtInOut_name,"/ObjFunc.txt"),
            row.names = FALSE,
            quote = FALSE,
            sep = ",")

ObjFunc_values_prev_path <- paste0(save_output_name,"/ObjFunc.txt")
if(file.exists(ObjFunc_values_prev_path)){
  ObjFunc_values_prev <- read.table(ObjFunc_values_prev_path, sep = ",", header = TRUE)
  ObjFunc_values_prev <- bind_rows(ObjFunc_values_prev, ObjFunc_values)
}else{
  ObjFunc_values_prev <- ObjFunc_values
}

write.table(ObjFunc_values_prev,paste0(save_output_name,"/ObjFunc.txt"),
            row.names = FALSE,
            quote = FALSE,
            sep = ",")

print(Sys.time() - start)
