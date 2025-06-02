###############################################################################
#File      : Ostrich_SWAT_setup.R
#Author    : Karles Pescoran Heredia

#An algorithm to create Ostrich input files from SWAT/SWAT-GL model files.

#Check the "SWAT calibration with Ostrich: User’s Guide" in ()
#More info about OSTRICH in (https://www.civil.uwaterloo.ca/envmodelling/Ostrich.html)

#Version History
#02-06-2025
###############################################################################


###############################################################################
# SET FILE PATHS (USER)
###############################################################################
# Set working directory (it should contain “TxtInOut” dir) ####################
R_exe                 <- "C:/Program Files/R/R-4.4.3/bin/Rscript.exe"
Workdir               <- "C:/OSTRICH_SWAT/executed/EX_2"
SWATParams_path       <- "C:/OSTRICH_SWAT/data/swatParam.txt"
targetParams_path     <- "C:/OSTRICH_SWAT/executed/EX_2/params.xlsx"
SWATexe               <- "C:/OSTRICH_SWAT/exe/glacier_module_git_release.exe"
source_path           <- "C:/OSTRICH_SWAT/executed/EX_2/source"
TxtInOut_path         <- "TxtInOut"


###############################################################################
###############################################################################
# DO NOT CHANGE ANYTHING BELOW THIS
###############################################################################
###############################################################################


###############################################################################
# Cheking files existence
###############################################################################
#knitr::opts_chunk$set(error = FALSE)

# TxtInOut_path         <- paste0(Workdir,"/",basename(TxtInOut_path))
# SWATParams_path       <- paste0(Workdir,"/",SWATParams_path)
# targetParams_path     <- paste0(Workdir,"/",targetParams_path)
#SWATexe               <- paste0(TxtInOut_path,"/",basename(SWATexe))
PerformanceFile       <- paste0(source_path,"/performance.R")
warnings_list <- list()
warnings_list[[1]] <- "Avisos:"

if(dir.exists(Workdir)){
  #knitr::opts_knit$set(root.dir = Workdir)
  setwd(Workdir)
  if(!dir.exists(TxtInOut_path) |
     !file.exists(SWATParams_path) |
     !file.exists(targetParams_path) |
     !file.exists(PerformanceFile)){
    stop("Files or Dirs do not exist")
  }
  if(!file.exists(SWATexe)){
    stop(paste0(basename(SWATexe)," does not exist"))
  }
  if(!file.exists(R_exe)){
    stop(paste0("Wrong path for Rscript.exe"))
  }

  if(nchar(TxtInOut_path) == 0 |
     nchar(SWATParams_path) == 0|
     nchar(targetParams_path) == 0|
     nchar(PerformanceFile) == 0){
    stop("Files or Dirs do not exist")
  }
  if(nchar(SWATexe) == 0){
    stop(paste0(basename(SWATexe)," does not exist"))
  }
  if(nchar(R_exe) == 0){
    stop(paste0("Wrong path for Rscript.exe"))
  }

}else{
  stop("Workdir do not exist")
}

###############################################################################
# LOADING PACKAGES
###############################################################################
requiredPackages <- c('dplyr',
                      'lubridate',
                      'openxlsx')

install.packages(setdiff(requiredPackages, rownames(installed.packages())),
                 dependencies = TRUE)

update.packages(requiredPackages,
                ask = FALSE)

invisible(
  suppressMessages(suppressWarnings(lapply(requiredPackages,
                                           library,
                                           character.only = TRUE)))
)

###############################################################################
# LOADING TARGET PARAMETERS
###############################################################################
# SWAT/SWATGL parameters listed in swatParam.txt ##############################
lines <- readLines(SWATParams_path)
param_lines <- lines[!grepl("!",lines)]

param_data <- do.call(rbind, lapply(param_lines, function(line) {
  parts <- unlist(strsplit(line, " "))
  parts[parts != ""]
}))
param_df <- as.data.frame(param_data, stringsAsFactors = FALSE)
names(param_df) <- c("Parameter", "AtLine"     , "StartPosition", "EndPosition",
                     "RoundUp"  , "AbsoluteMin", "AbsoluteMax")
for(i in 2:ncol(param_df)){param_df[,i] <- as.numeric(param_df[,i])}

# Target Parameters ###########################################################
target_params <- read.xlsx(targetParams_path)
if(ncol(target_params) > 7){stop(paste0("Wrong number of columns in ",targetParams_path))}
names(target_params) <- c("Parameter", "InitialValue", "Min", "Max",
                          "Subbasin" , "Landuse"     , "Soil")

target_params$Parameter <- trimws(target_params$Parameter)

target_params <- target_params %>%
  left_join(param_df, by = "Parameter") %>%
  mutate(Par = paste0("P",sprintf("%02d", 1:n())), .before = 1) %>%
  # mutate(Par_replace = ifelse(nchar(Par) < (EndPosition - StartPosition + 1 - 6),
  #                             paste0(Par, strrep(" ", (EndPosition - StartPosition + 1 - 6) - nchar(Par))),
  #                             Par))
  mutate(Par_replace = Par)

# Checking Parameter existence and format #####################################
if(sum(is.na(target_params$AbsoluteMax)) > 0){
  stop(paste0(target_params$Parameter[is.na(target_params$AbsoluteMax)]," does not exist"))
  }
if(!is.numeric(target_params$Min) | !is.numeric(target_params$Max)){stop(
  "Upper and Lower limit must be numeric")
  }
if(sum(target_params$AbsoluteMax < target_params$Max) > 0){
  stop(paste0("for ", paste(target_params$Parameter[target_params$AbsoluteMax < target_params$Max], collapse = ", "),
              " Upper limit is higher than allowed"))
  }
if(sum(target_params$AbsoluteMin > target_params$Min) > 0){
  stop(paste0("for ", paste(target_params$Parameter[target_params$AbsoluteMin > target_params$Min], collapse = ", "),
              " Lower limit is lower than allowed"))
  }
if(sum(target_params$Max < target_params$Min) > 0){
  stop(paste0("for ", paste(target_params$Parameter[target_params$Max < target_params$Min], collapse = ", "),
              " Upper limit is lower than Lower limit"))
  }


###############################################################################
# FINDING TARGET SWAT/SWAT-GL FILES
###############################################################################
# Listing files in TxtInOut ###################################################
archivos <- list.files(TxtInOut_path, recursive = TRUE, full.names = TRUE)

# Parameter types #############################################################
SWATParamsType <- c(".hru", ".gw", ".mgt", ".chm", ".sdr", ".sep", ".sol")
SWATParamsSUB <- c(".sub", ".rte", ".swq", ".pnd", ".res")
SWATParamsBSN <- c(".bsn",".wwq")
SWATParamsLULC <- c(".plant")
SWATGLParamsType <- c(".gl")

# Filtering target files ######################################################
param_extension <- paste0(".",sub(".*\\.", "", target_params$Parameter))


Param_paths <- list()
for(i in 1:length(target_params$Parameter)){
  withCallingHandlers({
  # Filtering SUB & HRU files #################################################
  if(param_extension[i] %in% c(SWATParamsSUB, SWATParamsType)){

    Param_paths[[i]] <- archivos[endsWith(archivos,param_extension[i])]
    nombre <- tools::file_path_sans_ext(basename(Param_paths[[i]]))
    Param_paths[[i]] <- Param_paths[[i]][grepl("^\\d+$", nombre)]

      # stop if there are no files ############################################
      if(length(Param_paths[[i]]) == 0){
        stop(paste0("Params ",param_extension[i]," does not exist"))
      }

      target_subbasin <- TRUE
      target_lulc <- TRUE
      target_soil <- TRUE

      # Filtering by subbasin #################################################
      if(is.na(target_params$Subbasin[i])){

        stop(paste0("There is no value for Subbasin in ", target_params$Parameter[i]))

      }else if(target_params$Subbasin[i] != "All"){

        target_subbasin <- as.numeric(unlist(strsplit(target_params$Subbasin[i],",")))
        available_subbasin <- as.numeric(substr(basename(Param_paths[[i]]),1,5))

        target_subbasin_warning <- target_subbasin %in% available_subbasin
        if(sum(!target_subbasin_warning) > 0){
          warning(paste0("Subbasin ",
                         paste(target_subbasin[!target_subbasin_warning], collapse = ", "),
                         " does not exist in your model"))
        }

        target_subbasin <- available_subbasin %in% target_subbasin
      }

      # Filtering HRU files ###################################################
      if(param_extension[i] %in% SWATParamsType){

        # Filtering by landuse ################################################
        if(is.na(target_params$Landuse[i])){

          stop(paste0("There is no value for Landuse in ", target_params$Parameter[i]))

        }else if(target_params$Landuse[i] != "All"){

          target_lulc <- gsub(" ","",unlist(strsplit(target_params$Landuse[i],",")))
          available_lulc <- sapply(Param_paths[[i]], function(x){
            tmp <- suppressWarnings(readLines(x)[1])
            gsub(" ","",sub('.*Luse:(.*?)\\s*Soil:.*', '\\1', tmp))
          })

          target_lulc_warning <- target_lulc %in% available_lulc
          if(sum(!target_lulc_warning) > 0){
            warning(paste0("Landuse ",
                           paste(target_lulc[!target_lulc_warning], collapse = ", "),
                           " does not exist in your model"))
          }

          target_lulc <- available_lulc %in% target_lulc
        }

        # Filtering by soil ###################################################
        if(is.na(target_params$Soil[i])){

          stop(paste0("There is no value for Soil in ", target_params$Parameter[i]))

        }else if(target_params$Soil[i] != "All"){

          target_soil <- gsub(" ","",unlist(strsplit(target_params$Soil[i],",")))
          available_soil <- sapply(Param_paths[[i]], function(x){
            tmp <- suppressWarnings(readLines(x)[1])
            gsub(" ","",sub('.*Soil:(.*?)\\s*Slope:.*', '\\1', tmp))
          })

          target_soil_warning <- target_soil %in% available_soil
          if(sum(!target_soil_warning) > 0){
            warning(paste0("Soil ",
                           paste(target_soil[!target_soil_warning], collapse = ", "),
                           " does not exist in your model"))
          }

          target_soil <- available_soil %in% target_soil
        }
      }

    Param_paths[[i]] <- Param_paths[[i]][target_subbasin & target_lulc & target_soil]

    # Stop if there is no files ###############################################
    if(length(Param_paths[[i]]) == 0){
      stop(paste0("Combination of Sub = ",
                  target_params$Subbasin[i],
                  ", Landuse = ",target_params$Landuse[i],
                  ", Soil = ",target_params$Soil[i],
                  " for Parameter ", target_params$Parameter[i]," does not exist"))
    }

  }else if(param_extension[i] %in% SWATParamsBSN){

    if(!file.exists(paste0(TxtInOut_path,"/basins",param_extension[i]))){
      # Stop if there is no files #############################################
      stop(paste0(TxtInOut_path,"/basins",param_extension[i]," does not exist"))
    }else{
      # Filter basins.bsn or basins.wwq files #################################
      Param_paths[[i]] <- paste0(TxtInOut_path,"/basins",param_extension[i])
    }
  }else if(param_extension[i] %in% SWATParamsLULC){

    if(!file.exists(paste0(TxtInOut_path,"/plant.dat"))){
      # Stop if there is no files #############################################
      stop(paste0(TxtInOut_path,"/plant.dat does not exist"))
    }else{
      # Filter plant.dat file #################################################
      Param_paths[[i]] <- paste0(TxtInOut_path,"/plant.dat")
    }
  }else if(param_extension[i] %in% SWATGLParamsType){

    if(!file.exists(paste0(TxtInOut_path,"/gl_hru_par.txt"))){
      # Stop if there is no files #############################################
      stop(paste0(TxtInOut_path,"/gl_hru_par.txt does not exist"))
    }else{
      # Filter gl_hru_par.txt file ############################################
      Param_paths[[i]] <- paste0(TxtInOut_path,"/gl_hru_par.txt")
    }
  }
  }, warning = function(w) {
    warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")  # Evita imprimir inmediatamente el warning
  })
}

###############################################################################
# Checking that the parameters combinations do not overlap
###############################################################################
u_param <- unique(target_params$Parameter)

for(u in u_param){
  index <- which(target_params$Parameter == u)
  if(paste0(".",sub(".*\\.", "", u)) %in% SWATParamsLULC){
    Param_landuse_select <- gsub(" ","",unlist(strsplit(target_params$Landuse[index],",")))
    if("All" %in% Param_landuse_select & length(Param_landuse_select) > 1){
      stop(paste0("the combination of Subbasin, Landuse and Soil of ", u, " in rows ",
                  paste(index, collapse = ","), " overlaps between them. Change values"))
    }else if(length(unique(Param_landuse_select)) != length(Param_landuse_select)){
      stop(paste0("the combination of Subbasin, Landuse and Soil of ", u, " in rows ",
                  paste(index, collapse = ","), " overlaps between them. Change values"))
    }
  }else{
    Param_paths_select <- unlist(lapply(index, function(x){Param_paths[[x]]}))
    if(length(unique(Param_paths_select)) != length(Param_paths_select)){
      stop(paste0("the combination of Subbasin, Landuse and Soil of ", u, " in rows ",
      paste(index, collapse = ","), " overlaps between them. Change values"))
    }
  }
}


###############################################################################
# Replacing parameters for .tpl files of Ostrich
###############################################################################
# plant.dat positions ###############################
PLANT_file <- readLines(paste0(TxtInOut_path,"/plant.dat"))
position <- seq(1,length(PLANT_file),5)
plant_nom <- c()
lista <- lapply(1:length(position), function(x){list()})
i =1
for(i in 1:length(position)){
  lista[[i]]$nom <- PLANT_file[position[i]]
  plant_nom <- c(plant_nom,substr(PLANT_file[position[i]],7,10))

  elementos <- unlist(strsplit(PLANT_file[position[i]+1], "\\s+"))
  elementos <- elementos[elementos != ""]
  lista[[i]]$line1 <- elementos

  elementos <- unlist(strsplit(PLANT_file[position[i]+2], "\\s+"))
  elementos <- elementos[elementos != ""]
  lista[[i]]$line2 <- elementos

  elementos <- unlist(strsplit(PLANT_file[position[i]+3], "\\s+"))
  elementos <- elementos[elementos != ""]
  lista[[i]]$line3 <- elementos

  elementos <- unlist(strsplit(PLANT_file[position[i]+4], "\\s+"))
  elementos <- elementos[elementos != ""]
  lista[[i]]$line4 <- elementos
}
plant_nom <- gsub(" ","",plant_nom)


# Creating TxtInOut_tpl dir (stores .tpl files) ###############################
TxtInOut_tpl_path <- paste0(TxtInOut_path,"_tpl")
replace_files <- unique(unlist(Param_paths))
if(!dir.exists(TxtInOut_tpl_path)){
  dir.create(TxtInOut_tpl_path)
}else{
  file.remove(list.files(TxtInOut_tpl_path, full.names = TRUE))
}

# Coping target SWAT files from TxtInOut to TxtInOut_tpl ######################
file.copy(paste0(TxtInOut_path,"/",basename(replace_files)),
          paste0(TxtInOut_tpl_path,"/",paste0(basename(replace_files),".tpl")),
          overwrite = TRUE)

# Target files in TxtInOut_tpl ################################################
replace_tpl_files <- lapply(Param_paths,function(x){
  paste0(TxtInOut_tpl_path,"/",paste0(basename(x),".tpl"))
})

# Replacing parameters for "param_xxx" (For Ostrich) in target .tpl files #####
i = 22
j = 1
Initial_Values <- list()
for(i in 1:nrow(target_params)){

  # Changing parameter for hru or bsn files ###################################
  if(param_extension[i] %in% c(SWATParamsType,SWATParamsSUB,SWATParamsBSN)){

    for(j in 1:length(replace_tpl_files[[i]])){

      # Reading target tpl file content #######################################
      tmp <- suppressWarnings(readLines(replace_tpl_files[[i]][j], encoding = "latin1"))
      tmp <- iconv(tmp, from = "latin1", to = "UTF-8")
      tmp_line <- tmp[target_params$AtLine[i]]

      # Saving Initial value ##################################################
      Initial_Values[[i]] <- as.numeric(substr(tmp_line, target_params$StartPosition[i], target_params$EndPosition[i]))

      # if target file extension is .sol (different aproach due to layers) ####
      if(param_extension[i] == ".sol"){

        # for parameters that depend on the layer #############################
        if(target_params$AtLine[i] >= 8){

          a <- target_params$StartPosition[i]
          b <- target_params$EndPosition[i]
          nchars <- b - a
          nlayer <- length(strsplit(trimws(substr(tmp[target_params$AtLine[i]],
                                                  a,
                                                  nchar(tmp[target_params$AtLine[i]]))),
                                    split = "\\s+")[[1]])

          # changing parameter by layer #######################################
          for(z in 1:nlayer){

            if(z > 1){
              a <- a + 3 # 3 is the number of chars of the parameter PXX
              b <- b + 3 # 3 is the number of chars of the parameter PXX
            }
            ini  <- substr(tmp_line, 1  , a-1)
            tmp_line <- paste0(ini,target_params$Par_replace[i])
            tmp[target_params$AtLine[i]] <- tmp_line
          }
          # for parameters that not depend on the layer #########################
        }else{
          ini  <- substr(tmp_line, 1                             , target_params$StartPosition[i]-1)
          fini <- substr(tmp_line, target_params$EndPosition[i]+1, nchar(tmp_line))
          tmp_line <- paste0(ini,target_params$Par_replace[i],fini)
          tmp[target_params$AtLine[i]] <- tmp_line
        }
        # if target file extension is not .sol ##################################
      }else{
        ini  <- substr(tmp_line, 1                             , target_params$StartPosition[i]-1)
        fini <- substr(tmp_line, target_params$EndPosition[i]+1, nchar(tmp_line))
        tmp_line <- paste0(ini,target_params$Par_replace[i],fini)
        tmp[target_params$AtLine[i]] <- tmp_line
      }

      # Replacing values in target tpl file ###################################
      con <- file(replace_tpl_files[[i]][j], open = "w", encoding = "latin1")
      writeLines(tmp, con)
      close(con)
    }

    # Changing parameter of landuse in plant.dat ################################
  }else if(param_extension[i] %in% SWATParamsLULC){
    if(target_params$Landuse[i] == "All"){
      for(j in (1:length(plant_nom))){
        Initial_Values[[i]] <- as.numeric(lista[[j]][[target_params$AtLine[i]]][target_params$StartPosition[i]])
        lista[[j]][[target_params$AtLine[i]]][target_params$StartPosition[i]] <- target_params$Par[i]
      }
    }else{
      target_lulc <- gsub(" ","",unlist(strsplit(target_params$Landuse[i],",")))

      target_lulc_warning <- target_lulc %in% plant_nom
      if(sum(!target_lulc_warning) > 0){
        warning(paste0("Soil ",
                       paste(target_lulc[!target_lulc_warning], collapse = ", "),
                       " does not exist in your model"))
      }

      target_lulc <- (1:length(plant_nom))[plant_nom %in% target_lulc]
      for(j in target_lulc){
        Initial_Values[[i]] <- as.numeric(lista[[j]][[target_params$AtLine[i]]][target_params$StartPosition[i]])
        lista[[j]][[target_params$AtLine[i]]][target_params$StartPosition[i]] <- target_params$Par[i]
      }
    }
    # Changing parameter of SWAT-GL tpl files ###################################
  }else if(param_extension[i] %in% SWATGLParamsType){
    tmp <- read.table(replace_tpl_files[[i]], sep = ",", header = TRUE)
    Initial_Values[[i]] <- tmp[,target_params$AtLine[i]][tmp[,1] != "0"][1]
    tmp[,target_params$AtLine[i]][tmp[,1] != "0"] <- target_params$Par_replace[i]
    write.table(tmp,replace_tpl_files[[i]][j],sep = ",",row.names = FALSE,quote = FALSE)
  }
}

if(sum(param_extension %in% SWATParamsLULC) > 0){
  PLANT <- c()
  a <- c()
  for(i in 1:length(position)){
    PLANT <- c(PLANT, lista[[i]]$nom)
    for(j in 2:5){
      plant_par_line <- c()
      for(z in 1:length(lista[[i]][[j]])){
        #a <- c(a, nchar(gsub(".*\\.","",lista[[i]][[j]][z])))
        if(lista[[i]][[j]][z] %in% target_params$Par){
          plant_par <-lista[[i]][[j]][z]
        }else if(j == 5 & z == 2){
          plant_par <- sprintf("%4.0f",as.numeric(lista[[i]][[j]][z]))
        }else{
          plant_par <- sprintf("%9.4f",as.numeric(lista[[i]][[j]][z]))
        }
        plant_par_line <- paste0(plant_par_line, " ", as.character(plant_par))
      }
      PLANT <- c(PLANT, plant_par_line)
    }
  }
  write(PLANT, paste0(TxtInOut_tpl_path,"/plant.dat.tpl"))
}
###############################################################################
# CREATING BeginParams.txt FILE
# User should copy and replace its contents into the ostIn.txt file
###############################################################################
# Creating BeginParams.txt content ############################################
BeginParams <- target_params %>%
  mutate(Initial_Values = unlist(Initial_Values)) %>%
  mutate(init. = ifelse(!is.na(as.numeric(InitialValue)),
                        as.numeric(InitialValue),
                        ifelse(InitialValue == "I",
                               Initial_Values,
                               "random"))
  ) %>%
  mutate(tx_in = "none", tx_ost = "none",  tx_out = "none") %>%
  mutate(fmt = ifelse(param_extension %in% SWATGLParamsType,
                      "free",
                      paste0("F", EndPosition-StartPosition+1, ".", RoundUp))) %>%
  dplyr::select(parameter = Par,
                init.,
                low = Min,
                higher = Max,
                tx_in,
                tx_ost,
                tx_out,
                fmt)

# Checking if parameters are whitin their given limits ########################
N_initialparameters <- BeginParams %>% filter(init. != "random") %>% nrow()
if(N_initialparameters > 0){
  parameters_notin_range <- BeginParams %>%
    filter(init. != "random") %>%
    mutate(outside = as.numeric(init.) > higher | as.numeric(init.) < low) %>%
    pull(outside) %>% sum()
  if(parameters_notin_range > 0){
    warning("InitialValues are outside of your calibration range (Check values)")
    warnings_list[[length(warnings_list) + 1]] <- "InitialValues are outside of your calibration range (Check values)"
  }
}

# Creating BeginParams.txt file ###############################################
# User should copy and replace its contents into the ostIn.txt file ###########
BeginParams_path <- paste0(Workdir,"/BeginParams.txt")
write.table(BeginParams,BeginParams_path, quote = FALSE, sep = "\t", row.names = FALSE)
tmp <- readLines(BeginParams_path)
tmp[1] <- paste0("#",tmp[1])
tmp <- c("BeginParams",tmp,"EndParams")
write(tmp, BeginParams_path)

###############################################################################
# CREATING BeginFilePairs.txt FILE
# User should copy and replace its contents into the ostIn.txt file
###############################################################################
u_replace_tpl_files <- unique(unlist(replace_tpl_files))
u_replace_files <- substr(u_replace_tpl_files,1,nchar(u_replace_tpl_files)-4)

replace_bat_files <- paste0(" ",u_replace_tpl_files,"; ",u_replace_files)
BeginFilePairs <- c("BeginFilePairs",
                    replace_bat_files,
                    "EndFilePairs")

write(gsub("/", "\\\\",BeginFilePairs), paste0(Workdir,"/BeginFilePairs.txt"))

###############################################################################
# CREATING Ost-SWAT.bat FILE
###############################################################################
copy_bat_files <- paste0("copy ",u_replace_files," ",replace_files)
BAT <- c("@echo off",copy_bat_files,"",
         paste0("cd ",TxtInOut_path),"",
         SWATexe,"",
         paste0("cd .."),"",
         paste0("\"",R_exe,"\" ",paste0("\"",PerformanceFile,"\""))
)
write(gsub("/", "\\\\",BAT),paste0(Workdir,"/Ost-SWAT.bat"))

for(i in warnings_list){print(i)}
