User_output_read <- function(TxtInOut_path){

  tmp <- list()

  #############################################################################
  #############################################################################
  # DO NOT CHANGE ANYTHING ABOVE THIS
  #############################################################################
  #############################################################################

  # If user wants to read an oouput file, should added in order to tmp
  # E.g. user is reading ouput_1 and ouput_2, should added to tmp[[1]] & tmp[[2]]

  #############################################################################
  # This is an example of how to extract total glacier area from gl_mb_aa.txt file
  # for SWAT-GL

  require(dplyr)

  output <- read.table(paste0(TxtInOut_path, "/gl_mb_aa.txt"), header = TRUE)
  output <- output %>%
    group_by(yr) %>%
    summarise(Agl_km2 = sum(Agl_km2)) %>%
    pull(Agl_km2)

  tmp[[1]] <- output


  return(tmp)
}
