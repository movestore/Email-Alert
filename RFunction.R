library('move')

rFunction <- function(data,variab,rel,valu,minlocs=1,minanimals=1,time=FALSE,emailtext="")
{
  Sys.setenv(tz="UTC")
  
  if (is.null(variab) | is.null(rel) | is.null(valu)) logger.info("One of your parameters has not been set. The alert cannot be checked.") else 
    {
      if (variab %in% names(data@data))
      {
        logger.info(paste("Your alert condition",variab," ",rel," [",valu,"] will be checked."))
        
        if (rel=="%in%") #works for numeric or character values
        {
          valus <- strsplit(as.character(valu),",")[[1]]
          
          if (any(data@data[,variab] %in% valus))
          {
            selix <- which(data@data[,variab] %in% valus)
            nloc <- length(selix)
            nani <- length(unique(as.data.frame(data)$trackId[selix]))
            logger.info(paste("Your required alert property:",variab," ",rel," [",valu,"] is fulfilled by",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))
            if (nloc>=minlocs & nani>=minanimals) writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"for", nloc, ">=",minlocs,"locations of", nani, ">=", minanimals,"animals.")), "email_alert_text.txt")
          } else 
          {
            logger.info("None of your data fulfill the required alert property. No alert artefact is written.")
          }  
        } else
        {
          if (time==TRUE) fullrel <- eval(parse(text=paste0("as.POSIXct(data@data$",variab,") ",rel," as.POSIXct('",valu,"')"))) else fullrel <- eval(parse(text=paste0("data@data$",variab," ",rel," ",valu)))
          if (any(fullrel))
          {
            nloc <- length(which(fullrel==TRUE))
            nani <- length(unique(as.data.frame(data)$trackId[which(fullrel==TRUE)]))
            logger.info(paste("Your required alert property:",variab," ",rel," [",valu,"] is fulfilled by",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))
            if (nloc>=minlocs & nani>=minanimals) writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"for", nloc, ">=",minlocs,"locations of", nani, ">=", minanimals,"animals.")), "email_alert_text.txt")
          } else  logger.info("None of your data fulfill the required property. No alert artefact is written.")
        }
      } else logger.info("You selected variable is not available in the data set. Please also check your spelling (Cargo Agent of previous App). Go back and reconfigure the App.")
    }

  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
