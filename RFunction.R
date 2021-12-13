library('move')
library('geodist')

rFunction <- function(data,variab=NULL,rel=NULL,valu=NULL,time=FALSE,emailtext="",attr="")
{
  Sys.setenv(tz="UTC")
  
  ## needs to be adapted for two possible variables! 16 Nov 2021
  
  if (is.null(variab) | is.null(rel) | is.null(valu)) logger.info("A parameter of your relation has not been set. The alert cannot be checked.") else 
    {
      
      if (variab %in% names(data))
      {
      
      logger.info(paste("Your alert condition:",variab," ",rel," [",valu,"] will be checked."))

        if (rel=="%in%") #works for numeric or character values
        {
          valus <- trimws(strsplit(as.character(valu),",")[[1]])
          
          if (any(data@data[,variab] %in% valus))
          {
            selix <- which(data@data[,variab] %in% valus)
            o <- order(data@data[selix,variab],decreasing=TRUE)
            attrc <- trimws(strsplit(as.character(attr),",")[[1]])
            selixo <- unique(apply(data.frame(data@data[selix[o],attrc]),1,paste,collapse=", "))
            #selixo10 <- selixo[1:min(10,length(selixo))]
            
            if (length(selixo)>0 & selixo[1]!="")
            {
              first.timestamp <- paste(apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("as.character(min(as.POSIXct(timestamps(data[which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")])),na.rm=TRUE))")))), "UTC")
              
              last.timestamp <- paste(apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("as.character(max(as.POSIXct(timestamps(data[which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")])),na.rm=TRUE))")))), "UTC")
              
              ix <- apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")"))))
              
              centrloc <- lapply(ix, function(x) coordinates(data[x])[min(which(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))==min(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))))),])
              centrlocm <- matrix(unlist(centrloc),nc=2,byrow=TRUE)
              
              centr.long <- centrlocm[,1]
              centr.lat <- centrlocm[,2]
              
              attrx <- paste(c(attrc,"first.timestamp", "last.timestamp", "centr.long", "centr.lat"), collapse=", ")
              selixox <- paste(selixo,first.timestamp,last.timestamp,centr.long,centr.lat,sep=", ")
            } else
            {
              attrx <- "< No attributes specified >"
              selixox <- selixo
            }
            
            nloc <- length(selix)
            nani <- length(unique(as.data.frame(data)$trackId[selix]))
            
            logger.info(paste("Your required alert property:",variab," ",rel," [",valu,"] is fulfilled by in total",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))

            writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"(for", nloc, "locations of", nani, "animals)."),"See all your unique data rows (if attr specified) with first and last timestamps and central location of the groups added:",attrx,as.vector(selixox)), paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"email_alert_text.txt"))
          } else 
          {
            logger.info("None of your data fulfill the required alert property. No alert artefact is written.")
          }  
        } else
        {
          if (time==TRUE) fullrel <- eval(parse(text=paste0("as.POSIXct(data@data$",variab,") ",rel," as.POSIXct('",valu,"')"))) else fullrel <- eval(parse(text=paste0("data@data$",variab," ",rel," ",valu)))
          if (any(fullrel))
          {
            selix <- which(fullrel==TRUE)
            o <- order(data@data[selix,variab],decreasing=TRUE)
            attrc <- trimws(strsplit(as.character(attr),",")[[1]])
            selixo <- unique(apply(data.frame(data@data[selix[o],attrc]),1,paste,collapse=", "))
            #selixo10 <- selixo[1:min(10,length(selixo))]
            
            if (length(selixo)>0 & selixo[1]!="")
            {
              first.timestamp <- paste(apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("as.character(min(as.POSIXct(timestamps(data[which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")])),na.rm=TRUE))")))), "UTC")
              
              last.timestamp <- paste(apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("as.character(max(as.POSIXct(timestamps(data[which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")])),na.rm=TRUE))")))), "UTC")
              
              ix <- apply(matrix(selixo), 1, function(x) eval(parse(text=paste0("which(",paste0("data@data[,'", attrc ,"'] == '",strsplit(x,", ")[[1]],"'",collapse=" & "),")"))))
              
              centrloc <- lapply(ix, function(x) coordinates(data[x])[min(which(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))==min(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))))),])
              centrlocm <- matrix(unlist(centrloc),nc=2,byrow=TRUE)
              
              centr.long <- centrlocm[,1]
              centr.lat <- centrlocm[,2]
              
              attrx <- paste(c(attrc,"first.timestamp", "last.timestamp", "centr.long", "centr.lat"), collapse=", ")
              selixox <- paste(selixo,first.timestamp,last.timestamp,centr.long,centr.lat,sep=", ")
            } else
            {
              attrx <- "< No attributes specified >"
              selixox <- selixo
            }

            nloc <- length(selix)
            nani <- length(unique(as.data.frame(data)$trackId[selix]))
            
            logger.info(paste("Your required alert property:",variab,rel,valu,"is fulfilled by",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))

            writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"(for", nloc, "locations of", nani, "animals)."),"See all your unique data rows (if attr specified) with first and last timestamps and central location of the groups added:",attrx,as.vector(selixox)), paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"email_alert_text.txt"))
            
          } else  logger.info("None of your data fulfill the required property. No alert artefact is written.")
        }
      } else logger.info("You selected variable(s) is/are not available in the data set. Please also check your spelling (Cargo Agent of previous App). Go back and reconfigure the App.")
    }

  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
