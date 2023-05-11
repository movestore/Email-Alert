library('move')
library('geodist')

#todo: what happens with NA entries in variab??

rFunction <- function(data,variab=NULL,rel=NULL,valu=NULL,time=FALSE,emailtext="",attr="",odir)
{
  Sys.setenv(tz="UTC")
  
  # add animal attributes to @data table
  data.df <- as.data.frame(data)
  ixo <- which(names(data.df) %in% names(data))
  data@data <- cbind(data@data,data.df[,-ixo])
  
  ## needs to be adapted for two possible variables! 16 Nov 2021
  
  if (is.null(variab) | is.null(rel) | is.null(valu)) logger.info("A parameter of your relation has not been set. The alert cannot be checked.") else 
    {
      
      if (variab %in% names(data))
      {
      
      logger.info(paste("Your alert condition:",variab," ",rel," [",valu,"] will be checked."))

        if (rel=="%in%") #works for numeric or character values
        {
          valus <- trimws(strsplit(as.character(valu),",")[[1]])
          
          if (any(data@data[,variab] %in% valus,na.rm=TRUE)) #dont consider NA values
          {
            selix <- which(data@data[,variab] %in% valus) #which ignores NA
            #o <- order(data@data[selix,variab],decreasing=TRUE)
            attrc <- trimws(strsplit(as.character(attr),",")[[1]])
            
            if (length(attrc)>5)
              {
              logger.info("You have selected more than 5 attributes. They have been cut to the first 5.")
              attrc <- attrc[1:5]
              attr <- paste0(attrc,collapse=",")
            }
            
            if (length(attrc)>0)
            {
              usel0 <- unique(data.frame(data@data[selix,attrc]))
              usel <- data.frame(usel0[complete.cases(usel0), ])
              names(usel) <- attrc
             
              NU <- dim(usel)[1]
              tx <- character(NU)
              ix <- vector(mode = "list", length = NU)
              
              # order
              if (length(attrc)==1) 
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],decreasing=TRUE) else o <- order(usel[,attrc[1]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
                
              } else if (length(attrc)==2)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else if (length(attrc)==3)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else if (length(attrc)==4)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]] & data@data[,attrc[4]]==usel[i,attrc[4]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else 
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],usel[,attrc[5]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],usel[,attrc[5]], decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]] & data@data[,attrc[4]]==usel[i,attrc[4]] & data@data[,attrc[5]]==usel[i,attrc[5]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              }
              
              first.timestamp <- paste(sapply(ix, function(x) as.character(min(as.POSIXct(timestamps(data[x])),na.rm=TRUE)))) #no idea why this gives an error now
              
              last.timestamp <- paste(sapply(ix, function(x) as.character(max(as.POSIXct(timestamps(data[x])),na.rm=TRUE))))
              
              centrloc <- lapply(ix, function(x) coordinates(data[x])[min(which(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))==min(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))))),])
              centrlocm <- matrix(unlist(centrloc),nc=2,byrow=TRUE)
              
              centr.long <- centrlocm[,1]
              centr.lat <- centrlocm[,2]
              
              attrx <- paste(c(attrc,"first.timestamp", "last.timestamp", "centr.long", "centr.lat"), collapse=", ")
              selixox <- paste(tx,first.timestamp,last.timestamp,centr.long,centr.lat,sep=", ")[o]
              
            } else
            {
              attrx <- "< No attributes specified >"
              selixox <- ""
            }
            
            #selixo <- unique(apply(data.frame(data@data[selix[o],attrc]),1,paste,collapse=", "))
            #selixo10 <- selixo[1:min(10,length(selixo))]
            
            nloc <- length(selix)
            nani <- length(unique(as.data.frame(data)$trackId[selix]))
            
            logger.info(paste("Your required alert property:",variab," ",rel," [",valu,"] is fulfilled by in total",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))

            writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,"[",valu,"] (for", nloc, "locations of", nani, "animals)."),"See all your unique data rows (if attr specified) with first and last timestamps and central location of the groups added:",attrx,as.vector(selixox)), paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"email_alert_text.txt"))
          } else 
          {
            logger.info("None of your data fulfill the required alert property. No alert artefact is written.")
          }  
        } else
        {
          if (time==TRUE) fullrel <- eval(parse(text=paste0("as.POSIXct(data@data$",variab,") ",rel," as.POSIXct('",valu,"')"))) else fullrel <- eval(parse(text=paste0("data@data$",variab," ",rel," ",valu)))

          fullrel[is.na(fullrel)] <- FALSE #for any NA the condition cannot be tested, so set it to FALSE
          
          if (any(fullrel))
          {
            selix <- which(fullrel==TRUE)
            #o <- order(data@data[selix,variab],decreasing=TRUE)
            attrc <- trimws(strsplit(as.character(attr),",")[[1]])
            
            if (length(attrc)>5)
            {
              logger.info("You have selected more than 5 attributes. They have been cut to the first 5.")
              attrc <- attrc[1:5]
              attr <- paste0(attrc,collapse=",")
            }
            
            if (length(attrc)>0)
            {
              usel0 <- unique(data.frame(data@data[selix,attrc]))
              usel <- data.frame(usel0[complete.cases(usel0), ])
              names(usel) <- attrc
              
              NU <- dim(usel)[1]
              tx <- character(NU)
              ix <- vector(mode = "list", length = NU)
              
              # order
              if (length(attrc)==1) 
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],decreasing=TRUE) else o <- order(usel[,attrc[1]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
                
              } else if (length(attrc)==2)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else if (length(attrc)==3)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else if (length(attrc)==4)
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]] & data@data[,attrc[4]]==usel[i,attrc[4]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              } else 
              {
                if (odir=="decr") o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],usel[,attrc[5]],decreasing=TRUE) else o <- order(usel[,attrc[1]],usel[,attrc[2]],usel[,attrc[3]],usel[,attrc[4]],usel[,attrc[5]],decreasing=FALSE)
                for (i in seq(along=usel[,1]))
                {
                  ix[[i]] <- which(data@data[,attrc[1]]==usel[i,attrc[1]] & data@data[,attrc[2]]==usel[i,attrc[2]] & data@data[,attrc[3]]==usel[i,attrc[3]] & data@data[,attrc[4]]==usel[i,attrc[4]] & data@data[,attrc[5]]==usel[i,attrc[5]])
                  tx[i] <- paste0(usel[i,],collapse=", ")
                }
              }
              
              first.timestamp <- paste(sapply(ix, function(x) as.character(min(as.POSIXct(timestamps(data[x])),na.rm=TRUE)))) #no idea why this gives an error now
              
              last.timestamp <- paste(sapply(ix, function(x) as.character(max(as.POSIXct(timestamps(data[x])),na.rm=TRUE))))
              
              centrloc <- lapply(ix, function(x) coordinates(data[x])[min(which(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))==min(rowMeans(geodist_vec(x1=coordinates(data[x])[,1],y1=coordinates(data[x])[,2],measure="vincenty"))))),])
              centrlocm <- matrix(unlist(centrloc),nc=2,byrow=TRUE)
              
              centr.long <- centrlocm[,1]
              centr.lat <- centrlocm[,2]
              
              attrx <- paste(c(attrc,"first.timestamp", "last.timestamp", "centr.long", "centr.lat"), collapse=", ")
              selixox <- paste(tx,first.timestamp,last.timestamp,centr.long,centr.lat,sep=", ")[o]
              
            } else
            {
              attrx <- "< No attributes specified >"
              selixox <- ""
            }

            #selixo <- unique(apply(data.frame(data@data[selix[o],attrc]),1,paste,collapse=", ")) #changed to tx, now relates to ix..
            #selixo10 <- selixo[1:min(10,length(selixo))]

            nloc <- length(selix)
            nani <- length(unique(as.data.frame(data)$trackId[selix]))
            
           logger.info(paste("Your required alert property:",variab,rel,valu,"is fulfilled by",nloc,"locations of",nani,"animals. An Email Alert txt file will be generated. The full data set will be passed on as output."))

            writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"(for", nloc, "locations of", nani, "animals)."),"See all your unique data rows (if attr specified) with first and last timestamps and central location of the groups added:",attrx,as.vector(selixox)), paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"email_alert_text.txt"))
            
          } else  logger.info("None of your data fulfill the required property. No alert artefact is written.")
        }
      } else logger.info("Your selected variable(s) is/are not available in the data set. Please also check your spelling (Cargo Agent of previous App). Go back and reconfigure the App.")
    }

  result <- data
  return(result)
}

  
  
  
  
  
  
  
  
  
  
