library('move2')
library('sf')
library(dplyr)
library(tidyr)
library(bit64)
library(mapview)
library(leafpop) 

rFunction = function(data, variab=NULL,rel=NULL,valu=NULL,time=FALSE,emailtext="", groupbyTrk=TRUE ,attr="",odir,csvout=TRUE,plot=TRUE, ...) {
  
  result <- data
  if (is.null(variab) | is.null(rel) | is.null(valu)) logger.warn("A parameter of your relation has not been set. The alert cannot be checked.") else {
  # add all track attributes to event table
  data <- mt_as_event_attribute(data, everything(), .keep = F)
  
  ## check spelling is correct
  if (variab %in% names(data)){logger.info(paste("Your alert condition:",variab," ",rel," [",valu,"] will be checked."))
    
    ## identify rows of selected attibute that meets condition
    if(rel %in% c("==" , "<" , ">")){
      if(time){
        selvar <- as.POSIXct(data[[variab]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
        valu <- as.POSIXct(valu, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      }else{
        selvar <- as.numeric(data[[variab]])
      }
      rel_fun <- match.fun(rel)                                       
      fullrel <- rel_fun(selvar, valu)
      fullrel[is.na(fullrel)] <- FALSE #for any NA the condition cannot be tested, so set it to FALSE
      selix <- which(fullrel)
      
    }else if(rel == "range"){ 
      valus <- trimws(strsplit(as.character(valu),",")[[1]])
      if(length(valus)>2){logger.warn("You have provided more than 2 values. Only the 2 first values will be used.")}
      if(time){
        selvar <- as.POSIXct(data[[variab]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
        low  <- as.POSIXct(valus[1], format="%Y-%m-%d %H:%M:%S", tz="UTC")
        high <- as.POSIXct(valus[2], format="%Y-%m-%d %H:%M:%S", tz="UTC")
        
      }else{
        selvar <- as.numeric(data[[variab]])
        low  <- as.numeric(valus[1]) #suppressWarnings(as.numeric(valus[1]))
        high <- as.numeric(valus[2]) #suppressWarnings(as.numeric(valus[2]))
      }
      
      fullrel <- selvar > low & selvar < high
      fullrel[is.na(fullrel)] <- FALSE
      selix <- which(fullrel)
      
    }else if(rel == "%in%"){
      valus <- trimws(strsplit(as.character(valu),",")[[1]]) ## TODO: check if valus beeing character works wehn they are numeric
      if(time){
        selvar <- as.POSIXct(data[[variab]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
        valus <- as.POSIXct(valus, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      }else{
        selvar <- data[[variab]]
      }
      selix <- which(selvar %in% valus) # which ignores NA  
    }
    
    ## if data meet condition 
    if (any(selix)) {
      
        attrc <- trimws(strsplit(attr, ",")[[1]])
        if (!all(attrc %in% names(data))){
          logger.warn("At least one of the attributes stated in 'Attributes of input data to be added' is not available in the data set. Please also check your spelling (Cargo Agent of previous App). Fallback: only the track id will be used.")
          attrc <- mt_track_id_column(data)
        }
        if(groupbyTrk){
          grby <- mt_track_id_column(data)
          attrc <- unique(c(mt_track_id_column(data),attrc))
        }else{
          grby <- attrc[1]
          attrc <- unique(c(mt_track_id_column(data),attrc))
          }

      usel <- data |>
        slice(selix) |>
        select(all_of(attrc)) #|>
        # distinct(across(all_of(attrc))) |> ## returns the unique combinations of values across only the columns listed in attrc, dropping all other columns and duplicate rows on those columns
        #drop_na()
    
      usel <- usel %>% relocate(!!sym(grby),timestamp)
      
      ## ToDo?? make 2 csv - all locations (usel) & central locations --add choice to seetings
      
      ### csv should contain: grby, central.lon, central.lat, first.ts, last.ts, attr (median/mode)
      ## calculate weighted mean central location for each unique value of grby
      ## calculate first, last timestamp for each unique group by grby
      ## for other attr calculate median (numeric), mode(factor/char)
      
      get_mode <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0L) return(NA)
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      summary_tbl <- usel |>
        arrange(!!sym(grby), timestamp) |>
        group_by(!!sym(grby)) |>
        summarise(
          # keep all geometries as MULTIPOINT for centroid
          geom_all = st_union(geometry),
          # first / last geometries for coords
          geom_first = first(geometry),
          geom_last  = last(geometry),
          
          # first / last timestamps
          first_timestamp = first(timestamp),
          last_timestamp  = last(timestamp),
          
          # numeric (!integer64): median
          across(
            where(\(.x) is.numeric(.x) && !inherits(.x, "integer64")),
            ~ median(.x, na.rm = TRUE),
            .names = "median_{.col}"
          ),
          
          # integer64, factor, character: mode
          across(
            where(\(.x) inherits(.x, "integer64") | is.factor(.x) | is.character(.x)),
            get_mode,
            .names = "mode_{.col}"
          ),
          .groups = "drop"
        ) |>
        mutate(
          # centroid from all points
          geometry = st_centroid(geom_all),
          lon_centroid = st_coordinates(geometry)[, 1],
          lat_centroid = st_coordinates(geometry)[, 2],
          # first / last lon/lat
          first_lon = st_coordinates(geom_first)[, 1],
          first_lat = st_coordinates(geom_first)[, 2],
          last_lon  = st_coordinates(geom_last)[, 1],
          last_lat  = st_coordinates(geom_last)[, 2]
        )
      
      summary_tbl_csv <- summary_tbl |>
        select(
          -geom_all,
          -geom_first,
          -geom_last,
          -geometry
        ) |>
        st_drop_geometry() |>
        tibble::as_tibble()
      
      summary_tbl_csv_sorted <- summary_tbl_csv %>%
        arrange(desc(pick(everything())))
      ## ToDo give option to use asc or desc??
      
      write.csv(summary_tbl_csv_sorted, row.names = F, appArtifactPath("centroid_points.csv"))
      
      ## email shows first 10 rows of csv
      nloc <- nrow(usel)
      nani <- length(unique(usel[[mt_track_id_column(data)]]))
      tb_mail <- if(nrow(summary_tbl_csv_sorted)>10){summary_tbl_csv_sorted[1:10,]}else{summary_tbl_csv_sorted}
      tb_mail <- as.matrix(tb_mail)  
      
      writeLines(c(emailtext,paste("Your following Alert Condition is fullfilled:",variab,rel,valu,"(for", nloc, "locations of", nani, "tracks)."),"Attached are the table of the centroid locations and a interactive map. First 10 rows of the table are displayed here:", tb_mail),appArtifactPath("email_alert_text.txt"))
      
      ### the plot should represent (maybe work with transparencey aswell)
      ## all locations colored by grby 
      ## central location colored by grby - bigger? border other color
      ## first and last location for each unique group by grby - make identifiable
      crsdata    <- st_crs(data)$epsg
      centroids  <- st_as_sf(summary_tbl_csv_sorted,
                             coords = c("lon_centroid","lat_centroid"),
                             crs = crsdata)
      first_loc  <- st_as_sf(summary_tbl_csv_sorted,
                             coords = c("first_lon","first_lat"),
                             crs = crsdata)
      last_loc   <- st_as_sf(summary_tbl_csv_sorted,
                             coords = c("last_lon","last_lat"),
                             crs = crsdata)
      if(!crsdata==4326){## leaflets expects 4326
        usel      <- st_transform(usel, 4326)
        centroids <- st_transform(centroids, 4326)
        first_loc     <- st_transform(first_loc, 4326)
        last_loc      <- st_transform(last_loc, 4326)
      }
      
      mv_allpoints <- mapview(
        usel,
        zcol        = grby,      
        cex         = 3,          
        alpha       = 0.5,
        color       = "white",  
        legend     = T,                 
        layer.name = grby,
        popup       = popupTable(st_drop_geometry(usel)) 
      )
      
      mv_first <- mapview(
        first_loc,
        zcol        = grby,
        cex         = 4,
        alpha       = 1,
        color       = "green4", 
        layer.name  = "first locations",
        legend      = FALSE,
        popup       = popupTable(first_loc %>% 
                                   select(-lon_centroid, -lat_centroid,-last_lon, -last_lat) %>% 
                                   st_drop_geometry())
      )
      
      mv_last <- mapview(
        last_loc,
        zcol        = grby,
        cex         = 4,
        alpha       = 1,
        color       = "red",
        layer.name  = "last locations",
        legend      = FALSE,
        popup       = popupTable(last_loc %>%
                                   select(-lon_centroid, -lat_centroid,-first_lon, -first_lat) %>% 
                                   st_drop_geometry())
      )
      
      mv_centroids <- mapview(
        centroids,
        zcol        = grby,
        cex         = 10,
        alpha       = 1,
        layer.name  = "centroids",
        legend      = FALSE,
        popup       = popupTable(centroids %>% 
                                   select(-first_lon, -first_lat, -last_lon, -last_lat) %>% 
                                   st_drop_geometry())
      )
      
      mv_tot <- mv_allpoints + mv_first + mv_last + mv_centroids
      
      # mapshot2(mv_tot, url = appArtifactPath("Interactive_plot.html"))
      dir.create(targetDirFiles <- tempdir())
      mapshot2(mv_tot, url=file.path(targetDirFiles, "Interactive_plot.html"))
      # Copy only the HTML file to target path
      file.copy(file.path(targetDirFiles, "Interactive_plot.html"), appArtifactPath("Interactive_plot.html"), overwrite=TRUE)
      
      
      
      logger.info(paste("Your required alert property:",variab,rel,valu,"is fulfilled by",nloc,"locations of",nani,"animals. An Email Alert and artefacts will be generated. The full data set will be passed on as output."))
      
      
    }else{logger.info("None of your data fulfill the required property. No alert artefact is written.")}
    
  }else{logger.warn("Your selected variable(s) is/are not available in the data set. Please also check your spelling (Cargo Agent of previous App). Go back and reconfigure the App.")}
  
}
  return(result) 
}



  
  
  
  
  

