
###################################
#### free geocode - not google #### 
###################################

#####--------------------Packages --------------------------######
library(leaflet)

#####--------------------Geocode Function --------------------------######



geo.dsk <- function(addr){
  require(httr)
  require(rjson)
  
  out <- tryCatch({
    url <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
    response <- GET(url,query=list(sensor="FALSE",address=addr))
    json <- fromJSON(content(response,type="text"))
    loc <- json['results'][[1]][[1]]$geometry$location
    return(c(address=addr,long=loc$lng, lat= loc$lat))
  },
  
  error = function(cond) {
    message(paste("Address not geocoded:", addr))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  },
  
  warning = function(cond) {
    message(paste("Address caused a warning:", addr))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
  },
  
  finally = {
    message(paste("Processed Address:", addr))
    message("One down...")
  }
  
  )
  return(out)
}



#######----------------- Print or Save Results in function -----------------#######


geo.result <- function(df, dataframe.dollar.fulladdress){
              cbind(df, #dataframe with addresses where latlon should go
                    as.data.frame(
                      do.call(rbind,
                              lapply(as.character(dataframe.dollar.fulladdress), geo.dsk))))  #geocode
        }

#######-----------------  Single Example  -----------------#######
#fake <- data.frame(c("300 North College St Northfield MN 55057"), c(1), c(22))

#geo.result(df = fake, 
#           dataframe.dollar.fulladdress = "300 North College St Northfield MN 55057")

#######-----------------  Dataframe Example  -----------------#######

#name   <- c('Carleton College', 'Pomona College', 'Reed College')
#street <- c('300 North College St', '333 N College Way', '3203 SE Woodstock Blvd') 
#city   <- c("Northfield", "Claremont", "Portland") 
#state  <- c('MN', 'CA', 'OR') 
#zip    <- c('55057', '91711', '97202')
#data   <- data.frame(name, street, city, state, zip)      


# create location variable 

#data$location <- paste(str_trim(as.character(data$street)),
#                       str_trim(as.character(data$city)),
#                       str_trim(as.character(data$state)),
#                       str_trim(as.character(data$zip)), sep=' ')


#geo.result(df = data, 
#          dataframe.dollar.fulladdress = data$location)





