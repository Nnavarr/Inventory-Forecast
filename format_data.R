
# Function 1: Format inidial data import ----
# Here, 1st col = Part Number, 2nd col = Date, 3rd col = Unit Count ----

function(x){
  
  require('dplyr')
  require('lazyeval')
  require('purrr')
  
  # Format Columns ----
  colnames(x)[1] <- 'Part_Number'
  colnames(x)[2] <- 'Date'
  
  # Convert "Date" column to Date values ----
  x$Date <- as.character(x$Date)
  x$Date <- as.Date(x$Date, format = '%m/%d/%Y')
  
  # Convert Part_Number column to character (not factor) ----
  x$Part_Number <- as.character(x$Part_Number)
  
  # Order by part number ASC ----
  x <-
    x %>%
    arrange(`Part_Number`)
  
  # Create trend and month columns ----
  x$Trend = ""
  x$Feb = ""
  x$Mar = ""
  x$Apr = ""
  x$May = ""
  x$Jun = ""
  x$Jul = ""
  x$Aug = ""
  x$Sep = ""
  x$Oct = ""
  x$Nov = ""
  x$Dec = ""
  
  # Create Trend column sequence ----
  x <- 
    x %>%
    group_by(Part_Number) %>%
    mutate(Trend = seq_along(Part_Number))
  
  
  for(i in seq_along(x$Date)){
    
    if(months(x$Date)[i] == 'February'){
      
      x$Feb[i] = 1
      
    } else if(months(x$Date)[i] == 'March'){
      
      x$Mar[i] = 1
      
    } else if(months(x$Date)[i] == 'April'){
      
      x$Apr[i] = 1
      
    } else if(months(x$Date)[i] == 'May'){
      
      x$May[i] = 1
      
    } else if(months(x$Date)[i] == 'June'){
      
      x$Jun[i] = 1
      
    } else if(months(x$Date)[i] == 'July'){
      
      x$Jul[i] = 1
      
    } else if(months(x$Date)[i] == 'August'){
      
      x$Aug[i] = 1
      
    } else if(months(x$Date)[i] == 'September'){
      
      x$Sep[i] = 1
      
    } else if(months(x$Date)[i] == 'October'){
      
      x$Oct[i] = 1
      
    } else if(months(x$Date)[i] == 'November'){
      
      x$Nov[i] = 1
      
    } else if(months(x$Date)[i] == 'December'){
      
      x$Dec[i] = 1
      
    } 
  }
  
  # Replace blank value with 0 ----
  x$Feb[x$Feb == ""] <- 0
  x$Mar[x$Mar == ""] <- 0
  x$Apr[x$Apr == ""] <- 0
  x$May[x$May == ""] <- 0
  x$Jun[x$Jun == ""] <- 0
  x$Jul[x$Jul == ""] <- 0
  x$Aug[x$Aug == ""] <- 0
  x$Sep[x$Sep == ""] <- 0
  x$Oct[x$Oct == ""] <- 0
  x$Nov[x$Nov == ""] <- 0
  x$Dec[x$Dec == ""] <- 0
  
  # Convert columns to numeric ----
  x$Feb <- as.numeric(x$Feb)
  x$Mar <- as.numeric(x$Mar)
  x$Apr <- as.numeric(x$Apr)
  x$May <- as.numeric(x$May)
  x$Jun <- as.numeric(x$Jun)
  x$Jul <- as.numeric(x$Jul)
  x$Aug <- as.numeric(x$Aug)
  x$Sep <- as.numeric(x$Sep)
  x$Oct <- as.numeric(x$Oct)
  x$Nov <- as.numeric(x$Nov)
  x$Dec <- as.numeric(x$Dec)
  
  return(x)
  
}


