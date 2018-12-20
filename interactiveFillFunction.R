interfill <-
  function(dataframe, naThreshold = .75, uniqueThreshold = 20 ) {
    # Begin Function
    
    fillList <- list()
    
    typeList <- map(dataframe, typeof)
    
    naList <- map(dataframe, function (x) {
      sum(is.na(x)) / nrow(dataframe)
    })
    
    uniqueList <- map(dataframe, unique)
    
    assign("typeList", typeList, envir = .GlobalEnv)
    assign("naList", naList, envir = .GlobalEnv)
    assign("uniqueList", uniqueList, envir = .GlobalEnv)
    
    iterateHere <- map_lgl(names(naList), function (x){
      (naList[[x]] > naThreshold) & (length(uniqueList[[x]]) < uniqueThreshold)
    }) %>% compact()
    
    
    map(names(typeList)[iterateHere], function (x) {
      
      if (typeList[x] == "character") {
        
        if (length(uniqueList[x]) < 30) {
          
          message <-
            paste(
              "The column",
              x,
              "is",
              round(naList[[x]], 2) * 100,
              "% missing.",
              "It has the following unique values:",
              "\n",
              uniqueList[x],
              ".",
              "Should we fill?"
            )
          
          shouldWeFill <- askYesNo(msg = message)
          
          if (shouldWeFill == TRUE) {
            message <- paste("What should we fill with?")
            charInput <- readline(prompt = message)
            fillList[[x]] <- str_trim(charInput)
          }
          
        } 
        
        else {
          print(paste(x, "has more than 30 NAs. It will be skipped."))
        }
      }
      
      if (typeList[[x]] == "double" | typeList[[x]] == "integer") {
        message <-
          paste(x,
                "is a double or integer.",
                "It is currently",
                round(naList[[x]],2) * 100,
                "% empty.",
                "Fill with 0?")
        
        yesNoResponse <- askYesNo(msg = message)
        
        if (yesNoResponse == TRUE) {
          fillList[[x]] <- 0
        }
    
      }
  
    })                    
    
    return(fillList)
    
  }                                             


# Execute

interfill(fillCopy)
