# Raghu Sidharthan 6/1/2016

#TODO
#1. Write a trnsit line write function which parses after a certain number of characters in each row. end with a comma



# Function to concat a set of lines
# PArameter allLines is a set of lines
# sttNum is the start index number and stpNum is the stop index number
concatLines <- function(allLines,sttNum,stpNum){
  return(paste0(unlist(str_trim(allLines[sttNum:stpNum])),collapse = ''))
}

replaceLineNodesUntil <- function(lineNodesUntil,keywordStr,valStr){
  indexHW <- gregexpr(keywordStr,lineNodesUntil)[[1]][1]
  if(indexHW!=-1){
    strInitial <- substr(lineNodesUntil,1,indexHW-1)
    strRest    <- substr(lineNodesUntil,indexHW+9,nchar(lineNodesUntil))
    indexRest   <- gregexpr(',',strRest)[[1]][1]
    strFinal <- substr(strRest,indexRest+1,nchar(strRest))
    if(valStr!=-1){
    paste0(strInitial,gsub("\\", "", keywordStr, fixed=TRUE),'=',valStr,',',strFinal)
    }
    else
      paste0(strInitial,strFinal)
    }
  else {
    indexHW <- gregexpr('HEADWAY',lineNodesUntil)[[1]][1]
    strInitial <- substr(lineNodesUntil,1,indexHW-1)
    strRest    <- substr(lineNodesUntil,indexHW,nchar(lineNodesUntil))
    if(valStr!=-1){
      paste0(strInitial,gsub("\\", "", keywordStr, fixed=TRUE),'=',valStr,',',strRest)
    }
    else{
      lineNodesUntil
    }
  }
}
#check <- allLines_Y15[1][[1]]$lineNodesUntil
#check
#replaceLineNodesUntil(check,'HEADWAY\\[1\\]',99)
#replaceLineNodesUntil(check,'HEADWAY\\[2\\]',99)
#replaceLineNodesUntil(check,'HEADWAY\\[3\\]',99)
#replaceLineNodesUntil(check,'HEADWAY\\[1\\]',-1)
#replaceLineNodesUntil(check,'HEADWAY\\[3\\]',-1)

extractKeyword <- function(lineNodesUntilSample,keywordStr){
  retval <- "-99"
  if(gregexpr(keywordStr,lineNodesUntilSample)[[1]][1]!=-1){
    retval <- gsub(',',' ',substr(lineNodesUntilSample,gregexpr(keywordStr,lineNodesUntilSample)[[1]][1]+nchar(keywordStr)+1,nchar(lineNodesUntilSample)))
    retval <- (substr(retval,1,gregexpr(' ',retval)[[1]][1]-1))
  }
  retval
}

extractHeadWays <- function(lineNodesUntilSample){
  # Getting the headways
  h_1 <- -1 
  h_2 <- -1
  h_3 <- -1
  if(gregexpr('HEADWAY\\[1\\]',lineNodesUntilSample)[[1]][1]!=-1){
    h_1 <- gsub(',',' ',substr(lineNodesUntilSample,gregexpr('HEADWAY\\[1\\]',lineNodesUntilSample)[[1]][1]+11,nchar(lineNodesUntilSample)))
    h_1 <- as.integer(substr(h_1,1,gregexpr(' ',h_1)[[1]][1]-1))
  }
  if(gregexpr('HEADWAY=',lineNodesUntilSample)[[1]][1]!=-1){
    h_1 <- gsub(',',' ',substr(lineNodesUntilSample,gregexpr('HEADWAY=',lineNodesUntilSample)[[1]][1]+8,nchar(lineNodesUntilSample)))
    h_1 <- as.integer(substr(h_1,1,gregexpr(' ',h_1)[[1]][1]-1))
  }  
  if(gregexpr('HEADWAY\\[2\\]',lineNodesUntilSample)[[1]][1]!=-1){
    h_2 <- gsub(',',' ',substr(lineNodesUntilSample,gregexpr('HEADWAY\\[2\\]',lineNodesUntilSample)[[1]][1]+11,nchar(lineNodesUntilSample)))
    h_2 <- as.integer(substr(h_2,1,gregexpr(' ',h_2)[[1]][1]-1))
  }
  if(gregexpr('HEADWAY\\[3\\]',lineNodesUntilSample)[[1]][1]!=-1){
    h_3 <- gsub(',',' ',substr(lineNodesUntilSample,gregexpr('HEADWAY\\[3\\]',lineNodesUntilSample)[[1]][1]+11,nchar(lineNodesUntilSample)))
    h_3 <- as.integer(substr(h_3,1,gregexpr(' ',h_3)[[1]][1]-1))
  }
  
  return(c(h_1,h_2,h_3))
  
}

extractNodesVector <- function(lineNodesSample){
  # Return the nodes in a vector format 
  temp1 <- str_split(gsub(' ',',',gsub('  ',' ',gsub('N =',' ',gsub('N=',' ',gsub(',',' ',lineNodesSample))))),',')
  temp2 <- lapply(temp1,function(x) grep("^[-0123456789]", x))
  nodeVector <- lapply(unlist(temp1)[unlist(temp2)],as.integer)
  return(nodeVector)
}

extractEndOfString <- function(strArg){
  strStartType <- substr(strArg,1,1)
  strArgRest <- substr(strArg,2,nchar(strArg))
  if(strStartType=='"'){
    retval <- substr(strArg,2, regexpr('["]',strArgRest)[1])
  }else if(strStartType=="'"){
    retval <- substr(strArg,2, regexpr("[']",strArgRest)[1])
  }else{
    retval <- substr(strArg,1, regexpr('[ ]',strArgRest)[1])
  }
  return(retval)
}

readTLFile <- function(TL_fname){
  inpLines <- readLines(TL_fname)  
  
  #Remove Comments
  linesThatAreComments <- unlist(lapply(inpLines,function(x) substr(x,1,1)==';'))
  inpLines <- inpLines[!linesThatAreComments]
  
  lineIdent <- unlist(lapply(inpLines,function(x) (substr(x,1,4)=='LINE')|(substr(x,2,5)=='LINE') ))
  table(lineIdent)
  sttPositions <- (1:length(lineIdent))[lineIdent]
  stpPositions <- sttPositions[2:length(sttPositions)]-1
  stpPositions[length(sttPositions)] <- length(lineIdent)
  
  linesComplete <- lapply(1:length(sttPositions),function(x) concatLines(inpLines,sttPositions[x],stpPositions[x]) )
  
  temp1 <- lapply(linesComplete,function(x) substr(x,regexpr('NAME=',x)[1]+5,nchar(x)) )
  #temp2 <- lapply(temp1,function(x) substr(x,1,regexpr('[ ,]',x)[1]) )
  #lineNames <- lapply(temp2,function(x) gsub('[ \"\',]','',x))
  lineNames <- unlist(lapply(temp1,function(x) extractEndOfString(x) ))
  
  temp1 <- lapply(linesComplete,function(x) substr(x,regexpr('LONGNAME=',x)[1]+9,nchar(x)) )
  lineLongNames <- unlist(lapply(temp1,function(x) extractEndOfString(x) ))

  lineNodesUntil <- lapply(linesComplete,function(x) substr(x,1,gregexpr('N=',x)[[1]][1]-1) )
  lineNodes      <- lapply(linesComplete,function(x) substr(x,gregexpr('N=',x)[[1]][1],nchar(x)) )
  
  nodeVector <- lapply(lineNodes,extractNodesVector)
  lineHeadway <- lapply(lineNodesUntil,extractHeadWays)
  
  
  
  lineMode <- lapply(lineNodesUntil,extractKeyword,'MODE')
  

  retObject <- lapply(1:length(linesComplete),
                      function(x) 
                        list(lineNames=unlist(lineNames[x]),
                             lineLongNames=unlist(lineLongNames[x]),
                             lineNodesUntil = unlist(lineNodesUntil[x]),
                             lineNodes = unlist(lineNodes[x]),
                             nodeVector=unlist(nodeVector[x]),
                             linesComplete=unlist(linesComplete[x]),
                             lineHeadway= unlist(lineHeadway[x]),
                             lineMode=unlist(lineMode[x])
                             ))
  names(retObject) <- lineNames
  
  return(retObject)
}

fillna = function(DT,val) {
  DT_ret <- copy(DT)
  for (j in seq_len(ncol(DT_ret)))
    set(DT_ret,which(is.na(DT_ret[[j]])),j,val)
  return(DT_ret)
}

