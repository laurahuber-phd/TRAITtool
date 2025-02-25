library(shiny)
library(vroom)
library(bslib)

uploadFunction <- function(file,id,sep=',',header=TRUE,type) {
  
  tmp <- vroom::vroom(file$datapath,
                      delim=sep, 
                      col_select = c(-1),
                      #skip=1,col_names=FALSE, #skip the first column, then say there are no column names
                      n_max = 10, #only read the first 10 lines
                      show_col_types = FALSE
                      )
  if(type=="meta") {
    confirm <- any(lapply(tmp, class) == "character" | lapply(tmp, class) == "factor")
    textwarn <- "Values are not characters"
  } else if (type=="tax") {
    confirm <- any(lapply(tmp, class) == "character" | lapply(tmp, class) == "factor") & any(colnames(tmp)=="Kingdom" | colnames(tmp)=="Phylum" | colnames(tmp)=="Species")
    #confirm <- any(lapply(tmp, class) == "character" | lapply(tmp, class) == "factor")
    textwarn <- "Values do not have traditional taxonomic levels"
  } else if (type=="otu") {
    confirm <- all(lapply(tmp, class) == "double" | lapply(tmp, class) == "integer"| lapply(tmp, class) == "numeric")
    textwarn <- "Values are not numeric"
  } else {
    confirm <- FALSE
    textwarn <- "File is not formatted correctly, see examples for help"
  }
  if(confirm!=TRUE) {shinyFeedback::feedbackWarning(inputId = id,
                                                    show = !confirm,
                                                    text = textwarn)}
  req(confirm#,
      #cancelOutput = TRUE
  )

  if(header==TRUE) {
    x <- vroom::vroom(file$datapath,
                      delim=sep,
                      show_col_types = FALSE)
    } else
    {x <- vroom::vroom(file$datapath,
                     delim=sep,
                     col_names = FALSE,
                     show_col_types = FALSE)}
  return(x)
}
