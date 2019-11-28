
#' @rdname ExpandBranchingLogic
#'
#' @name format_branching_logics
#'
#' @title Convert Redcap branching logic contained in metadata to R parlance
#'
#' @description This is a utility function that converts branching logic to R code.
#'
#' @details For code generation, code has to be translated from one DSL or syntax to another.
#'
#' In this case, conversion from REDCap logic to the appropriate R syntax.
#'
#' This function helps map logical expressions from REDCap to R
#'
#' @param metadata REDCap metadata
#' @param data Data associated with metadata
#'
#' @export
#' 
#' @family RedcapToR
#' @return Transformed metadata with a formatted branching logic in the column name \emph{f.branching_logic} 
format_branching_logics<- function(metadata=stop('Provide metadata')
                                   ,data=stop('Provide data')){
  force(metadata)
  force(data)
  if(!is.data.frame(metadata))
    stop("'Metadata' is not a data.frame")
  if(!is.data.table(metadata))
    setDT(metadata)
  if(!is.data.frame(data) )
    stop("'data' is not a data.frame")
  if(!is.data.table(data))
    setDT(data)
  records<- data.table::copy(data)
  if(!is.element('branching_logic', names(metadata))){
    stop("Metadata has no`branching_logic`!")
  }
  metadata[, f.branching_logic:=NA_character_]
  metadata[ ,
            f.branching_logic:=apply(.SD
                                     , 1L
                                     , function(br){
                                       get_evaluated_branching_logics(br=br)
                                     })
            , .SDcols=c('branching_logic')]
  return(metadata)
}




get_evaluated_branching_logics<- compiler::cmpfun(function(br){
  records=get("data", parent.frame())
  toformat<- br %>% 
    gsub("and\\[", " & ", .) %>%
    gsub("and[ \t]+\\[", " & ", .) %>%
    gsub("\\[|\\]", "", .) %>% 
    gsub("[ \t]and[ \t]|[ \t]+((AND)|(and))[ \t]+", " & ", .) %>% 
    gsub("[ \t]or[ \t]|[ \t]+((OR)|(or))[ \t]+", " | ", .) %>% 
    gsub("={1}[ \t]*", " == ", .) 
  str_trim(gsub("<>|(<>)[ \t]", " != ", toformat)) %>% 
    gsub("! == ", " != ", .) %>% 
    gsub("> == ", " >= ", .) %>% 
    gsub("< == ", " <= ", .) %>% 
    gsub('"', "'", .)-> toformat
  
  #cascades for 'other' input
  if(grepl("\\([0-9]+\\)"
           , toformat)){ 
    if(grepl("&|\\|", toformat)){
      toformat2=str_trim(unlist(str_split(toformat, "&|\\|")))
      and.sep=grepl("\\&",toformat) & !grepl("\\|",toformat)
      or.sep=grepl("\\|",toformat) & !grepl("\\&",toformat)
      or.and.sep=grepl("\\|",toformat) & grepl("\\&",toformat)
      
      idx_=which(grepl("\\([0-9]+\\)"
                       , toformat2))
      idx_not=which(!(grepl("\\([0-9]+\\)"
                            , toformat2)))
      sepToUse=ifelse(and.sep, " & ",  ' | ' )
      if(length(idx_)==1L){
        tfmt=toformat2[idx_]
        toAppend=expand_other_widgets(toformat =tfmt )
        if(!is_empty(idx_not)){
          if(length(idx_not)>1L){
            toformat= paste0( paste(toformat2[idx_not], collapse = sepToUse)
                              , sepToUse
                              , toAppend)
          }else{
            toformat=paste0(toformat2[idx_not]
                            , sepToUse
                            , toAppend)
          }
          
        }else{
          toformat=toAppend
        }
      }else{
        vapply(toformat2, function(xx){
          expand_other_widgets(toformat =xx )
        }, character(1L))-> tocombine
        if(!or.and.sep){
          combined=paste0(tocombine, collapse =sepToUse)
        }else{
          combined=paste0(tocombine, collapse =" & ")
        }
        if(is_empty(idx_not)){
          toformat= combined 
        }else{
          toformat=paste0(toformat2[idx_not], combined)
        }
      }
    }else{
      toformat=expand_other_widgets(toformat)
    }
  }
  return(
    as.character(
      toformat
    ))
})




expand_other_widgets<- compiler::cmpfun(function(toformat){
  br=get("br", envir = parent.frame())
  records=get("records", envir = parent.frame())
  txt=str_trim(unlist(str_split(toformat, " ")))
  if(any(
    all(is.na(txt)) ||
    length(txt)<=1
  )){
    issues<<- as.character(c(issues, br))
  }
  txt=txt[txt!='']
  toformat=txt[1]
  xtract=stringr::str_extract(toformat, "\\([0-9]+\\)$")
  level.n=gsub("\\(|\\)", "", xtract)
  xx=substring(text=toformat
               ,first =1L,
               last=nchar(toformat)-nchar(xtract))
  
  txt.f=grep(level.n
             , grep(xx,
                    names(records),
                    v=T), v=T)
  if(length(txt.f)>1){
    underscores<-(str_extract_all(txt.f
                                  , regex("[_]{2,}+"
                                          , TRUE)) %>%
                    unlist() %>%
                    table() %>%
                    names())[1]
    
    toformat=paste0(xx
                    , underscores
                    ,level.n
                    ,txt[2]
                    , txt[3]
    )
    
  }else{
    toformat=paste0(txt.f
                    , txt[2]
                    , txt[3]
    )
    
  }
  return(toformat)
})