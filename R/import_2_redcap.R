

#' @rdname ExpandBranchingLogic
#'
#' @name prepareDatatoImport
#'
#' @title Prepare the data to be imported to Redcap.
#'
#' @description This is a utility function that helps prepare the data to be imported based on the target redcap project.
#'
#' @details
#'
#' @param metadata REDCap metadata
#' @param data Data to be imported to Redcap. Should be associated with metadata
#'
#' @export prepareDatatoImport
#'
#' @family RedcapToR
#' @return Formatted data ready to be imported to Redcap.
#'
require(data.table)


exportdataMeta2r<- function(token, url){
  cat("Exporting the metadata & data from target project...\n")


  facilityproject <- redcap_project(api_url = url,
                                       token = token,
                                       chunked = T,
                                       chunksize = 500,
                                       local = FALSE
  )

  metadata<-facilityproject$get_metadata()
  setDT(metadata)

  facilityproject$load_data()

  metadata<-facilityproject$get_metadata()
  setDT(metadata)
  facilitydata<-facilityproject$get_raw_data()
  return(list(records=facilitydata, metadata=metadata))
  cat("Data exported from Redcap...\n")
}


# Decimal places

adjust_to_one_decimal <- function(x) {
  # Convert to character for processing
  x <- as.character(x)

  if (x == "-1") {
    return("")
  } else if (grepl("^\\d+$", x)) {
    return(format(round(as.numeric(x), 1), nsmall = 1))
  } else if (grepl("^\\d+\\.\\d{1}$", x)) {
    return(format(x, nsmall = 1))
  } else if (grepl("^\\d+\\.\\d{2,}$", x)) {
    return(format(round(as.numeric(x), 1), nsmall = 1))
  } else {
    return(x)
  }
}

#clean data based on the expected fomart
cat("Defining reshaping helpers ..\n")

reshape.labels<-function(x)
{
  if(!(tolower(x[, field_type]) %in% c("descriptive"))){
    if (tolower(x[, field_type]) %in% c("checkbox", "dropdown", "radio")

    ) {
      choices <- t(sapply(strsplit(x[, select_choices_or_calculations], "\\|")[[1L]],
                          function(a)  {
                            lev <- strsplit(a, ",")[[1L]][1L]
                            lev <- paste0(lev, "L")
                            lab <- strsplit(a, ",")[[1L]][2L]
                            return(c(trim.string(lev), trim.string(lab)))
                          }))
      if (x[, field_type] == "checkbox" & x[, select_choices_or_calculations]!="") {
        variable.suffix <-try( gsub("L$", "", choices[,1]), silent = T)
        variable <- paste0(x[, field_name], "___", variable.suffix)
        variable <- gsub('_-1', '__1', variable)
        label <- paste0(gsub("\n", "", eliminate.htm(x[, field_label])), "(", choices[, 2L], ")")
        levels <- rep("c(0L, 1L)", length(choices[, 2L]))
        labels.levels <- rep("c(\"No\", \"Yes\")", length(choices[, 2L]))
        retVal<-data.table(Variable = variable, Label = label, Levels = levels, Label_Levels = labels.levels)
        return(retVal)
      }  else {
        if(x[, select_choices_or_calculations]!=""){
          variable <- x[, field_name]
          label <- gsub("\n", "", eliminate.htm(x[, field_label]))
          choices[, 2L] <- sapply(choices[, 2L], function(x) paste0('"', x, '"'))
          levels <- paste0("c(", paste0(
            gsub("L", "", #unique
                 (choices[, 1L])) %>% sapply(function(x){
                   paste0("'", stringr::str_trim(x), "'")
                 })

            , collapse = ", "), ")")

          labels.levels <- paste0("c(", paste0(#unique
            (choices[, 2L]), collapse = ", "), ")")

          retVal<-data.table(Variable = variable, Label = label, Levels = levels, Label_Levels = labels.levels)
          return(retVal)
        }

      }

    }
    else if (tolower(x[, field_type]) == "yesno" & x[, select_choices_or_calculations]!="") {
      variable <- x[, field_name]
      label <- gsub("\n", "", eliminate.htm(x[, field_label]))
      levels <- "c(0L, 1L)"
      labels.levels <- "c(\"No\", \"Yes\")"
      retVal<-data.table(Variable = variable, Label = label, Levels = levels, Label_Levels = labels.levels)
      return(retVal)
    }
    else {
      variable <-x[, field_name]
      label <- gsub("\n", "", eliminate.htm(x[, field_label]))
      levels <- NA_character_
      labels.levels <- NA_character_
      retVal<-data.table(Variable = variable, Label = label, Levels = levels, Label_Levels = labels.levels)
      return(retVal)
    }
  }

}



trim.string <- function(x) {
  if(is.null(x))
  {
    return("")
  }
  if(is.na(x))
  {
    return("")
  }
  x <- as.character(x)
  if(nchar(x) > 0L) {
    left.end <- regexpr("[^[:space:]]", x)
    x <- substr(x, left.end, nchar(x))
    rm(left.end)
    reverse <- paste0(rev(strsplit(x, "")[[1L]]), collapse = "")
    right.end <- regexpr("[^[:space:]]", reverse)
    ret.val <- substr(reverse, right.end, nchar(reverse))
    ret.val<-paste0(rev(strsplit(ret.val, "")[[1L]]), collapse = "")
    return(ret.val)
  }
  return(x)
}

varsToAdd<- NULL
checkboxes.ds<- data.table()
require(stringr)

prepareDatatoImport<- function(
    metadata=stop('Provide metadata for the redcap project to import to')
         ,dataToImport=stop('Provide raw data to import to redcap')
    ){
  require(data.table)
  setDT(metadata)
  setDT(dataToImport)
  toexclude<- metadata[field_type=='descriptive', field_name]

  cat("Dropping the  descriptive fields...\n")

  dataToImport.1<- copy(dataToImport[,
                                     .SD,
                                     .SDcols=which(!(names(dataToImport) %in% toexclude))])

  checkboxes<- metadata[field_type=='checkbox', field_name]
  checkboxes.choices<- metadata[field_type=='checkbox', .(field_name,select_choices_or_calculations)]

  cat("Creating the hash table for checkbox fields ...\n")


  checkboxes %>%
    lapply(function(xx){
      options<-  checkboxes.choices[field_name==xx,
                                    select_choices_or_calculations]
      strsplit(options, "\\|") %>% unlist() %>%
        lapply(function(x){
          nmbers<- sub(",", "\001", x) %>%
            strsplit("\001") %>%
            unlist() %>%
            str_trim()
          nmbers.x<- paste0(xx, "__",(nmbers[1]))
          nmbers.x<- gsub("\\-", "_",nmbers.x)
          toreturn<- data.table(checkbox=xx, choices=nmbers[1],fieldName=nmbers.x)
          checkboxes.ds<<- rbindlist(list(checkboxes.ds, toreturn))
          varsToAdd<<- c(varsToAdd,nmbers.x )
        })

    })


  #___

  cat("Dropping the unexpanded checkbox fields ...\n")

  #_____remove old checkboxes
  dataToImport.2<- copy(dataToImport.1[,
                                       .SD,
                                       .SDcols=which(!(names(dataToImport.1) %in% checkboxes))])

  setDT(dataToImport.2)
  dataToImport.2<- dataToImport.2[, .SD, .SDcols=!grepl(paste0(checkboxes, collapse = "|"), names(dataToImport.2))]


  # add the formatted with default choice
  cat("Adding the expanded checkbox fields ...\n")

  dataToImport.2[, (varsToAdd) := 0L]

  dataToImport.2[, key:=.I]
  checkboxes.tomodify<- copy(dataToImport.2[, .SD, .SDcols = c('key', varsToAdd)])
  dataToImport[, key:=.I]


  checkbox.data.notfound<-NULL

  idx<- NULL
  checkboxes<- checkboxes[is.element(checkboxes ,names(dataToImport))]
  lapply(checkboxes,
         function(xs){
           idx<- 1L
           dataToImport[, xs, with=F] %>%
             unlist() %>%
             as.character() %>%
             lapply(function(x){

               if(grepl("_{2,}", x)){
                 last_group_underscores <- regmatches(x, regexpr("_{2,}", x))
                 underscore_count <- nchar(last_group_underscores)
                 if(underscore_count>=2){
                   if(nrow(checkboxes.ds[fieldName==x,])==0){
                     checkbox.data.notfound<<- c(checkbox.data.notfound, x)
                   }else{

                   }
                 }else{

                 }
               }else{
                 if(grepl("\\,", x)){
                   split_result <- strsplit(x, ",")
                   # Convert the result to a vector for easier access
                   split_parts <- unlist(split_result)
                   lapply(split_parts, function(x){
                     toPickfrom<- copy(checkboxes.ds[checkbox==xs & choices==x,])
                     txt=paste0('dataToImport.2[key==',idx, ',toPickfrom$fieldName:=1L]')
                     eval(parse(text=txt))
                     dataToImport.2<<- dataToImport.2
                   })
                   idx<<- idx+1
                 }else{
                   toPickfrom<- copy(checkboxes.ds[checkbox==xs & choices==x,])
                   txt=paste0('dataToImport.2[key==',idx, ',toPickfrom$fieldName:=1L]')
                   eval(parse(text=txt))
                   dataToImport.2<<- dataToImport.2
                   idx<<- idx+1
                 }
               }
             })
         })

  cat("Expanded checkbox fields added to dataset...\n")

  cat("Validating by data type...\n")

  # validation type
  validationtypes<- metadata[, text_validation_type_or_show_slider_number] %>% unique()

  validationtypes<- validationtypes[validationtypes!=""]

  # date formating
  cat("\tValidating by dates...\n")

  metadata[text_validation_type_or_show_slider_number=="date_ymd",
           field_name] %>%
    lapply(function(dateName){
      eval(parse(text = paste0('dataToImport.2[,', dateName,':=as.Date(',dateName,', "%Y-%m-%d")]')))
    })

  # integer
  cat("\tValidating by integers...\n")

  metadata[text_validation_type_or_show_slider_number=="integer",
           field_name] %>%
    lapply(function(intype){
      eval(parse(text = paste0('dataToImport.2[,',
                               intype,':=as.integer(',intype,')]')))
    })


  cat("\tValidating by decimal places...\n")


  metadata[text_validation_type_or_show_slider_number=="number_1dp",
           field_name] %>%
    lapply(function(onedp){
      eval(parse(text=paste0('dataToImport.2[, ',onedp,':=apply(.SD,
                                           1L,
                                           function(xx){
                                   xx= as.character(xx)
                                   sapply(xx, adjust_to_one_decimal) %>% as.character()

    }), .SDcols = "',onedp,'"]'))
      )
    })

  cat("\tValidating by number...\n")

  metadata[text_validation_type_or_show_slider_number=="number",
           field_name]%>%
    lapply(function(nm){
      eval(parse(text = paste0('dataToImport.2[,',
                               nm,':=as.numeric(',nm,')]')))
    })

  cat("\tValidating by time ... \n")

  metadata[text_validation_type_or_show_slider_number=="time",
           field_name]%>%
    lapply(function(tme){
      eval(parse(text = paste0('dataToImport.2[,',
                               tme,':=format(strptime(',tme,',"%H:%M:%S"), "%H:%M")]')))
    })


  metadata[, key:=.I]
  cat("Creating data reshaping options ..\n")
  labels.lookup.table <- metadata[,reshape.labels(.SD),by=key]

  #dropdown
  metadata[field_type %in% c("dropdown", "radio", 'yesno'),
           field_name]%>%
    lapply(function(fname){
      if(is.element(fname, names(dataToImport.2))){

        datavls<- labels.lookup.table[Variable==fname, Levels]
        eval(parse(text=paste0('dataToImport.2[!(',fname, '%in% eval(parse(text=datavls))),
                   ',fname,':=""]'
        )
        ))
      }
    })

  cat("Data reshaped according to set options ..\n")


  nms.ds<- names(dataToImport.2)

  cat("Setting NA to blank ..\n")

  dataToImport.2[, (nms.ds):=lapply(
    dataToImport.2[
      ,nms.ds
      ,with=FALSE] ,function(x){
        if(any(is.na(x))){
          x[which(is.na(x))]<-""
          return(as.character(x))
        }else{
          return(x)
        }

      })
  ]

  # branching logic
  cat("Cleaning data based on the redcap branching logic ...\n")

  metadata[, key:=.I]
  metadata[, branching_logic.m:=NA_character_]
  metadata[branching_logic!="", branching_logic.m:=convert_redcap2r(branching_logic),
           by=key]

  metadata[!is.na(branching_logic.m), field_name] %>%
    lapply(function(xx){
      blogic<- metadata[field_name==xx, branching_logic.m]
      ftype<- metadata[field_name==xx, field_type]
      if(ftype=='checkbox'){
        grep(paste0(xx, "__"), names(dataToImport.2), v=T) %>%
          lapply(function(xs){
            toeval<- paste0('try(dataToImport.2[!(',blogic,') & ',xs,'!=0, ',xs,':=0], silent=T)')
            eval(parse(text=toeval))
          })
      }else{
        toeval<- paste0('try(dataToImport.2[!(',blogic,') & ',xx,'!="", ',xx,':=""],silent=T)')
        eval(parse(text=toeval))
      }
    })

  cat("Branching logic cleaning done\n")

  return(dataToImport.2)
}



