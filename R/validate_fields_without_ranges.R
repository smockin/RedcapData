
cellHasNoData <- compiler::cmpfun(function(x) {
  is.null(x) || is.na(x) || str_trim(x) == "" 
})

vectorHasNoData <- compiler::cmpfun(function(x) {
  as.logical(purrr::map_dbl(x, cellHasNoData))
})




#' @rdname validateDatainBranchingLogic
#'
#' @name validate_data_in_branching_logic
#'
#' @title Generate custom codes and evaluate based on branching logics 
#'
#' @description This is a utility function that uses branching logic to validate data entry workflow for errors of omission.
#'
#' @details Using the redcap metadata's newly created variable \emph{f.branching_logic}, code is generated that validates data entry during the data capture process.
#'
#' @param rec Single record in the data
#' @param metadataName Metadata name
#' @param dataName Data name
#' @param ipno_var Name of variable for patient IP Number
#' @param dateOfEntry_var Name of variable that captures the date of entry
#' @param recordID_var Name of variable that uniquely identifies records
#' @param hospitalID_var Name of variable that holds the hospital code 
#' @param individual.vars A \emph{character} name of the object containing variables to be validated individually     
#' @param group.names A vector of \emph{character} names of the objects containing variables to be validated as a group     
#' @param n.groups Numebr of groups provided in \strong{group.names}
#' @param validateTreatmentDates Logical. If FALSE, treatment dates will not be validated 
#' @param treatmentFormName  Name of the treatment section in the metadata. This is required if \strong{validateTreatmentDates} is TRUE
#' @param dischargeDateVar Name of variable that holds the date of discharge. This is required if \strong{validateTreatmentDates} is TRUE
#' @param dischargeDateVar Name of variable that holds the date of admission. This is required if \strong{validateTreatmentDates} is TRUE


#' @export
#'
#' @return A data frame with details of the errors.
#'
#' @family Redcap-to-R parlance converters

validate_data_in_branching_logic<- compiler::cmpfun(function(rec
                                                             ,metadataName='metadata.formatted'
                                                             ,dataName='data.raw'
                                                             ,ipno_var=stop("Provide variable name for IP Number")
                                                             ,dateOfEntry_var=stop("Provide variable name for date of entry")
                                                             ,recordID_var=stop("Provide variable name for record ID")
                                                             ,hospitalID_var=stop("Provide variable name for hospitals")
                                                             ,individual.vars=stop("Provide a list of variables to be validated individually")
                                                             ,n.groups=NULL
                                                             ,group.names=NULL
                                                             ,validateTreatmentDates=F
                                                             ,treatmentFormName=NULL
                                                             ,dischargeDateVar=NULL
                                                             ,admissionDateVar=NULL
){
  force(individual.vars)
  force(ipno_var)
  force(dateOfEntry_var)
  force(recordID_var)
  force(hospitalID_var)
  
  if(validateTreatmentDates){
    if((is_null(dischargeDateVar) ||
        is_null(admissionDateVar))){
      stop("Dates cannot be validated without providing variable names both for admission and discharge dates")
    }
    if(is_null(treatmentFormName)){
      stop("Provide form name for treatment section in the metadata")
    }
  }  
  
  enV_<- environment()
  records<- try(get(dataName
                    , envir = globalenv())
                ,silent = T)
  metadata<- try(get(metadataName
                     , envir = globalenv())
                 , silent=T)
  
  if(any(
    is.element('try-error'
               , c(class(records)
                   , class(metadata)
               )
    )
  )){
    stop(paste0(dataName,
                ' or ' 
                ,metadataName,
                ' is not defined')
    )
  }else{
    records<- as.data.table(records)
    metadata<- as.data.table(metadata)
    
  }
  
  if(!is_null(n.groups) || !is_null(group.names)){
    if(isTRUE(n.groups != length(group.names))){
      stop('Number of groups of variables must be equal to `n.groups`')
    } else { 
      group.names=group.names[which(lapply(group.names, function(x){
        length(eval(parse(text=x)))})!=0)]
      if(length(group.names)!=0L){
        grp.ds=group.names %>% map(function(grp){
          get_errors(GroupVariable = get(grp,
                                         envir = parent.frame())
          )
        }) %>% reduce(rbind) %>% setDT()
        ind.ds=  get_errors(listOfVariables=get(individual.vars
                                                , envir = parent.frame()
        ))
        if(
          !all(c(is_empty(grp.ds)
                 , is_empty(ind.ds)
                 
          )
          )
        ){
          flush.console()
          cat(paste0('Record ID: ',rec[,recordID_var, with=F], ' validated\n'))
          return(
            rbindlist(list(grp.ds
                           , ind.ds
                           
            )
            )
          )
        }
      }else{
        flush.console()
        cat(paste0('Record ID: ',rec[,recordID_var, with=F], ' validated\n'))
        return(
          get_errors(listOfVariables=get(individual.vars
                                         , envir = parent.frame()
          )))
      }
    }
  }else{
    flush.console()
    cat(paste0('Record ID: ',rec[,recordID_var, with=F], ' validated\n'))
    return(
      get_errors(listOfVariables=get(individual.vars
                                     , envir = parent.frame()
      ))
    )
  }
})


# Generate errors 
#____________________________________________________________

get_errors<- compiler::cmpfun(function(listOfVariables=NA
                                       , GroupVariable=NA){
  rec=get("rec", parent.frame())
  ipno_var=get("ipno_var", parent.frame())
  dateOfEntry_var=get("dateOfEntry_var", parent.frame())
  recordID_var=get("recordID_var", parent.frame())
  hospitalID_var=get("hospitalID_var", parent.frame())
  metadata=get("metadata", parent.frame())
  dischargeDateVar=get('dischargeDateVar',
                       envir = parent.frame()
  )
  
  admissionDateVar=get('admissionDateVar',
                       envir = parent.frame()
  )
  
  treatmentFormName=get('treatmentFormName',
                        envir = parent.frame()
  )
  validateTreatmentDates=get("validateTreatmentDates", envir = parent.frame())
  
  ipn_=rec[,ipno_var, with=F][[1L]]
  id_=rec[,recordID_var, with=F][[1L]]
  datetoday=rec[, dateOfEntry_var, with=F][[1L]]
  hspId=rec[, hospitalID_var, with=F][[1L]]
  msg=NA_character_
  Entry<<-NA_character_
  Type<<-NA_character_
  
  if(all(!is.na(
    GroupVariable
  )
  )){
    xx=variable_toCheck=GroupVariable
    cond_=metadata[is.element(field_name, xx)
                   , f.branching_logic]
    
    form_=lab_=metadata[is.element(field_name, xx)
                        , form_name][1L]
    cellValue=paste("c(",paste0(
      'rec[',cond_,'
      ,', variable_toCheck,
      ']'
      , collapse = ","), ")") %>% 
      parse(text=.) %>%
      eval()
    
    if(length(cellValue)!=0 && !is_empty(cellValue) 
    ){
      if(
        isTRUE(
          all(
            vectorHasNoData (as.character(cellValue))
          )
        )){
        msg<- paste0('Provide at least one `', form_ ,'`')
        Type<<-"No Entry"
      }
    }
    if(!is.na(msg) && !is_empty(msg)){
      err.ds<-  data.table(RecordID=id_
                           ,Identifier=ipn_
                           ,DateOfEntry=datetoday
                           ,Hospital=hspId
                           ,Form=form_
                           ,Section=NA_character_
                           ,Variable=form_
                           ,Type=Type
                           ,Entry=Entry
                           ,Message=remove_html_tags(msg)
                           ,Logic=NA_character_
                           
      )
      Entry<<-NA_character_
      return(err.ds)
      
    }
  }else{
    listOfVariables %>% 
      map(function(xx){ 
        cond_=metadata[field_name==xx
                       , f.branching_logic]
        form_=metadata[field_name==xx
                       , form_name]
        lab_=metadata[field_name==xx
                      , field_label]
        sect_=metadata[field_name==xx
                       , section_header]
        
        # Redacap v7+ : hide field functionality
        #__________________________________________________
        
        has.hidden.fun<- grepl("hidden"
                               , metadata[field_name==xx
                                          , field_annotation]
                               , ignore.case = T)
        
        type_<-metadata[field_name==xx
                        , field_type]
        isCheckbox<- type_=="checkbox"
        date_=metadata[field_name==xx
                       , text_validation_type_or_show_slider_number]
        isDate=(date_=='date_ymd')
        
        msg=NA_character_
        if(isCheckbox){
          Checkbox=grep(xx,
                        names(rec)
                        , v=T)
          
          # validate Checkbox returns
          #_______________________________________
          
          checkbox_xtended<- paste0(xx
                                    ,str_extract_all(Checkbox
                                                     , regex("[_]{2,}[0-9]+$"
                                                             , TRUE)) %>% unlist)
          if(
            isTRUE(
              !(
                all(
                  checkbox_xtended
                  %in%
                  metadata[,field_name ]
                ))
            )
          ){
            checkbox_xtended<-checkbox_xtended[
              which(
                !(checkbox_xtended 
                  %in%
                    metadata[,field_name ]
                )
              )]
          }
          
          variable_toCheck<-checkbox_xtended
        }else{
          variable_toCheck<-xx
        }
        
        #field not hidden
        #_______________________________________
        
        if(
          isTRUE(
            !is_hidden(b.logic = cond_)
          )
        ){
          msg=determine_if_cell_has_value()
        }
        if(!is.na(msg) && !is_empty(msg)){
          err.ds<-  data.table(RecordID=id_
                               ,Identifier=ipn_
                               ,DateOfEntry=datetoday
                               ,Hospital=hspId
                               ,Form=form_
                               ,Section=sect_
                               ,Variable=xx
                               ,Type=Type
                               ,Entry=Entry
                               ,Message=remove_html_tags(msg)
                               ,Logic=cond_
                               
          )
          Entry<<-NA_character_
          return(err.ds)
          
        }
      }) %>% 
      do.call(rbind,.)
  }
  
  
})

# detertermine if field is hidden: no data expected
#___________________________________________________________

is_hidden<-compiler::cmpfun(
  function(b.logic){
    has.hidden.fun=get('has.hidden.fun', envir = parent.frame())
    if(has.hidden.fun) {
      return(T)  
    }else{
      if(grepl("&", b.logic)){
        toformat_<-str_split(b.logic,
                             "&") %>%
          unlist() %>%
          str_trim() %>% 
          gsub("\\(|\\)", "", .) %>%
          vapply(function(x_){
            gsub("[ \t]", "",x_)
          }, character(1L)) %>% as.character()
        
        if(length(toformat_)>1L){
          logic_ds<- toformat_ %>% 
            vapply(function(fmt){
              str_split(fmt, "==") %>% 
                unlist()-> splts
              vr=splts[1]
              vl=splts[2]
              data.table(rbind(vr, vl))
            }, data.table(tst="")) %>% 
            do.call(rbind, .) %>% as.data.table(keep.rownames = F) %>% 
            setnames(c("f.name", "f.value"))
          
          # get all duplicates: locally hidden
          #____________________________________
          
          keepAllDup <- function (value)
          {
            duplicated(value) | duplicated(value
                                           , fromLast = TRUE)
          }
          dups<-logic_ds[keepAllDup(logic_ds$f.name),]
          if(nrow(dups)!=0){
            if(length(
              unique(dups$f.value)
            )!=1L
            ){
              return(T)
            }else return(F)
          }else{
            return(F)
          }
        }else{
          return(F)
        }
      }else{
        return(F)
      }
    }
  })

# Assess if cell has data
#____________________________________

determine_if_cell_has_value<- compiler::cmpfun(function(){ 
  isCheckbox=get('isCheckbox', envir = parent.frame())
  variable_toCheck=get('variable_toCheck', envir = parent.frame())
  cond_=get('cond_', envir = parent.frame())
  lab_=get('lab_', envir = parent.frame())
  rec=get("rec", envir = parent.frame())
  xx=get("xx", envir = parent.frame())
  validateTreatmentDates=get("validateTreatmentDates", envir = parent.frame())
  
  dischargeDateVar=get('dischargeDateVar',
                       envir = parent.frame()
  )
  
  admissionDateVar=get('admissionDateVar',
                       envir = parent.frame()
  )
  
  treatmentFormName=get('treatmentFormName',
                        envir = parent.frame()
  )
  
  metadata=get("metadata", envir = parent.frame())
  if(isCheckbox) {
    cellValue=
      paste0(
        'rec[',cond_,'
        , variable_toCheck
        , with=F]'
      ) %>% 
      parse(text=.) %>%
      eval()
    
    if(nrow(cellValue)!=0 &&
       !is_empty(cellValue)){
      if(
        isTRUE(
          all(
            (as.numeric(cellValue))==0L
          )
        )){
        msg<- paste0("`" ,lab_,"` has no data!")
        Type<<-"No Entry"
        return(msg)
      }
    }
  }else{
    cellValue=try(
      paste0(
        '(
        rec[',cond_,',', variable_toCheck,']
      )') %>%
               parse(text=.) %>%
        eval(), silent = T)
    if(class(cellValue)!="try-error"){
      if(length(cellValue)!=0 &&
         !is_empty(cellValue)
      ){
        if(
          cellHasNoData(
            cellValue
          )){
          msg<- paste0("`" ,lab_,"` has no data!")
          Type<<-"No Entry"
          return(msg)
        }else{
          isDate=get("isDate"
                     , envir = parent.frame())
          if(isDate){
            
            msg=get_logical_dates()
            return(msg)
          }
        }
      } 
    }
    
  }
})

# Evaluate if date is logical
# ____________________________

get_logical_dates<- function(){
  dischargeDateVar=try(get('dischargeDateVar',
                           envir = parent.frame()
  ), silent = T)
  
  admissionDateVar=try(get('admissionDateVar',
                           envir = parent.frame()
  ), silent=T)
  
  treatmentFormName=try(get('treatmentFormName',
                            envir = parent.frame()
  ), silent = T)
  
  
  cellValue=get('cellValue',
                envir = parent.frame()
  )
  
  xx=get('xx',
         envir = parent.frame()
  )
  
  
  metadata=get('metadata',
               envir = parent.frame()
  )
  lab_=get('lab_',
           envir = parent.frame()
  )
  rec=get("rec", envir = parent.frame())
  validateTreatmentDates=get("validateTreatmentDates", envir = parent.frame())
  if(isTRUE(
    all(
      !is.element("try-error",
                  c(class(dischargeDateVar)
                    , class(admissionDateVar)
                  ))))){
    dateDischarged=rec[, dischargeDateVar, with=F]
    dateAdmitted=rec[, admissionDateVar, with=F]
    if(isTRUE(
      as.character(dateDischarged) !='' &&
      !is.na(as.character(dateDischarged)) && 
      isTRUE(
        any(
          (try(as.Date.character(cellValue),silent=T) > try(as.Date.character(dateDischarged), silent = T) &
           try(as.Date.character(dateDischarged), silent = T) >as.Date.character("1950-01-01"))|
          (try(as.Date.character(cellValue), silent = T) > Sys.Date() )
        )
      )
    )){
      msg<- paste0("`" ,lab_,"` cannot be after discharge date or in the future")
      Entry<<-cellValue
      Type<<-"Invalid date"
      return(msg)
    } else if(!is.na(dateAdmitted) &&
              as.character(dateAdmitted)!=''){
      if(class(treatmentFormName) != 'try-error'){
        isTreatment=grepl(treatmentFormName, metadata[field_name==xx  , form_name], ignore.case = T)
        if(validateTreatmentDates & isTreatment){
          if(isTRUE(try(as.Date.character(cellValue), silent = T) < try(as.Date.character(dateAdmitted),silent = T)) &
             isTRUE(try(as.Date.character(dateAdmitted), silent = T) >as.Date.character("1950-01-01"))){
            if(isTRUE(try(as.Date.character(cellValue),silent = T)> as.Date.character("1950-01-01"))){
              msg<- paste0("`" ,lab_,"` cannot be earlier than the date of admission")
              Entry<<-cellValue
              Type<<-"Invalid date"
              return(msg)
            }
          }
        }
      }
      
    }else if( !is.na(as.character(dateAdmitted)) && 
              as.character(dateAdmitted) !='' &&
              !is.na(as.character(dateDischarged)) && 
              as.character(dateDischarged) !='' &&
             isTRUE(
               any(try(as.Date.character(dateAdmitted), silent=T)> Sys.Date() |
                   (try(as.Date.character(dateDischarged), silent=T)> as.Date.character("1950-01-01") &&
                    try(as.Date.character(dateAdmitted), silent=T)> as.Date.character("1950-01-01") &&
                    try(as.Date.character(dateAdmitted), silent = T) > try(as.Date.character(dateDischarged), silent = T))
               )
             )
    ){
      msg<- paste0("Admission Date cannot be in the future or ealier than date of discharge!")
      Entry<<-dateAdmitted
      Type<<-"Invalid date"
      return(msg)
    } else {
      if( as.character(dateDischarged) !='' &&
          as.character(dateAdmitted) !='' &&
          !is.na(as.character(dateDischarged)) &&
          !is.na(as.character(dateAdmitted)) &&
          isTRUE(
            any(
              (try(as.Date.character(dateDischarged), silent = T)> as.Date.character("1950-01-01") &&
               try(as.Date.character(dateDischarged), silent = T) < try(as.Date.character(dateAdmitted), silent = T)) |
              try(as.Date.character(dateDischarged), silent = T) > Sys.Date()
            )
          )
      ){
        msg<- paste0("Date of discharge cannot be in the future or earlier than the date of admission")
        Entry<<-dateDischarged
        Type<<-"Invalid date"
        return(msg)
      }
    } 
  }
}
