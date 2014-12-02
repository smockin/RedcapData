#' @name create_updates_info
#'
#' @title Wrapper for creating REDCap update objects
#'
#' @description Cleaner way for instantiating REDCap update objects.
#'
#' Avoids messing up instantiation in R6 classes which is more complex and error prone.
#'
#' @details This function acts as a wrapper for instantiating an object that abstracts the REDCap updates.
#'
#' It performs the necessary checks for instantiation hence avoids messy objects.
#'
#' It then calls the new method of the underlying reference class.
#'
#'
#' The update dataset must contain the following variables:
#'
#' \describe{
#'  \item{update}{The update name, eg Nov-14, Feb-13}
#'  \item{site}{The hospital id of the site, must be integer and is replicated for each update}
#'  \item{date}{The date of the update for each specific site}
#'  \item{new_vars}{
#'  The new variables introduced during the update <must have at least one entry per update>.
#'
#'  The first non misssing entry will be used.
#'
#'  The variable names must be separated by semi-colons.
#'  }
#' }
#'
#' @param updates_data A dataset containing the updates info. For the column specifications, see details.
#'
#' @export
#'
#' @seealso \code{\link{redcap_update}}, \code{\link{redcap_update_list}}
#'
#' @return A redcap updates class that contains the redcap updates
#'
#' @include redcap_update.R
#'

create_updates_info = function(updates_data) {
  if (!is.data.frame(updates_data))
    stop("updates_data not a data frame")
  updates_data = data.frame(updates_data)
  if (any(is.na(updates_data$update) | str_trim(updates_data$update) == ""))
    stop("some updates(s) missing")
  if (!all(c("update", "site", "date", "new_vars") %in% names(updates_data)))
    stop("some column(s) missing")
  if (any(is.na(updates_data$site) | str_trim(updates_data$site) == ""))
    stop("some site(s) missing")
  if (any(is.na(updates_data$date) | str_trim(updates_data$date) == ""))
    stop("some date(s) missing")
  if (!all(sapply(str_trim(updates_data$site), is_int)))
    stop("some site(s) are invalid")
  updates_data$site = as.integer(updates_data$site)
  if (!all(sapply(str_trim(updates_data$date), is_date)))
    stop("some date(s) are invalid")
  updates_data$date = as.Date(updates_data$date)
  value = list()
  updates_data = split(updates_data, updates_data$update)
  updates = names(updates_data)
  value = lapply(updates, function(up_name) {
    update_curr = updates_data[[up_name]]
    name = up_name
    site_info = update_curr[, c("site", "date")]
    if (length(unique(site_info$site)) != length(site_info$site))
      stop(paste0("some sites duplicated [", name, "]"))
    new_vars = as.character(update_curr[, "new_vars"])
    new_vars[str_trim(new_vars) == ""] = NA
    new_vars = na.omit(new_vars)[1]
    new_vars = str_trim(unlist(strsplit(new_vars, ";")))
    new_vars = unique(new_vars)
    redcap_update$new(name = name, site_info = site_info, new_vars = new_vars)
  })
  redcap_update_list$new(updates = value)
}
