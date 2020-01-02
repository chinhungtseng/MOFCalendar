convert_date <- function(date) {
  date_tmp <- vector("character", length(date))
  if (stringr::str_detect(date[1], "\u5e74.*\u6708.*\u65e5")) {
    for (i in seq_along(date)) {
      a <- unlist(stringr::str_split(date[i], "\u5e74|\u6708|\u65e5"))
      year = as.numeric(a[1]) + 1911
      month = as.numeric(a[2])
      day  = as.numeric(a[3])
      date_tmp[[i]] <- format(as.Date(paste0(year, "-", month, "-", day)), "%m/%d/%Y")
    }
  } else {
    for (i in seq_along(date)) {
      a <- unlist(stringr::str_split(date[i], "\u5e74|\u6708"))
      year = as.numeric(a[1]) + 1911
      month = as.numeric(a[2])
      date_tmp[[i]] <- format(as.Date(paste0(year, "-", month, "-01")), "%m/%Y")
    }
  }
  date_tmp
}
