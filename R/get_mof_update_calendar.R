#' get_mof_update_calendar
#'
#' @param destdir output directory
#'
#' @return csv file
#' @export
get_mof_update_calendar <- function(destdir = NULL) {
  if (!is.null(destdir)) {
    stopifnot(fs::dir_exists(destdir))
  }

  mof_nm <- c("\u521d\u6b65\u503c\u767c\u5e03\u6642\u9593",
              "\u6b63\u5f0f\u503c\u767c\u5e03\u6642\u9593")

  pdf_urls <- list("https://portal.sw.nat.gov.tw/APGA/GA02_link?fileName=file4",  # 1st release
                   "https://portal.sw.nat.gov.tw/APGA/GA02_link?fileName=file2")  # 2nd release

  con_checks <- unlist(lapply(pdf_urls, function(x) {
    httr::GET(x)[["status_code"]]
  }))

  if (!all(con_checks == 200L)) {
    stop("cannot download pdf files!", call. = FALSE)

  } else {
    mof_list <- lapply(pdf_urls, textreadr::read_pdf, skip = 3)

    for(i in 1:2) {
      doc <- tibble::as_tibble(mof_list[[i]][["text"]])
      names(doc) <- "date"

      doc <- dplyr::transmute(doc,
        "\u9810\u5b9a\u767c\u5e03\u6642\u9593" = stringr::str_replace_all(stringr::str_sub(date, 1, 20), " ", ""),
        "\u8cc7\u6599\u6240\u5c6c\u671f\u9593" =  stringr::str_replace_all(stringr::str_sub(date, 20, stringr::str_length(date)), " ", "")
      )
      doc <- doc[doc[["\u8cc7\u6599\u6240\u5c6c\u671f\u9593"]] != "", ]
      doc <- dplyr::transmute(doc,
        "Subject"       = mof_nm[i],
        "Start Date"    = convert_date(!! rlang::sym("\u9810\u5b9a\u767c\u5e03\u6642\u9593")),
        "All Day Event" = TRUE,
        "Description"   = paste0("\u8cc7\u6599\u6240\u5c6c\u671f\u9593: ", convert_date(!! rlang::sym("\u8cc7\u6599\u6240\u5c6c\u671f\u9593")))
      )

      if (is.null(destdir)) {
        readr::write_excel_csv(doc, file.path(".", paste0(mof_nm[i], ".csv")))
      } else {
        readr::write_excel_csv(doc, file.path(destdir, paste0(mof_nm[i], ".csv")))
      }

    }
  }
}
