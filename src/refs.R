get_openbd <- function (isbn) {
  if (length(isbn) == 0) {
    rlang::abort("ISBNを入力してください")
  }
  else {
    x <- 
      httr2::request("https://api.openbd.jp/v1/get") |> 
      httr2::req_url_query(isbn = paste0(isbn, collapse = ","))
    x |> 
      httr2::req_perform() |> 
      httr2::resp_body_json()
  }
}

isbn <- 
  c("978-4-297-12170-9",
    "978-4-295-01361-7",
    "978-4-06-535219-9",
    "978-4-297-12524-0",
    "978-4-320-11463-0")
x <- 
  get_openbd(isbn)
opendb_extract_author <- function(x, yomi = FALSE) {
  # res <- 
  #   x |> 
  #   purrr::keep(~ .x$ContributorRole == "A01")
  res <-
    x
  if (length(res) == 0L) {
    NULL
  } else {
    author_name <-
      res |> 
      purrr::map("PersonName")
    if (yomi == FALSE) {
      author_name |> 
        purrr::map("content") |> 
        purrr::reduce(c)      
    } else {
      author_name |> 
        purrr::map("collationkey") |> 
        purrr::reduce(c)
    }
  }
}
opendb_extract_jauthor <- function(x, yomi = FALSE) {
  # res <- 
  #   x |> 
  #   purrr::keep(~ .x$ContributorRole == "B06")
  res <-
    x
  if (length(res) == 0L) {
    NULL
  } else {
    author_name <-
      res |> 
      purrr::map("PersonName")
    if (yomi == FALSE) {
      author_name |> 
        purrr::map("content") |> 
        purrr::reduce(c)      
    } else {
      author_name |> 
        purrr::map("collationkey") |> 
        purrr::reduce(c)
    }
  }
}
opendb_extract_editor <- function(x) {
  # res <- 
  #   x |> 
  #   purrr::keep(~ .x$ContributorRole == "B01")
  res <- 
    x
  if (length(res) == 0L) {
    NULL
  } else {
    author_name <-
      res |> 
      purrr::map("PersonName")
    author_name |> 
      purrr::map("content") |> 
      purrr::reduce(c)
  }
}

make_bibentry <- function(x) {
  x |> 
    purrr::map(
      ~ bibentry(bibtype = "book", 
                 key = paste0("isbn", .x$onix$RecordReference),
                 isbn = .x$onix$RecordReference,
                 title = .x$summary$title,
                 series = .x$onix$DescriptiveDetail$Collection$TitleDetail$TitleElement[[1]]$TitleText$content,
                 author = opendb_extract_author(.x$onix$DescriptiveDetail$Contributor),
                 yomi = opendb_extract_author(.x$onix$DescriptiveDetail$Contributor, yomi = TRUE),
                 editor = opendb_extract_editor(.x$onix$DescriptiveDetail$Contributor),
                 year = stringr::str_sub(.x$summary$pubdate, 1, 4),
                 publisher = .x$summary$publisher)
    )
}

.make_bibentry <- function(i) {
  bibentry(bibtype = "book",
           key = paste0("isbn", x[[i]]$onix$RecordReference),
           isbn = x[[i]]$onix$RecordReference,
           title = x[[i]]$summary$title,
           subtitle = x[[i]]$onix$DescriptiveDetail$TitleDetail$TitleElement$Subtitle$content,
           # series = x[[i]]$onix$DescriptiveDetail$Collection$TitleDetail$TitleElement[[1]]$TitleText$content,
           author = opendb_extract_author(x[[i]]$onix$DescriptiveDetail$Contributor),
           translator = opendb_extract_jauthor(x[[i]]$onix$DescriptiveDetail$Contributor),
           yomi = opendb_extract_author(x[[i]]$onix$DescriptiveDetail$Contributor, yomi = TRUE),
           editor = opendb_extract_editor(x[[i]]$onix$DescriptiveDetail$Contributor),
           year = stringr::str_sub(x[[i]]$summary$pubdate, 1, 4),
           publisher = x[[i]]$summary$publisher)
}
.make_bibentry(1)
print(.make_bibentry(1), style = "Bibtex")
