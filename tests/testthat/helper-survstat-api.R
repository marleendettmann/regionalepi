soap_response <- function(operation, content) {
  paste0(
    "<s:Envelope xmlns:s='http://www.w3.org/2003/05/soap-envelope' ",
    "xmlns:b='http://schemas.datacontract.org/2004/07/Rki.SurvStat.WebService.Contracts.Mdx' ",
    "xmlns:i='http://www.w3.org/2001/XMLSchema-instance'>",
    "<s:Body><", operation, "Response xmlns='http://tools.rki.de/SurvStat/'>",
    "<", operation, "Result>", content, "</", operation, "Result>",
    "</", operation, "Response></s:Body></s:Envelope>"
  )
}

survstat_member_response <- function(captions, hierarchy) {
  members <- paste0(
    "<b:HierarchyMember><b:Caption>", captions,
    "</b:Caption><b:Id>", hierarchy, ".&amp;[", captions,
    "]</b:Id></b:HierarchyMember>", collapse = ""
  )
  soap_response(
    "GetAllHierarchyMembers",
    paste0("<b:HierarchyMembers>", members, "</b:HierarchyMembers>")
  )
}

survstat_olap_response <- function(
    geography = c("LK Alpha", "Unbekannt"),
    weeks = c("2020-KW01", "2020-KW53"),
    values = list(c("1,25", NA), c("0,00", "9,99")),
    duplicate_week = FALSE) {
  columns <- paste0(
    "<b:QueryResultColumn><b:Caption>", geography,
    "</b:Caption><b:ColumnName>[Geo].&amp;[", seq_along(geography),
    "]</b:ColumnName></b:QueryResultColumn>", collapse = ""
  )
  row_xml <- function(index) {
    cells <- paste(vapply(values[[index]], function(value) {
      if (is.na(value)) "<b:string i:nil='true'/>" else
        paste0("<b:string>", value, "</b:string>")
    }, character(1L)), collapse = "")
    paste0(
      "<b:QueryResultRow><b:Caption>", weeks[[index]],
      "</b:Caption><b:RowName>[ReportingDate].[YearWeek].&amp;[",
      weeks[[index]], "]</b:RowName><b:Values>", cells,
      "</b:Values></b:QueryResultRow>"
    )
  }
  rows <- paste(vapply(seq_along(weeks), row_xml, character(1L)), collapse = "")
  if (duplicate_week) rows <- paste0(rows, row_xml(1L))
  soap_response(
    "GetOlapData",
    paste0("<b:Columns>", columns, "</b:Columns><b:QueryResults>",
           rows, "</b:QueryResults>")
  )
}

survstat_api_transport <- function(
    olap = survstat_olap_response(), data_status = "2026-08-30T12:33:59+02:00",
    pathogens = c("Influenza, saisonal", "COVID-19"),
    states = "Berlin") {
  force(olap)
  function(operation, envelope) {
    body <- switch(operation,
      GetCubeInfo = soap_response(
        "GetCubeInfo", paste0("<b:LastDataUpdate>", data_status,
                              "</b:LastDataUpdate>")
      ),
      GetAllHierarchyMembers = if (grepl("FedStateKey71", envelope, fixed = TRUE)) {
        survstat_member_response(states, "[State]")
      } else {
        survstat_member_response(pathogens, "[Disease]")
      },
      GetOlapData = olap,
      stop("unexpected operation")
    )
    list(status_code = 200L, body = body)
  }
}

with_survstat_api_transport <- function(transport, code) {
  namespace <- asNamespace("regionalepi")
  old <- get(".survstat_api_transport", envir = namespace)
  unlockBinding(".survstat_api_transport", namespace)
  assign(".survstat_api_transport", transport, envir = namespace)
  lockBinding(".survstat_api_transport", namespace)
  on.exit({
    unlockBinding(".survstat_api_transport", namespace)
    assign(".survstat_api_transport", old, envir = namespace)
    lockBinding(".survstat_api_transport", namespace)
  }, add = TRUE)
  force(code)
}
