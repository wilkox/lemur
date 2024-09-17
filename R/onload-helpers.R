#' Set the vendor
#'
set_vendor <- function() {
  vendor <- Sys.getenv("LEMUR_VENDOR")
  if (vendor == "") {
    vendor <- "OpenAI"
    msg <- paste0(
      cli::col_yellow("!"),
      " Cannot find environmental variable `LEMUR_VENDOR`, setting default vendor"
    )
    rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
  }
  Sys.setenv(LEMUR_VENDOR = vendor)
  msg <- paste0(
    cli::col_green("{cli::symbol$tick}"),
    " Vendor set to: ",
    cli::col_blue(vendor)
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

#' Get the vendor
get_vendor <- function() Sys.getenv("LEMUR_VENDOR")

#' Set the model
#'
#' @export
set_model <- function() {
  model <- Sys.getenv("LEMUR_MODEL")
  if (model == "") {
    vendor <- get_vendor()
    msg <- paste0(
      cli::col_yellow("!"),
      " Cannot find environmental variable `LEMUR_MODEL`, setting default model"
    )
    rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
    if (vendor == "OpenAI") {
      model <- Sys.getenv("OPENAI_MODEL")
      if (model == "") {
        model <- "gpt-4o"
      } else {
        msg <- paste0(
          cli::col_green("{cli::symbol$tick}"),
          " Using model from legacy `OPENAI_MODEL` environmental variable"
        )
        rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
      } 
    } else {
      cli::cli_abort("No default model for vendor {.var {vendor}}")
    }
  }
  Sys.setenv(LEMUR_MODEL = model)
  msg <- paste0(
    cli::col_green("{cli::symbol$tick}"),
    " Model set to: ",
    cli::col_blue(model)
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

#' Get the model
get_model <- function() Sys.getenv("LEMUR_MODEL")
