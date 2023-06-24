box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, fluidRow,
        fluidPage, column, br, ],
  shiny.blueprint[Navbar, NavbarGroup, NavbarHeading, Button,
                  Card, Select.shinyInput, H4, ],
  httr[GET, add_headers, http_type, content, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    Navbar(
      NavbarGroup(
        NavbarHeading("App Controller")
      ),
      NavbarGroup(
        align = "right",
        Button(minimal = TRUE, icon = "user"),
        Button(minimal = TRUE, icon = "refresh")
      )
    ),
    fluidPage(
    fluidRow(column(2, ),
             column(6,
      br(),
      Card(
        interactive = TRUE,
        H4("Select Image"),
        uiOutput(ns("images_dropdown")))
      )
    ))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    headers <- c("Content-Type" = "application/json")
    output$images_dropdown <- renderUI({
      url <- "http://localhost:2375/v1.41/images/json"
      response <- GET(url, add_headers(headers))
      images <- unlist(process_response(response)$RepoTags)
      Select.shinyInput(
        inputId = ns("select_images"),
        items = images,
        selected = images[1],
        noResults = "Not available"
      )
    })

    # Function to handle API call errors
    handle_api_error <- function(response) {
      if (inherits(response, "response")) {
        if (status_code(response) == 0) {
          return("Error: Failed to connect to Docker API.
                 Make sure the Docker API is running.")
        } else {
          return(content(response, "text"))
        }
      } else {
        return("An error occurred while making the API call.")
      }
    }
    process_response <- function(response) {
      if (is.null(response)) {
        return(handle_api_error(response))
      } else {
        if (http_type(response) != "application/json") {
          return(handle_api_error(response))
        } else {
          json_data <- content(response, "text")
          return(jsonlite::fromJSON(json_data))
        }
      }
    }
  })
}
