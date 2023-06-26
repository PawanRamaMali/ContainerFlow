box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, fluidRow,
        fluidPage, column, br, textInput, actionLink ],
  shiny.blueprint[Navbar, NavbarGroup, NavbarHeading, Button,
                  Card, Select.shinyInput, H4, H5, NumericInput.shinyInput, ],
  httr[GET, add_headers, http_type, content, ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    Navbar(
      NavbarGroup(
        NavbarHeading("ContainerFlow")
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
        uiOutput(ns("images_dropdown"))),
      br(),
      Card(
        interactive = TRUE,
        H4("Create Container"),
        textInput(inputId = ns("container_name"),
                  label = "Container Name", value = ""),
        H5("Container Port (optional)"),
        NumericInput.shinyInput(
          inputId = ns("container_port"),
          intent = "primary",
          value = 8081
        ),
        actionLink(inputId = ns("start_container_btn"),
                   "Start Container"),
        actionLink(inputId = ns("stop_container_btn"),
                   "Stop Container"))
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
      tryCatch({
        url <- "http://localhost:2375/v1.41/images/json"
        response <- GET(url, add_headers(headers))
        images <- unlist(process_response(response)$RepoTags)
        Select.shinyInput(
          inputId = ns("select_images"),
          items = images,
          selected = images[1],
          noResults = "Not available"
        )
      }, error = function(e) {
        # Error handling code
        div(class = "error", "Unable to connect with Docker API")
      })
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
