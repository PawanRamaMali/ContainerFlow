box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, fluidRow,
        fluidPage, column, br, textInput, actionLink, observeEvent, tagList,
        reactiveVal, req, renderPrint, verbatimTextOutput, ],
  shiny.blueprint[Navbar, NavbarGroup, NavbarHeading, Button, Pre, Collapse,
                  Card, Select.shinyInput, H4, H5, renderReact, Button.shinyInput,
                  reactOutput, ],
  shiny.fluent[DetailsList, Stack, DefaultButton.shinyInput,
               TextField.shinyInput, ],
  httr[GET, POST, add_headers, http_type, content, ],
  jsonlite[toJSON, ],
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
             column(8,
      br(),
      Card(
        interactive = TRUE,
        H5("Select Image"),
        uiOutput(ns("images_dropdown"))),
      br(),
      Card(
        interactive = TRUE,
        H5("Create Container"),
        Stack(
          TextField.shinyInput(inputId = ns("container_name"),
                    label = "Container Name", value = "alpha_test"),
          TextField.shinyInput(inputId = ns("container_port"),
                               label = "Port Number", value = "8081"),
          horizontal = TRUE,
          tokens = list(childrenGap = 20)
        ),
        br(),
        DefaultButton.shinyInput(inputId = ns("create_container_btn"),
                                 text = "Create Container")),
      br(),
      Card(
        interactive = TRUE,
        H5("Manage Container"),
        uiOutput(ns("container_list")),
        br(),
        Stack(
          DefaultButton.shinyInput(inputId = ns("start_container_btn"),
                                   text = "Start Container"),
          DefaultButton.shinyInput(inputId = ns("stop_container_btn"),
                                   text = "Stop Container"),
          DefaultButton.shinyInput(inputId = ns("delete_container_btn"),
                                   text = "Delete Container"),
          horizontal = TRUE,
          tokens = list(childrenGap = 20)
        )),
      br(),
      Card(
        interactive = TRUE,
        tagList(
          Button.shinyInput(ns("logs"), "View logs"),
          reactOutput(ns("logs_ui")),
          br(),
          verbatimTextOutput("response")
        ),
        )
      )
    ))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    base_url <- "http://localhost:2375/v1.41/containers/"
    headers <- c("Content-Type" = "application/json")
    logs <- Pre(
      "[11:53:30] Finished 'typescript-bundle-blueprint' after 769 ms\n",
      "[11:53:30] Starting 'typescript-typings-blueprint'...\n",
      "[11:53:30] Finished 'typescript-typings-blueprint' after 198 ms\n",
      "[11:53:30] write ./blueprint.css\n",
      "[11:53:30] Finished 'sass-compile-blueprint' after 2.84 s\n"
    )
    show <- reactiveVal(FALSE)
    observeEvent(input$logs, show(!show()))
    output$logs_ui <- renderReact({
      Collapse(isOpen = show(), logs)
    })
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
    output$container_list <- renderUI({
      tryCatch({
        url <- "http://localhost:2375/v1.41/containers/json"
        response <- GET(url, add_headers(headers))
        containers <- unlist(process_response(response)$Names)
        Select.shinyInput(
          inputId = ns("select_containers"),
          items = containers,
          selected = containers[1],
          noResults = "Not available"
        )
      }, error = function(e) {
        # Error handling code
        div(class = "error", "Unable to connect with Docker API")
      })
    })
    # Create Container
    observeEvent(input$create_container_btn, {
      req(input$container_name)
      print(paste("Creating container:", input$container_name))
      url <- "http://localhost:2375/v1.41/containers/create"
      params <- list(name = input$container_name)
      body <- paste0('{
        "Hostname": "localhost",
        "ExposedPorts": {
          "3838/tcp": {}
        },
        "HostConfig": {
          "PortBindings": {
            "3838/tcp": [
              {
                "HostPort": "' , as.numeric(input$container_port), '"
              }
            ]
          }
        },
        "Image": "', as.character(input$select_images), '"
      }')
      response <- POST(url, query = params, add_headers(headers), body = body)
      output$response <- renderPrint(content(response, "text"))
    })
    # Start Container
    observeEvent(input$start_container_btn, {
      req(input$container_name)
      print(paste("Starting container:", input$container_name))
      url <- paste0("http://localhost:2375/v1.41/containers/", input$container_name, "/start")
      response <- POST(url, add_headers(headers), body = NULL)
      output$response <- renderPrint(content(response, "text"))
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
