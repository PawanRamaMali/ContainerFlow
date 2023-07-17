box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, fluidRow,
        fluidPage, column, br, textInput, actionLink, observeEvent, tagList,
        reactiveVal, req, renderPrint, verbatimTextOutput, renderText, ],
  shiny.blueprint[Navbar, NavbarGroup, NavbarHeading, Button, Pre, Collapse,
                  Card, Select.shinyInput, H4, H5, renderReact, Button.shinyInput,
                  reactOutput, Toaster, ],
  shiny.fluent[DetailsList, Stack, DefaultButton.shinyInput, Link,
               TextField.shinyInput, Dropdown.shinyInput, ],
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
        #Button(minimal = TRUE, icon = "user"),
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
        H5("Deploy Container"),
        Stack(
          TextField.shinyInput(inputId = ns("user_name"),
                               label = "User Name", value = "Name"),
          br(),
          DefaultButton.shinyInput(inputId = ns("deploy_container_btn"),
                                   text = "Deploy Container"),
          horizontal = TRUE,
          tokens = list(childrenGap = 20)
        ),
        br(),
        uiOutput(ns("deployed_info")),
        br()
      ),
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
          uiOutput("message")
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
    toasterTop <- Toaster$new(position = "top")
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
        options <- lapply(unique(images), function(item) {
          list(key = item, text = item)
        })
        Dropdown.shinyInput(ns("select_images"),
                            options = options,
                            value = options[[1]]$key)
      }, error = function(e) {
        # Error handling code
        div(class = "error", "Unable to connect with Docker API")
      })
    })

    
    # Global variable to hold the sequence of port numbers
    available_ports <- 8080:8083
    
    # Function to get and remove the next available port number
    get_port_number <- function() {
      if (length(available_ports) > 0) {
        port <- available_ports[1]
        # Update the global variable by removing the used port number
        available_ports <<- available_ports[-1]
        return(port)
      } else {
        return(NULL)
      }
    }
    
    get_container_name <- function(input_string) {
      # Remove spaces from the input_string
      name_without_spaces <- gsub(" ", "_", input_string)
      # Replace invalid characters (except letters, digits, and underscores) with underscores
      clean_name <- gsub("[^[:alnum:]_]", "_", name_without_spaces)
      # Add timestamp to the name
      timestamp <- gsub("[-: ]", "_", Sys.time())
      # Combine the cleaned name and timestamp
      container_name <- paste0(clean_name, "_", timestamp)
      # Ensure that the name starts with a letter
      if (!grepl("^[A-Za-z]", container_name)) {
        container_name <- paste0("A_", container_name)
      }

      return(container_name)
    }
    
    observeEvent(input$deploy_container_btn, {
      req(input$user_name)
      print("Checking for available ports . . .")
      available_port <- get_port_number()
      container_name <- get_container_name(input$user_name)
      
      
      if (!is.null(available_port)) {
        message("Port ", available_port, " is available.")
      } else {
        message("No available port found in the list.")
        toasterTop$show(message = "Failed to deploy Container !", intent = "danger")
        output$deployed_info <- renderUI({
          tags$div("No available port found to deploy. Contact R&D")
        })
        return(NULL)
      }
      # Creating the container
      print(paste("Creating container:", container_name, "on port ",available_port))
      url <- "http://localhost:2375/v1.41/containers/create"
      params <- list(name = container_name)
      body <- paste0('{
        "Hostname": "localhost",
        "ExposedPorts": {
          "3838/tcp": {}
        },
        "HostConfig": {
          "PortBindings": {
            "3838/tcp": [
              {
                "HostPort": "' , as.numeric(available_port), '"
              }
            ]
          }
        },
        "Image": "', as.character(input$select_images), '"
      }')
      response <<- POST(url, query = params, add_headers(headers), body = body)
      output$message <- renderUI(
        p(content(response))
      )
      
      # Deploy the container
      print(paste("Deploying container:", container_name, "on port ",available_port))
      
      
      toasterTop$show(message = "Starting Container !", intent = "primary")
      url <- paste0("http://localhost:2375/v1.41/containers/", container_name, "/start")
      response <- POST(url, add_headers(headers), body = NULL)
      output$message <- renderUI({
        h5(content(response))
      }
      )
      
      # Show the deployed URL
      print(paste("Deployed :", container_name, "on port ",available_port))
      toasterTop$show(message = "Deployed Container !", intent = "success")
        output$deployed_info <- renderUI({
          Link(target = "_blank", href = paste0("http://localhost:",available_port), paste0("Deployed URL: http://localhost:",available_port))
        })
    })
    
    output$container_list <- renderUI({
      tryCatch({
        url <- "http://localhost:2375/v1.41/containers/json"
        response <- GET(url, add_headers(headers))
        containers <- unlist(process_response(response)$Names)
        options <- lapply(unique(containers), function(item) {
          list(key = item, text = item)
        })
        Dropdown.shinyInput(ns("select_containers"),
                            options = options,
                            value = options[[1]]$key)
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
      response <<- POST(url, query = params, add_headers(headers), body = body)
      output$message <- renderUI(
        p(content(response))
        )
    })
    # Start Container
    observeEvent(input$start_container_btn, {
      req(input$container_name)
      print(paste("Starting container:", input$container_name))
      toasterTop$show(message = "Starting Container !", intent = "primary")
      url <- paste0("http://localhost:2375/v1.41/containers/", input$container_name, "/start")
      response <- POST(url, add_headers(headers), body = NULL)
      output$message <- renderUI({
        h5(content(response))
      }
      )
    })
    # Stop Container
    observeEvent(input$stop_container_btn, {
      req(input$container_name)
      print(paste("Stopping container:", input$container_name))
      url <- paste0("http://localhost:2375/v1.41/containers/", input$container_name, "/stop")
      response <- POST(url, add_headers(headers), body = NULL)
      output$message <- renderUI(
        p(content(response))
      )
    })
    # Kill Container
    observeEvent(input$stop_container_btn, {
      req(input$container_name)
      print(paste("Killing container:", input$container_name))
      url <- paste0("http://localhost:2375/v1.41/containers/", input$container_name, "/kill")
      response <- POST(url, add_headers(headers), body = NULL)
      output$message <- renderUI(
        p(content(response))
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
