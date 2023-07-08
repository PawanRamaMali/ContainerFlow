# ContainerFlow
Manage containers using ContainerFlow App

ContainerFlow is a R Shiny web application built using Rhino framework that allows you to manage Docker containers. With this app, you can perform various actions such as creating containers, starting, stopping, and deleting them. You can also view container logs and manage container images.


## Dependencies

This app requires the following R packages:

- shiny
- shiny.blueprint
- shiny.fluent
- httr
- jsonlite
 
Please make sure these packages are installed before running the app.

## App Features

- Select Image
  Use the dropdown menu to select an image from the available Docker images.
- Create Container
  Enter a name for the container and specify a port number.
  Click the "Create Container" button to create the container using the selected image.
- Manage Container
  Use the container list dropdown to select a container.
  Click the "Start Container" button to start the selected container.
  Click the "Stop Container" button to stop the selected container.
  Click the "Delete Container" button to delete the selected container.
- View Logs
  Click the "View Logs" button to toggle the visibility of the container logs.

## Troubleshooting
If you encounter any errors related to connecting with the Docker API, please ensure that the Docker API is running and accessible.
Check the R package dependencies and make sure they are installed correctly.

## Disclaimer
The ContainerFlow app is provided as-is without any guarantees or warranties. Use it at your own risk. The app is intended for educational purposes and should be used responsibly.
