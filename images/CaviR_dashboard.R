# packages
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "CaviR statistics",
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "New update",
                 icon("users"))
  )),
  
  dashboardSidebar(
    width = 250,
    
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    
    sidebarMenu(id = "sidebar",
        menuItem("Dataset", tabName = "Correlations", icon = icon("chart-line"),
                         badgeLabel = "new", badgeColor = "green")  ,         
        menuItem("Correlations", tabName = "Correlations", icon = icon("chart-line"),
             badgeLabel = "new", badgeColor = "green")
   # menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),
  
  dashboardBody(
    
    tags$head(tags$style(HTML('/* logo */
                                .skin-blue .main-header .logo {
                                  background-color: #1b2633;
                                  font-family: "Helvetica";
                                  font-weight: bolder;
                                  font-size: 24px;
                                  color: #ffffff;
                                  font-variant: Small-Caps;
                                }
                              
                              /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #1b2633;
                                }
                              
                              /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                  background-color: #1b2633;
                                }        
                              
                              /* main sidebar */
                                .skin-blue .main-sidebar {
                                  background-color: #1b2633;
                                }
                              
                              /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                  background-color: #1b2633;
                                  font-family: "Helvetica";
                                  font-weight: normal;
                                  font-size: 20px;
                                  color: #ffffff;
                                  font-variant: Small-Caps;
                                }
                              
                              /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                  background-color: #1b2633;
                               
                                    
                                }
                              
                              /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                  background-color: #1b2633;
                                }
                              /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                  background-color: #1b2633;
                                }
                              '))),


    tabItems(
      tabItem(tabName = "Dataset",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      # First tab content
      tabItem(tabName = "Correlations",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      )
      
      # # Second tab content
      # tabItem(tabName = "widgets",
      #         h2("Widgets tab content")
      # )
     
    )
  )
  
)

server <- function(input, output) { }

shinyApp(ui, server)