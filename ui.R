# Shiny UI file

#### App Header ####
header =  dashboardHeader(
  title = span(tags$a(href = 'http://ajaykulkarni.in/',        # <- link for the text in the header
                      "Ajay Kulkarni",            
                      width = 180))
)

#### App Sidebar ####

# Inserting sidebar and menus in the sidebar
sidebar = dashboardSidebar(
  sidebarMenu(
    sidebarMenu(id = "sidebarmenu"),       # <- id is used for a shiny input value and
                                           #    will report which tab is selected
    
# Inserting dropdown menu in sidebar for selecting
# Boxplot or Scatterplot
    convertMenuItem(menuItem("Dashboard",                             # <- text to show for the menu item
                            tabName = "t1",                           # <- name of the first tab 
                             icon = icon("dashboard"), 
                             selected = TRUE, uiOutput("slider1"),
                     radioButtons("rad",                              # <- id for the radiobutton menu
                                  "Plot Selection",                   # <- the name of the tab 
                                   c("Scatterplot" = "sp",            # <- list of values to select from
                                    "Boxplot" = "bp"))),
                    "t1"),

# Inserting dropdown menu in sidebar for selecting
# Population density, Percentage proficiency and Median income (tab 2)  
    convertMenuItem(menuItem("Full map",
                             tabName = "t2",                          # <- name of the second tab 
                             icon = icon("map"),                      # <- An icon tag for map
                              selected = FALSE, uiOutput("slider2"),
                              selectInput("var1", "Select map",
                                          choices = c("Population Density" = 1, 
                                                      "Percentage Proficiency" = 2, 
                                                      "Median Income" = 3), 
                                          multiple = FALSE, 
                                          selectize = TRUE,
                                          width = '98%')),
                    "t2")
)
)
      
#### App Body ####

body = dashboardBody(

# We are using two tabs on our sidebar so to enclose two tabs
# we need to use tabItems function
  tabItems(
# First tab    
    tabItem(tabName = "t1",                           # <- the name of a tab
# Adding box to hold content in the main body of dashboard
            fluidRow(
            # Plot 1 - Row 1
              box(title = "Interactive Boxplot/Scatterplot",
                  width = 12,
                  height = "100%",
                  solidHeader = FALSE,
                  collapsible  = TRUE,
                  background = "green",
                  plotlyOutput("plot1")               # <- display the plotly output
            ),
            # Plot 2 - Row 2
            box(title = "Statewide percentage proficiency in Math and Median income (2015)",
                width = 12,
                height = "100%",
                solidHeader = FALSE,
                collapsible  = TRUE,
                background = "black",
                highchartOutput("plot2")              # <- display the highchart output
            )
            )
    ),

# Second tab      
    tabItem(
      tabName = "t2",
      fluidRow(
        box(title = "Full Map",
            width = 12,
            height = "600px",
            solidHeader = FALSE,
            collapsible  = TRUE,
            background = "yellow",
            leafletOutput("mymap1", height = 520)  # <- display the leaflet output and set height for the map
        )
    )
  )
)
)

#### App Page ####

ui = fluidPage(dashboardPage(
  skin = "black",       # <- app skin/themes
  header,               # <- app header
  sidebar,              # <- app sidebar
  body                  # <- app body
  )
)