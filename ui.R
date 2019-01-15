library(shiny)
library(plotly)

shinyUI(dashboardPage(
  #Define the app header where will be the name and logo of the application
  dashboardHeader(
    disable = FALSE,
    title = htmltools::span(img(src='app_logo.png', width=25), "PoGoDex")
  ),
  dashboardSidebar(
    #here we defined the different tabs of our application
    sidebarMenu(
      #We have different tabs, with subtabs inside it.
      menuItem("Home",icon = icon("home"), tabName = 'home'),
      menuItem("Gyms",icon = icon("map-marked-alt"), startExpanded = TRUE,
               #inside section gyms, we have sub pages will detailed information
               menuSubItem("Raids Location", tabName = 'raids_location', icon = icon("map-marker-alt")),
               menuSubItem("Find Gym", tabName = 'find_gym', icon = icon("search")),
               menuSubItem("Statistics",tabName = 'raidsloc_stats', icon = icon("chart-bar"))
      ),
      menuItem("Players",icon = icon("user-alt"), startExpanded = TRUE,
               #inside section players, we have sub pages will detailed information
               menuSubItem("Database",tabName = "personal_info", icon = icon("user-circle")),
               menuSubItem("Infered Friendships", tabName = "network", icon = icon("user-friends"))
      ),
      menuItem("Teams",icon = icon("users"), startExpanded = TRUE,
               #inside section uers, we have sub pages will detailed information
               menuSubItem("Raid Participation", tabName = "team_stats", icon = icon("chart-bar")))
    )
  ),
  dashboardBody(
    # Include the custom styling
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    tags$head(
      #create the reference to the css file
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Home tab content
      tabItem(tabName = 'home',
              tags$div(id='outter',
                       tags$div(id='home_content',
                                tags$h1('PoGoDéx'),
                                tags$br(),
                                tags$p("Hey there gamer/pokemon fan/control freak who likes to keep track of all sorts of activities going around in town!
                                       
                                       Welcome to the PoGoDéx, the PokémonGo pokédex designed to keep track of all the PokémonGo players', teams' and raid activities!
                                       Not sure where to find the closest gyms? want to figure out when and where more raid activity occured? curious about 
                                       the social network that has emerged in this popular mobile game? Just in the mood to peep out all the different players in Lisbon and their stats?
                                       Well this is the app for you!"),
                                tags$img(id = 'pokedex', src='home_gif.gif'),
                                tags$p("So, what can you expect to find here?
                                       an iterative “Pokedéx” that, instead of containing information regarding Pokémons, has information
                                       about the PokémonGo mobile app players, teams and raids! Sounds fun doesn't it? 
                                       Think of this PoGoDéx as your new PokémonGo companion, and let it help you at every step of the way!")
                       )
              )
      ),
      #Network tab content
      tabItem(tabName = "network",
              visNetworkOutput("network", width = "100%",height=800),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            useShinyjs(),
                            h3('Select a player:'),
                            selectInput("username", "",
                                        as.list(players_info[,2]), selected = '', multiple=TRUE),
                            radioButtons("team_filter", label = h3("Select the team:"),
                                         choices = list("Red" = 'red', "Blue" = 'blue', "Yellow" = 'yellow', 'All' = 'NULL'), 
                                         selected = 'NULL'),
                            radioButtons("centrality_filter", label = h3("Select the centrality:"),
                                         choices = list("Betweennes" = 'between', "Closeness" = 'close', "Degree" = 'degree', 'None' = 'NULL'), 
                                         selected = 'NULL'),
                            div(
                              sliderInput("top_players", h3("Top players:"),
                                          min = 1, max = 10, value = 1
                              )
                            ),
                            hr(),
                            h4('Note!'),
                            p(strong('Betweenness Centrality: '), 'the most influent person is the one who mediates the most relationships between pairs of people'),
                            p(strong('Closeness Centrality: '), 'the most influent person is viewed as the one who is, in average, closest to the rest'),
                            p(strong('Degree Centrality: '), 'the most influent person is the one with more “ties”, that is, the person with most connections in the network')                  )
              
      ),
      #Raids location tab content
      tabItem(tabName = "raids_location",
              leafletOutput('mdynamic',height=800),
              absolutePanel(id = "search_settings", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",p("In this tab you can explore raid occurrences over the past year. By clicking on the heatmap checkbox you can turn heatmapping on and off. You can also see raids that occurred during a specific day, during a range of days or during a range of months by tickering with the options bellow! Clicking on the markers will show you how many raids happened in a specific location and how many total players went to thoose raids."),br(),checkboxInput('group','Heatmap',value = FALSE), selectInput('time', 'Timescale', choices = c('Day','Days(Range)','Months(Range)'), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL),
                            uiOutput('timeSel'), 
                            uiOutput('slider')
              )
              
              
      ),
      #Raids stats tab content
      tabItem(tabName = "raidsloc_stats",
              fluidPage(h3("Raid Frequency"),uiOutput('text'), plotlyOutput("lineplot", width = "65%"),
                        absolutePanel(id = "search_settings", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",selectInput('t2', 'Timescale', choices = c('Per Hour','Per Day','Per Month'), selected = 'Per Hour', multiple = FALSE,
                                                                               selectize = TRUE, width = NULL, size = NULL)
                        ),height="100%")
              
              
      ),
      
      #Find gym tab content
      tabItem(tabName = "find_gym",
              leafletOutput('search_map',height=800),
              absolutePanel(id = "search_settings", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            textInput("place_search", "", "", placeholder = 'Insert a location'),
                            sliderInput("search_radius", h6("Choose a search radius(meters):"),
                                        min = 250, max = 2500, value = 250, step = 250
                            )
              )
              
      ),
      #Personal Info tab content
      tabItem(tabName = "personal_info",
              fluidPage(dataTableOutput('info',width = "65%"),left=20,top=60),
              absolutePanel(id = "search_settings", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            selectInput("players", h3("Find player:"),as.list(players_info$username),selected='', multiple=TRUE),
                            hr(),
                            radioButtons(
                              inputId = "team",
                              label = h3("Choose team:"),
                              choiceNames = list(
                                HTML("<font color='blue'>Blue</font>"), 
                                tags$span(style = "color:red", "Red"), 
                                HTML("<font color='orange'>Yellow</font>"), 
                                "All"
                              ),
                              choiceValues = c("blue", "red", "yellow","all"),
                              selected='all'
                            ),
                            hr(),
                            #Decidir se metemos ou nao o step!
                            #ver depois as cores...
                            tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                      background: #000000;
                                                      border-top: 1px solid #808080 ;
                                                      border-bottom: 1px solid #808080 ;}
                                                      .irs-from, .irs-to, .irs-single { background: #000000 }'))),
                            sliderInput("slider2", label = h3("Level"), min = 0, 
                                        max = 40, value = c(0, 40),step = 1)
              )
              
      ),
      #Team Stats tab, that plots a waffle
      tabItem(tabName = "team_stats",
              plotOutput("waf",width="65%"),
              #Full page
              absolutePanel(id = "search_settings", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",p("Here you can see which team is more active during a chosen period! Check out the % of players from each team given a time window of your choice. You can choose between seeing the stats of just registered players or include the guests they may have brought along with them!"),
                            br(),
                            #By default we have the whole time. By using min and max we are forcing
                            #people to select only between those dates. Otherwise it would give error
                            dateRangeInput("dates",
                                           "Date range:",
                                           start = "2018-01-06",
                                           end = "2018-12-27",
                                           min = "2018-01-06",
                                           max = "2018-12-27"),
                            #Buttons to decide if they want all players or just the registered ones.
                            #By default, all players are being shown
                            radioButtons("num_filter", label = h4("What do you want to see:"),
                                         choices = list("Database Players + Unregistered Guests" = 'all', "Only Database Players" = 'only'), 
                                         selected = 'all')
              )
              
      )
    )
  ),
  title = "PokemonGo Pokedex"
))