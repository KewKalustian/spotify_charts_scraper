# NOT RUN w/out credentials:

# v0.2.6
# stability and performance improvements (see spotifyR_scrapeR(), 
# sprintf() vs. paste0(), full_join vs. merge()), and other enhancements
# (HTML/CSS).

library(shiny)
library(shinydashboard)   
library(shinydashboardPlus)
library(shinyauthr)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(RSQLite)
library(DBI)
library(sodium)
library(htmlwidgets)
library(tidyverse)
library(rvest)
library(lubridate)
library(magrittr)
library(spotifyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(GGally)
library(countrycode)


###########################
### L O G I N / P R E P ###
###########################

# How many days should sessions last?

cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid. 

get_sessions_from_db <-
  function(conn = db, expiry = cookie_expiry) {
    dbReadTable(conn, "sessions") %>%
      mutate(login_time = ymd_hms(login_time)) %>%
      as_tibble() %>%
      filter(login_time > now() - days(expiry))
  }

# This function must accept two parameters: user and sessionid. 
# It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user,
         sessionid = sessionid,
         login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessions", ., append = T)
}

db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db,
              "sessions",
              c(
                user = "TEXT",
                sessionid = "TEXT",
                login_time = "TEXT"
              ))

#############################
### for server deployment ###
#############################

user_base <- read_csv("user_base.csv")

#####################
####### U I #########
#####################


Imp <- function() {
  fluidPage(
    br(),
    hr(),
    a(
      href = "https://twitter.com/KewKalustian",
      h6(strong("Developed and built by Kework K. Kalustian")),
      target = "_blank",
      style = "display: block; color: white"
    ),
    br(),
    br(),
    strong("Powered by"),
    br(),
    br(),
    a(
      href = c("https://developer.spotify.com/"),
      img(src = "https://raw.githubusercontent.com/KewKalustian/spotify_charts_scraper/master/img/Spotify_Logo_CMYK_White.png",
          width = "50%"),
      target = "_blank"
    ),
    hr(),
    a(
      href = c("http://music-psychology.org/index.html"),
      img(
        src = "https://raw.githubusercontent.com/KewKalustian/spotify_charts_scraper/master/img/dgmwhite.png",
        width = 84,
        height = 42,
        style = "object-position: center"
      ),
      br(),
      h6(strong("German Society for Music Psychology")),
      style = "display: block; color: white",
      target = "_blank"
    ),
    hr(),
    a(
      href = c("https://www.aesthetics.mpg.de/en.html"),
      img(src = "https://raw.githubusercontent.com/KewKalustian/spotify_charts_scraper/master/img/190814-MPIEA-Logo-E-XS-L-WHITE-digital.png",
          width = "75%"),
      target = "_blank"
    )
  )
}

# H E A D E R

header <- dashboardHeader(
  title = "Spotify Charts Scraper",
  titleWidth = 300,
  tags$li(
    class = "dropdown",
    a(
      icon("github"),
      href = "https://github.com/KewKalustian/spotify_charts_scraper",
      title = "Browse Source Code on GitHub",
      target = "_blank",
      style = "font-size: 25px; display: contents"
    )
  ),
  
  tags$li(
    class = "dropdown",
    style = "padding: 8px",
    shinyauthr::logoutUI("logout")
  )
)


sidebar <- dashboardSidebar(
  width = 290,
  sidebarMenu(
    actionButton(
      "about",
      label = div("About", icon("info-circle")),
      width = "270px",
      style = "text-align:center;
      color: white; background-color: #283747; display: contents;
      font-size: 21px;
      font-weight: bold; "
    ),
    
    # Removing the sidebar toggle element
    tags$script(
      JS(
        "document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"
      )
    ),
    
    br(),
    br(),
    br(),
    
    selectInput(
      "country",
      "1. Choose a Country (Region)",
      list(
        "Global",
        "Argentina"           ,
        "Australia"           ,
        "Austria"             ,
        "Belgium"             ,
        "Bulgaria"            , 
        "Bolivia"             ,
        "Brazil"              , 
        "Canada"              ,
        "Chile"               , 
        "Colombia"            ,
        "Costa Rica"          ,
        "Czech Republic"      ,
        "Denmark"             ,
        "Dominican Republic"  ,
        "Ecuador"             ,
        "Egypt"               ,
        "El Salvador"         ,
        "Estonia"             ,
        "Finland"             ,
        "France"              ,
        "Germany"             ,
        "Greece"              ,
        "Guatemala"           ,
        "Hong Kong"           ,
        "Honduras"            ,
        "Hungary"             ,
        "Iceland"             ,
        "India"               ,
        "Indonesia"           ,
        "Ireland"             ,
        "Israel"              ,
        "Italy"               ,
        "Japan"               ,
        "Korea, Republic of"  ,
        "Latvia"              ,
        "Lithuania"           ,
        "Luxembourg"          ,
        "Malaysia"            ,
        "Mexico"              ,
        "Morocco"             ,
        "Netherlands"         ,
        "New Zealand"         ,
        "Nicaragua"           ,
        "Norway"              ,
        "Panama"              ,
        "Paraguay"            ,
        "Peru"                ,
        "Philippines"         ,
        "Poland"              ,
        "Portugal"            ,
        "Romania"             ,
        "Russian Federation"  ,
        "Saudi Arabia"        ,
        "Singapore"           ,
        "Slovakia"            ,
        "South Africa"        ,
        "Spain"               ,
        "Sweden"              ,
        "Switzerland"         ,
        "Taiwan"              ,
        "Thailand"            ,
        "Turkey"              ,
        "Ukraine"             ,
        "United Arab Emirates",
        "United Kingdom"      ,
        "United States"       ,
        "Uruguay"             ,
        "Viet Nam"           
      ),
      width = "100%"
    ),
    br(),
    br(),
    tags$style(
      HTML(
        ".shiny-notification {position:fixed; top: 35%; left: 283px; right: 0px;
         width: 270px; background-color: #283747; color: white}"
      )
    ),
    dateRangeInput(
      inputId = "Dates",
      label =  p(
        tags$style(HTML(
          "#date_help {display:contents; color: white }"
        )),
        "2. Choose a Date Range",
        actionButton(
          'date_help',
          icon = icon("info-circle"),
          label = ""
        )
      ),
      min = "2017-01-01",
      max = Sys.Date() - 2,
      start = Sys.Date() - 3,
      end =  Sys.Date() - 2,
      weekstart = 1
    ),
    br(),
    br(),
    
    actionButton(
      "go",
      label = div("Scrape!", icon("play-circle")),
      width = 270,
      style = "color: white; font-size: 32px; font-weight: bold; text-align:
               center; background-color: #283747;
               display: contents;"
    ),
    
    Imp()
  )
)

# B O D Y
body <-  dashboardBody(
  tags$head(tags$style(
    HTML(
      "
            /* logo */
           .main-header .logo {font-weight: bold;
           font-size: 22px}
           .skin-blue .main-header .logo {
           background-color:#283747}
           
           /* logo when hovered */
           .skin-blue .main-header .logo:hover {
           background-color: #283747}
           
           /* rest */
           .skin-blue .main-header .navbar-static-top {
           background-color: #283747}
           
           /* main sidebar */
           .skin-blue .main-sidebar {
           background-color: #283747}
           
           /* links + when links hovered*/
           a {color: #F47920}
           a:hover {color: #283747 }
           
           /* buttons/icons + when buttons/icons hovered */
           .fa-download:hover,
           .fa-play-circle:hover ,
           .fa-info-circle:hover ,
           .fa-github:hover {color: #F47920}
  
           #login-button {background-color: #283747; border: none}
           #login-button:hover {background-color: #F47920}
           #logout-button {background-color: #8b0000; border: none}
           #logout-button:hover {background-color:#6f0000}
           
           /* github icon position */
           .fa-github {padding-top: 12px; padding-bottom: 10px;
           padding-right:10px}
           
           /* login panel / body */
           #rmve .with-border {display:none}
           #rmve .box-body  {background-color: #283747}
           h6:nth-child(1):hover, br+ h6 strong:hover  {color: #F47920}
          
      "
    )
  )),
  
  box(
    collapsible = T,
    closable = T,
    id = "rmve",
    width = 12,
    shinyauthr::loginUI(
      "login",
      cookie_expiry = cookie_expiry,
      additional_ui = tagList(
        tags$p(
          br(),
          "Please contact",
          a("Kework K. Kalustian", 
            href = "mailto:kework.kalustian@ae.mpg.de?subject=Credentials Request | Spotify Charts Scraper"),
          "if you do not have already your credentials, forgot them, or want to de-register with this app.",
          class = "text-justify"
        )
      )
    )
  ),
  
  
  box(
    id = "table", width = 12, 
    title =  p(
      tags$style(
        HTML(
          type = "text/css",
          "#help {display: contents; color: #283747} #downloadData
          {background-color: white; color: #283747; border-style: none;
          position: absolute; right:0px; top:1px; padding:4px}"
        )
      ),
      "Daily Top 200 Spotify Charts",
      actionButton('help', icon("info-circle"), label = "")
    ),
    downloadButton(
      'downloadData',
      label = div("Download", icon("arrow-alt-circle")),
      width = "10%"
    ),
    
    dataTableOutput("table")
  ),
  
  
  box(
    id = "histo",
    title = "Histogram",
    collapsible = T,
    collapsed = T,
    width = 5,
    solidHeader = T,
    plotOutput("plot", height ="680px")
  ),
  
  box(
    id = "matrix",
    title = "Correlation Matrix (Spearman’s ρ) of Mood-Related Audio Features Against Stream Counts",
    collapsible = T,
    collapsed = T,
    width = 7,
    solidHeader = T,
    withSpinner(
      color = "#283747",
      type = 8,
      size = 0.66,
      plotOutput("plot2",  height="680px")
    )
  ))
  
ui <- dashboardPage(header, sidebar, body)


#####################
#### S E R V E R ####
#####################


server <- function(input, output, session) {
  
  ###################
  #### L O G I N ####
  ###################
  
  
  # call login module supplying data frame, user and password cols and 
  # reactive trigger
  credentials <- shinyauthr::loginServer(
    
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = T,
    reload_on_logout = T,
    cookie_logins = T,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))
  
  
  #################
  #### I N F O ####
  #################
  
  observeEvent(once = T, session, {
    showModal(modalDialog(  
      title = p(
        style = "text-align: center; font-size: 42px; font-weight:bold",
        "Spotify Charts Scraper",
        br(),
        p(style = "font-weight:normal; font-size: 21px; text-align: center",
          "v0.2.6")
      ),
      h2('What is the Spotify Charts Scraper?'),
      
      style = "text-align: justify; font-size: 14pt;",
      p("The",
        strong("Spotify Charts Scraper"),
        "is a",
        a(href = "https://shiny.rstudio.com/", "Shiny application", 
          target = "_blank"),
        "to retrieve daily",
        a(href="https://spotifycharts.com/regional", "Spotify charts", 
          target ="_blank"),
        em(strong("with")),
        a(href = "https://developer.spotify.com/discover/", "audio features", 
          target="_blank"),
        "(such as danceability, valence, energy, tempo, mode, key/pitch,
 and many more) in real-time—for each song, of each country, and of each date 
 that is currently available on Spotify. This app is developed especially for 
 users who want to gain insights into everyday music listening or want to build 
 an individual database or large datasets for research in music psychology or 
 computational musicology or even for cross-cultural research while benefiting 
        from the perks of Spotify’s",
        a(href = "https://developer.spotify.com/",
          "API", target = "_blank"),
        "without writing hundreds of code lines.
 Retrieved data can be downloaded as a CSV file and can easily be
 imported into any statistical computing software—e.g.,",
        a(href = "https://jasp-stats.org/about/",
          "JASP", target = "_blank"),
        "or",
        a(href = "https://www.ibm.com/products/spss-statistics", "SPSS.", 
          target = "_blank"),
        "Additionally, some basic data visualizations provide first insights
into the data structure. All in all, the",
        strong("Spotify Charts Scraper"),
        "makes music streaming data retrieval more accessible:
It’s easy to use, and it’s open-source. Feel free to browse the source code on",
        a(href = "https://github.com/KewKalustian/Shiny_Spotify_Charts_Scraper",
          "GitHub,", target = "_blank"),
        "where you can also open an issue or even do a pull request."),
      p(
        style = "text-align: justify; font-size: 14pt;",
        "Since this app relies on Spotify’s metadata, it is currently 
mandatory that users must be registered by the developer to receive their log-in 
credentials via e-mail. This is necessary in order to comply with the",
        a(
          href = "https://developer.spotify.com/policy/",
          "Spotify Developer Policy.",
          target = "_blank"
        ),
        "Those credentials are stored encrypted—a hashing algorithm protects 
from brute-force attacks. If users do not want to be registered anymore, they 
have to inform the developer accordingly as unambiguously as possible so they 
can be de-registered with this app. Their user data and their received 
credentials will then be deleted."
      ),
      p(
        style = "text-align: justify; font-size: 14pt;",
        "Once a bigger audience has been onboarded, it is intended to 
facilitate access to this app—this depends ultimately on Spotify’s approval."
      ),
      br(),
      tags$blockquote(
        p(
          "Special thanks to",
          a(href = "https://developer.spotify.com/", "Spotify", 
            target = "_blank"),
          "for making it possible to retrieve their metadata by using their 
            developer API, to",
          a(href = "https://github.com/klausfrieler" , "Klaus Frieler", 
            target = "_blank"),
          "from the",
          a(
            href = "https://www.aesthetics.mpg.de/en.html",
            "MPI for Empirical
              Aesthetics,",
            target = "_blank"
          ),
          "and to the",
          a(
            href = "http://music-psychology.org/index.html",
            "German Society
              for Music Psychology (DGM),",
            target = "_blank"
          ),
          "who agreed to host this app on
            their server."
        )
      ),
      p("Please look forward to future updates and stay tuned!"),
      hr(),
      h5("Recommended Browser: Google Chrome"),
      h5("Recommended Minimum Display Size: 24-inch" ),
      hr(),
      h6(
        "The linked sites are not under the control of the developer. The
developer is also not responsible for the contents of any linked website.
Those links are provided as a convenience only and shall not be construed as an
endorsement of, sponsorship of, or as affiliated with the linked website by the
developer—unless it is explicitly stated."
      ),
      footer = modalButton("OK"),
      size = "l",
      easyClose = T
    ))
  })
  
  # removing log-in panel after log-in
  observeEvent(req(credentials()$user_auth), {
    updateBox("rmve", action = "remove")
  })
  
  
  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = h3("About"),
        style = "text-align: justify; font-size: 14pt;",
        strong("Spotify Charts Scraper"),
        "(v0.2.6) is a",
        a(href = "https://shiny.rstudio.com/", "Shiny application", 
          target = "_blank"),
        "to retrieve daily", 
        a(href="https://spotifycharts.com/regional", "Spotify charts", 
          target ="_blank"),
        em(strong("with")),
        a(href = "https://developer.spotify.com/discover/", "audio features", 
          target="_blank"),
        "(such as danceability, valence, energy, tempo, mode, key/pitch, and
many more) in real-time—for each song, of each country, and of each date that is 
currently available on Spotify. This app is developed especially for users who 
want to gain insights into everyday music listening or want to build an 
individual database or large datasets for research in music psychology or 
computational musicology or even for cross-cultural research while 
        benefiting from the perks of Spotify’s",
        a(href = "https://developer.spotify.com/",
          "API", target = "_blank"),
        "without writing hundreds of code lines.
 Retrieved data can be downloaded as a CSV file and can easily be
 imported into any statistical computing software—e.g.,",
        a(href = "https://jasp-stats.org/about/",
          "JASP", target = "_blank"),
        "or",
        a(href = "https://www.ibm.com/products/spss-statistics", "SPSS.", 
          target = "_blank"),
        "Additionally, some basic data visualizations provide first insights 
into the data structure. All in all, the",
        strong("Spotify Charts Scraper"),
        "makes music streaming data retrieval more accessible:
It’s easy to use, and it’s open-source. Feel free to browse the source code on",
        a(href = "https://github.com/KewKalustian/Shiny_Spotify_Charts_Scraper",
          "GitHub,", target = "_blank"),
        "where you can also open an issue or even do a pull request.",
        h3("How does it work?"),
        hr(),
        p(
          style = "text-align: justify; font-size: 14pt;",
          "Once a country and a date range are selected in the sidebar, this
input information will be used accordingly to retrieve daily top 200 Spotify
charts by using scraping techniques—this is the most time-consuming part. The
retrieved charts and their respective IDs are then used to retrieve the
respective audio features—a sped-up implementation of a function of the R
          package",
          a(href = "https://www.rcharlie.com/spotifyr/", "spotifyr", 
            target = "_blank") ,
          "does here the heavy lifting. Once this second step is also done,
a cleaned-up data table is made available. The basic plots for exploratory data
analysis are entirely created with the",
          a(href = "https://ggplot2.tidyverse.org/", "ggplot2", 
            target = "_blank"),
          "and the",
          a(href = "https://ggobi.github.io/ggally/", "GGally", 
            target = "_blank"),
          "packages."
        ),
        p(
          "Since this app relies on Spotify’s metadata, it is currently 
mandatory that users must be registered by the developer to receive their log-in 
credentials via e-mail. This is necessary in order to comply with the",
          a(
            href = "https://developer.spotify.com/policy/",
            "Spotify Developer Policy.",
            target = "_blank"
          ),
          "Those credentials are stored encrypted—a hashing algorithm 
protects from brute-force attacks. If users do not want to be registered 
anymore, they have to inform the developer accordingly as unambiguously as 
possible so they can be de-registered with this app. Their user data and their 
received credentials will then be deleted."
        ),
        p(
          "Once a bigger audience has been onboarded, it is intended to 
facilitate access to this app—this depends ultimately on Spotify’s approval."
        ),
        p("Further information can be found in the modal infoboxes."),
        tags$blockquote(
          p(
            "Special thanks to",
            a(href = "https://developer.spotify.com/", "Spotify", 
              target = "_blank"),
            "for making it possible to retrieve their
metadata by using their developer API, to",
            a(href = "https://github.com/klausfrieler" , "Klaus Frieler", 
              target = "_blank"),
            "from the",
            a(
              href = "https://www.aesthetics.mpg.de/en.html",
              "MPI for Empirical Aesthetics,",
              target = "_blank"
            ),
            "and to the",
            a(
              href = "http://music-psychology.org/index.html",
              "German Society for Music Psychology (DGM),",
              target = "_blank"
            ),
            "who agreed to host this app on their server."
          )
        ),
        p("Please look forward to future updates and stay tuned!"),
        hr(),
        h5("Recommended Browser: Google Chrome"),
        h5("Recommended Minimum Display Size: 24-inch" ),
        hr(),
        h6(
          "The linked sites are not under the control of the developer. The
developer is also not responsible for the contents of any linked website.
Those links are provided as a convenience only and shall not be construed as an
endorsement of, sponsorship of, or as affiliated with the linked website by the
developer—unless it is explicitly stated."
        ),
        footer = modalButton("OK"),
        size = "l",
        easyClose = T
      )
    )
  })
  
  observeEvent(input$help, {
    showModal(
      modalDialog(
        title = "Information on Daily Top 200 Spotify Charts",
        h4(
          "For some countries, Spotify provides less than 200 chart positions
    per day while those tracks are still listed in Spotify’s top 200
    charts—e.g., Iceland, Lithuania, Estonia. The",
          strong("Spotify Charts Scraper"),
          "always retrieves all available songs dynamically. However, to 
          properly scrape the data, a sample size greater than 400 is currently 
          necessary. So, if a sampling error occurs, please refresh the page and 
          pick a date range of at least 5 days—just to be on the safe side.",
          style = "text-align: justify"
        )
        ,
        footer = modalButton("OK"),
        easyClose = T
      )
    )
  })
  
  
  observeEvent(input$date_help, {
    #
    showModal(
      modalDialog(
        title = "Information on Available Dates",
        h4(
          "The",
          strong("Spotify Charts Scraper"),
          "retrieves currently all available dates from January 1st, 2017, to 
       two days before today. A picked date range will automatically be adjusted 
       to the next two possible sequentially following dates if an invalid date 
       range has been picked. The start date will always be before the end 
       date. However, Spotify does currently not provide daily charts for each 
       date for some countries—e.g., Lithuania, Luxembourg. If a HTTP error 404 
       occurs, please refresh the page and pick another date 
       range. You can find further information on available dates on Spotify’s",
          a(href = "https://spotifycharts.com/regional", "web page.", 
            target="_blank"),
          style = "text-align: justify"
        ),
        footer = modalButton("OK"),
        easyClose = T
      )
    )
  })
  
  # Are valid dates picked?
  
  observeEvent(input$Dates, {
    
    if (as.Date(input$Dates[1]) > (as.Date(input$Dates[2]) - 1)) {
      updateDateRangeInput(session, "Dates", 
                           start = (as.Date(input$Dates[1] - 1)))
      showNotification(
        "Date rage has been automatically adjusted. Click the date range info
        button for further information.",
        closeButton = F,
        duration = 4,
      )
    }
  })
  
  
  observeEvent(input$Dates, {
    if (sum(as.Date(input$Dates[2]) - as.Date(input$Dates[1])) > 28) {
      time <-
        # seconds to retrieve 1 day and render plots. See MCS in the Github-repo
        dseconds(c(1.08 , 1.51))*as.numeric((as.Date(input$Dates[2]) - as.Date(input$Dates[1]))) 
      showNotification(
        paste0(
          "That is a wider date range. The greater the selected period is,
           the more scraping and plot rendering time is needed. Based on 
           simulations, the estimated scraping and plot rendering time will 
          approximately take in total ",
         round(as.numeric(time, "minutes")[1], 1),
          " to ",
         round(as.numeric(time, "minutes")[2], 1),
          " minutes."
        ),
        closeButton = F,
        duration = 12,
      )
    }
  })
  
  
  # Adjusting UI if invalid dates were picked
  
  output$Dates <- renderUI({
    #
    dateRangeInput(
      "date",
      "2. Choose a Date Range",
      min = "2017-01-01",
      max = Sys.Date() - 2,
      start = Sys.Date() - 3,
      end = Sys.Date() - 2
    )
  })
  
  ############
  #### GO ####
  ############
  
  observeEvent(input$go, {
    req(credentials()$user_auth)
    show_modal_spinner(
      spin = "fulfilling-bouncing-circle",
      color = "#2e4057",
      text = h3(
        "Spotify charts and Audio Features are being scraped…"
      )
    )
    
    country <- reactive({
      country <- isolate(input$country)
    })
    
    
    Sdate <- reactive({
      isolate(as.Date(input$Dates[1]))
    })
    
    NDdate <- reactive({
      isolate(as.Date(input$Dates[2]))
    })
    
    ##################################
    #### M A I N  F U N C T I O N ####
    ##################################
    
    main = reactive({
      
      spotifyR_scrapeR <- function(x) {
        
        page <- read_html(x)
        
        # Retrieving the 200 chart positions of each day.
        
        chart_pos <- html_text(html_nodes(page,".chart-table-position"))
        
        #Retrieving the 200 song/track titles of each day.
        
        title <- html_text(html_nodes(page,"strong"))
        
        # Retrieving the 200 artist names of each day.
        
        artist <- html_text(html_nodes(page,".chart-table-track span")) 
        
        # Retrieving the 200 stream counts of each day.
        
        streams <- html_text(html_nodes(page,"td.chart-table-streams"))
        
        # Retrieving the dates of for each day of the period.
        
        date <- html_text(html_nodes(page,
                                     ".responsive-select~ .responsive-select+
                   .responsive-select .responsive-select-value"))
        
        # Retrieving the track_id of for each day of the period.
        
        track_id <- html_nodes(page, "a") %>%
          html_attr("href") %>% 
          str_remove("https://open.spotify.com/track/") %>% 
          # only ID strings (i.e. char string with a length of 22)
         .[which(nchar(.[1:length(.)]) == 22)]
        
        # Putting these chunks together in a table of the class.
        
        tab <- data.frame(chart_pos, title, artist, streams, date, track_id) %>% 
               na.omit()
        
        return(tab)
      }
      
      
      Feat_scraper <- function(x, token) {
        options(warning = -1)
        # assigning length of an ID vector to a proxy object
        entire <- length(x)
        # setting seed for repo purposes
        set.seed(1, sample.kind = "Rounding")
        # assigning 100 sampled IDs to a vector to account for Spotify’s limit
        v1a <- as.character(sample(x, 100, replace = F))
        # assigning a tibble with features of those 100 IDs. This tibble will be
        # extended below.
        tib <- spotifyr::get_track_audio_features(v1a, token)
        # replacing any IDs with new ones if those IDs are already in the tibble
        if (any(x %in% tib$id) == T) {
          x = x[which(!x %in% tib$id)]
        }
        # creating a while loop on the condition that the rows of the tibble are
        # less and/or equal to the length of the entire object
        while (nrow(tib) <= entire) {
          # Setting seed for repo purposes
          set.seed(42, sample.kind = "Rounding")
          # assigning 100 sampled IDs from the new IDs from above to a base
          # vector according to Spotify’s limit as long as the object IDs are
          # greater than 100. If the remaining IDs are less than 100, these
          # remaining IDs will be sampled.
          v1b <-
            as.character(sample(x, ifelse(length(x) > 100, 100, length(x)),
                                replace = F))
          # extending the tibble from above to create a complete tibble with all
          # retrieved audio features of all track IDs of the object in question
          tib %<>% full_join(
            spotifyr::get_track_audio_features(v1b, token),
            by = c(
              "danceability",
              "energy",
              "key",
              "loudness",
              "mode",
              "speechiness",
              "acousticness",
              "instrumentalness",
              "liveness",
              "valence",
              "tempo",
              "type",
              "id",
              "uri",
              "track_href",
              "analysis_url",
              "duration_ms",
              "time_signature"
            ), match ="first"
          )
          # replacing any IDs with new ones if those IDs are already in the
          # tibble
          if (any(x %in% tib$id) == T) {
            x = x[which(!x %in% tib$id)]
          }
          # If the rows of the tibble are equal to the length of the entire
          # object in question…,
          if (nrow(tib) == entire)
            #…break the loop.
            break
        }
        # outputting the entire tibble
        return(tib)
      }
      
      iso_country <-
        countrycode::countrycode(
          country(),
          origin = 'country.name',
          destination = 'iso2c',
          custom_match = c('Global' = 'Global')
        )
      
      # Here we specify the entire streaming period (i.e., the sequence of n
      # days).
      
      streaming_period <- seq(Sdate(), NDdate(), by = "day")
      
      # Bringing all url-strings together.
      all_urls <- sprintf(
        "https://spotifycharts.com/regional/%s/daily/%s",
        as.character(tolower(iso_country)),
        as.character(streaming_period)
      )
      
      # Actual Charts Scraping procedure
      Tracks <- map_df(all_urls, spotifyR_scrapeR)
      
      # Identifying unique tracks
      id <- unique(Tracks$track_id)
      
      # sampling WHITHOUT replacement all tracks to overcome scraping 
      # protections, i.e., mimicking human behavior
      set.seed(1, sample.kind = "Rounding")
      id_s <- sample(id, replace = F)
      
      
      # saved elsewhere
      spotify_credentials <- read_csv("spotify_credentials.csv")
      
      # Generating an access token to use Spotify’s API
      token <-
        get_spotify_access_token(spotify_credentials$SPOTIFY_CLIENT_ID,
                                 spotify_credentials$SPOTIFY_CLIENT_SECRET)
      
      Feats <- Feat_scraper(id_s, token)
      Feats_id <- Feats %>%
        rename(track_id = id)
      
      Full_charts = full_join(Tracks, Feats_id, by = "track_id", 
                              match = "first") %>% 
        group_by(date, chart_pos) %>%
        mutate(
          chart_pos = as.integer(chart_pos),
          # sub: Replacing of a matching string pattern by a
          # replacement string (i.e., we simply omit the string "by"
          # and the whitespace before the artist names).
          artist = sub("by\\s", "", artist),
          # Converting/coercing streams
          streams = gsub("," , "", streams),
          streams = as.numeric(streams),
          # Dates as Dates
          date = as.Date(date, "%m/%d/%Y"),
          Period = as.factor(ifelse(date <= max(date), "Period", "")),
          tempo_bpm = as.integer(tempo),
          loudness_db = as.integer(loudness),
          country_or_region = as.character(country())
        ) %>% 
        dplyr::select(-c(track_id, type, track_href, analysis_url, tempo,
                         loudness)) %>%
        arrange(date, chart_pos)
      
      Full_charts <- na.omit(Full_charts)
      # Must be here. Otherwise no output can be generated. Don‘t know why.
      remove_modal_spinner()
      
      # Sorting the columns for the final table
      Full_charts <-
        Full_charts[, c(1:3, 5, 4, 6, 7, 19, 17, 20, 14, 8:13, 16, 15, 18, 21)]
      
    })
    
    
    #####################
    #### O U T P U T ####
    #####################
    
    # Table
    
    output$table <- renderDataTable(datatable({
      main()[, -20]
    }, caption = as.character(country()), 
    options = list(
      scrollX = T,
      scrollY = "480px",
      info = T,
      lengthMenu = list(c( 20, 100, 200),
                        c("20", "100", "200"))
    )))
    
    
    # Histo
    
    output$plot <- renderPlot({
      reactive(credentials()$user_auth)
      
      quants <- main() %>%
        group_by(Period) %>%
        summarize(
          Q1 = quantile(streams, .25),
          Median = median(streams),
          Q3 = quantile(streams, .75),
          Max = max(streams)
        )
      
   
      
      p <- ggplot(data = main()) +
        geom_histogram(
          aes(x = streams, fill = Period),
          color = "grey20",
          fill = "#2e4057",
          alpha = .7,
          binwidth = function(x) {
            2 * IQR(x) / (length(x) ^ (1 / 3))
          }
        ) +
        
        scale_y_continuous(labels = label_number_si(accuracy = NULL)) +
        
        scale_x_log10(labels = label_number_si(accuracy = NULL)) +
        
        labs(
          x = paste0(
            "\nStream Counts per Song (",
            country(),
            ") from ",
            Sdate(),
            " to ",
            NDdate(),
            "\n(log-scaling with base 10)",
            collapse = " "
          ),
          y = "Frequency\n"
        ) +
        
        theme_bw(base_size = 12) +
        theme(
          plot.margin = unit(c(.33, .66, .33, .11), "cm"),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10)
        )
      
      label_pos <-
        ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]
      
      p + geom_rect(
        data = quants,
        aes(
          x = NULL,
          y = NULL,
          xmin = Q1,
          xmax = Q3,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.3,
        fill = "darkred",
        show.legend = F
      ) +
        
        geom_vline(
          data = quants,
          aes(xintercept = Median),
          color = "darkred",
          linetype = "solid",
          size = .5
        ) +
        
        geom_label(
          data = quants,
          aes(x = Q1, y = label_pos * 0.75),
          fill = "white",
          label = sprintf("italic(Q)[1] == %s", quants$Q1),
          parse = T
        ) +
        
        geom_label(
          data = quants,
          aes(x = Median, y = label_pos * 0.5),
          fill = "white",
          label = sprintf("italic(Mdn) == %s", quants$Median),
          parse = T
        ) +
        
        geom_label(
          data = quants,
          aes(x = ((Q3-Q1)/2) + Q1, y = label_pos * 0.975),
          alpha = 0.3,
          fill = "darkred",
          label = "italic(IQR)",
          colour = "white", 
          parse = T
        ) +
        
        geom_label(
          data = quants,
          aes(x = Q3, y = label_pos * 0.75),
          fill = "white",
          label = sprintf("italic(Q)[3] == %s", quants$Q3),
          parse = T
        ) +
        
        geom_label(
          data = quants,
          aes(x =  Max * 0.75, y = label_pos * 0.75),
          fill = "white",
          label = sprintf("italic(N)[Total] == %s",nrow(main())),
          parse = T
        ) 
      
    }, execOnResize = T)
    
    # Corr. Matrix
    
    output$plot2 <- renderPlot({
      
      main() %>%
        group_by(streams) %>%
        mutate(log10_streams = log10(streams)) %>% 
        ungroup() %>% 
        dplyr::select(log10_streams,
                      tempo_bpm,
                      danceability,
                      energy,
                      loudness_db,
                      valence,
                      mode) %>% 
        mutate(mode = factor(
          mode,
          levels = c(0, 1),
          labels = c("Minor", "Major")
        )) %>% 
        ggpairs(
          aes(
            color = mode,
            fill = mode,
            alpha = 0.05,
          ), 
          method = "spearman",
          showStrips = T,
          progress = T
        ) +
        scale_color_manual(values = c("darkblue", "darkgoldenrod2")) +
        scale_fill_manual(values = c("darkblue", "darkgoldenrod2")) +
        labs(
          title = country(),
          x = "",
          y = ""
        ) +
        theme_bw(base_size = 10) +
        theme(
          plot.margin = unit(c(.11, .11, .11, .11), "cm"),
          axis.title.y = element_text(size = 8),
          axis.title.x = element_text(size = 8)
        )
      
    },
    execOnResize = T)
  
    output$downloadData <- downloadHandler(
      
      filename = function() {
        
        paste0("Spotify_Charts_(",
               country(),
               ")_from_",
               Sdate(),
               "_to_",
               NDdate(),
               ".csv")
      },
      
      content = function(con) {
        
        write.csv(main()[-20], con, row.names = T)
      }
    )
    
  })
  
}


#####################
#### D E P L O Y ####
#####################

shinyApp(ui = ui, server = server)


## End(**Not run**)

################################################################################
