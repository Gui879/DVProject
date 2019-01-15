library(shiny)
library(tm)


shinyServer(function(input, output, session) {
  
  
  observe({
    if(input$num_filter=='only'){
      #if the user only wants to see the registered players we get here
      ### reactive function -- I am responsible for the data
      dataInput <- reactive({
        #date range
        from <- which(pokes$date == input$dates[1])
        to <- which(pokes$date == input$dates[2])
        #selecting only the columns and rows that matter to us
        out <- colSums(pokes[from:to, c("Red Team", "Blue Team", "Yellow Team")])
        out
      })
      
      ###plotting --- I am responsible for the drawing the waffle
      output$waf <- renderPlot({
        #getting the data and change it to percentage so that we can draw the waffle
        parts <- dataInput()
        total = sum(parts)
        for( i in 1:length(parts)){
          parts[i] = parts[i]/total*100
        }
        parts = round(parts, digits = 0)
        if (total == 0){
          #if there are no raids between the dates selected we have to do a different thing
          pl = c("Nothing" = 100)
          waffle(pl, rows=10, colors = c("#333333"), legend_pos = "bottom", title = "There were no Raids between those dates") + theme(panel.background = element_rect(fill = "transparent",color = NA))
        } else {
          #When calculating percentages it can give us sums of 99 or 101 and we need to compensate it
          if (sum(parts) == 99){
            #adding one to the most frequent
            max_index = which.max(parts)
            parts[max_index] = parts[max_index] + 1
          }
          else if (sum(parts) == 101){
            #Taking one from the least frequent
            min_index = which.min(parts)
            parts[min_index] = parts[min_index] -1
          }
          #plotting the waffle
          names(parts) = c(sprintf("Red Team (%s%%)",parts[1]), sprintf("Blue Team (%s%%)",parts[2]), sprintf("Yellow Team (%s%%)",parts[3]))
          waffle(parts, rows=10, colors = c("#3333CC", "#CC0000", "#FFFF33"),legend_pos = "right", title = "Raids Participation per Team:", xlab= "Each square represents 1%") + theme(panel.background = element_rect(fill = "transparent",color = NA), plot.background = element_rect(fill = "transparent", color = NA))
        }
      },bg = "transparent")
    }
    else{
      #If they select all players
      ### reactive function -- I am responsible for the data
      dataInput <- reactive({
        from <- which(pokes$date == input$dates[1])
        to <- which(pokes$date == input$dates[2])
        #getting all rows and columns we need 
        out <- colSums(pokes[from:to, c("Red Team", "Blue Team", "Yellow Team","red_friends","blue_friends","yellow_friends","Unknown Team")])
        out
      })
      
      ###plotting --- I am responsible for the drawing
      output$waf <- renderPlot({
        #Sum values based on colour.
        parts <- dataInput()
        parts["Red Team"] = parts["Red Team"] + parts["red_friends"]
        parts["Blue Team"] = parts["Blue Team"] + parts["blue_friends"]
        parts["Yellow Team"] = parts["Yellow Team"] + parts["yellow_friends"]
        #Eliminating rows we don't need
        parts=parts[-4]
        parts=parts[-4]
        parts=parts[-4]
        #Calculating percentages
        total = sum(parts)
        for( i in 1:length(parts)){
          parts[i] = parts[i]/total*100
        }
        parts = round(parts, digits = 0)
        if (total == 0){
          #if there are no raids between the dates selected we have to do a different thing
          pl = c("Nothing" = 100)
          waffle(pl, rows=10, colors = c("#333333"), legend_pos = "bottom", title = "There were no Raids between those dates") + theme(panel.background = element_rect(fill = "transparent",color = NA), plot.background = element_rect(fill = "transparent", color = NA))
        } else {
          #When calculating percentages it can give us sums of 99 or 101 and we need to compensate it
          if (sum(parts) == 99){
            #Adding one to the most frequent
            max_index = which.max(parts)
            parts[max_index] = parts[max_index] + 1
          }
          else if (sum(parts) == 101){
            #Taking one from the least frequent
            min_index = which.min(parts)
            parts[min_index] = parts[min_index] -1
          }
          #Plotting waffle
          names(parts) = c(sprintf("Red Team (%s%%)",parts[1]), sprintf("Blue Team (%s%%)",parts[2]), sprintf("Yellow Team (%s%%)",parts[3]),sprintf("Unknown Team (%s%%)",parts[3]))
          waffle(parts, rows=10, colors = c("#3333CC", "#CC0000", "#FFFF33", "#666666"),legend_pos = "right", title = "Raids Participation per Team:", xlab= "Each square represents 1%") + theme(panel.background = element_rect(fill = "blue", color = "blue"),legend.text=element_text(size=12))
          }
      },bg="transparent")
    }
  })
  
  
  #responsible for loading the search map (in lisbon)
  output$search_map <- renderLeaflet({
    
    leaflet()%>%
      addTiles()%>%
      setView(
        lng = -9.139444,
        lat = 38.713889,
        zoom = 12
      )
    
    
  })
  
  #resposible for check when the place_search and radius inputs changes
  observe({
    #if place_search is not null and not empty, then update the map
    if(!is.null(input$place_search) & trimws(input$place_search, which = 'both')!=''){
      #store the target address
      query <- trimws(input$place_search, which = 'both')
      query <- paste(query," lisboa")
      #send the address to get the coordinates
      coord <- geocodeAdddress(query)
      #if the coordinates are not null, then update the map
      if(!is.null(coord)){
        #calculate all distances between the user location and all the gyms location
        distances = list()
        for(i in 1:length(locations$location)){
          distances[[i]] <- distm(c(coord$lat, coord$log), c(locations$lat[i], locations$log[i]), fun = distHaversine)
        }
        locations$distances = distances
        #select all the gyms inside the radius
        temp <- locations[locations$distances<input$search_radius,]
        #if we have gyms inside the radius, load gyms markers
        if(length(temp$log)!=0){
          leafletProxy('search_map', session)%>%
            clearMarkers() %>%
            clearShapes() %>%
            addTiles()%>%
            setView(
              lng = coord$log,
              lat = coord$lat,
              zoom = 14
            )%>%
            addCircles(lng = coord$log, lat = coord$lat, radius = input$search_radius)%>%
            addMarkers(lng=temp$log, lat=temp$lat, 
                       label = as.character(temp$location), 
                       popup = paste('<img src=',temp$image,' height="100" width="100" border-radius: 50%>',
                                     '<p style="text-transform: uppercase; text-align:center;"><strong>',temp$location,'</strong></p>',
                                     '<p style="text-align:center;">',round(as.numeric(temp$distances), 2),'meters away</p>'),
                       icon = gymIcon)
          #otherwise, load only the circle buffer
        }else{
          leafletProxy('search_map', session)%>%
            clearMarkers() %>%
            clearShapes() %>%
            addTiles()%>%
            setView(
              lng = coord$log,
              lat = coord$lat,
              zoom = 14
            )%>%
            addCircles(lng = coord$log, lat = coord$lat, radius = input$search_radius)
        }
      }
      
    }
    
  })
  
  
  
  #responsible for loading the network of players
  output$network <- renderVisNetwork({
    # create dataframe with all the nodes (players)
    nodes <- data.frame(id = players_info$X,
                        color.background = players_info$team,
                        team = players_info$team,
                        value = rep(25,401),
                        group = paste(players_info$team,rep(25,401)),
                        title = paste('<b>Username: </b>',players_info$username,'<br>
                                      <b>Level: </b>',players_info$level,'<br>
                                      <b>Team: </b>',players_info$team))
    
    # create dataframe with all the links between the nodes
    links = friendships[,c('from','to')]
    links['color'] = 'black'
    edges <- links
    
    # update color of yellow team to orange
    nodes$color.background = revalue(nodes$color.background, c("yellow"="orange"))
    
    # load the network
    visNetwork(nodes, edges)%>%
      visIgraphLayout()%>%
      visOptions(selectedBy = list(variable = "group", style = 'opacity: 0; pointer-events : none;'), clickToUse = FALSE)%>%
      visNodes(shape = "circle", borderWidth = 0)
    
  })
  
  #responsible for check when username changes
  observe({
    #if username is not null, then select the correspondent node
    if (!is.null(input$username)){
      #define a new group to the nodes(0 to all)
      group <- rep(0,401)
      #mark the target node(s) with 1
      group[players_info$username %in% input$username] = 1
      
      #recreate all the nodes
      nodes <- data.frame(id = players_info$X,
                          color.background = players_info$team,
                          team = players_info$team,
                          value = rep(25,401),
                          group = group,
                          title = paste('<b>Username: </b>',players_info$username,'<br>
                                        <b>Level: </b>',players_info$level,'<br>
                                        <b>Team: </b>',players_info$team))
      #change color from yellows
      nodes$color.background = revalue(nodes$color.background, c("yellow"="orange"))
      
      #update the network, with the target nodes highlighted
      visNetworkProxy("network") %>%
        visNodes(scaling = list(min = 20, max = 50))%>%
        visUpdateNodes(nodes = nodes)%>%
        visOptions(selectedBy = list(variable = "group", selected = 1, style = 'opacity: 0; pointer-events : none;'), clickToUse = FALSE)%>%
        visOptions(selectedBy = list(variable = "group", selected = 1, style = 'opacity: 0; pointer-events : none;'), clickToUse = FALSE)
      
    }
  })
  
  #responsible for check when team and centrality input change
  observe({
    
    #recreate all the nodes
    nodes <- data.frame(id = players_info$X,
                        color.background = players_info$team,
                        team = players_info$team,
                        value = rep(25,401),
                        group = paste(players_info$team,rep(25,401)),
                        title = paste('<b>Username: </b>',players_info$username,'<br>
                                      <b>Level: </b>',players_info$level,'<br>
                                      <b>Team: </b>',players_info$team))
    
    #change color from yellows
    nodes$color.background = revalue(nodes$color.background, c("yellow"="orange"))
    
    
    if(input$team_filter!='NULL'){
      
      if(input$centrality_filter=='NULL'){
        #show slider input to select how many players to select
        shinyjs::hide("top_players")
        #store the query filter
        query <- input$team_filter
        #assign a new group to the nodes
        nodes$group = players_info$team
      }else if(input$centrality_filter=='between'){
        #show slider input to select how many players to select
        shinyjs::show("top_players")
        #select target nodes
        temp = head(bcentrality[bcentrality$team==input$team_filter & order(-bcentrality$B_centrality), ],n=input$top_players)
        #reassing new values to the nodes
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        #store the query filter
        query <- paste(input$team_filter,50)
        #update the value of each node
        nodes$value = sizes
        #update the group of each node
        nodes$group = paste(players_info$team, sizes)
      }else if(input$centrality_filter=='degree'){
        shinyjs::show("top_players")
        temp = head(dcentrality[dcentrality$team==input$team_filter & sort(dcentrality$D_centrality), ],n=input$top_players)
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        query <- paste(input$team_filter,50)
        nodes$value = sizes
        nodes$group = paste(players_info$team, sizes)
      }else if(input$centrality_filter=='close'){
        shinyjs::show("top_players")
        temp = head(ccentrality[ccentrality$team==input$team_filter & sort(ccentrality$C_centrality), ],n=input$top_players)
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        query <- paste(input$team_filter,50)
        nodes$value = sizes
        nodes$group = paste(players_info$team, sizes)
      }
    }else{
      if(input$centrality_filter=='NULL'){
        shinyjs::hide("top_players")
        query <- input$team_filter
        nodes$group = players_info$team
      }else if(input$centrality_filter=='between'){
        shinyjs::show("top_players")
        temp = head(arrange(bcentrality, desc(B_centrality)),input$top_players)
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        query <- 50
        nodes$value = sizes
        nodes$group = sizes
      }else if(input$centrality_filter=='degree'){
        shinyjs::show("top_players")
        temp = head(arrange(dcentrality, desc(D_centrality)),n=input$top_players)
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        query <- 50
        nodes$value = sizes
        nodes$group = sizes
      }else if(input$centrality_filter=='close'){
        shinyjs::show("top_players")
        temp = head(arrange(ccentrality, desc(C_centrality)),n=input$top_players)
        sizes = rep(25,401)
        sizes[players_info$username %in% temp$username]=50
        query <- 50
        nodes$value = sizes
        nodes$group = sizes
      }
      
    }
    
    
    #update the network with the target nodes
    visNetworkProxy("network") %>%
      visNodes(scaling = list(min = 20, max = 50))%>%
      visUpdateNodes(nodes = nodes)%>%
      visOptions(selectedBy = list(variable = "group", selected = query, style = 'opacity: 0; pointer-events : none;'), clickToUse = FALSE)%>%
      visOptions(selectedBy = list(variable = "group", selected = query, style = 'opacity: 0; pointer-events : none;'), clickToUse = FALSE)
    
  })
  
  #RaidsLocation
  dataset <- raids
  
  output$timeSel <- renderUI({
    if(input$time == "Day"){
      return(dateInput('date',label = 'Date input:', min = head(raids$date, n=1), max = tail(raids$date, n=1), startview = "month",value = head(raids$date, n=1)))
    }
    else if(input$time == "Days(Range)"){
      return(dateRangeInput('date',
                            label = 'Date range:',
                            startview = 'month',
                            start = head(raids$date, n=1), 
                            end = raids$date[5], 
                            min = head(raids$date, n=1), 
                            max = tail(raids$date, n=1)))
    }else if(input$time == "Months(Range)"){
      return(dateRangeInput('datem',
                            label = 'Month Range:',
                            startview = 'month',
                            start = head(raids$date, n=1), 
                            end = tail(raids$date,n=1), 
                            min = head(raids$date, n=1), 
                            max = tail(raids$date, n=1)))
    }
  })
  
  
  output$slider <- renderUI({
    if(input$time == "Day"){
      
      return(sliderInput("hours", 
                         "Hour:", 
                         min = as.POSIXct("2016-03-01 00:00"),
                         max = as.POSIXct("2016-03-01 23:59"),
                         value = c(as.POSIXct("2016-03-01 00:00")),
                         timeFormat = "%H:%M", ticks = F, animate = T, step = 1800))
      
    } else if(input$time == "Days(Range)"){
      return(sliderTextInput("range", "Day:",selected = NULL, choices = dates[dates >= input$date[1] & dates <= input$date[2]], grid = FALSE, animate =animationOptions(interval = 1800)))
      
    } else if(input$time == "Months(Range)"){
      if(is.null(input$datem[1]) | is.null(input$datem[2])){
        
      }else{
        t <- input$datem[1]
        t2 <- input$date[2]
        ms <- month(input$datem[1]):month(input$datem[2])
        return(sliderTextInput("rangem", "Month:",selected = NULL, choices = ms, grid = FALSE, animate =animationOptions(interval = 1800)))
      }
    }
  })
  
  filteredData <- reactive({
    
    if(input$time == "Day"){
      hour <- format(input$hours, "%H:%M")
      t1 <- raids[raids$date == input$date & hour >= raids$hour_start & hour <= raids$hour_end,]
      
      if(nrow(t1) > 0){
        temp <- t1[!duplicated(t1[,'loc']),]
        temp$total <-aggregate(t1$player_nmr, by=list(Category=t1$loc), FUN=sum)[2]
        temp$nmr_raids <- data.frame(table(t1$loc))[2]
        return(temp)
      }else{
        return(t1)
      }
    }else if(input$time == "Days(Range)"){
      
      t1 <- raids[raids$date == input$range,]
      
      if(nrow(t1) > 0){
        temp <- t1[!duplicated(t1[,'loc']),]
        temp$total <-aggregate(t1$player_nmr, by=list(Category=t1$loc), FUN=sum)[2]
        temp$nmr_raids <- data.frame(table(t1$loc))[2]
        return(temp)
      }else{
        return(t1)
      }
    }else if(input$time == "Months(Range)"){
      if(is.null(input$rangem)){
        d1 <- paste(c("2018","01","01"),collapse = "-")
      }else{
        d1 <- paste(c("2018",toString(input$rangem),"01"),collapse = "-")
      }
      
      a <- as.POSIXlt(d1)
      a$mon <- a$mon + 1 
      a$day <- a$mday - 1
      t1 <- raids[raids$date <= a & raids$date >= d1,]
      
      if(nrow(t1) > 0){
        temp <- t1[!duplicated(t1[,'loc']),]
        temp$total <- aggregate(t1$player_nmr, by=list(Category=t1$loc), FUN=sum)[2]
        temp$nmr_raids <- data.frame(table(t1$loc))[2]
        return(temp)
      }else{
        return(t1)
      }
    }
  })
  
  
  output$mdynamic <- renderLeaflet({
    
    leaflet(data = raids[raids$date == dates[1],],options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      setView(
        lng = -9.175153,
        lat = 38.819176,
        zoom = 11
      ) %>%
      clearMarkers() %>%
      addMarkers(~lat,~log, 
                 labelOptions = labelOptions(
                   style = list(
                     "color" = "red",
                     "font-family" = "serif",
                     "font-style" = "italic",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)"
                   )),layerId = ~X,icon = gymIcon)
    
  })
  
  observe({
    dts <- filteredData()
    if(input$group){
      leafletProxy("mdynamic", data = dts) %>%
        clearMarkers() %>% clearHeatmap() %>%
        addHeatmap(lng = ~lat, lat = ~log,
                   blur = 20, max = 0.05, radius = 15)
      
    }else{
      leafletProxy("mdynamic", data = dts) %>%
        clearMarkers() %>% clearHeatmap() %>%
        addMarkers(~lat,~log,
                   labelOptions = labelOptions(
                     style = list(
                       "color" = "red",
                       "font-family" = "serif",
                       "font-style" = "italic",
                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)"
                     )),layerId = ~X,icon = gymIcon
        )
    }
  })
  
  observe({
    leafletProxy("mdynamic") %>% clearPopups()
    event <- input$mdynamic_marker_click
    if (is.null(event))
      return()
    else{
      isolate({
        showRaidPopup(event$id, event$lat, event$lng)
      })
    }
    
  })
  
  showRaidPopup <- function(raidid, lat, lng) {
    data <- filteredData()
    selectedRaid <- data[data$X == raidid,]
    tg1 <- tagList(
      tags$strong(HTML(toupper(selectedRaid$location)
      )), tags$br(),sprintf("Number of Raids: %s", selectedRaid$nmr_raids),tags$br(),sprintf("Player Participation: %s", selectedRaid$total)
    )
    
    content <- as.character(tg1)
    leafletProxy("mdynamic") %>% addPopups(lng,lat, content, layerId = raidid)
  }
  
  #Statistics(Raids Locations)
  
  output$text <- renderUI({
    if (input$t2 == 'Per Hour'){
      return(tagList(tags$h4("As we look at the frequency of raids per time of day we can easily tell",tags$br(),tags$h4("that the peak hours for raiding are between 16:00 and 18:00!"))))
    }else if (input$t2 == 'Per Day'){
      return(tagList(tags$h4("Looking at the distribution of the raid occurrences per weekday,",tags$br(),tags$h4("we surprisingly found a constant frequency of raids!"))))
      
    }else if(input$t2 == 'Per Month'){
      return(tagList(tags$h4("The distribution of the raids per month tells us that the period at the",tags$br(),tags$h4(" beggining of the fall semester is when more people gather for raiding!"))))
    }
  })
  
  getData <- reactive({
    if (input$t2 == 'Per Hour'){
      
      return(perhour)
    }else if (input$t2 == 'Per Day'){
      
      return(perday)
    }else if(input$t2 == 'Per Month'){
      
      return(permonth)
    }
  })
  
  output$lineplot <- renderPlotly({
    dt <- getData()
    if(input$t2 == 'Per Hour'){
      #plot(x = dt$X1, y = dt$X2, type= 'ol',ylab="Raid Frequency", xlab="Hour")
      plot_ly(y = perhour$X2,x = perhour$X1,type="bar",marker = list(color= "#474747")) %>% layout(margin = list(r = 60),xaxis = list(title = "Hour",tickangle=45),yaxis = list(title = "Raid Frequency"),hovermode= 'compare', plot_bgcolor='rgba(255, 255, 255,0.1)') %>% 
        layout(paper_bgcolor='rgba(255, 255, 255,0.1)') %>% config(displayModeBar = FALSE)
    }else if(input$t2 == 'Per Day'){
      #plot(perday$X2, xaxt = "n", type = "ol",ylab="Raid Frequency",xlab="WeekDay")
      #axis(1, perday$X1, perday$X1,cex.axis = 1)
      plot_ly(x= perday$X1,y=perday$X2,type="bar",marker = list(color= "#474747")) %>% layout(xaxis = list(title = "Weekday"),yaxis = list(title = "Raid Frequency"),hovermode= 'compare',plot_bgcolor='rgba(255, 255, 255,0.1)') %>% 
        layout(paper_bgcolor='rgba(255, 255, 255,0.1)') %>% config(displayModeBar = FALSE)
    }else if(input$t2 == 'Per Month'){
      #plot(permonth$X2, type="ol",xlab ="Month",ylab="Raid Frequency")
      plot_ly(x = permonth$X1, y = permonth$X2, type="bar",marker = list(color= "#474747")) %>% layout(xaxis = list(title = "Month"),yaxis = list(title = "Raid Frequency"),hovermode= 'compare',plot_bgcolor='rgba(255, 255, 255,0.1)') %>% 
        layout(paper_bgcolor='rgba(255, 255, 255,0.1)') %>% config(displayModeBar = FALSE)
    }
  })
  
  #Players Info
  df<-players_info
  
  mydata <- reactive({
    b=players_info$team=='blue'
    r=players_info$team=='red'
    y=players_info$team=='yellow'
    
    print(input$team)
    
    
    
    team <- players_info
    
    
    if (!is.null(input$players)) {
      team<-players_info[players_info$username %in% input$players,]
      print(team)
    }
    if (input$team!='all'){
      team<-team[team$team==input$team,]
      print('equipa')
      print(team)
    }
    if (input$slider2[1]==input$slider2[2]){
      team<-team[team$level==input$slider2[1],]
      
    }else {
      team<-team[team$level>=input$slider2[1]&team$level<=input$slider2[2],]
    
    }
    
  })
  
  
  
  
  
  output$info <- renderDataTable({
    test <- df
    datatable(mydata(), filter="none", selection="multiple", escape=FALSE, 
              options = list(sDom  = '<"top">lrt<"bottom">ip'))%>% 
      
      formatStyle(
        columns= 'team',
        color = styleEqual(
          unique(df$team), c('red', 'orange', 'blue')
        )
      )
    
  })
  
})
