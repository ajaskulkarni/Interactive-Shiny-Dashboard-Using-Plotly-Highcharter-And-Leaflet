# Shiny server file

server = function(input, output, session) {
  # Tab - 1 (Dashboard)
  
  # Displyaing stackked bar plot with median income   
  output$plot2 = renderHighchart(
    highchart()%>%
      # Adding theme to plot.
      hc_add_theme(hc_theme_elementary()) %>%
      # Adding two y-axes in the plot.
      hc_yAxis_multiples(
        list(showLastLabel = TRUE,                           # <- turn on the 1st y-axis's last label 
             linewidth = 3,                                  # <- set the linewidth
             title = list(text = "Percent Proficiency in Math (2015)"),     # <- set the title for 1st y-axis
             max = 100,                                      # <- set the max for 1st y-axis
             tickAmount = 5),                                # <- set the maximum tickamount
        
        list(showLastLabel = TRUE,                           # <- turn on the 2nd y-axis's last label 
             opposite = TRUE, # <- ploting 2nd y-axis to the opposite side of 1st y-axis 
             title = list(text = "Median Income (2015)"),  
             color = "red",# <- set the title for 2nd y-axis
             max = max(data2_sorted$Median_Income),           # <- set the max for 2nd y-axis
             min = min(data2_sorted$Median_Income),           # <- set the min for 2nd y-axis
             tickAmount = 4)                                  # <- set the maximum tickamount
      )%>%
      # Specifying to plot stacked barplot
      hc_plotOptions(column = list(stacking = "normal"))%>%
      # Specifying to plot stacked barplot
      hc_add_series(data = data2_sorted$State_Failing_Percentage,type = "column", name = "Below Proficient", color = "#FF9999", pointWidth = 15)%>%                 # <- adding stacked coloumn chart for below proficient students
      hc_add_series(data = data2_sorted$State_Passing_Percentage,type = "column", name = "Proficient & above Proficient", pointWidth = 15, color = "#00CC66")%>%    # <- adding stacked coloumn chart for proficient students
      hc_add_series(data = data2_sorted$Median_Income, type = "spline",  name = "Median Income", color = "cornflowerblue", yAxis = 1)%>%                                     # <- adding line plot for median income
      # Adding x-axis labels
      hc_xAxis(categories = data2_sorted$State) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE) %>%
      hc_legend(align = "left")%>%     # <- alignment of the legend
      hc_chart(marginTop = 35)         # <- set the top margin
  )

  # Plot 2 - Row 1
  # Displyaing boxplots or scatter plot based on the user's selection
  # if user selected boxplot then "bp" will be called from the ui.R file and 
  # in if else loop it will be matched for the condition. Similarly, for 
  # scatter plot "sp" will be called
  
  output$plot1 = renderPlotly(
    if(input$rad == "bp")
    {
      plot_ly(type = "box") %>%
      add_boxplot(y = data2$State_Passing_Percentage, name = "All") %>%
      add_boxplot(y = data2$State_Percentage_Disabilities, name = "Students with disabilities") %>%
      add_boxplot(y = data2$State_Percentage_Low_Income, name = "Low income") %>%
      add_boxplot(y = data2$State_Percentage_White, name = "White") %>%
      add_boxplot(y = data2$State_Percentage_Black, name = "Black") %>%
      add_boxplot(y = data2$State_Percentage_Hispanic, name = "Hispanic") %>%
      add_boxplot(y = data2$State_Percentage_Asian, name = "Asian") %>%
      layout(yaxis = list(title = "Percentage Proficiency in Math (2015)"),
             xaxis = list(tickangle = 45),            # <- rotating x-axis labels by 45 degrees
             margin = list(b = 120))                  # <- set the bottom margin for boxplots 
    }
    else if(input$rad == "sp")
    {
      plot_ly(data = data2, x = data2$Median_Income, y = data2$State_Passing_Percentage, type = "scatter", 
              mode = 'markers', color = data2$State, colors = "Set1",
              marker = list(size = 10)) %>%
        layout(#title = "Scatter plot for median income and percentage proficiency in Math",
               yaxis = list(title = "Proficiency Percentage in Math (2015)"),
               xaxis = list(title = "Median Income (2015)"))
    }
  )
  
  # Tab 2 (Full map)

  # Displyaing leaflet maps based on the user's selection. If user want to
  # analyze population density "1" will be returned to the server.R and it 
  # will render the leaflet map for the population density. Similar for 
  # median income and percentage proficiency
  # Adding custom markers
  
  # Defining customs marker for school. 
  school = makeIcon(iconUrl = "School.png",
                    iconWidth = 30, iconHeight = 30,
                    iconAnchorX = 0, iconAnchorY = 0)
  
  # Defining customs marker for university.
  university = makeIcon(iconUrl = "University.png",
                        iconWidth = 30, iconHeight = 30,
                        iconAnchorX = 0, iconAnchorY = 0)
  
  # Pop-up for school data
  site_HS = as.character(data1$School_Url)
  name_HS = as.character(data1$School_Name)
  school_popup = paste('<a href=', shQuote(site_HS), "target=_blank", 
                       '>', name_HS, '</a>',
                       "<br>",
                       data1$City_State,
                       "<br>",
                       "Rank:", data1$School_Rank
  )
  # Pop-up for university data
  site_HS_1 = as.character(data1$University_Url)
  name_HS_1 = as.character(data1$University_Name)
  university_popup = paste('<a href=', shQuote(site_HS_1), "target=_blank", 
                           '>', name_HS_1, '</a>',
                           "<br>",
                           "Rank:", data1$University_Rank
  )
  
  output$mymap1 = renderLeaflet({
    if(input$var1 == 1)
    {
      # Defining bins and palatte for population density.
      bins = c(0, 10, 20, 50, 100, 200, 500, 1000, 1100)
      pal = colorBin("YlOrRd", domain = states$density, bins = bins)
      
      html_legend = "<img src = 'https://upload.wikimedia.org/wikipedia/commons/0/0e/Ski_trail_rating_symbol-green_circle.svg'
     style='width:15px;height:15px;'> Cluster size = 1-10<br/>
      <img src = 'http://www.clker.com/cliparts/o/b/y/x/Z/c/yellow-dot-md.png'
      style='width:15px;height:15px;'> Cluster size = 11-99<br/>
      <img src = 'http://www.clker.com/cliparts/X/9/P/m/2/g/transparent-red-circle.svg'
      style='width:15px;height:15px;'> Cluster size = 99+<br/>"
      
      # Constructing labels for all the states.
      labels = sprintf(
        "<strong>%s</strong>
        <br/><strong>Population Density: </strong>%g people/mi<sup>2</sup>
        <br/><strong>Median Income: </strong>%s
        <br/><strong>Percentage Proficiency in Math: </strong>%s%%",
        toupper(states$name), states$density, prettyNum(data2$Median_Income,big.mark = ','), data2$State_Passing_Percentage
      ) %>% lapply(htmltools::HTML)
      
      leaflet(states) %>%
        setView(-96, 37.8, 3) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
        addPolygons(
          fillColor = ~pal(density),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addMarkers(lng = data1$lon, lat = data1$lat,
                   clusterOptions = markerClusterOptions(data1$School_Name),
                   icon = school, popup = school_popup, group = "Schools (Top 100)") %>%
        addMarkers(lng = data1$ulon, lat = data1$ulat,
                   clusterOptions = markerClusterOptions(data1$University_Name),
                   icon = university, popup = university_popup, group = "Universities (Top 100)")%>%
        addLegend(pal = pal, values = ~density, opacity = 0.7,
                  title = "Population density (people/mi<sup>2</sup>)",
                  position = "bottomright")%>%
        addControl(html = html_legend, position = "bottomright")%>%
        addLayersControl(
          overlayGroups = c("Schools (Top 100)",
                            "Universities (Top 100)"),
          options = layersControlOptions(collapsed = FALSE) 
        )
    }
    # Percentage Proficiency in Math map
    else if(input$var1 == 2){
      # Percentage Proficiency
      bins2 = c(0,10, 20, 30, 35, 40, 50)
      pal2 = colorBin("Greens", domain = data2$State_Passing_Percentage, bins = bins2)
      
      html_legend = "<img src = 'https://upload.wikimedia.org/wikipedia/commons/0/0e/Ski_trail_rating_symbol-green_circle.svg'
     style='width:15px;height:15px;'> Cluster size = 1-10<br/>
      <img src = 'http://www.clker.com/cliparts/o/b/y/x/Z/c/yellow-dot-md.png'
      style='width:15px;height:15px;'> Cluster size = 11-99<br/>
      <img src = 'http://www.clker.com/cliparts/X/9/P/m/2/g/transparent-red-circle.svg'
      style='width:15px;height:15px;'> Cluster size = 99+<br/>"
      
      labels2 = sprintf(
        "<strong>%s</strong><br/>
        <strong>Percentage Proficiency in Math: </strong>%s%%
        <br/><strong>Population Density: </strong>%g people/mi<sup>2</sup>
        <br/><strong>Median Income: </strong>%s",
        toupper(states$name), data2$State_Passing_Percentage, states$density, prettyNum(data2$Median_Income, big.mark = ',')
      ) %>% lapply(htmltools::HTML)
      leaflet(states) %>%
        setView(-96, 37.8, 3) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
        addPolygons(
          fillColor = ~pal2(data2$State_Passing_Percentage),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels2,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        )%>%
        addMarkers(lng = data1$lon, lat = data1$lat,
                   clusterOptions = markerClusterOptions(data1$School_Name),
                   icon = school, popup = school_popup, group = "Schools (Top 100)") %>%
        addMarkers(lng = data1$ulon, lat = data1$ulat,
                   clusterOptions = markerClusterOptions(data1$University_Name),
                   icon = university, popup = university_popup, group = "Universities (Top 100)")%>%
        addLegend(pal = pal2, values = c(), opacity = 0.7,
                  title = "Percentage Proficiency in Math",
                  position = "bottomright") %>%
        addControl(html = html_legend, position = "bottomright")%>%
        addLayersControl(
          overlayGroups = c("Schools (Top 100)",
                            "Universities (Top 100)"),
          options = layersControlOptions(collapsed = FALSE) 
        )
    }
    # Median Income map
    else if(input$var1 == 3)
    {
      bins1 = c(30000,40000, 50000, 60000, 70000, 80000)
      pal1 = colorBin("Blues", domain = data2$Median_Income, bins = bins1)
      
      html_legend = "<img src = 'https://upload.wikimedia.org/wikipedia/commons/0/0e/Ski_trail_rating_symbol-green_circle.svg'
     style='width:15px;height:15px;'> Cluster size = 1-10<br/>
      <img src = 'http://www.clker.com/cliparts/o/b/y/x/Z/c/yellow-dot-md.png'
      style='width:15px;height:15px;'> Cluster size = 11-99<br/>
      <img src = 'http://www.clker.com/cliparts/X/9/P/m/2/g/transparent-red-circle.svg'
      style='width:15px;height:15px;'> Cluster size = 99+<br/>"
      
      labels1 = sprintf(
        "<strong>%s</strong>
        <br/><strong>Median Income: </strong>%s
        <br/><strong>Population Density: </strong>%s people/mi<sup>2</sup>
        <br/><strong>Percentage Proficiency in Math: </strong>%s%%",
        toupper(states$name), prettyNum(data2$Median_Income,big.mark = ','), states$density, data2$State_Passing_Percentage
      )%>% lapply(htmltools::HTML)
      leaflet(states) %>%
        setView(-96, 37.8, 3) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas)%>%
        addPolygons(
          fillColor = ~pal1(data2$Median_Income),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels1,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        )%>%
        addMarkers(lng = data1$lon, lat = data1$lat,
                   clusterOptions = markerClusterOptions(data1$School_Name),
                   icon = school, popup = school_popup, group = "Schools (Top 100)") %>%
        addMarkers(lng = data1$ulon, lat = data1$ulat,
                   clusterOptions = markerClusterOptions(data1$University_Name),
                   icon = university, popup = university_popup, group = "Universities (Top 100)")%>%
        addLegend(pal = pal1, values = c(), opacity = 0.7,
                  title = "Median Income",
                  position = "bottomright") %>%
        addControl(html = html_legend, position = "bottomright") %>%
        addLayersControl(
          overlayGroups = c("Schools (Top 100)",
                            "Universities (Top 100)"),
          options = layersControlOptions(collapsed = FALSE) 
        )
    }
  }
      )
  
}
