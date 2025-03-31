options(shiny.maxRequestSize=30*1024^2)
suppressPackageStartupMessages(library(shiny, attach.required = T))
suppressPackageStartupMessages(library(DT, attach.required = T))
suppressPackageStartupMessages(library(readxl, attach.required = T))
suppressPackageStartupMessages(library(gridExtra, attach.required = T))
suppressPackageStartupMessages(library(tidyr, attach.required = T))
suppressPackageStartupMessages(library(dplyr, attach.required = T))
suppressPackageStartupMessages(library(ggplot2, attach.required = T))
suppressPackageStartupMessages(library(sf, attach.required = T))
suppressPackageStartupMessages(library(lubridate, attach.required = T))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leafsync))
suppressPackageStartupMessages(library(leafpop))
suppressPackageStartupMessages(library(shinyBS))
# library(profvis)

# setwd("Z:/Terrestrial/Species/1 - Multi-Species/R_Projects/LEH Tool/Tool")
wild <- read_excel("WILD LEH Table - For Tool.xlsx")
seasons <- readRDS("Wild Seasons.rds")
specieslist <- unique(wild$Species)
leh.boundaries <- readRDS("LEH Boundaries_May24_2024.rds")
regions <- readRDS("RegionsPoly.rds")
labels <- readRDS("LEH_LabelPoints.rds")


greens <- colorRampPalette(c("white", "dark green"))(100)

Class <- list()
for(s in 1:length(specieslist)){
  Class[[s]] <- unique(wild$AnimalClass[wild$Species == specieslist[s]])
  if(specieslist[s] == "Moose"){
    Class[[s]] <- c( "Bull", "Cow Or Calf Only", "Calf Only")
    # Class[[s]] <- c( "Bull Only", "Spike-fork/Tripalm/10 point", "Cow Or Calf Only", "Calf Only")
  }
}
names(Class) <- specieslist

# profvis({
#   sApp <- shinyApp(

ui <- fluidPage(
  navbarPage(title = "B.C. Limited Entry Hunt Filtering Tool", theme = "bcgov.css", position = "static-top"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "speciesInput", label = "Species",
                  choices = c("Caribou", "Elk", "Mountain Goat", "Moose", "Mountain Sheep", 
                              "Mule Deer", "Bison", "Turkey" ), multiple = F, selected = "Moose"),
      selectInput(inputId = "classInput", label = "Animal Class",
                  choices = c( "Bull Only", "Spike-fork/Tripalm/10 point", "Cow Or Calf Only", "Calf Only"), 
                  multiple = T, selected = c( "Bull Only", "Spike-fork/Tripalm/10 point")),
      selectInput(inputId = "regionInput", label = "Region", multiple = T,
                  choices = c("All", "1", "2", "3", "4", "5","6", "7A", "7B", "8"),
                  selected = "All"),
      textInput(inputId = "successInput", label = "Minimum Hunter Success (%)", value = ""),
      helpText("Enter as a number between 0 and 100"),
      textInput(inputId = "oddsInput", label = "Max Draw Odds", value = ""),
      helpText("Enter as a number (e.g. enter `10` to remove hunts with higher than 10:1 draw odds)"),
      dateInput("inputDate", "Date Filter", format = "MM-d", min = "2024-08-01", max = "2025-03-31"),
      helpText("If desired, enter a date to only see hunts that include the specified date"),
      checkboxInput(inputId = "inputBOS", label = "Include Bow Only Hunts", value = T),
      checkboxInput(inputId = "inputYOS", label = "Include Youth Only Hunts", value = T),
      selectInput(inputId =  "filterInput", label = "Statistic to Map", choices = c("Hunter Success", "Draw Odds")),
      # selectInput(inputId =  "filterInput", label = "Statistic to Map", choices = c("Hunter Success")),
      br(),
      width = 3
    ),
    mainPanel(
      helpText("Use caution when examining hunts that are described as having limited data. Estimates of 
              hunter sucess from these hunts may be unreliable. Averages are calculated using up to 5 years of data,
              wherever possible."),
        DT:: dataTableOutput("table", width = 500),
              uiOutput("maptitle"),
              br(),
              helpText("LEH Polygons may take a moment to display, please be patient. "),
              leafletOutput("coolmap", height = 500),
              br(),
              actionButton("inputmaphelp", "Learn more about this map"),
              br()
    )
  ),
  ## add this chunk for the footer =================================
  column(width = 12,
         style = "background-color:#003366; border-top:2px solid #fcba19;",
         tags$footer(class="footer",
                     tags$div(class="container", 
                      style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                          tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; 
                            align-items:center; height:100%;",
                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", 
                                            style="font-size:1em; font-weight:normal; color:white; padding-left:5px; 
                                   padding-right:5px; border-right:1px solid #4b5e7e;")),
                                   tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", 
                                             style="font-size:1em; font-weight:normal; color:white; padding-left:5px;
                                   padding-right:5px; border-right:1px solid #4b5e7e;")),
                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", 
                                            style="font-size:1em; font-weight:normal; color:white; padding-left:5px; 
                                   padding-right:5px; border-right:1px solid #4b5e7e;")),
                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility",
                                            style="font-size:1em; font-weight:normal; color:white; padding-left:5px; 
                                  padding-right:5px; border-right:1px solid #4b5e7e;")),
                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", 
                                             style="font-size:1em; font-weight:normal; color:white; padding-left:5px; 
                                  padding-right:5px; border-right:1px solid #4b5e7e;")),
                              tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", 
                                            "Contact", 
                                                style="font-size:1em; font-weight:normal; color:white; padding-left:5px; 
                                            padding-right:5px; border-right:1px solid #4b5e7e;"))
                              ) ) ) )
  ## end of footer chunk =========================================
)
# ,

server <- function(input, output, session){
    showModal(modalDialog(title = "Disclaimer",
      "This tool has been built to provide a user friendly interface to British Columbia Limited Entry
      Hunt data, and to help hunters search for LEH hunts based on specified criteria. The data shown in this tool
      is only reflective of prior years, and this tool is not in any way predictive of future outcomes. This tool
      is for information purposes only. Please consult the regulations before making any LEH applications."
    ))

  observe({
    updateSelectInput(session, 'classInput', choices = Class[input$speciesInput],
                      selected = Class[input$speciesInput][[1]][1])
  })
  
  observeEvent(input$inputmaphelp,{
    showModal(modalDialog(
      title = "Map Information", 
      HTML(
      "LEH hunts are displayed as grey in this map when we do not yet have historical data available for
      this specific hunt, as is the case for new LEH hunts. Many estimates of hunter success for sheep and goats
      are not yet available but will become available in future years. This is because we estimated harvest 
      using Compulsory Inspection and did not develop estimates of hunter effort for each LEH hunt until 2023.
      <br>
      <br>
      In many cases there are multiple hunts within the same LEH Hunt Area. When this happens, 
      this mapping tool only displays the hunt code with either the highest estimated hunter kill rate,
      or the lowest draw odds. You can modify this using the `Statistic to Map` dropdown on the left hand side.
      The full list of available hunts can be found in the table above.")
    ))
  })
  
  updateDateInput(session = session, inputId = "inputDate", value = NA)

  inputs <- reactive({
    species <- input$speciesInput
    if(is.null(species)) return()
    if(all(is.null(input$classInput))) return()
    if(!all(input$classInput %in% Class[species][[1]])) return() 

    region <- input$regionInput
    if("All" %in% region) region <- c("1", "2", "3", "4", "5", "6", "7A", "7B", "8")
    if(is.null(region)) return()
    
    if(input$successInput != "") success.filter <- as.numeric(input$successInput) else({
      success.filter <- 0
    })
    if(input$oddsInput != "") odds.filter <- as.numeric(input$oddsInput) else({
      odds.filter <- 10000
    })
    if(odds.filter == 0) odds.filter <- 10000

    animalclass <- input$classInput
    if(species == "Moose" & any(animalclass == "Bull")){
      animalclass <- c(animalclass, "Bull Only", "Spike-fork/Tripalm/10 point")
    }
    if(all(is.null(animalclass))) animalclass <- Class[[which(specieslist == species)]]

    date <- suppressWarnings(ymd(input$inputDate))
    if(length(date) == 0) date <- NA
    
    params <- list()
    params[[1]] <- species
    params[[2]] <- NULL
    params[[3]] <- region
    params[[4]] <- success.filter
    params[[5]] <- odds.filter
    params[[6]] <- animalclass
    params[[7]] <- input$filterInput
    params[[8]] <- input$inputBOS
    params[[9]] <- date
    params[[10]] <- input$inputYOS

    params
  })
 
  table <- reactive({
    if(is.null(inputs())) return()
    
    params <- inputs()
    species <- params[[1]]
    region <- params[[3]]
    success.filter <- params[[4]]
    odds.filter <- params[[5]]
    animalclass <- params[[6]]
    bos <- params[[8]]
    date <- params[[9]]
    yos <- params[[10]]

    if(species %in% c("Mountain Sheep", "Mountain Goat", "Turkey")){
      showNotification("Some estimates of hunter success are not available for this species, use with caution")
    }
    
    tt <- subset(wild, Species == species) %>%
      subset(Region %in% region) %>%
      subset(AnimalClass %in% animalclass) %>%
      subset(DrawOdds <= odds.filter)
    tt <- subset(tt, HunterSuccess >= success.filter | is.na(tt$HunterSuccess))
    if(!bos) tt <- subset(tt, BowOnly == "No")
    if(!yos) tt <- subset(tt, YouthOnly == "No")
    
    tt$HunterSuccess <- cut(tt$HunterSuccess, breaks = c(-1, 10, 20, 30, 40, 50, 100),
                            c("Under 10%", "10-20%", "20-30%", "30-40%", "40-50%", "Over 50%"))
    
    if(!is.na(date)){
      temp <- lapply(seasons, function(x) date %within% x) %>%
        unlist()
      code.in <- names(temp)[temp]
      tt <- subset(tt, Code %in% code.in); rm(temp, code.in)
    }
    
    names(tt) <- c("2024 Hunt Code", "Species", "Hunt Area", "Region", "Season Dates", "Animal Class", 
                   "Draw Odds (Average)", "Average Estimated Hunter Kill Rate", "Bow Only Season",
                  "Youth Only Season", "Limited Data")
    
    tt$`2024 Hunt Code` <- as.numeric(tt$`2024 Hunt Code`)

    if(nrow(tt) == 0){
      showNotification("No LEH hunts fit the selected filters", duration = 10)
      return()
    }

    tt
  })

  map.leh <- reactive({
    if(is.null(inputs())) return()
    if(is.null(table())) return()
    if(nrow(table()) == 0) return()
    
    params <- inputs()
    species <- params[[1]]
    region <- params[[3]]
    success.filter <- params[[4]]
    odds.filter <- params[[5]]
    
    data <- table()
    names(data) <- c("Code", "Species", "Area", "Region", "Season", "AnimalClass", 
                     "DrawOdds","HunterSuccess", "BowOnlySeason", "YouthOnlySeason", "LimitedData")
    
    leh <- subset(leh.boundaries, Species == unique(data$Species)) %>%
      subset(LEH %in% paste(data$Area))
    leh$num <- 1:nrow(leh)
    leh$HuntNumber <- 0
    leh$Season <- NA
    # leh$TentativeAuthorizations <- NA
    leh$HunterSuccess <- NA
    leh$DrawOdds <- NA
    # leh$HuntCode <- NA
    
    data$leh.num <- 0
    for(d in 1:nrow(data)){
      temp <- subset(leh, LEH == data$Area[d])
      if(nrow(temp) > 0) {
        data$leh.num[d] <- temp$num
      }
    }
    
    if(input$filterInput == "Draw Odds") data$stat <- data$DrawOdds
    if(input$filterInput == "Hunter Success") data$stat <- data$HunterSuccess %>% as.character()
    data$stat[is.infinite(data$stat)] <- NA
    
    for(i in 1:nrow(leh)){
      temp <- subset(data, leh.num == leh$num[i])
      leh$HuntNumber[i] <- nrow(temp)
      if(leh$HuntNumber[i] > 0) {
        temp <- temp[order(temp$stat),]
        if(input$filterInput == "Draw Odds") {
          temp <- temp[order(temp$DrawOdds),][1,]
        }
          # temp <- subset(temp, DrawOdds == min(DrawOdds, na.rm = T))
        if(input$filterInput == "Hunter Success") {
          if(!all(is.na(temp$HunterSuccess))){
            temp <- temp[order(temp$HunterSuccess, decreasing = T),][1,]
            # temp <- subset(temp, HunterSuccess == max(HunterSuccess, na.rm = T))
          }
        }
        leh$Season[i] <- temp$Season[1]
        # leh$TentativeAuthorizations[i] <- unique(temp$TentativeAuthorizations)[1]
        leh$HunterSuccess[i] <- temp$HunterSuccess[1] %>% as.character()
        leh$DrawOdds[i] <- temp$DrawOdds[1]
        # leh$HuntCode[i] <- temp$Code[1]
        leh$BowOnlySeason[i] <- temp$BowOnlySeason[1]
        leh$YouthOnlySeason[i] <- temp$YouthOnlySeason[1]
        leh$LimitedData[i] <- temp$LimitedData[1]
        leh$stat[i] <- temp$stat[1]
      }
    }
   leh 
  })
  
  tableOut <- reactive({
    if(is.null(table())) return()
    tt <- table()
    tt <- tt[,2:ncol(tt)]
    tt$`Draw Odds (Average)` <- paste(tt$`Draw Odds (Average)`, ":", 1, sep = " ") 
    tt$`Average Estimated Hunter Kill Rate` <- tt$`Average Estimated Hunter Kill Rate` %>%
      as.character()
    tt$`Average Estimated Hunter Kill Rate`[is.na(tt$`Average Estimated Hunter Kill Rate`)] <- "Unknown"
    tt$`Average Estimated Hunter Kill Rate` <- factor(tt$`Average Estimated Hunter Kill Rate`,
                              levels = c("Unknown", "Under 10%", "10-20%", "20-30%", "30-40%", "40-50%", "Over 50%"))
    tt
  })
  
  output$table <- DT:: renderDataTable(datatable(tableOut(), rownames = F))
  output$coolmap <- renderLeaflet({
    leaflet(regions) %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      addPolylines(weight = 0, stroke = F, color = ~"grey", fillOpacity = 1)
  })
  
  
  observeEvent(inputs(), {
    if(is.null(table())) return()
    tt <- map.leh()
    labs <- subset(labels, Species %in% unique(tt$Species)) %>%
      subset(LEH %in% tt$LEH)
    
    if(input$filterInput == "Hunter Success") {
      tt$stat <- factor(tt$stat,  levels = c("Under 10%", "10-20%", "20-30%", "30-40%", "40-50%", "Over 50%"))
    }
    
    tt.na <- subset(tt, is.na(tt$stat))
    if(nrow(tt.na) > 0) {
      tt.na$stat <- factor("Historical Data Not Yet Available")
      tt.na$lab <- paste(tt.na$LEH, "Historical data not yet available", sep = " - ")
      pal.na <- colorFactor(palette = "grey", domain = tt.na$stat, na.color = "grey")
    }
    
    tt <- subset(tt, !is.na(tt$stat))
    if(nrow(tt) > 0){
      tt$DrawOdds <- paste(tt$DrawOdds, ":", 1, sep = " ")
    }
    
    if(input$filterInput == "Draw Odds") {
      tt$stat <- cut(tt$stat, breaks = c(-1, 1.999, 4.9999, 9.9999, 19.999, 39.9999, 100),
          c("Under 2:1", "2:1 to 5:1", "5:1 to 10:1", "10:1 to 20:1", "20:1 to 40:1", "Over 40:1"))
      pal <- colorFactor(palette = "Greens", domain = tt$stat, na.color = "grey", reverse = T)
    } else({
      pal <- colorFactor(palette = "Greens", domain = tt$stat, na.color = "grey", reverse = F)
    })


    leafletProxy("coolmap") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()
    
    if(nrow(tt.na) > 0) {
      leafletProxy("coolmap") %>%
        addPolygons(data = tt.na, weight = 1.5, stroke = T, 
                    color = "grey", fillColor = "grey",
                    fillOpacity = 0.5, popup = ~ popupTable(tt.na, zcol = c(2, 7:11), row.numbers = F, feature.id = F),
                    highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = T),
                    label = ~lab) %>%
        addLegend(data = tt.na, position = "bottomright", pal = pal.na, values = ~stat, title = NULL)
    }

    leafletProxy("coolmap") %>%
      addPolygons(data = tt, weight = 1.5, stroke = T, color = "grey", fillColor = ~pal(stat),
                  fillOpacity = 0.5, popup = ~ popupTable(tt, zcol = c(2, 7:11), row.numbers = F, feature.id = F),
                  highlightOptions = highlightOptions(color = "blue",weight = 2, bringToFront = T),
                  label = ~LEH) %>%
      addLabelOnlyMarkers(data = labs, label = ~LEH, labelOptions = labelOptions(noHide = T, textOnly = TRUE),
                        group = "Labels") %>%
      groupOptions("Labels", zoomLevels = 7:9)
    
    if(input$filterInput == "Draw Odds"){
      leafletProxy("coolmap") %>%
        addLegend(data = tt, position = "bottomright", pal = pal, values = ~stat, title = "Avg. Odds")
    } else({
      leafletProxy("coolmap") %>%
        addLegend(data = tt, position = "bottomright", pal = pal, values =~stat, title = "Hunter Success")
    })
  })
   
}

#   )
#   
#   runApp(sApp)
# })
# profvis::profvis(runApp(shinyApp(ui, server)))
shinyApp(ui, server)