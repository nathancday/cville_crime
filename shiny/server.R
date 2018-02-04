

library(shiny)
library(leaflet)
library(magrittr)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    
    
    grd <- eventReactive(input$render, {
            grd <- st_make_grid(census, n = input$num_squares)
            grd %<>% st_intersection(st_union(census))
            
            crime$grd <- st_within(crime, grd) %>% as.numeric()
            
            grd_sum <- st_set_geometry(crime, NULL) %>%
                group_by(grd, drug_flag) %>%
                count() %>%
                spread(drug_flag, n) %>% rowwise() %>%
                mutate(total = sum(drugs, not_drugs),
                       frac_drugs = drugs/total)
            
            grd_sum %<>% mutate_at(vars(drugs:frac_drugs), funs(ifelse(is.na(.), 0, .)))
            
            grd %<>% st_sf() %>% set_names("geometry") %>%
                mutate(grd = 1:nrow(.)) %>% 
                full_join(grd_sum) %>%
                rename(id = grd)
        
        return(grd)
        
    })
   
    box <- st_make_grid(census, n = 1) %>% st_bbox()
    
    pal <- colorNumeric("viridis", census$frac_drugs)
    output$map <- renderLeaflet({
        leaflet(grd(), options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
              addProviderTiles("CartoDB.Positron") %>%
              addPolygons(fillColor = ~pal(frac_drugs), opacity = 1, weight = .5, color = "black",
                          popup = ~paste0(round(frac_drugs, 3), "<br>",
                                          drugs),
                          layerId = ~id)
        })
    output$test <- renderPrint(input$map_shape_click)
    
})
