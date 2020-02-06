library(ggvis)
library(dplyr)
if (FALSE) {
    library(RSQLite)
    library(dbplyr)
}

# Set up handles to database tables on app start
#db <- src_sqlite("movies.db")
#omdb <- tbl(db, "omdb")
#tomatoes <- tbl(db, "tomatoes")

# Join tables, filtering out those with <10 reviews, and select specified columns
#all_movies <- inner_join(omdb, tomatoes, by = "ID") %>%
 #   filter(Reviews >= 10) %>%
 #   select(ID, imdbID, Title, Year, Rating_m = Rating.x, Runtime, Genre, Released,
  #         Director, Writer, imdbRating, imdbVotes, Language, Country, Oscars,
  #         Rating = Rating.y, Meter, Reviews, Fresh, Rotten, userMeter, userRating, userReviews,
  #         BoxOffice, Production, Cast)
all_ecosystems <- comp

function(input, output, session) {
    
    # Filter the movies, returning a data frame
    ecosystems <- reactive({
        # Due to dplyr issue #318, we need temp variables for input values
        difference <- input$diff * 1000000
        # oscars <- input$oscars
        #minyear <- input$year[1]
        #maxyear <- input$year[2]
        minCurAcres <- input$curAcres[1] * 1e6
        maxCurAcres <- input$curAcres[2] * 1e6
        
        # Apply filters
        e <- all_ecosystems %>%
            filter(
                DIFF >= difference,
                #Oscars >= oscars,
                #Year >= minyear,
                #Year <= maxyear,
                ACRES_CURR >= minCurAcres,
                ACRES_CURR <= maxCurAcres
            ) %>%
            arrange(ACRES_CURR)
        
        # Optional: filter by genre
        #if (input$genre != "All") {
        #   genre <- paste0("%", input$genre, "%")
        #   m <- m %>% filter(Genre %like% genre)
        #}
        # Optional: filter by director
        #if (!is.null(input$director) && input$director != "") {
        #   director <- paste0("%", input$director, "%")
        #   m <- m %>% filter(Director %like% director)
        #}
        # Optional: filter by cast member
        #if (!is.null(input$cast) && input$cast != "") {
        #   cast <- paste0("%", input$cast, "%")
        #   m <- m %>% filter(Cast %like% cast)
        #}
        
        
        e <- as.data.frame(e)
        
        # Add column which says whether the movie won any Oscars
        # Be a little careful in case we have a zero-row data frame
        #m$has_oscar <- character(nrow(m))
        #m$has_oscar[m$Oscars == 0] <- "No"
        #m$has_oscar[m$Oscars >= 1] <- "Yes"
        #m
    })
    
    # Function for generating tooltip text
    movie_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$CLASSNAME)) return(NULL)
        
        # Pick out the movie with this ID
        all_ecosystems <- isolate(ecosystems())
        ecosystem <- all_ecosystems[all_ecosystems$CLASSNAME == x$CLASSNAME, ]
        
        paste0("<b>", ecosystem$CLASSNAME, "</b>",
               ecosystem$CURR_ACRES, "<br>",
               format(ecosystem$ACRES_CURR, big.mark = ",", scientific = FALSE), " current acres",
               "<br>", format(ecosystem$ACRES_HIST, big.mark = ",", scientific = FALSE), " historical acres",
               "<br>", format(ecosystem$DIFF, big.mark = ",", scientific = FALSE), " difference in acres"
        )
    }
    
    # A reactive expression with the ggvis plot
    vis <- reactive({
        # Lables for axes
        
        
        # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
        # but since the inputs are strings, we need to do a little more work.
        xvar <- prop("x", as.symbol("ACRES_CURR"))
        yvar <- prop("y", as.symbol("DIFF"))
        
        ecosystems %>%
            ggvis(x = xvar, y = yvar) %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                         key := ~CLASSNAME) %>%
            add_tooltip(movie_tooltip, "hover") %>%
            add_axis("x", title = "Current Acres") %>%
            add_axis("y", title = "Current to Historical Difference") %>%
            add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
            scale_nominal("stroke", domain = c("Yes", "No"),
                          range = c("orange", "#aaa")) %>%
            set_options(width = 500, height = 500)
    })
    
    vis %>% bind_shiny("plot1")
    
    output$n_ecosystems <- renderText({ nrow(ecosystems()) })
}