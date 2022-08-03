# -------------------------------------------------------------------------
# Title: reads_app.R
# Purpose: Create shiny dashboard based on data from reads_scrape.R
# Author: Jacob Allen
# Created: May 30, 2022
# Modified: August 3, 2022
# -------------------------------------------------------------------------

# Install and load packages -----------------------------------------------

# install.packages("shinythemes")
# install.packages("thematic")
# install.packages("shinydashboard")
# install.packages("plotly")
# install.packages("gghighlight")
# install.packages("ggrepel")

library(shiny)
library(shinythemes)
library(shinydashboard)
library(thematic)
library(plotly)
library(ggplot2)
library(gghighlight)
library(ggrepel)

# Read data
final <- readRDS(("...\\reads.rds"))

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cyborg"), # darkly # slate # cyborg
    titlePanel("I Like to Read"),
    mainPanel(
        fluidRow(box(valueBoxOutput("total_book_count"), width = 4),
                 box(valueBoxOutput("total_rereads"), width = 4),
                 box(valueBoxOutput("total_page_count"), width = 4)
                ),
        fluidRow(tabBox(
                    id = "tabset1", 
                    tabPanel("Books", plotOutput("book_ct"), click = "book_ct_click"),
                    tabPanel("Pages", plotOutput("pages")),
                    width = 4
                        ),
                 tabBox(
                     tabPanel("Books", plotOutput("read_cuml")),
                     tabPanel("Pages", plotOutput("pgs_cuml")),
                     width = 4
                       ),
                 tabBox(
                     tabPanel("Books", plotOutput("month_ct")),
                     tabPanel("Pages", plotOutput("month_pgs")),
                     width = 4
                        )
                 ),
        fluidRow(
                 column(4, plotOutput("days"), style = "padding-top:5px; padding-bottom:5px"),
                 column(2, plotOutput("rating"), style = "padding-top:5px; padding-bottom:5px"),
                 column(2, plotOutput("rating_comp"), style = "padding-top:5px; padding-bottom:5px"),
                 column(4, plotOutput("pub_yr"), style = "padding-top:5px; padding-bottom:5px")
                 ),
        fluidRow(column(3, plotOutput("genre"), style = "max-height:400px; overflow-y: scroll; position: relative"),
                 column(3, h5("Most Popular Books Read"), tableOutput("most_pop")),
                 column(6, h5("Least Popular Books Read"), tableOutput("least_pop"))
                 ),
        fluidRow(#column(4, plotlyOutput("sex"), click = "sex_click"),
                 column(4, plotOutput("sex")),
                 column(4, plotOutput("country"), height = "500px", style = "height:400px; overflow-y: scroll"),
                 column(4, plotOutput("authors"))
                 ),
        fluidRow(
            selectInput("year", label = "Year", choices = as.list(unique(final$date_finish_yr))),
                ),
        fluidRow(
            tableOutput("info"))
                ),
        style = "width: 2500px;"
            )


# Define server logic 
server <- function(input, output, session) {
    
    thematic::thematic_shiny()
    
    # Create ggplot themes
    dark_mode <- theme(
        plot.title = element_text(color = "white", size = 20, face = "bold"),
        text = element_text(color = "White"),
        axis.text = element_text(color = "white"),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"),
        legend.key = element_blank()
    )

    dark_mode_no_x <- theme(
        plot.title = element_text(color = "white", size = 20, face = "bold"),
        text = element_text(color = "White"),
        axis.text = element_text(color = "white"),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_blank()
    )
    
    dark_mode_no_y <- theme(
        plot.title = element_text(color = "white", size = 20, face = "bold"),
        text = element_text(color = "White"),
        axis.text = element_text(color = "white"),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.text.y = element_blank()
    )
    
    # Update geom defaults
    update_geom_defaults("bar", list(fill = "sky blue"))
    
    # Genre
    genre <- final %>%             
        group_by(book_genre) %>% 
        summarize(n = n())
    
    output$genre <- renderPlot({
         genre %>% 
            ggplot(aes(x = reorder(book_genre, (n)), y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Books by Genre",
                 caption = "Genre voted on by Goodreads user votes. Lightly cleaned") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 2)) +
            coord_flip() +
            dark_mode_no_x
    })
    
    # Sex (Distinct Author)
    sex <- final %>% 
        distinct () %>% 
        group_by(author_sex) %>% 
        summarize(n = n())
 
    output$sex <- renderPlot({
        sex %>%
            ggplot(aes(x = reorder(author_sex, (n)), y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Author Gender",
                 caption = "Via Wikidata") +
            geom_text(aes(label = n),
                      color = "white",
                      stat = "identity",
                      position = position_nudge(y = 3)) +
            coord_flip() +
            dark_mode_no_x
    })

    # Country
    country <- final %>% 
        group_by(author_country) %>% 
        summarize(n = n())
    
    output$country <- renderPlot({
        country %>% 
        ggplot(aes(x = reorder(author_country, (n)), y = n)) +
        geom_bar(stat = "identity") +
        labs(title = "Author Birth Country",
                 caption = "Via Wikidata") +
        geom_text(aes(label = n), 
                  color = "white", 
                  stat = "identity", 
                  position = position_nudge(y = 3)) +
        coord_flip() +
        dark_mode_no_x
    })
    
    # Avg days to read
    days <- final %>% 
        filter(date_finish > "2015-01-01") %>% 
        group_by(date_finish_yr) %>% 
        summarize(avg_days_to_read = round(mean(days_to_read), 0))
        
    output$days <- renderPlot({
        days %>% 
            ggplot(aes(x = date_finish_yr, y = avg_days_to_read)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Days to Read by Year") +
            geom_text(aes(label = avg_days_to_read), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 2)) +      
            coord_flip() +
            dark_mode_no_x
    })
    
    # Read count by year
    book_ct <- final %>% 
        group_by(date_finish_yr) %>% 
        summarize(n = n()) %>% 
        na.omit()
    
    output$book_ct <- renderPlot({
        book_ct %>% 
            ggplot(aes(x = date_finish_yr, y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Books Read by Year") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 1)) + 
            coord_flip() +
            dark_mode_no_x
    })
    
    # Page count by year
    pages <- final %>% 
        group_by(date_finish_yr) %>% 
        summarize(n = sum(pages)) %>% 
        na.omit()
    
    output$pages <- renderPlot({
        pages %>% 
            ggplot(aes(x = date_finish_yr, y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Pages Read by Year") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 0)) + 
            coord_flip() +
            dark_mode_no_x
    })

    # Rating
    rating <- final %>% 
        group_by(my_rating) %>% 
        summarize(n = n())
    
    output$rating <- renderPlot({
        rating %>% 
            ggplot(aes(x = my_rating, y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "My Ratings") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 2)) +
            dark_mode_no_y
    })
    
    # Book publish year
    pub_yr <- final %>% 
        mutate(yr_pub_group = floor(yr_pub/100) * 100,
               yr_pub_group = ifelse(yr_pub_group < 0, -1000, yr_pub_group),
               yr_pub_group = ifelse(yr_pub_group < 1001 & yr_pub_group >= 0, 1000, yr_pub_group)) %>% 
        group_by(yr_pub_group) %>% 
        summarize(n = n()) %>% 
        na.omit()
    
    output$pub_yr <- renderPlot({
        pub_yr %>% 
            ggplot(aes(x = as.character(yr_pub_group), y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Books Read by Publish Year") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 2)) +
            dark_mode_no_y
    })
    
    # Books read by month
    month_ct <- final %>% 
        filter(date_finish > "2015-01-01") %>% 
        group_by(date_finish_month) %>% 
        summarize(n = n())
    
    output$month_ct <- renderPlot({
        month_ct %>% 
            ggplot(aes(x = date_finish_month, y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Books Read by Month") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 1)) +
            dark_mode_no_y
    })
    
    # Pages read by month
    month_pgs <- final %>% 
        filter(date_finish > "2015-01-01") %>% 
        group_by(date_finish_month) %>% 
        summarize(n = sum(pages))
    
    output$month_pgs <- renderPlot({
        month_pgs %>% 
            ggplot(aes(x = date_finish_month, y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Pages Read by Month") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 10)) +
            dark_mode_no_y
    })
    
    # Most read authors
    authors <- final %>% 
        group_by(author) %>% 
        summarize(n = n()) %>% 
        top_n(10)
    
    output$authors <- renderPlot({
        authors %>% 
            ggplot(aes(x = reorder(author, n), y = n)) +
            geom_bar(stat = "identity") +
            labs(title = "Most Read Authors") +
            geom_text(aes(label = n), 
                      color = "white", 
                      stat = "identity", 
                      position = position_nudge(y = 1)) +
            coord_flip() +
            dark_mode_no_x
    })
    
    # Summary of my rating vs average user
    rating_comp <- final %>% 
        group_by(my_rating_vs_gr) %>% 
        summarize(n = n()) %>% 
        na.omit()
    
    output$rating_comp <- renderPlot({
        rating_comp %>% 
        ggplot(aes(x = my_rating_vs_gr, y = n)) +
        geom_bar(stat = "identity") +
            labs(title = "My Ratings vs Average",
                 caption = "Avg Goodreads rounded rating") +
        scale_x_discrete(limits = c("lower", "equal", "higher")) +
        geom_text(aes(label = n), 
                  color = "white", 
                  stat = "identity", 
                  position = position_nudge(y = 2)) +
        dark_mode_no_y
    })
    
    # Cumulative book read count by month
    read_cuml <- final %>% 
        filter(date_finish > "2015-01-01") %>% 
        group_by(date_finish_yr, date_finish_month) %>% 
        summarize(n = n()) %>% 
        mutate(cuml = cumsum(n))
    
    output$read_cuml <- renderPlot({
        read_cuml %>% 
        ggplot(aes(x = date_finish_month, y = cuml, group = date_finish_yr, color = "blue")) +
        labs(title = "Cumulative Books Read by Year") +
        geom_line() +
        gghighlight(as.numeric(date_finish_yr) == year(Sys.Date())) +
        dark_mode
    })
    
    # Cumulative book page read count by month
    pgs_cuml <- final %>% 
        filter(date_finish > "2015-01-01") %>% 
        group_by(date_finish_yr, date_finish_month) %>% 
        summarize(sum = sum(pages)) %>% 
        mutate(cuml = cumsum(sum))
    
    output$pgs_cuml <- renderPlot({
        pgs_cuml %>% 
        ggplot(aes(x = date_finish_month, y = cuml, group = date_finish_yr, color = "blue")) +
        labs(title = "Cumulative Pages Read by Year") +
        geom_line() +
        gghighlight(as.numeric(date_finish_yr) == year(Sys.Date())) +
        dark_mode
    })

    # Most popular books read
   output$most_pop <- renderTable({
       most_pop <- final %>% 
        select(c(title, ratings)) %>% 
        mutate(ratings = format(round(ratings, 1), big.mark = ",")) %>% 
        arrange(desc(ratings)) %>% 
        top_n(10)
    })
   
   # Least popular books read
   output$least_pop <- renderTable({
       least_pop <- final %>% 
           select(c(title, ratings)) %>% 
           mutate(ratings = format(round(ratings, 1), big.mark = ",")) %>% 
           arrange(ratings) %>% 
           top_n(-10)
   })

    # Count of books
    output$total_book_count <- renderValueBox({
        valueBox(
            nrow(final),
            "Total Books Read", 
            color = "blue"
            )
    })
    
    # Count of books reread
    output$total_rereads <- renderValueBox({
        valueBox(
            nrow(final) - n_distinct(final$title)
            ,"Total Re-reads", 
            color = "blue"
        )
    })
    
    # Count of pages
    output$total_page_count <- renderValueBox({
        valueBox(
            format(sum(final$pages), big.mark=","),
            "Total Pages Read",
            color = "blue"
        )
    })
    
    # Book info table
    output$info <- renderTable({
        info <-
            final %>%
            filter(input$year == date_finish_yr) %>%
            mutate(rating_diff = my_rating - avg_rating_rounded,
                   date_start = as.character(date_start),
                   date_finish = as.character(date_finish),
                   my_rating = as.character(my_rating),
                   rating_diff = as.character(rating_diff),
                   pages = as.character(pages),
                   yr_pub = as.character(yr_pub)) %>%
            select(c(title, author, book_desc, book_genre, my_rating, rating_diff, review, date_start, date_finish, pages, yr_pub))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
