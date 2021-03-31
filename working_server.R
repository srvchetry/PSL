## server.R  -- rename file name

# load functions


source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures
source('functions/helpers.R') # similarity measures

## get_user_ratings adopted from book recommender

get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"),
                     function(x)
                       ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]

  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$MovieID,
                               j = rep(1,nrow(dat)),
                               x = dat$Rating,
                               dims = c(nrow(ratingmat), 1))
}


###############System1###################################################

# genre based recommendation

get_movie_genre  = function(InputGenre = "Action") {
  recom = ratings1 %>%
    group_by(MovieID) %>%
    summarize(ratings_per_movie = n(),
              ave_ratings = round(mean(Rating), dig = 3)) %>%
    inner_join(movies, by = 'MovieID') %>%
    filter(ratings_per_movie > 1000) %>%
    filter(grepl(InputGenre, Genres)) %>%
    top_n(10, ave_ratings) %>% # select top 10 highest average rated movies for the selected genre. ###### Approach 1
    select('MovieID',
           'Title',
           'ave_ratings',
           'ratings_per_movie',
           'Genres') %>%
    arrange(desc(ave_ratings))
  #return(recom)
  # arrange(desc(ave_ratings)) %>%
  # datatable(class = "nowrap hover row-border",
  #           escape = FALSE,
  #           options = list(dom = 't',
  #                          scrollX = TRUE, autoWidth = TRUE))
}

#################System1################################################

# read in data

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))



# Ratings data from 

ratings1 = read.csv(
  paste0(myurl, 'ratings.dat?raw=true'),
  sep = ':',
  colClasses = c('integer', 'NULL'),
  header = FALSE
)
colnames(ratings1) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings1$Timestamp = NULL
colnames(ratings1) = c('UserID', 'MovieID', 'Rating')
ratings1[is.na(ratings1)] = 0  ## assigning zeros to NA's in the ratings dataframe

# reshape to movies x user matrix adapted from book recommender   *********************************************

ratingmat <- sparseMatrix(ratings1$MovieID, ratings1$UserID, x=ratings1$Rating) # book x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
#ratingmat[is.na(ratingmat)] <- 2.5
dimnames(ratingmat) <- list(MovieID = as.character(1:3952), UserID = as.character(sort(unique(ratings1$UserID))))


shinyServer(function(input, output, session) {

  # show the movies to be rated based on user ratings
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = movies$image_url[(i - 1) * num_movies + j], height = 150
          )),
          #div(style = "text-align:center; color: #999999; font-size: 80%", movies$authors[(i - 1) * num_movies + j]),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the submit button is clicked
  df_ratings <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)

      rmat <- cbind(user_ratings, ratingmat)

      # get the indices of which cells in the matrix should be predicted
      # predict all books the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      # run the ubcf-alogrithm
      res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
      user_results <- sort(res[,1], decreasing = TRUE)[1:10]
      user_predicted_ids <- as.numeric(names(user_results))
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = user_predicted_ids, 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)

    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$genreresults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    selGen = input$selectedGenre  ##new
    print(selGen) ##new
    #recom_result <- df_genre()
    recom_result <- get_movie_genre(selGen)  ##new
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  
  # display the recommendations
  output$ratingresults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_ratings()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          
          div(style = "text-align:center",
              a(
                img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
              )),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
          
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function