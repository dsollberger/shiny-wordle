library(shiny)

source("wordlist.R")

ui = fluidPage(
  textInput("guess", "Type in a guess"),
  actionButton("go", "Submit Guess"),
  verbatimTextOutput("result", placeholder = TRUE)
)

set.seed(as.integer(Sys.Date())) #same word for everyone each day?
#target <- sample(words_common, 1)
target <- "dread"

server <- function(input, output){
  
  
  output$result <- renderText({
    # if(nchar(input$guess) != 5){ #an earlier draft
    if(!(input$guess %in% words_all)){
      #return("") #returned nothing #an earlier draft
      req(FALSE, cancelOutput = TRUE)
    }
    #paste("You Entered: ", input$guess) #an earlier draft
    check_report <- check_words(target, input$guess)
    format_report(check_report)
  }) |>
    bindEvent(input$go)
}


compare_words <- function(target_string, guess_string){
  # TO DO: more than just check if target == guess
  # TO DO: return correct letters and/or correct positions
  
  if(nchar(target_string) != nchar(guess_string)){
    stop("Target and guess strings must have the same length.")
  }
  
  target_vec <- stringr::str_split(target_string, "")[[1]]
  guess_vec  <- stringr::str_split(guess_string, "")[[1]]
  result_vec <- rep("", 5)
  
  for(letter in seq_along(target_vec)){
    if(guess_vec[letter] == target_vec[letter]){
      result_vec[letter] <- "correct"
    }else if(guess_vec[letter] %in% target_vec){
      result_vec[letter] <- "in_word"
    }else{
      result_vec[letter] <- "not_in_word"
    }
  }
  
  #return
  result_vec
}

check_words <- function(target_string, guess_string){
  compare_result <- compare_words(target_string, guess_string)
  correct <- FALSE
  if(all(compare_result == "correct")){
    correct <- TRUE
  }
  
  #return
  list(
    guess = guess_string,
    guess_letters = stringr::str_split(guess_string, "")[[1]],
    result = compare_result,
    correct = correct
  )
}

format_report <- function(check_report){
  out_string <- rep("", 5)
  for(letter in seq_along(check_report$guess_letters)){
    if(check_report$result[letter] == "correct"){
      out_string[letter] <- paste0("[", check_report$guess_letters[letter], "]")
    }else if(check_report$result[letter] == "in_word"){
      out_string[letter] <- paste0("(", check_report$guess_letters[letter], ")")
    }else{
      out_string[letter] <- check_report$guess_letters[letter]
    }
  }
  out_string
}

shinyApp(ui, server)