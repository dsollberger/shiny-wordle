library(shiny)

source("wordlist.R")

ui = fluidPage(
  textInput("guess", "Type in a guess"),
  actionButton("go", "Submit Guess"),
  verbatimTextOutput("result", placeholder = TRUE)
)

set.seed(as.integer(Sys.Date())) #same word for everyone each day?
#target <- sample(words_common, 1)
target <- "creek"

server <- function(input, output){
  
  all_guesses <- character()
  
  output$result <- renderText({
    user_guess <- stringr::str_to_lower(input$guess)
    if(!(user_guess %in% words_all)){
      req(FALSE, cancelOutput = TRUE)
    }
    
    all_guesses <<- c(all_guesses, user_guess)
    
    out_str <- vapply(all_guesses, function(guess) {
      result <- check_words(target, guess)
      format_report(result)
    }, character(1))
    
    paste(out_str, collapse = "\n")
  }) |>
    bindEvent(input$go)
}


compare_words <- function(target_string, guess_string){
  if(nchar(target_string) != nchar(guess_string)){
    stop("Target and guess strings must have the same length.")
  }
  
  target_vec <- stringr::str_split(target_string, "")[[1]]
  guess_vec  <- stringr::str_split(guess_string, "")[[1]]
  remaining_vec <- character(0)
  result_vec <- rep("not_in_word", 5)
  
  for(letter in seq_along(guess_vec)){
    if(guess_vec[letter] == target_vec[letter]){
      result_vec[letter] <- "correct"
    }else {
      remaining_vec <- c(remaining_vec, target_vec[letter])
    }
  }
    
  for(letter in seq_along(guess_vec)){
    if(guess_vec[letter] != target_vec[letter] && 
       guess_vec[letter] %in% remaining_vec){
      result_vec[letter] <- "in_word"
      remaining_vec <- remaining_vec[-match(guess_vec[letter], remaining_vec)]
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
  out_string <- ""
  for(letter in seq_along(check_report$guess_letters)){
    this_letter <- stringr::str_to_upper(check_report$guess_letters[letter])
    if(check_report$result[letter] == "correct"){
      out_string <- paste0(out_string, "[", this_letter, "]")
    }else if(check_report$result[letter] == "in_word"){
      out_string <- paste0(out_string, "(", this_letter, ")")
    }else{
      out_string <- paste0(out_string, " ", this_letter, " ")
    }
  }
  out_string
}

shinyApp(ui, server)