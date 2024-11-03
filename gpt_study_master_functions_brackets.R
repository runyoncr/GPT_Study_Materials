
# Master Function(s) for GPT study

## OpenAI / ChatGPT API
## "gpt-3.5-turbo"; "gpt-4o"; "gpt-4o-mini"
## temp range is 0 to 2; default is 1

hey_openai_point_array <- function(gpt_model = NA, two_step = FALSE, 
                                   temp1 = 1, temp2 = NA, prompt1 = NA, prompt2 = NA){
  
    gpt_output1 <-  POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", openai_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = gpt_model,
        messages = list(
          list(role = "user",
               content = prompt1)),
        temperature = temp1))
    
    answer1 <- str_trim(content(gpt_output1)$choices[[1]]$message$content)
    num1 <- NA
    
    if(two_step == TRUE){
      
      Sys.sleep(5) # To keep requests per minute down to acceptable levels
      
      full_prompt2 <- paste0(prompt2, " : ", answer1)
      
      gpt_output2 <-  POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", openai_key)),
        content_type_json(),
        encode = "json",
        body = list(
          model = gpt_model,
          messages = list(
            list(role = "user",
                 content = full_prompt2)),
          temperature = temp2))
      
      answer2 <- str_trim(content(gpt_output2)$choices[[1]]$message$content)
      point_array <- str_extract(answer2, "\\[[0-9|]+\\]")
      point_array  <- gsub("\\[|\\]", "", point_array )
      # Step 2: Split the string by the pipe delimiter, convert to numeric
      points_numeric <- as.numeric(strsplit(point_array, "\\|")[[1]])
      # Step 3: Sum the numeric values
      num2 <- sum(points_numeric)
      
      rdf = data.frame(
        answer1 = answer1,
        num1 = num1,
        answer2 = answer2,
        num2 = num2)
      
      Sys.sleep(5)
      
      } else {
        rdf = data.frame(
          answer1 = answer1,
          num1 = num1,
          answer2 = NA,
          num2 = NA)
        Sys.sleep(5)
        
      }
    
    return(rdf)
    
}

## Anthropic / Claude API
## "claude-3-5-sonnet-20240620"; "claude-3-haiku-20240307"
## Temp range 0 to 1; default is 1

hey_anthropic_point_array <- function(gpt_model = NA, two_step = FALSE, 
                                      temp1 = 1, temp2 = NA, prompt1 = NA, prompt2 = NA){

  gpt_output1 <- claudeR(prompt = list(list(role = "user", 
                                           content = prompt1)), 
                        model = gpt_model, 
                        temperature = temp1,
                        max_tokens = 2000,
                        system = "You are a medical educator",
                        api_key = claude_key)
  
  answer1 <- gpt_output1
  num1 <- NA
  
  if(two_step == TRUE){
    
    Sys.sleep(5)

    full_prompt2 <- paste0(prompt2, ": ", answer1)
    
    gpt_output2 <- claudeR(prompt = list(list(role = "user", 
                                              content = full_prompt2)), 
                           model = gpt_model, 
                           temperature = temp2,
                           max_tokens = 2000,
                           system = "You are a medical educator",
                           api_key = claude_key)
    
    answer2 <- gpt_output2
    point_array <- str_extract(answer2, "\\[[0-9|]+\\]")
    point_array  <- gsub("\\[|\\]", "", point_array )
    # Step 2: Split the string by the pipe delimiter, convert to numeric
    points_numeric <- as.numeric(strsplit(point_array, "\\|")[[1]])
    # Step 3: Sum the numeric values
    num2 <- sum(points_numeric)
    
    rdf <- data.frame(
      answer1 = answer1,
      num1 = num1,
      answer2 = answer2,
      num2 = num2)
    Sys.sleep(5)
  } else {
    rdf <- data.frame(
      answer1 = answer1,
      num1 = num1,
      answer2 = NA,
      num2 = NA)
    Sys.sleep(5)
    
  }
  
  return(rdf)

}

## Google / Gemini API
## "gemini-1.5-pro"; "gemini-1.5-flash" (similar to 4o and 4o-mini)  
## Temp range 0 to 2; default is 1

hey_google_point_array <- function(gpt_model = NA, two_step = FALSE,
                                   temp1 = 1, temp2 = NA, prompt1 = NA, prompt2 = NA){
  
    model_query <- paste0(gpt_model, ":generateContent")
    
    gpt_output1 <- POST(
      url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", 
                  model_query),
      query = list(key = gemini_key),
      content_type_json(),
      encode = "json",
      body = list(
        contents = list(
          parts = list(
            list(text = prompt1)
          )),
        generationConfig = list(
          temperature = temp1,
          maxOutputTokens = 2000)))
    
    candidates <- content(gpt_output1)$candidates
    final_output1 <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
    num1 <- NA
  
    if(two_step == TRUE){
      
      Sys.sleep(5)
      
      full_prompt2 <- paste0(prompt2, " : ", final_output1)
      
      gpt_output2 <- POST(
        url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", 
                      model_query),
        query = list(key = gemini_key),
        content_type_json(),
        encode = "json",
        body = list(
          contents = list(
            parts = list(
              list(text = full_prompt2)
            )),
          generationConfig = list(
            temperature = temp2,
            maxOutputTokens = 2000)))
      
      candidates <- content(gpt_output2)$candidates
      final_output2 <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
      point_array <- str_extract(final_output2, "\\[[0-9|]+\\]")
      point_array  <- gsub("\\[|\\]", "", point_array )
      # Step 2: Split the string by the pipe delimiter, convert to numeric
      points_numeric <- as.numeric(strsplit(point_array, "\\|")[[1]])
      # Step 3: Sum the numeric values
      num2 <- sum(points_numeric)
      
      rdf <- data.frame(
        answer1 = final_output1,
        num1 = num1,
        answer2 = final_output2,
        num2 = num2)
      Sys.sleep(5)
    } else {
      rdf <- data.frame(
        answer1 = final_output1,
        num1 = num1,
        answer2 = NA,
        num2 = NA)
      Sys.sleep(5)
    }
    
    return(rdf)
}

