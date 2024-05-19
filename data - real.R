# Preliminary Data Loading and Cleaning/Filtering to get desired text area from file -- removing metadata

text.v <- scan("cybersecurity2023legislation.txt", what="character", sep="\n")
start.v <- which(text.v == "Alabama")
end.v <- which(text.v == "Establishes the Cyber Maryland Program in the Maryland Technology Development Corporation to increase the cybersecurity workforce in the state, build an advanced cybersecurity workforce, and carry out other purposes related to the cybersecurity workforce in the state; establishes the Cyber Maryland Fund; establishes the Cyber Maryland Board; requires the Maryland Higher Education Commission, in consultation with a certain organization, to expand the Cyber Warrior Diversity Program.")
start.metadata.v <- text.v[1:(start.v - 1)]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)
laws.v <- text.v[start.v:end.v]
laws.v <- laws.v[!grepl("^[A-Z]+ \\d+$", laws.v)] # removes the H 132

# Sorting each of the laws into the keywords

keywords <- c("Pending", "Enacted", "Vetoed", "Failed", "Adopted")  # Add more keywords if needed

status_lists <- list() # initialize blocks list

laws_by_state_status <- list() # initialize state list

# Initialize empty lists for each status
for (keyword in keywords) {
  status_lists[keyword] <- list()
}

for (i in 3:length(text.v)) {
  state <- text.v[i - 3] #ALASKA -- LINE 0
  number <- text.v[i - 2] # H 132 -- LINE 1
  title <- text.v[i - 1] # Elections Ballot Security -- LINE 2
  status <- text.v[i] # Pending - Carryover -- LINE 3
  summary <- text.v[i + 1] # Relates to election security, voting and ballots -- LINE 4
  
  # Check if the line contains a status indicator
  #if (grepl("Pending", status)) {
    #status_line <- paste(state, number, title, summary, sep = " ") if you need to add more lines add them here
  #  status_line <- paste(title, summary, sep = " ") #if you need to add more lines add them here
  #  status_lists[["Pending"]][[length(status_lists[[current_status]]) + 1]] <- status_line
  #  laws_by_state_status[[state]][[current_status]] <- c(laws_by_state_status[[state]][[current_status]], status_line)
  #}
  # if (grepl("Failed", status)) {
  #   #status_line <- paste(state, number, title, summary, sep = " ") if you need to add more lines add them here
  #   status_line <- paste(title, summary, sep = " ") #if you need to add more lines add them here
  #   status_lists[["Failed"]][[length(status_lists[[current_status]]) + 1]] <- status_line
  #   laws_by_state_status[[state]][[current_status]] <- c(laws_by_state_status[[state]][[current_status]], status_line)
  # }
   if (grepl("Enacted", status)) {
     status_line <- paste(title, summary, sep = " - ") #concatenates items and seperates them with a " - "
#     status_lists[["Enacted"]][[length(status_lists[["Enacted"]]) + 1]] <- status_line
     laws_by_state_status[[state]][["Enacted"]] <- c(laws_by_state_status[[state]][["Enacted"]], status_line)
   }
  # if (grepl("Vetoed", status)) {
  #   status_line <- paste(title, summary, sep = " ") #if you need to add more lines add them here
  #   status_lists[["Vetoed"]][[length(status_lists[[current_status]]) + 1]] <- status_line
  #   laws_by_state_status[[state]][[current_status]] <- c(laws_by_state_status[[state]][[current_status]], status_line)
  # }
  # if (grepl("Adopted", status)) {
  #   #status_line <- paste(state, number, title, summary, sep = " ") if you need to add more lines add them here
  #   status_line <- paste(title, summary, sep = " ") #if you need to add more lines add them here
  #   status_lists[["Adopted"]][[length(status_lists[[current_status]]) + 1]] <- status_line
  #   laws_by_state_status[[state]][[current_status]] <- c(laws_by_state_status[[state]][[current_status]], status_line)
  # }
}

#print the descriptions sorted by keyword by state

 for (state in names(laws_by_state_status)) {
   cat("\n", "\n", "State:", state, "\n", "\n")
   for (status in names(laws_by_state_status[[state]])) {
     cat("\n", "Status:", status, "- Count:", length(laws_by_state_status[[state]][[status]]), "\n")
     for (law_text in laws_by_state_status[[state]][[status]]) {
       cat("---------------\n")
       cat(law_text, "\n")
     }
   }
 }

#Example Output:

  #State: West Virginia 

  #Status: Enacted - Count: 1 
  #---------------
  #  Funding and Requirements for Federal Elections - Relates to the expenditure of federal appropriations from Congress to the secretary of state for purposes that further the administration of federal elections held in the state, payable from the County Assistance Voting Equipment Fund; clarifies the uniform statewide deadline for electronically submitted voter registration applications; changes the deadline by which county clerks must report voter participation history after an election into the statewide voter registration system. 





#TF-IDF -- Testing for one state and status output, It should concatanate all the text into one huge text block and find the top 50 or so words out of that. -- REMOVE LATER AFTER IT WORKS

#concatanate

#cat("\n", "\n", "\n", "------ LAW TOTAL TEXT --------")
#cat(sapply(laws_by_state_status[["Virginia"]][["Enacted"]], paste, collapse = " next:"))
#cat("\n", "\n", "\n", "------ END --------", "\n", "\n", "\n")

#TF-IDF -- Testing just one state and status output -- REMOVE LATER AFTER IT WORKS

#It goes through each of the laws in this section and outputs the 10 lowest frequency and highest frequency items in each of the laws

#serialize 

#state_lists <- list("Alabama",
#               "Alaska",
#               "Arizona",
#               "Arkansas",
#               "California",
#               "Colorado",
#               "Connecticut",
#               "Delaware",
#               "Florida",
#               "Georgia",
#               "Hawaii",
#               "Idaho",
#               "Illinois",
#               "Indiana",
#               "Iowa",
#               "Kansas",
#               "Kentucky",
#               "Louisiana",
#               "Maine",
#               "Maryland",
#               "Massachusetts",
#               "Michigan",
#               "Minnesota",
#               "Mississippi",
#               "Missouri",
#               "Montana",
#               "Nebraska",
#               "Nevada",
#               "New Hampshire",
#               "New Jersey",
#               "New Mexico",
#               "New York",
#               "North Carolina",
#               "North Dakota",
#               "Ohio",
#               "Oklahoma",
#               "Oregon",
#               "Pennsylvania",
#               "Rhode Island",
#               "South Carolina",
#               "South Dakota",
#               "Tennessee",
#               "Texas",
#               "Utah",
#               "Vermont",
#               "Virginia",
#               "Washington",
#               "West Virginia",
#               "Wisconsin",
#              "Wyoming")
#status_keyword_lists <- list("Pending", "Enacted", "Vetoed", "Failed", "Adopted")

#for(state in state_lists) {
#  cat(state, "\n", "\n", "\n")
#state = "Alabama"
#  for(law_text in laws_by_state_status[["Alabama"]][["Enacted"]]) {
#    law_text = sapply(laws_by_state_status[["Alabama"]][["Enacted"]], paste, collapse = "\n")
#    cat(law_text, "\n", "\n")
#    tryCatch({
#      law_text.lower.v <- tolower(law_text)
#      law_text.words.l <- strsplit(law_text.lower.v, "\\W")
#      law_text.word.v <- unlist(law_text.words.l)
#      
#      not.blanks.v  <-  which(law_text.word.v!="")
#      law_text.word.v <-  law_text.word.v[not.blanks.v]
#      law_text.freqs.t <- table(law_text.word.v)
#      
#      law_text.status.freqs.t <- sort(law_text.freqs.t , decreasing=TRUE)
#      law_text.status.rel.freqs.t <- 100*(law_text.status.freqs.t/sum(law_text.status.freqs.t))
#      
#      # Extract the 10 lowest frequency terms
#      low_terms <- tail(names(law_text.status.rel.freqs.t[order(law_text.status.rel.freqs.t)]), 20)
#      low_terms_names <- names(law_text.status.rel.freqs.t)[tail(order(law_text.status.rel.freqs.t), 20)]
#      low_terms_freqs <- law_text.status.rel.freqs.t[low_terms]
#    
#    #Output of low term and frequency
#    
#    cat("low_terms_names: ", low_terms_names, "\n")
#    cat("low_terms_freqs: ", low_terms_freqs, "\n", "\n")
#    
#    # Extract the 10 highest frequency terms
#    high_terms <- head(names(law_text.status.rel.freqs.t[order(law_text.status.rel.freqs.t)]), 20)
#    high_terms_names <- names(law_text.status.rel.freqs.t)[head(order(law_text.status.rel.freqs.t), 20)]
#    high_terms_freqs <- law_text.status.rel.freqs.t[high_terms]
#    
#    #Output of low term and frequency
#    
#    cat("high_terms_names: ", high_terms_names, "\n")
#    cat("high_terms_freqs: ", high_terms_freqs, "\n", "\n")
#    
#  }, error = function(e) {
#    cat("\n", "Skipping block due to error:", conditionMessage(e), "\n")
#  })
#}
#}

 
 
#THIS WORKS INDIVIDUALLY -- NEED TO MANUALLY ENTER IN STATE NAME AND STATUS

law_text = sapply(laws_by_state_status[["Virginia"]][["Enacted"]], paste, collapse = "\n")
   cat(law_text, "\n", "\n")
   tryCatch({
     law_text.lower.v <- tolower(law_text)
     law_text.words.l <- strsplit(law_text.lower.v, "\\W")
     law_text.word.v <- unlist(law_text.words.l)
     
     not.blanks.v  <-  which(law_text.word.v!="")
     law_text.word.v <-  law_text.word.v[not.blanks.v]
     law_text.freqs.t <- table(law_text.word.v)
     
     law_text.status.freqs.t <- sort(law_text.freqs.t , decreasing=TRUE)
     law_text.status.rel.freqs.t <- 100*(law_text.status.freqs.t/sum(law_text.status.freqs.t))
     
     # Extract the 10 lowest frequency terms
     low_terms <- tail(names(law_text.status.rel.freqs.t[order(law_text.status.rel.freqs.t)]), 20)
     low_terms_names <- names(law_text.status.rel.freqs.t)[tail(order(law_text.status.rel.freqs.t), 20)]
     low_terms_freqs <- law_text.status.rel.freqs.t[low_terms]
     
     #Output of low term and frequency
     
     cat("low_terms_names: ", low_terms_names, "\n")
     cat("low_terms_freqs: ", low_terms_freqs, "\n", "\n")
     
     # Extract the 10 highest frequency terms
     high_terms <- head(names(law_text.status.rel.freqs.t[order(law_text.status.rel.freqs.t)]), 20)
     high_terms_names <- names(law_text.status.rel.freqs.t)[head(order(law_text.status.rel.freqs.t), 20)]
     high_terms_freqs <- law_text.status.rel.freqs.t[high_terms]
     
     #Output of low term and frequency
     
     cat("high_terms_names: ", high_terms_names, "\n")
     cat("high_terms_freqs: ", high_terms_freqs, "\n", "\n")
     
   }, error = function(e) {
     cat("\n", "Skipping block due to error:", conditionMessage(e), "\n")
   })
