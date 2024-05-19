# Preliminary Data Loading and Cleaning/Filtering to get desired text area from file -- removing metadata

text.v <- scan("cybersecurity2023legislation.txt", what="character", sep="\n")
start.v <- which(text.v == "Alabama")
end.v <- which(text.v == "Establishes the Cyber Maryland Program in the Maryland Technology Development Corporation to increase the cybersecurity workforce in the state, build an advanced cybersecurity workforce, and carry out other purposes related to the cybersecurity workforce in the state; establishes the Cyber Maryland Fund; establishes the Cyber Maryland Board; requires the Maryland Higher Education Commission, in consultation with a certain organization, to expand the Cyber Warrior Diversity Program.")
start.metadata.v <- text.v[1:(start.v - 1)]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)
laws.v <- text.v[start.v:end.v]
laws.v <- laws.v[!grepl("^[A-Z]+ \\d+$", laws.v)] # all good up until here

# Sorting each of the laws into the statuses

statuses <- c("Pending", "Enacted", "Vetoed", "Failed", "Adopted")  # Add more statuses if needed

status_lists <- list() # initialize blocks list

# Initialize empty lists for each status
for (status in statuses) {
  status_lists[status] <- list()
}

Pending_count <- 0
Enacted_count <- 0
Vetoed_count <- 0
Failed_count <- 0
Adopted_count <- 0

for (i in 3:length(text.v)) {  # Start from the third line to ensure we can access two previous lines
  line <- text.v[i] # Pending - Carryover
  prev_line <- text.v[i - 1] # Foreign Contracts and Commerce
  prev_prev_line <- text.v[i - 2] # H 132
  prev_prev_prev_line <- text.v[i - 3] #ALASKA
  after_line <- text.v[i + 1] # Relates to the....
  
  # Check if the line contains a status indicator
  if (grepl("Pending", line)) {
    current_status <- "Pending"
    Pending_count <- Pending_count + 1
    #status_line <- paste(prev_prev_prev_line, prev_prev_line, prev_line, after_line, sep = " ") if you need to add more lines add them here
    status_line <- paste(prev_line, after_line, sep = " ") #if you need to add more lines add them here
    status_lists[[current_status]][[length(status_lists[[current_status]]) + 1]] <- status_line
  }
  if (grepl("Failed", line)) {
    current_status <- "Failed"
    Failed_count <- Failed_count + 1
    #status_line <- paste(prev_prev_prev_line, prev_prev_line, prev_line, after_line, sep = " ") if you need to add more lines add them here
    status_line <- paste(prev_line, after_line, sep = " ") #if you need to add more lines add them here
    status_lists[[current_status]][[length(status_lists[[current_status]]) + 1]] <- status_line
  }
  if (grepl("Enacted", line)) {
    current_status <- "Enacted"
    Enacted_count <- Enacted_count + 1
    #status_line <- paste(prev_prev_prev_line, prev_prev_line, prev_line, after_line, sep = " ") if you need to add more lines add them here
    status_line <- paste(prev_line, after_line, sep = " ") #if you need to add more lines add them here
    status_lists[[current_status]][[length(status_lists[[current_status]]) + 1]] <- status_line
  }
  if (grepl("Vetoed", line)) {
    current_status <- "Vetoed"
    Vetoed_count <- Vetoed_count + 1
    #status_line <- paste(prev_prev_prev_line, prev_prev_line, prev_line, after_line, sep = " ") if you need to add more lines add them here
    status_line <- paste(prev_line, after_line, sep = " ") #if you need to add more lines add them here
    status_lists[[current_status]][[length(status_lists[[current_status]]) + 1]] <- status_line
  }
  if (grepl("Adopted", line)) {
    current_status <- "Adopted"
    Adopted_count <- Adopted_count + 1
    #status_line <- paste(prev_prev_prev_line, prev_prev_line, prev_line, after_line, sep = " ") if you need to add more lines add them here
    status_line <- paste(prev_line, after_line, sep = " ") #if you need to add more lines add them here
    status_lists[[current_status]][[length(status_lists[[current_status]]) + 1]] <- status_line
  }
}

for (status in statuses) {
  cat(paste("status:", status, "- Count:", eval(parse(text = paste0(status, "_count"))), "\n")) #status: Adopted - Count: 26 
  for (block in status_lists[[status]]) {
    cat("---------------\n")
    cat(block, "\n")      #each of these are the laws descriptions
  }
  cat("\n")
}

for (status in statuses) {
  cat("\n")
  cat(status)
  cat("\n")
  cat("\n")
  for(block in status_lists[[status]]) {
    cat(block)
    cat("\n")
  }
}

# Tf-idf

for(status in statuses) {
  cat(status, "\n")
  cat("\n")
  cat("Length of status_lists[[status]]:", length(status_lists[[status]]), "\n")
 # cat("Number of blocks:", length(status_lists[[status]]), "\n")  # Debugging output
  for(block in status_lists[[status]]) {
  #  cat(status, "\n")
  #  cat("Processing block:", block, "\n")  # Debugging output
    tryCatch({
      block.lower.v <- tolower(block)
      block.words.l <- strsplit(block.lower.v, "\\W")
      block.word.v <- unlist(block.words.l)
      
      #print(paste("Status:", status, "Block:", block))
      #print(paste("Length of block.word.v:", length(block.word.v)))
      
      not.blanks.v  <-  which(block.word.v!="")
      block.word.v <-  block.word.v[not.blanks.v]
      block.freqs.t <- table(block.word.v)
      
      #print(paste("Length of block.freqs.t:", length(block.freqs.t)))
      
      block.status.freqs.t <- sort(block.freqs.t , decreasing=TRUE)
      block.status.rel.freqs.t <- 100*(block.status.freqs.t/sum(block.status.freqs.t))

      cat("Status:", status, "Block:", block, "\n") # Debugging output
      
      #barplot(block.status.rel.freqs.t[1:10], main = paste("Top Ten Words in", block), xlab = "Words", ylab = "Percentage of Full Text")
      
      
      plot(block.status.rel.freqs.t, type="b", xlab= paste("Top Ten Words in ", status), ylab="Percentage of Full Text", xaxt ="n", main = paste("Top Ten Words in", block))
      axis(1,1:10000, labels=names(sorted.block.rel.freqs.t[1:10000]))
    }, error = function(e) {
      cat("\n", "Skipping block due to error:", conditionMessage(e), "\n")
    })
  } 
}


#idf
#for (status in status_lists[[status]]) { #right now only does it for adopted, need it for all of them
#  status.lower.v <- tolower(status)
#  status.words.l <- strsplit(status.lower.v, "\\W")
#  status.word.v <- unlist(status.words.l)
#  not.blanks.v  <-  which(status.word.v!="")
#  status.word.v <-  status.word.v[not.blanks.v]
#  status.freqs.t <- table(status.word.v)
#  sorted.status.freqs.t <- sort(status.freqs.t , decreasing=TRUE)
#  sorted.status.rel.freqs.t <- 100*(sorted.status.freqs.t/sum(sorted.status.freqs.t))
#  plot(sorted.status.rel.freqs.t, type="b", xlab= "Top Ten Words", ylab="Percentage of Full Text", xaxt ="n", main = paste("Top Ten Words in", status))
#  axis(1,1:10000, labels=names(sorted.status.rel.freqs.t [1:10000]))
#  cat("\n")
#}

