# Function to determine the average age of a professor at a university.

evalAverage <- function(fileName)
{
  # Open and read source text file
  text <- readChar(fileName, file.info(fileName)$size)
  
  # Get all years from string
  matches <- gregexpr('(?=(\\d{4})\\,\\s(?:AB|BA|BS))', text, perl=TRUE)
  attr(matches[[1]], 'match.length') <- as.vector(attr(matches[[1]], 'capture.length')[,1])
  years <- regmatches(text, matches)
  ages <- sapply(years, function(x) 2015-strtoi(x,10)+22)
  
  # Counting average
  print(mean(ages))
}
