#' Load and Preprocess a Text File
#'
#' Reads a plain-text file and returns a vector of individual lowercase words
#' with punctuation removed.
#'
#' @param file_path A character string with the path to the .txt file.
#'
#' @return A character vector of lowercase words.
#'
#' @examples
#' words <- load_and_preprocess("Example_negative.txt")
#' head(words, 20)
#'
#' @export
load_and_preprocess <- function(file_path) {


  # Read the text file
  raw <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)

  # Collapse all lines into one single string
  full_text <- paste(unlist(raw[1,]), collapse = " ")

  # Convert all letters to lowercase
  full_text <- tolower(full_text)

  # Split the text into individual words
  # We remove tabs, spaces, periods, commas, and semicolons
  words <- strsplit(full_text, split = "\t", fixed = TRUE)[[1]]
  words <- unlist(strsplit(words, split = " ", fixed = TRUE))
  words <- unlist(strsplit(words, split = ".", fixed = TRUE))
  words <- unlist(strsplit(words, split = ",", fixed = TRUE))
  words <- unlist(strsplit(words, split = ";", fixed = TRUE))

  # Remove any empty strings left over
  words <- words[nchar(words) > 0]

  return(words)
}


positive_words <- c("happy", "joy*", "wonder*", "love", "great*",
                    "excit*", "beautiful", "cheer*", "delight*",
                    "hope*", "grateful", "good", "nice*")

negative_words <- c("sad", "bad", "terr*", "awful", "horrible",
                    "disappoint*", "nause*", "angr*", "hate*",
                    "misera*", "stress*", "worr*", "frustrat*",
                    "fatigue", "restless", "overwhelm*", "trapped")


#' Match Sentiment Words Against a Token Vector
#'
#' Compares a vector of sentiment words against the words found in a document.
#' Supports wildcard * at the end of a word, for example "nause*" will match
#' both "nauseous" and "nauseated".
#'
#' @param word_vector A character vector of words produced by load_and_preprocess().
#' @param sentiment_words A character vector of sentiment words to search for.
#'
#' @return A named integer vector with the count of matches for each sentiment word.
#'
#' @examples
#' words <- load_and_preprocess("Example_negative.txt")
#' match_sentiment_words(words, negative_words)
#'
#' @export
match_sentiment_words <- function(word_vector, sentiment_words) {

  # Create an empty vector to store the counts
  counts <- integer(length(sentiment_words))
  names(counts) <- sentiment_words

  # Loop over every word in the sentiment list
  for (i in seq_along(sentiment_words)) {
    pattern <- sentiment_words[i]

    # Check if the pattern has a wildcard * at the end
    if (endsWith(pattern, "*")) {
      stem <- sub("\\*$", "", pattern)  # remove the *
      matched <- startsWith(word_vector, stem)

    } else {
      # No wildcard, exact match
      matched <- word_vector == pattern
    }

    counts[i] <- sum(matched)
  }

  return(counts)
}


#' Summarise Sentiment of a Document
#'
#' Uses match_sentiment_words() for both positive and negative word lists
#' and returns the total counts, a ratio, and a verdict.
#'
#' @param word_vector A character vector of words produced by load_and_preprocess().
#' @param positive_words A character vector of positive sentiment words.
#' @param negative_words A character vector of negative sentiment words.
#'
#' @return Prints the total positive and negative word counts, the ratio,
#' and a verdict of POSITIVE, NEGATIVE or NEUTRAL.
#'
#' @examples
#' words <- load_and_preprocess("Example_negative.txt")
#' summarise_sentiment(words, positive_words, negative_words)
#'
#' @export
summarise_sentiment <- function(word_vector, positive_words, negative_words) {

  # Run matching for both lists
  pos_counts <- match_sentiment_words(word_vector, positive_words)
  neg_counts <- match_sentiment_words(word_vector, negative_words)

  # Add up the totals
  n_positive <- sum(pos_counts)
  n_negative <- sum(neg_counts)

  # Calculate ratio (adding 1 to avoid dividing by zero)
  ratio <- (n_positive + 1) / (n_negative + 1)

  # Give a verdict
  if (ratio > 1) {
    verdict <- "POSITIVE"
  } else if (ratio < 1) {
    verdict <- "NEGATIVE"
  } else {
    verdict <- "NEUTRAL"
  }

  # Print a summary
  cat("Total positive words:", n_positive, "\n")
  cat("Total negative words:", n_negative, "\n")
  cat("Ratio (pos/neg):", round(ratio, 3), "\n")
  cat("Verdict:", verdict, "\n")

}
