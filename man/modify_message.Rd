% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai-messages.R
\name{modify_message}
\alias{modify_message}
\title{Modify a message within a thread}
\usage{
modify_message(thread_id, message_id, metadata = NULL)
}
\arguments{
\item{thread_id}{The ID of the thread to create the message in (required)}

\item{message_id}{The ID of the message (required)}

\item{metadata}{A named character vector of up to 16 metadata values, with
names (keys) maximum 64 characters long and values maximum 512 characters
long}
}
\value{
A message object
}
\description{
Modify a message within a thread
}
\references{
\url{https://platform.openai.com/docs/api-reference/messages/modifyMessage}
}
