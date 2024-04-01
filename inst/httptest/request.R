set_requester(function (request) {
  request <- gsub_request(request, "https\\://api.openai.com/v1/", "a/")
  request <- gsub_request(request, "/threads", "/t")
  request <- gsub_request(request, "/chat", "/c")
  request <- gsub_request(request, "/files", "/f")
  request <- gsub_request(request, "/messages", "/m")
  request <- gsub_request(request, "/assistants", "/a")
  request <- gsub_request(request, "thread_", "t_")
  request <- gsub_request(request, "msg_", "m_")
  request <- gsub_request(request, "json", "j")
  request <- gsub_request(request, "file-sN6eG4TkNq2Quj1LpgCdUVdo", "file-s")
  request
})
