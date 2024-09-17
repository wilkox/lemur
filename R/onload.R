.onLoad <- function(libname, pkgname) {
  set_vendor()
  set_model()
  if (get_vendor() == "OpenAI") check_openai_API_key()
}
