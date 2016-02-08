#---
#title: "[REPLACE_WITH_COURSE_NAME]"
#author: "[REPLACE_WITH_COURSE_INSTRUCTOR]"
#date: "[REPLACE_WITH_DATE]"
#output: html_document
#---

lGetTitleInfo <- function(){
  return(list(title = "Angewandte Statistische Methoden fuer Nutztierwissenschaften",
         author = "Peter von Rohr",
         date = format(Sys.Date(), "%d.%m.%Y"),
         output = "html_document"))
}
