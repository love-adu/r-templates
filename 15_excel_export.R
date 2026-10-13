library(writexl)

# each item in the list is one tab
tables_list <- list(
  "1 ____" = ____,
  "2 ____" = ____,
  "3 ____" = ____)

write_xlsx(tables_list, "____.xlsx", format_headers = TRUE)
