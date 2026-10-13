library(officer)
library(flextable)

doc_path <- "____.docx"
doc <- read_docx()

h1 <- function(x) doc <<- body_add_par(doc, x, style = "heading 1")

cap <- function(kind, n, x) {
  doc <<- body_add_fpar(doc, fpar(
    ftext(paste0(kind, " ", n, ". "), fp_text(bold = TRUE, font.family = "Calibri")),
    ftext(x, fp_text(font.family = "Calibri"))))
}

tab <- function(df, size = 9) {
  ft <- flextable(df) |> theme_booktabs() |> fontsize(size = size, part = "all") |>
    font(fontname = "Calibri", part = "all") |> bold(part = "header") |> autofit()
  doc <<- body_add_flextable(doc, ft)
  doc <<- body_add_par(doc, "")
}

# w and h are the inches you used in ggsave
fig <- function(file, w, h) {
  s <- min(1, 6.4 / w)
  doc <<- body_add_img(doc, src = file, width = w * s, height = h * s)
}

# title
doc <- body_add_fpar(doc, fpar(ftext("____", fp_text(bold = TRUE, font.size = 18, font.family = "Calibri"))))

# a section, copy paste for more
h1("____")
cap("Table", 1, "____")
tab(____)
cap("Figure", 1, "____")
fig("____.png", ____, ____)

# yellow rows
ft <- flextable(____) |> theme_booktabs() |>
  bg(i = which(____$____ == "____"), bg = "#FFFF00", part = "body")
doc <- body_add_flextable(doc, ft)

# wide landscape page for the huge table
doc <- body_end_block_section(doc, block_section(prop_section(
  page_size = page_size(orient = "portrait", width = 8.5, height = 11), type = "continuous")))
h1("____")
tab(____, size = 5)
doc <- body_end_block_section(doc, block_section(prop_section(
  page_size = page_size(orient = "landscape", width = ____, height = ____), type = "nextPage")))

# back to normal and save
doc <- body_set_default_section(doc, prop_section(
  page_size = page_size(orient = "portrait", width = 8.5, height = 11), type = "nextPage"))
print(doc, target = doc_path)
