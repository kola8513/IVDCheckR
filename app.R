#libraries
pacman::p_load(shiny,bslib,rhandsontable,shinyjs,rmarkdown,pagedown,pdftools,mcr,tinytex,shinycssloaders,knitr,digest,htmlwidgets)

setwd(tempdir())

# Define a function to sanitize inputs
sanitize_input <- function(input) {
  # Specify allowed patterns using regular expressions
  allowed_pattern <- "^[a-zA-Z0-9.\\s-]*$"
  # Check if input matches allowed pattern
  if (!grepl(allowed_pattern, input)) {
    # If input does not match, replace with empty string or handle appropriately
    input <- ""
  }
  return(input)
}


writeLines(
  con = file.path(tempdir(), 'ivdcheckr.rmd'),
  text = '---
output: pdf_document
params:
  r: ""
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.align = "center",
                      fig.width = 6.5, fig.height = 5.5, out.width = "78%")
r <- params$r
```

```{r, results="asis"}
cat(report_intro(r))
```

# Method comparison

```{r, results="asis"}
cat(report_mc(r$mc))
```

```{r, eval=r$mc$ok}
try(plot1(r$mc$data), silent = TRUE)
try(plot2(r$mc$data), silent = TRUE)
```

```{r, results="asis", eval=r$mc$ok}
cat(report_mc_raw(r$mc))
```

# Quality control

```{r, results="asis"}
cat(report_qc(r$qc))
```

# Chemicals and software

```{r, results="asis"}
cat(report_chem(r$chem))
```

```{r, results="asis"}
cat(report_attachments(r$docs))
```
'
)

#-----------------------------------------------------------------------------------
# PDF report helpers

# LaTeX preamble of the report; only uses what the default pandoc template already loads
report_preamble <- function(project) {
  c("\\usepackage{xcolor}",
    "\\definecolor{ivdblue}{HTML}{40668D}",
    "\\definecolor{ivddark}{HTML}{00406B}",
    "\\renewcommand{\\familydefault}{\\sfdefault}",
    "\\setlength{\\parindent}{0pt}",
    "\\setlength{\\parskip}{.5em}",
    "\\AtBeginDocument{\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{\\fill}}",
    # characters pdflatex does not know by itself
    "\\DeclareUnicodeCharacter{2264}{\\ensuremath{\\leq}}",
    "\\DeclareUnicodeCharacter{2265}{\\ensuremath{\\geq}}",
    "\\DeclareUnicodeCharacter{2260}{\\ensuremath{\\neq}}",
    "\\DeclareUnicodeCharacter{2192}{\\ensuremath{\\rightarrow}}",
    "\\DeclareUnicodeCharacter{2212}{\\ensuremath{-}}",
    "\\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}",
    "\\DeclareUnicodeCharacter{03B2}{\\ensuremath{\\beta}}",
    "\\DeclareUnicodeCharacter{03B3}{\\ensuremath{\\gamma}}",
    "\\DeclareUnicodeCharacter{03BC}{\\ensuremath{\\mu}}",
    "\\makeatletter",
    "\\newcommand{\\needroom}[1]{\\par\\begingroup\\dimen@=\\pagegoal\\advance\\dimen@ by -\\pagetotal\\ifdim\\dimen@<#1\\relax\\newpage\\fi\\endgroup}",
    "\\renewcommand\\section{\\@startsection{section}{1}{0pt}{1.6em}{.7em}{\\normalfont\\Large\\bfseries\\color{ivddark}}}",
    "\\renewcommand\\subsection{\\@startsection{subsection}{2}{0pt}{1em}{.2em}{\\normalfont\\normalsize\\bfseries\\color{ivdblue}}}",
    "\\def\\ps@ivdreport{\\def\\@oddhead{}\\def\\@evenhead{}",
    paste0("  \\def\\@oddfoot{\\footnotesize\\color{gray}IVDCheckR report", if (nzchar(project)) paste0(" -- ", tex_text(project)),
           "\\hfill Page \\thepage}\\let\\@evenfoot\\@oddfoot}"),
    "\\makeatother",
    "\\pagestyle{ivdreport}")
}

# Replace everything pdflatex cannot print, so unusual input can never break the download
report_chars <- function(x) {
  x <- paste(as.character(x), collapse = " ")
  if (is.na(x) || !nzchar(x)) return("")
  u <- utf8ToInt(enc2utf8(x))
  if (anyNA(u)) return(iconv(x, to = "ASCII", sub = "?"))
  u <- u[u != 13L]
  u[u == 9L] <- 32L
  known <- c(0x2018, 0x2019, 0x201C, 0x201D, 0x2013, 0x2014, 0x2026, 0x2022, 0x2264, 0x2265, 0x2260, 0x2192, 0x2212, 0x3B1, 0x3B2, 0x3B3, 0x3BC)
  u[!(u == 10L | (u >= 32L & u < 127L) | (u >= 160L & u < 256L) | u %in% known)] <- 63L
  if (length(u)) intToUtf8(u) else ""
}

# User text as literal markdown: all ASCII punctuation is escaped
md_escape <- function(x) gsub("([!-/:-@\\[-`{-~])", "\\\\\\1", report_chars(x), perl = TRUE)

# Free text for the report: line breaks are kept, empty fields are marked
md_text <- function(x, empty = "*Not provided*") {
  x <- trimws(report_chars(x))
  if (!nzchar(x)) return(empty)
  lines <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
  gsub("  \n  \n", "\n\n", paste0(vapply(lines, md_escape, ""), collapse = "  \n"), fixed = TRUE)
}

md_table <- function(d, align = NULL, widths = NULL) {
  cells <- as.data.frame(lapply(d, function(col) vapply(col, function(v) trimws(gsub("\n", " ", md_escape(v), fixed = TRUE)), "", USE.NAMES = FALSE)),
                         stringsAsFactors = FALSE)
  colnames(cells) <- vapply(colnames(d), md_escape, "", USE.NAMES = FALSE)
  lines <- knitr::kable(cells, format = "pipe", align = align)
  # the dashes set the relative column widths when a table has to wrap
  if (!is.null(widths)) lines[2] <- paste0("|", paste0(":", strrep("-", widths), collapse = "|"), "|")
  paste0("\n\n", paste(lines, collapse = "\n"), "\n\n")
}

tex_text <- function(x) {
  x <- gsub("\n", " ", report_chars(x), fixed = TRUE)
  x <- gsub("([{}$&#_%])", "\\\\\\1", gsub("\\", "\001", x, fixed = TRUE), perl = TRUE)
  x <- gsub("^", "\\^{}", gsub("~", "\\~{}", x, fixed = TRUE), fixed = TRUE)
  gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
}

report_section <- function(title, ...) paste(c("\\needroom{3cm}", paste("#", title), ...), collapse = "\n\n")
report_item <- function(title, text) paste0("## ", md_escape(title), "\n\n", text)

report_intro <- function(r) {
  title <- if (nzchar(r$title)) r$title else "Untitled project"
  band <- c("```{=latex}",
            "\\noindent\\colorbox{ivdblue}{\\parbox{\\dimexpr\\textwidth-2\\fboxsep\\relax}{\\color{white}\\vspace{.7em}",
            "{\\small\\bfseries IVDCheckR REPORT}\\\\[.5em]",
            sprintf("{\\LARGE\\bfseries %s}\\\\[.6em]", tex_text(title)),
            "{\\small Documentation of a laboratory developed test according to Article 5 (5) of the EU-IVDR (EU) 2017/746}\\\\[.3em]",
            sprintf("{\\small Evaluation period: %s \\quad Report created: %s}\\vspace{.7em}}}", tex_text(r$period), tex_text(r$created)),
            "```")

  n_doc <- sum(vapply(r$docs, function(d) d$provided, TRUE))
  n_na <- sum(vapply(r$docs, function(d) !d$applicable, TRUE))
  n_pdf <- sum(vapply(r$docs, function(d) !is.null(d$file), TRUE))
  n_filled <- sum(vapply(r$items, function(i) nzchar(trimws(report_chars(i$value))), TRUE))
  overview <- data.frame(
    Section = c("Project and measurand (items 1-8)", "IVDR documentation (items 9-15)", "Method comparison", "Quality control",
                "Chemicals and software", "Attached PDF documents"),
    Status = c(sprintf("%d of %d items completed", n_filled, length(r$items)),
               sprintf("%d provided, %d not applicable, %d open", n_doc, n_na, length(r$docs) - n_doc - n_na),
               if (r$mc$ok) sprintf("%d value pairs", nrow(r$mc$data)) else "No data entered",
               if (r$qc$ok) sprintf("%d QC level(s)", ncol(r$qc$results) - 1L) else "No data entered",
               if (nrow(r$chem)) sprintf("%d entries", nrow(r$chem)) else "None listed",
               if (n_pdf) sprintf("%d, appended after this report", n_pdf) else "None"),
    check.names = FALSE)

  paste(c(paste(band, collapse = "\n"),
          report_section("Overview", md_table(overview)),
          report_section("Project and measurand",
                         vapply(r$items, function(i) report_item(i$label, md_text(i$value)), "")),
          report_section("IVDR documentation",
                         vapply(r$docs, function(d) {
                           body <- if (!d$applicable) "*Not applicable*" else c(
                             if (nzchar(trimws(report_chars(d$text)))) md_text(d$text),
                             if (!is.null(d$file)) paste0("Attached PDF: ", md_escape(d$file$name)),
                             if (!d$provided) "*Marked as applicable, but nothing was provided*")
                           report_item(d$title, paste(body, collapse = "\n\n"))
                         }, "")),
          report_section("Further comments", md_text(r$extra, "*None*"))),
        collapse = "\n\n")
}

report_mc <- function(mc) {
  if (!mc$ok) return(paste0("*", mc$msg, "*"))
  paste0(md_table(mc$info, align = "ll"), md_table(mc$summary, align = "lrrr"))
}

report_mc_raw <- function(mc) {
  paste0("\\needroom{7cm}\n\n## Raw data\n\n", md_table(mc$raw, align = "rrr"))
}

report_qc <- function(qc) {
  if (!qc$ok) return("*No data entered (optional step).*")
  paste0(md_table(qc$info, align = "ll"),
         "## Results\n\n", md_table(qc$results, align = c("l", rep("r", ncol(qc$results) - 1L))),
         if (qc$no_target) "*Bias and RMSD need a target value above 0.*\n\n",
         "\\needroom{11cm}\n\n## Raw data\n\n", md_table(qc$raw, align = rep("r", ncol(qc$raw))))
}

report_chem <- function(chem) {
  if (!nrow(chem)) return("*None listed.*")
  colnames(chem) <- gsub("/", " / ", colnames(chem), fixed = TRUE)  # lets the long header wrap
  md_table(chem, widths = c(22, 20, 14, 28, 16))
}

report_attachments <- function(docs) {
  files <- Filter(function(d) !is.null(d$file), docs)
  if (!length(files)) return("")
  d <- data.frame(Item = vapply(files, function(f) f$title, ""), File = vapply(files, function(f) f$file$name, ""), check.names = FALSE)
  paste0("# Attached PDF documents\n\nThe following documents are appended after this page, in this order.", md_table(d))
}

# Format numbers for the report (no NaN/Inf from empty values or a target of 0)
report_num <- function(x, digits = 2) {
  vapply(x, function(v) if (is.finite(v)) format(round(v, digits), nsmall = digits, trim = TRUE) else "-", "", USE.NAMES = FALSE)
}

# for resizing the column widths of tables
jsCode <- c(
  "function(el, x) {",
  "  Handsontable.hooks.add('afterColumnResize', function(index, size){",
  "    Shiny.setInputValue('newsize', {index: index+1, size: size});",
  "  });",
  "}"
)

#-----------------------------------------------------------------------------------
# Layout helpers

# Pages in the order the user works through them (tab value = name, button text = value)
STEPS <- c(Info = "Overview", Project_Details = "LDT project description", MCTool = "Method comparison",
           QCTool = "Quality control", ChemSoft = "Chemicals and software")

EU_IVDR <- "https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A02017R0746-20230320"
IVDR_HTML <- "https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:02017R0746-20230320#"

fa <- function(name) icon(name, `aria-hidden` = "true")

# Any element with data-goto switches to that page (see js below)
goto_btn <- function(tab, ..., class = "btn-primary") tags$button(type = "button", class = paste("btn", class), `data-goto` = tab, ...)

# Small row of links below a field: templates, IVDR excerpts and a shortcut to another step
help_links <- function(ivdr = NULL, docs = NULL, tool = NULL) {
  div(class = "help",
      lapply(names(docs), function(n) a(href = docs[[n]], target = "_blank", fa("file-lines"), n)),
      lapply(seq_along(ivdr), function(i) a(href = paste0(IVDR_HTML, ivdr[i]), target = "_blank", fa("book-open"),
                                            if (length(ivdr) > 1) paste("IVDR excerpt", i) else "Official IVDR excerpt")),
      if (!is.null(tool)) a(href = "#", `data-goto` = tool, fa("arrow-right"), paste("Go to", STEPS[[tool]])))
}

step_head <- function(n, title, desc, optional = FALSE) {
  div(class = "step-head",
      div(class = "eyebrow", sprintf("Step %d of 4", n), if (optional) span(class = "opt", "optional")),
      h1(title), p(desc))
}

# Back / Download / Next bar at the bottom of every page
step_footer <- function(tab) {
  i <- match(tab, names(STEPS))
  prv <- if (i > 1) names(STEPS)[i - 1]
  nxt <- if (i < length(STEPS)) names(STEPS)[i + 1]
  div(class = "step-footer",
      if (!is.null(prv)) goto_btn(prv, fa("arrow-left"), paste("Back:", STEPS[[prv]]), class = "btn-outline-secondary"),
      div(class = "ms-auto d-flex flex-wrap gap-2",
          if (!is.null(nxt)) {
            tagList(downloadButton(paste0("dl_", tab), "Download PDF", class = "btn-outline-primary"),
                    goto_btn(nxt, paste("Next:", STEPS[[nxt]]), fa("arrow-right")))
          } else {
            downloadButton(paste0("dl_", tab), "Finish: Download PDF report", class = "btn-primary")
          }))
}

step_row <- function(n, tab, ico, title, desc, optional = FALSE) {
  a(class = "tool", href = "#", `data-goto` = tab, span(class = "ico", fa(ico)),
    div(strong(paste0(n, ". ", title)), if (optional) span(class = "opt", "optional"), div(desc)),
    fa("chevron-right"))
}

# text box fields
# Enter the title of the project:
# 1. Analyte Description
# 2. Intended use of the analyte
# 3. Indication
# 4. Scientific validity
# 5. Measurement Scale:
# 6. Sample material :
# 7. Risk classification : Details
# 8. Modification of a existing product (standard) OR new product/development


# Documents to upload or describe (items 9-15)
doc_items <- list(
  list(title = "9. Standard Operating Procedures (SOPs)",
       placeholder = "e.g. document name ‘QM-SOP-12-Analyte’ or https://lab/qm/sops/12.pdf",
       docs = c("SOP template" = "SOP.pdf"), ivdr = c("tocId1131", "tocId1139")),
  list(title = "10. Justification for use of the LDT",
       placeholder = "Compare the LDT with all its characteristics with CE-marked devices available on the EU market",
       ivdr = "tocId115"),
  list(title = "11. Declaration of Conformity",
       placeholder = "Add document(s) declaring that a manufactured or modified CE labelled assay complies with relevant EU law, including analytical and clinical performance.",
       docs = c("Declaration of conformity template" = "Declaration_of_conformity.pdf"), ivdr = "tocId36"),
  list(title = "12. Risk management",
       placeholder = "Add information about safety requirements and safety procedure to minimize the risk for the user and environment. A risk management plan based on the risk assessment over all individual components of an assay has to be created. For modified CE-labelled assays, the manufacturer's safety data sheets for the assay and the additional components added (e.g. diluent) can be used.",
       ivdr = "tocId1131"),
  list(title = "13. Monitoring and review activities",
       placeholder = "Implementation of a system to detect and evaluate effects that can pose an unnecessary risk or damage by implementing appropriate measures",
       ivdr = "tocId115"),
  list(title = "14. Analytical performance", placeholder = "e.g., Consider using QC tool", tool = "QCTool"),
  list(title = "15. Clinical performance", placeholder = "e.g., Consider using MC tool", tool = "MCTool"))

doc_item <- function(i, d) {
  div(class = "doc-item",
      radioButtons(paste0("fileOption", i), label = d$title, inline = TRUE,
                   choiceNames = c("Not applicable", "Upload PDF and/or add text"),
                   choiceValues = c("Not Applicable", "Upload PDF and/or Add Text"),
                   selected = "Not Applicable"),
      conditionalPanel(
        condition = paste0("input.fileOption", i, " != 'Not Applicable'"),
        div(class = "file-row",
            fileInput(paste0("file", i), label = "Upload PDF", accept = ".pdf"),
            actionButton(paste0("resetBtn", i), "Clear", icon = icon("trash"), class = "btn-sm btn-outline-secondary")),
        textAreaInput(paste0("text", i), label = "Enter text", placeholder = d$placeholder, rows = 4)),
      help_links(d$ivdr, d$docs, d$tool))
}

css <- "
body { background: #ced7e0 }
header.top { display: flex; align-items: center; justify-content: space-between; gap: 1rem; min-height: 64px; padding: 0 1rem; background: #40668d }
header.top img { height: 50px }
.brand { color: #ced7e0; font-size: .8rem; line-height: 1.3 }
.brand strong { display: block; color: #fff; font-size: 1.25rem; font-weight: 660; letter-spacing: -.02em; line-height: 1.25 }
nav.steps { position: sticky; top: 0; z-index: 1020; display: flex; align-items: center; justify-content: space-between; gap: 1rem;
            min-height: 44px; padding: 0 1rem; background: #40668d; border-top: 1px solid rgba(255,255,255,.12);
            box-shadow: 0 2px 6px rgba(16,24,40,.15) }
nav.steps .nav { flex-wrap: nowrap; overflow-x: auto; gap: .25rem; scrollbar-width: none }
nav.steps .nav-link { display: flex; align-items: center; gap: .45rem; white-space: nowrap; font-size: .875rem; padding: .55rem .4rem;
                      color: #ced7e0; border-bottom: 2px solid transparent }
nav.steps .nav-link:is(.active, :hover, :focus-visible) { color: #fff; border-color: #fff }
nav.steps .nav-link.active { font-weight: 500 }
.num { display: inline-grid; place-items: center; width: 1.35rem; height: 1.35rem; border-radius: 50%; border: 1px solid currentColor;
       font-size: .72rem; font-weight: 600 }
.nav-link.active .num { background: #fff; border-color: #fff; color: #40668d }
nav.steps .legal { color: #ced7e0; font-size: .8rem; text-decoration: none; white-space: nowrap }
nav.steps .legal:hover { color: #fff; text-decoration: underline }
nav.steps .btn { flex: none; white-space: nowrap }
.card { --bs-card-border-color: rgba(16,24,40,.09); box-shadow: 0 1px 2px rgba(16,24,40,.04), 0 6px 16px -6px rgba(16,24,40,.1) }
.card-header { background: none; font-weight: 600; letter-spacing: -.005em }
.card-header > i { color: #40668d; margin-right: .4rem }
.hero { color: #fff; background: linear-gradient(135deg, #40668d, #00406b); border-radius: .75rem; padding: 2rem; margin-bottom: 1.5rem;
        box-shadow: 0 6px 16px -6px rgba(16,24,40,.3) }
.hero h1 { font-size: clamp(1.5rem, 3vw, 2.1rem); font-weight: 660; letter-spacing: -.02em; max-width: 40ch }
.hero p { max-width: 72ch; color: #e8ecf1; font-size: 1.05rem; line-height: 1.6 }
.eyebrow { font-size: .75rem; font-weight: 600; letter-spacing: .06em; text-transform: uppercase; margin-bottom: .5rem }
.hero .eyebrow { color: #ced7e0 }
.step-head { margin-bottom: 1.25rem }
.step-head .eyebrow { color: #40668d }
.step-head h1 { font-size: 1.6rem; font-weight: 660; letter-spacing: -.02em; margin: 0 0 .35rem }
.step-head p { margin: 0; max-width: 85ch; color: #40444c }
.opt { display: inline-block; margin-left: .5rem; padding: .1rem .5rem; border-radius: 99px; background: #e8ecf1; color: #00406b;
       font-size: .68rem; font-weight: 600; letter-spacing: .04em; text-transform: uppercase; vertical-align: middle }
.workflow { overflow: hidden }
.tool { display: flex; align-items: center; gap: .9rem; padding: .9rem 1rem; color: inherit; text-decoration: none; line-height: 1.4 }
.tool + .tool { border-top: 1px solid rgba(16,24,40,.09) }
.tool:hover { background: #f4f6fa; color: inherit }
.tool:focus-visible { outline: 2px solid #40668d; outline-offset: -2px }
.tool > div { flex: 1 }
.tool strong { font-weight: 600 }
.tool > div > div { font-size: .875rem; color: #40444c }
.tool > i { font-size: .8rem; color: #ced7e0 }
.tool:hover > i { color: #40668d }
.ico { display: grid; place-items: center; flex: none; width: 2.5rem; height: 2.5rem; border-radius: .6rem; background: #e8ecf1; color: #00406b }
.tool:hover .ico, .tool-final .ico { background: #40668d; color: #fff }
.tool-final { background: #f4f6fa }
.about ul { list-style: none; padding: 0; margin: 0 }
.about li { display: flex; gap: .6rem; padding: .35rem 0; font-size: .925rem; line-height: 1.5; color: #40444c }
.about li > i { flex: none; width: 1.1em; margin-top: .25rem; color: #40668d }
.about a { text-decoration: none }
.about a:hover { text-decoration: underline }
.section-title { margin: .25rem 0 .75rem; color: #40668d; font-size: .78rem; font-weight: 600; letter-spacing: .05em; text-transform: uppercase }
.shiny-input-container:not(.shiny-input-container-inline) { width: 100% }
.control-label { font-weight: 500 }
textarea.form-control { resize: vertical }
.help { display: flex; flex-wrap: wrap; gap: .2rem 1rem; margin: -.6rem 0 1.1rem; font-size: .825rem }
.help a { text-decoration: none }
.help a:hover { text-decoration: underline }
.help i { margin-right: .3rem }
.doc-item { margin-bottom: .85rem; padding: .85rem 1rem .1rem; border: 1px solid rgba(16,24,40,.09); border-radius: .6rem }
.doc-item > .shiny-input-container > .control-label { font-weight: 600 }
.file-row { display: flex; gap: .5rem; align-items: flex-end }
.file-row .btn-sm { margin-bottom: 1rem; white-space: nowrap }
.table-scroll { overflow: auto; max-width: 100% }
.bslib-grid-item { min-width: 0 }
.step-footer { display: flex; flex-wrap: wrap; gap: .5rem; align-items: center; margin-top: 1.5rem; padding: .85rem 1rem; background: #fff;
               border-radius: .75rem; box-shadow: 0 1px 2px rgba(16,24,40,.04), 0 6px 16px -6px rgba(16,24,40,.1) }
"

js <- "
$(document).on('click', '[data-goto]', function (e) {
  e.preventDefault();
  Shiny.setInputValue('goto', this.dataset.goto, {priority: 'event'});
});
$(function () {
  Shiny.addCustomMessageHandler('ivd-tab', function (tab) {
    $('nav.steps [data-goto]').each(function () {
      var on = this.dataset.goto === tab;
      $(this).toggleClass('active', on).attr('aria-current', on ? 'page' : null);
    });
    window.scrollTo({top: 0, behavior: 'smooth'});
  });
});
"

#-----------------------------------------------------------------------------------

ui <- page(
  title = "IVDCheckR", lang = "en",
  theme = bs_theme(version = 5, preset = "shiny", primary = "#40668d", "border-radius" = "0.75rem", "card-cap-padding-y" = "0.5rem"),
  tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
  useShinyjs(),

  tags$header(class = "top",
              div(class = "brand", strong("IVDCheckR"),
                  span(class = "d-none d-sm-block", "Documentation of laboratory developed tests according to the EU-IVDR")),
              img(src = "umo.svg", alt = "Universitätsmedizin Oldenburg")),

  # Step bar: always visible (sticky), replaces the old sidebar
  tags$nav(class = "steps", `aria-label` = "Steps",
           tags$ul(class = "nav", lapply(seq_along(STEPS), function(i) {
             tags$li(a(class = c("nav-link", if (i == 1) "active"), href = "#", `data-goto` = names(STEPS)[i],
                       if (i == 1) fa("house") else span(class = "num", i - 1),
                       c("Overview", "Project description", "Method comparison", "Quality control", "Chemicals & software")[i]))
           })),
           div(class = "d-flex align-items-center gap-3",
               a(class = "legal d-none d-md-inline", href = "https://kc.uol.de/disclaimer/", target = "_blank", "Disclaimer"),
               downloadButton("dl_header", "Download PDF", class = "btn-sm btn-light"))),

  tags$main(class = "container-xxl py-4", navset_hidden(id = "tabs",

    nav_panel_hidden("Info",
      div(class = "hero",
          div(class = "eyebrow", "EU-IVDR · Article 5(5) · In-house devices"),
          h1("Document your laboratory developed test step by step"),
          p("IVDCheckR guides you through the documentation of laboratory developed tests (LDTs) required by Article 5 (5) of the
            In Vitro Diagnostic Medical Device Regulation (EU) 2017/746 (EU-IVDR). Work through the steps below – a combined
            PDF report can be downloaded from every page at any time."),
          div(class = "d-flex flex-wrap gap-2 mt-3",
              goto_btn("Project_Details", "Start with step 1", fa("arrow-right"), class = "btn-light btn-lg"),
              a(class = "btn btn-outline-light btn-lg", href = EU_IVDR, target = "_blank", fa("up-right-from-square"), "Official EU-IVDR text"))),

      layout_columns(col_widths = breakpoints(xs = 12, lg = c(7, 5)),
        card(class = "workflow", card_header("Your workflow"), card_body(padding = 0, gap = 0,
          step_row(1, "Project_Details", "paperclip", "LDT project description",
                   "Measurand, intended use, risk class and the documents required by the IVDR (SOPs, risk management, performance, ...)."),
          step_row(2, "MCTool", "vials", "Method comparison",
                   "Passing-Bablok regression and difference plot against a comparison method.", optional = TRUE),
          step_row(3, "QCTool", "check-to-slot", "Quality control",
                   "Imprecision (CV), bias and RMSD from within-run or between-run QC measurements.", optional = TRUE),
          step_row(4, "ChemSoft", "flask-vial", "Chemicals and software",
                   "List of reagents, instruments and software used for the test."),
          downloadLink("dl_overview", class = "tool tool-final", tagList(
            span(class = "ico", fa("file-pdf")),
            div(strong("Download PDF report"), div("One combined PDF with all entries, plots and your uploaded documents.")),
            fa("download"))))),

        div(class = "d-flex flex-column gap-4",
          card(class = "about", card_header("Good to know"), card_body(tags$ul(
            tags$li(fa("arrow-right"), span("Use the ", strong("Next"), " button at the bottom of each page, or jump to any step in the bar at the top.")),
            tags$li(fa("file-pdf"), span("The ", strong("Download PDF"), " button is on every page. The report always contains everything entered so far.")),
            tags$li(fa("circle-check"), span("Method comparison and quality control are optional – skip them if they do not apply.")),
            tags$li(fa("triangle-exclamation"), span("Entries are not saved on the server. Download your PDF before closing or reloading the page."))))),
          card(class = "about", card_header("Links"), card_body(tags$ul(
            tags$li(fa("book-open"), a(href = EU_IVDR, target = "_blank", "Regulation (EU) 2017/746 (EU-IVDR)")),
            tags$li(fa("toolbox"), a(href = "https://kc.uol.de/", target = "_blank", "More LabTools of University Medicine Oldenburg")),
            tags$li(fa("scale-balanced"), a(href = "https://kc.uol.de/disclaimer/", target = "_blank", "Disclaimer"))))))),
      step_footer("Info")),

    nav_panel_hidden("Project_Details",
      step_head(1, "LDT project description",
                "Describe the measurand and its intended use (items 1-8), then state for each IVDR document (items 9-15) whether it
                applies. Existing documents can be uploaded as PDF; they are appended to the report."),
      layout_columns(col_widths = breakpoints(xs = 12, lg = c(6, 6)),
        card(card_header(fa("paperclip"), "Project and measurand"), card_body(
          layout_columns(col_widths = breakpoints(xs = 12, md = c(6, 6)),
            textInput("projectInput", "Project title", value = "", placeholder = "e.g. Sodium in serum"),
            dateRangeInput("dates", "Evaluation period", start = as.character(Sys.Date()), end = as.character(Sys.Date()), separator = "-")),

          textAreaInput("ParameterInfo", label = "1. Measurand description", rows = 3,
                        placeholder = "Add the measurand name, give a general description and information regarding the measurand, e.g., sodium"),
          help_links("tocId1139"),

          textAreaInput("ParameterUse", label = "2. Intended use of the measurand", rows = 3,
                        placeholder = "Specify the intended purpose of the measurand, e.g., detection of hypernatremia or hyponatremia in the general population independent of age, gender and ethnicity."),
          help_links("tocId1138"),

          textAreaInput("Indication", label = "3. Indication", rows = 3,
                        placeholder = "Add the diagnostic indication e.g., hypernatremia or hyponatremia."),
          help_links(c("tocId1131", "tocId1139")),

          textAreaInput("ScientificValidity", label = "4. Scientific validity", rows = 3,
                        placeholder = "Enter the scientific association of the analyte with a specific clinical or physiological condition."),
          help_links("tocId1191"),

          radioButtons("MeasurementScale", label = "5. Measurement scale", inline = TRUE,
                       choices = c("Quantitative", "Qualitative", "Semi-quantitative"), selected = "Quantitative"),
          help_links("tocId1139"),

          checkboxGroupInput("SampleMaterial", label = "6. Sample material", inline = TRUE,
                             choices = c("Serum", "Heparin plasma", "Citrate plasma", "EDTA", "Whole blood", "Urine", "CSF", "Other")),
          conditionalPanel(condition = "(input.SampleMaterial || []).includes('Other')",
                           textInput("others_SampleMaterial", "Please specify:")),
          help_links(c("tocId1131", "tocId1139")),

          selectInput("RiskClass", label = "7. Risk classification", choices = c("A", "B", "C", "D"), selected = "B"),
          textAreaInput("RiskClassDetails", label = "Details", rows = 3,
                        placeholder = "The risk classification of an IVD is divided into: A-D classes (low to high risk) which is classified to indicate the risk arising from the device/assay regarding users, patients and third parties"),
          help_links("tocId1169", c("Detailed risk classification" = "Table 1_ RiskClassification.png")),

          radioButtons("ModificationProduct", label = "8. Modification of an existing product (standard) OR new product/development",
                       choices = c("Yes", "No"), selected = "Yes", inline = TRUE))),

        card(card_header(fa("folder-open"), "IVDR documentation"), card_body(
          p(class = "text-muted small mb-1", "For each item choose whether it applies. If it does, upload a PDF and/or add a short text."),
          lapply(seq_along(doc_items), function(i) doc_item(i, doc_items[[i]])),
          # additional textAreaInput area for extra comments
          textAreaInput("extraText", label = "Further comments", placeholder = "Enter text", rows = 4)))),
      step_footer("Project_Details")),

    nav_panel_hidden("MCTool",
      step_head(2, "Method comparison",
                "Compare two methods with Passing-Bablok regression and a difference plot. Enter or paste the paired results of
                method x and method y and press Calculate. The plots are included in the PDF report.", optional = TRUE),
      layout_columns(col_widths = breakpoints(xs = 12, lg = c(6, 6)),
        card(card_header(fa("vials"), "Data entry"), card_body(
          textInput("descx", "Method x", "X"),
          div(class = "control-label mb-2", "Reference range of method x (from / to)"),
          layout_columns(col_widths = c(6, 6), numericInput("refllx", NULL, 0), numericInput("refulx", NULL, 0)),
          textInput("descy", "Method y", "Y"),
          div(class = "control-label mb-2", "Reference range of method y (from / to)"),
          layout_columns(col_widths = c(6, 6), numericInput("reflly", NULL, 0), numericInput("refuly", NULL, 0)),
          actionButton("action", label = "Calculate", icon = icon("calculator"), class = "btn-primary"),
          div(class = "section-title mt-3", "Raw values for method x and y (type or copy/paste, e.g. from Excel)"),
          div(class = "table-scroll", rHandsontableOutput("mctable")),
          uiOutput("validation_message")  # Display validation message
        )),
        card(card_header(fa("chart-line"), "Plots"), card_body(
          p(class = "text-muted small", "The plots appear here after pressing Calculate."),
          plotOutput("pbblot"),
          plotOutput("diffplot")))),
      step_footer("MCTool")),

    nav_panel_hidden("QCTool",
      step_head(3, "Quality control",
                "Enter the target values and the measured QC results (up to three levels) and press Calculate to get mean, CV,
                bias and RMSD. The results are included in the PDF report.", optional = TRUE),
      layout_columns(col_widths = breakpoints(xs = 12, lg = c(6, 6)),
        card(card_header(fa("check-to-slot"), "Data entry"), card_body(
          textInput('analyte', label = "Analyte", value = "Analyte"),
          textInput('description', label = "Description", value = NULL),
          radioButtons("withbetw", "QC type", c("within-run" = "withinrun", "between-run" = "betweenrun"), inline = TRUE),
          div(class = "control-label mb-2", "Target values"),
          layout_columns(col_widths = c(4, 4, 4),
                         numericInput('qc1target', label = 'QC1', value = 0),
                         numericInput('qc2target', label = 'QC2', value = 0),
                         numericInput('qc3target', label = 'QC3', value = 0)),
          actionButton("action2", label = "Calculate", icon = icon("calculator"), class = "btn-primary"),
          div(class = "section-title mt-3", "Measured QC values"),
          div(class = "table-scroll", rHandsontableOutput("QCtable")))),
        card(card_header(fa("table"), "Results"), card_body(tableOutput("restab")))),
      step_footer("QCTool")),

    nav_panel_hidden("ChemSoft",
      step_head(4, "Chemicals and software",
                "List all reagents, calibrators, controls, instruments and software used for the LDT. Use Add row for more lines
                (or right-click into the table)."),
      card(card_header(fa("flask-vial"), "List of used chemicals and software"), card_body(
        div(actionButton("add_row", "Add row", icon = icon("plus"), class = "btn-outline-primary")),
        div(class = "table-scroll", rHandsontableOutput("exctable", width = 800, height = 800)))),
      step_footer("ChemSoft"))
  ))
)

server <- function(input, output, session) {


  observeEvent(input$projectInput, {
    # Remove special characters from projectInput
    updateTextInput(session, "projectInput", value = gsub("[^[:alnum:] ]", "", input$projectInput))
  })

  # navigation: step bar, step cards, Back/Next and Start buttons all send input$goto
  observeEvent(input$goto, nav_select("tabs", input$goto))
  observeEvent(input$tabs, session$sendCustomMessage("ivd-tab", input$tabs))

  #ChemSoft
  # Create the initial data frame
  initial_data <- data.frame(
    Product = "",
    Manufacturer = "",
    Product_number = "",
    Use_case = "",
    Refered_document = "",
    stringsAsFactors = FALSE
  )

  # Define the columns for the table
  columns <- data.frame(
    title = c('Product/Instrument/Software', 'Manufacturer', 'Product Number', 'Use Case','Referred Document'),
    type = c("text","text","text","text","text"),
    stringsAsFactors = FALSE
  )


  # Rename the columns
  renamed_data <- dplyr::rename(initial_data,
                                "Product/Instrument/Software" = Product,
                                "Manufacturer"= Manufacturer,
                                "Product Number"= Product_number,
                                "Use Case" = Use_case,
                                "Referred Document" = Refered_document)

  #  a reactiveVal to hold the data
  table_data <- reactiveVal(renamed_data)

  # row button logic
  observeEvent(input$add_row, {
    new_row <- renamed_data[1, ]
    new_row[] <- ""   # blank row
    table_data(rbind(table_data(), new_row))
  })

  # Update table from user edits
  observe({
    if (!is.null(input$exctable)) {
      table_data(hot_to_r(input$exctable))
    }
  })

  output$exctable <- renderRHandsontable({
    rhandsontable(table_data(), manualColumnResize = TRUE, useTypes = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })


  # Individual reset buttons for each file input; a cleared file must not end up in the report
  cleared <- reactiveValues()
  lapply(1:7, function(i) {
    id <- paste0("file", i)
    observeEvent(input[[id]], {
      cleared[[id]] <- FALSE
      shinyjs::enable(paste0("resetBtn", i))
    })
    observeEvent(input[[paste0("resetBtn", i)]], {
      shinyjs::reset(id)
      cleared[[id]] <- TRUE
    })
  })

  #mc part
  mcdf <- data.frame(x = rep(NA, 50),
                     y = rep(NA, 50))
  mcdf$x = as.numeric(mcdf$x)
  mcdf$y = as.numeric(mcdf$y)

  output$mctable <-
    renderRHandsontable(rhandsontable(mcdf, width = 500, height = 800))


  plot1 <- function(mcdf) {
    par(pty = "s")
    reg <- mcreg(mcdf$x, mcdf$y, method.reg = "PaBa")
    MCResult.plot(
      reg,
      main = input$method,
      sub = " ",
      equal.axis = TRUE,
      x.lab = input$descx,
      y.lab = input$descy,
      points.cex = 1,
      reg = TRUE,
      reg.col = "#003f6b",
      identity.col = "#00abda",
      add.grid = FALSE,
      add.legend = FALSE,
      ci.area = TRUE,
      add.cor = TRUE,
      cor.method = "spearman"
    )
    list(reg)
    includeLegend(
      colors = c("#003f6b"),
      model.names = c("PaBa"),
      models = list(reg),
      bg = NULL,
      cex = 1.0,
      design = "2",
      digits = 2
    )
    refrangecol = adjustcolor("black", alpha.f = 0.05)
    if (input$refllx >= 0 &&
        input$reflly >= 0 &&
        input$refulx > input$refllx &&
        input$refuly > input$reflly) {
      rect(
        input$refllx,input$reflly,input$refulx,input$refuly,
        col = refrangecol,
        border = FALSE
      )
      abline(v = input$refllx, col = refrangecol)
      abline(v = input$refulx, col = refrangecol)
      abline(h = input$reflly, col = refrangecol)
      abline(h = input$refuly, col = refrangecol)
    }
  }

  plot2 <- function(mcdf) {
    par(pty = "s")
    mean <- (mcdf$x + mcdf$y) / 2
    diff <- (mcdf$y - mcdf$x) * 100 / mean
    plot(
      mean,
      diff,
      axis = TRUE,
      main = "difference plot",
      xlab = "(x + y) / 2",
      ylab = "(y - x) / ((x + y) / 2) [%]",
      xlim = c(min(mcdf), max(mcdf)),
      ylim = c(min(c(diff,-100)), max(c(diff, 100)))
    )
    abline(h = 0,col = "#00abda",lty = 2)
    abline(h = mean(diff), col = "#003f6b")
    abline(h = mean(diff) - sd(diff),col = "gray",lty = 2)
    abline(h = mean(diff) + sd(diff),col = "gray",lty = 2)
  }


  observeEvent(input$action, {
    mcdf <- hot_to_r(input$mctable)
    mcdf <- mcdf[!is.na(as.numeric(mcdf$x)),]
    mcdf <- mcdf[!is.na(as.numeric(mcdf$y)),]
    output$pbblot <- renderPlot(plot1(mcdf))
    output$diffplot <- renderPlot(plot2(mcdf))
  })

  # QC part
  qc_df <-
    data.frame(qc1 = rep(NA, 21),
               qc2 = rep(NA, 21),
               qc3 = rep(NA, 21))
  qc_df$qc1 = as.numeric(qc_df$qc1)
  qc_df$qc2 = as.numeric(qc_df$qc2)
  qc_df$qc3 = as.numeric(qc_df$qc3)

  calcs <- function(df) {
    cv_qc1 <- sd(df$qc1, na.rm = TRUE) / mean(df$qc1, na.rm = TRUE) * 100
    cv_qc2 <- sd(df$qc2, na.rm = TRUE) / mean(df$qc2, na.rm = TRUE) * 100
    cv_qc3 <- sd(df$qc3, na.rm = TRUE) / mean(df$qc3, na.rm = TRUE) * 100
    bias_qc1 <- (mean(df$qc1, na.rm = TRUE) - input$qc1target) / input$qc1target * 100
    bias_qc2 <- (mean(df$qc2, na.rm = TRUE) - input$qc2target) / input$qc2target * 100
    bias_qc3 <- (mean(df$qc3, na.rm = TRUE) - input$qc3target) / input$qc3target * 100
    rmsd_qc1 <- sqrt(mean((df$qc1 - input$qc1target) ^ 2, na.rm = TRUE)) / input$qc1target * 100
    rmsd_qc2 <- sqrt(mean((df$qc2 - input$qc2target) ^ 2, na.rm = TRUE)) / input$qc2target * 100
    rmsd_qc3 <- sqrt(mean((df$qc3 - input$qc3target) ^ 2, na.rm = TRUE)) / input$qc3target * 100

    QCresults <-
      data.frame(
        c(mean(df$qc1, na.rm = TRUE),input$qc1target,cv_qc1,bias_qc1,rmsd_qc1),
        c(mean(df$qc2, na.rm = TRUE),input$qc2target,cv_qc2,bias_qc2,rmsd_qc2),
        c(mean(df$qc3, na.rm = TRUE),input$qc3target,cv_qc3,bias_qc3,rmsd_qc3)
      )
    colnames(QCresults) <- c("QC1", "QC2", "QC3")
    rownames(QCresults) <- c("Mean", "Target value", "CV [%]", "Bias [%]", "RMSD [%]")
    QCresults
  }

  # Define rendering of QC table
  output$QCtable <- renderRHandsontable({
    rhandsontable(qc_df, width = 500, height = 800)
  })

  # Define rendering of QC results
  output$restab <- renderTable({
    if (!is.null(input$QCtable) && nrow(qc_df) >= 2) {
      df <- hot_to_r(input$QCtable)
      calcs(df)
    }
  }, rownames = TRUE)

  # Define the server logic for processing QC data upon button click
  observeEvent(input$action2, {
    if (!is.null(input$QCtable) && nrow(qc_df) >= 2) {
      df <- hot_to_r(input$QCtable)
      output$restab <- renderTable(calcs(df), rownames = TRUE)
    }
  })


  # The same PDF report can be downloaded from the header and from every page
  report_download <- function() downloadHandler(
    filename = function() {
      title <- gsub(" ", "_", trimws(input$projectInput))
      paste0(if (nzchar(title)) title else "IVDCheckR_report", ".pdf")
    },

    content = function(file) {
      showNotification("Creating PDF report ...", id = "pdf_busy", duration = NULL)
      on.exit(removeNotification("pdf_busy"), add = TRUE)

      # Every download gets its own folder so simultaneous users cannot overwrite each other's report
      work <- tempfile("report")
      dir.create(work)
      on.exit(unlink(work, recursive = TRUE), add = TRUE)
      file.copy(file.path(tempdir(), 'ivdcheckr.rmd'), work)

      samples <- setdiff(input$SampleMaterial, "Other")
      if ("Other" %in% input$SampleMaterial) {
        other <- trimws(input$others_SampleMaterial)
        samples <- c(samples, if (nzchar(other)) paste("Other:", other) else "Other")
      }

      # Items 9-15: what applies, what was written and which PDF belongs to it
      docs <- lapply(seq_along(doc_items), function(i) {
        applicable <- !identical(input[[paste0("fileOption", i)]], "Not Applicable")
        upload <- input[[paste0("file", i)]]
        if (!applicable || isTRUE(cleared[[paste0("file", i)]])) upload <- NULL
        text <- input[[paste0("text", i)]]
        list(title = doc_items[[i]]$title, applicable = applicable, text = text, file = upload,
             provided = applicable && (!is.null(upload) || nzchar(trimws(report_chars(text)))))
      })

      # MC part (the tables only exist once their page has been opened)
      mc <- list(ok = FALSE, msg = "No data entered (optional step).")
      mc_raw <- if (is.null(input$mctable)) mcdf else hot_to_r(input$mctable)
      mc_raw <- data.frame(x = suppressWarnings(as.numeric(mc_raw$x)), y = suppressWarnings(as.numeric(mc_raw$y)))
      mc_raw <- mc_raw[!is.na(mc_raw$x) & !is.na(mc_raw$y), ]
      if (nrow(mc_raw) == 1) mc$msg <- "Only one value pair was entered; at least two are needed."
      if (nrow(mc_raw) >= 2) {
        reg <- tryCatch(mcreg(mc_raw$x, mc_raw$y, method.reg = "PaBa"), error = function(e) NULL)
        if (is.null(reg)) {
          mc$msg <- "The regression could not be calculated from the entered values."
        } else {
          co <- getCoefficients(reg)
          ref_range <- function(lo, up) if (isTRUE(up > lo)) paste(lo, "to", up) else "Not specified"
          mc <- list(
            ok = TRUE, data = mc_raw,
            summary = data.frame(Coefficient = c("Intercept", "Slope"), Estimate = report_num(co[, "EST"], 3),
                                 `95% CI, lower` = report_num(co[, "LCI"], 3), `95% CI, upper` = report_num(co[, "UCI"], 3), check.names = FALSE),
            info = data.frame(
              Parameter = c("Method x", "Method y", "Reference range x", "Reference range y", "Regression function", "Spearman correlation", "Value pairs"),
              Value = c(input$descx, input$descy, ref_range(input$refllx, input$refulx), ref_range(input$reflly, input$refuly),
                        sprintf("y = %s + %s * x", report_num(co[1, "EST"], 3), report_num(co[2, "EST"], 3)),
                        report_num(cor(mc_raw$x, mc_raw$y, method = "spearman"), 3), nrow(mc_raw))),
            raw = setNames(data.frame(seq_len(nrow(mc_raw)), mc_raw$x, mc_raw$y), make.unique(c("No.", input$descx, input$descy))))
        }
      }

      # QC part
      qc_raw <- if (is.null(input$QCtable)) qc_df else hot_to_r(input$QCtable)
      qc_raw[] <- lapply(qc_raw, function(v) suppressWarnings(as.numeric(v)))
      levels <- names(qc_raw)[vapply(qc_raw, function(v) any(!is.na(v)), TRUE)]
      qc <- list(ok = length(levels) > 0)
      if (qc$ok) {
        res <- calcs(qc_raw)[, toupper(levels), drop = FALSE]
        rows <- qc_raw[rowSums(!is.na(qc_raw[levels])) > 0, levels, drop = FALSE]
        qc$info <- data.frame(Parameter = c("Analyte", "Description", "QC type"),
                              Value = c(input$analyte, input$description, if (identical(input$withbetw, "betweenrun")) "Between-run" else "Within-run"))
        qc$results <- cbind(Metric = rownames(res), as.data.frame(lapply(res, report_num)), stringsAsFactors = FALSE)
        qc$no_target <- any(unlist(res["Target value", ]) <= 0, na.rm = TRUE)
        qc$raw <- setNames(cbind(seq_len(nrow(rows)), rows), c("No.", toupper(levels)))
        qc$raw[] <- lapply(qc$raw, function(v) ifelse(is.na(v), "", as.character(v)))
      }

      # List of chemicals and software: only filled rows
      chem <- table_data()
      chem <- chem[Reduce(`|`, lapply(chem, function(v) nzchar(trimws(ifelse(is.na(v), "", as.character(v)))))), , drop = FALSE]

      r <- list(
        title = trimws(input$projectInput),
        period = paste(input$dates[1], input$dates[2], sep = " to "),
        created = format(Sys.Date(), "%Y-%m-%d"),
        items = list(
          list(label = "1. Measurand description", value = input$ParameterInfo),
          list(label = "2. Intended use of the measurand", value = input$ParameterUse),
          list(label = "3. Indication", value = input$Indication),
          list(label = "4. Scientific validity", value = input$ScientificValidity),
          list(label = "5. Measurement scale", value = input$MeasurementScale),
          list(label = "6. Sample material", value = paste(samples, collapse = ", ")),
          list(label = "7. Risk classification", value = paste0(
            "Class ", input$RiskClass, if (nzchar(trimws(input$RiskClassDetails))) paste0("\n", input$RiskClassDetails))),
          list(label = "8. Modification of an existing product (standard) OR new product/development", value = input$ModificationProduct)),
        docs = docs, extra = input$extraText, mc = mc, qc = qc, chem = chem)

      preamble <- file.path(work, "preamble.tex")
      writeLines(enc2utf8(report_preamble(r$title)), preamble, useBytes = TRUE)

      # Render the R Markdown file (plot1/plot2 are found through the parent environment)
      out <- rmarkdown::render(
        file.path(work, 'ivdcheckr.rmd'),
        output_file = "ivdcheckr.pdf",
        params = list(r = r),
        envir = new.env(parent = environment()),
        quiet = TRUE,
        output_format = pdf_document(latex_engine = "pdflatex", includes = includes(in_header = preamble),
                                     md_extensions = "-tex_math_single_backslash")
      )

      # Append the uploaded PDF files
      uploaded_pdf_paths <- unlist(lapply(docs, function(d) d$file$datapath))

      if (length(uploaded_pdf_paths) > 0) {
        pdf_combine(c(out, uploaded_pdf_paths), file)
      } else {
        file.copy(out, file, overwrite = TRUE)
      }
    }
  )

  for (id in c("dl_header", "dl_overview", paste0("dl_", names(STEPS)))) {
    output[[id]] <- report_download()
  }

}



shinyApp(ui, server)
