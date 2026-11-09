#libraries
pacman::p_load(shiny,bslib,rhandsontable,shinyjs,pdftools,png,mcr,shinycssloaders,htmlwidgets)

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


#-----------------------------------------------------------------------------------
# PDF report engine (base-R graphics on a cairo_pdf device, no LaTeX)
# Same engine and page design as the Peer Review reports (render_flex_report_pdf
# in peerreviewrelease/app.R) and the Wartungsplan report; only the labels are in
# English here and very long words are broken up in .pdfreport_wrap_tokens().

PDFREPORT_COL_PRIMARY <- "#003B73"
PDFREPORT_COL_ACCENT  <- "#136377"
PDFREPORT_COL_ROW_ALT <- "#eef3f8"
PDFREPORT_COL_BORDER  <- "#c2cede"
PDFREPORT_COL_TEXT    <- "#1a1a1a"
PDFREPORT_COL_MUTED   <- "#555555"

# Opens a cairo_pdf device (falls back to the base pdf() device if cairo
# support isn't available on the server), matching render_wartungsplan_pdf().
.pdfreport_open_device <- function(file, width, height) {
  opened <- tryCatch({
    grDevices::cairo_pdf(file, width = width, height = height, onefile = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!opened) grDevices::pdf(file, width = width, height = height)
  invisible(opened)
}

# Same fix as wartungsplan's fix_uplus(): on a server without a UTF-8
# locale, characters R can't display get replaced with "<U+00D8>"-style
# placeholders; this converts them back to the real character.
.pdfreport_fix_uplus <- function(x) {
  x <- enc2utf8(as.character(x))
  m <- regmatches(x, gregexpr("<U\\+[0-9A-Fa-f]{4,6}>", x))
  for (i in seq_along(x)) {
    if (!length(m[[i]])) next
    for (tok in unique(m[[i]])) {
      cp <- strtoi(substr(tok, 4, nchar(tok) - 1), 16L)
      ch <- tryCatch(intToUtf8(cp), error = function(e) "")
      if (nzchar(ch)) x[i] <- gsub(tok, ch, x[i], fixed = TRUE)
    }
  }
  x
}

# ---- rich-text markup: "**bold**", "++underline++", "- " bullet lines ----
# Same markup convention the old LaTeX renderer (format_richtext_cell)
# understood, so text users already typed keeps rendering the same way.
.pdfreport_parse_richtext <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text) ||
      !nzchar(trimws(as.character(text))))
    return(list())
  text <- .pdfreport_fix_uplus(text)
  lines <- strsplit(as.character(text), "\n")[[1]]
  lapply(lines, function(ln) {
    bullet <- grepl("^- ", ln)
    if (bullet) ln <- sub("^- ", "", ln)
    runs <- list()
    remaining <- ln
    bold_open <- FALSE
    underline_open <- FALSE
    repeat {
      pb <- regexpr("**", remaining, fixed = TRUE)
      pu <- regexpr("++", remaining, fixed = TRUE)
      pb_val <- if (pb > 0) pb else Inf
      pu_val <- if (pu > 0) pu else Inf
      if (is.infinite(pb_val) && is.infinite(pu_val)) {
        if (nzchar(remaining))
          runs[[length(runs) + 1]] <- list(text = remaining, bold = bold_open, underline = underline_open)
        break
      }
      if (pb_val <= pu_val) {
        seg <- substr(remaining, 1, pb_val - 1)
        if (nzchar(seg))
          runs[[length(runs) + 1]] <- list(text = seg, bold = bold_open, underline = underline_open)
        bold_open <- !bold_open
        remaining <- substring(remaining, pb_val + 2)
      } else {
        seg <- substr(remaining, 1, pu_val - 1)
        if (nzchar(seg))
          runs[[length(runs) + 1]] <- list(text = seg, bold = bold_open, underline = underline_open)
        underline_open <- !underline_open
        remaining <- substring(remaining, pu_val + 2)
      }
    }
    list(bullet = bullet, runs = runs)
  })
}

.pdfreport_tokenize_runs <- function(runs) {
  toks <- list()
  for (r in runs) {
    words <- strsplit(r$text, " ", fixed = TRUE)[[1]]
    words <- words[nchar(words) > 0]
    for (w in words)
      toks[[length(toks) + 1]] <- list(text = w, bold = r$bold, underline = r$underline)
  }
  toks
}

# Wraps a flat token list to `avail_w` (in the caller's 0-100 units), using
# physical-inch text metrics converted through `x_scale` (units per inch).
.pdfreport_wrap_tokens <- function(tokens, avail_w, cex, x_scale) {
  if (!length(tokens)) return(list(list()))
  w_of <- function(txt, font) strwidth(txt, units = "inches", cex = cex, font = font) * x_scale
  space_w <- w_of(" ", 1)
  # A single word wider than the column (file name, URL) is broken up, otherwise
  # it would run into the next column or off the page.
  split_long <- function(tok) {
    fnt <- if (isTRUE(tok$bold)) 2 else 1
    if (w_of(tok$text, fnt) <= avail_w) return(list(tok))
    parts <- character(0); part <- ""
    for (ch in strsplit(tok$text, "")[[1]]) {
      if (nzchar(part) && w_of(paste0(part, ch), fnt) > avail_w) {
        parts <- c(parts, part); part <- ch
      } else part <- paste0(part, ch)
    }
    lapply(c(parts, part), function(p) { tok$text <- p; tok })
  }
  tokens <- unlist(lapply(tokens, split_long), recursive = FALSE)
  out <- list(); cur <- list(); cur_w <- 0
  for (tok in tokens) {
    fnt <- if (isTRUE(tok$bold)) 2 else 1
    tw <- w_of(tok$text, fnt)
    add <- tw + if (length(cur)) space_w else 0
    if (length(cur) && cur_w + add > avail_w) {
      out[[length(out) + 1]] <- cur
      cur <- list(tok); cur_w <- tw
    } else {
      cur[[length(cur) + 1]] <- tok
      cur_w <- cur_w + add
    }
  }
  if (length(cur)) out[[length(out) + 1]] <- cur
  out
}

# Lays out a full text cell (possibly multi-line/bulleted) into wrapped
# visual lines ready to draw. `plain = TRUE` skips **/++ markup parsing
# (used for the Frage/Antwort columns, which never contain that markup).
.pdfreport_layout_text <- function(text, avail_w, cex, x_scale, plain = FALSE, bold = FALSE) {
  text <- if (is.null(text) || (length(text) == 1 && is.na(text))) "" else as.character(text)
  if (!nzchar(trimws(text))) return(list(list(tokens = list(), indent = 0, bullet = FALSE)))
  
  if (plain) {
    text <- .pdfreport_fix_uplus(text)
    words <- strsplit(text, "\\s+")[[1]]
    words <- words[nchar(words) > 0]
    toks <- lapply(words, function(w) list(text = w, bold = bold, underline = FALSE))
    wrapped <- .pdfreport_wrap_tokens(toks, avail_w, cex, x_scale)
    return(lapply(wrapped, function(w) list(tokens = w, indent = 0, bullet = FALSE)))
  }
  
  parsed <- .pdfreport_parse_richtext(text)
  bullet_indent <- strwidth("•  ", units = "inches", cex = cex) * x_scale
  out <- list()
  for (ln in parsed) {
    toks <- .pdfreport_tokenize_runs(ln$runs)
    indent <- if (ln$bullet) bullet_indent else 0
    wrapped <- .pdfreport_wrap_tokens(toks, avail_w - indent, cex, x_scale)
    if (!length(wrapped)) wrapped <- list(list())
    for (i in seq_along(wrapped)) {
      out[[length(out) + 1]] <- list(
        tokens = wrapped[[i]],
        indent = indent,
        bullet = ln$bullet && i == 1
      )
    }
  }
  if (!length(out)) out <- list(list(tokens = list(), indent = 0, bullet = FALSE))
  out
}

# Draws pre-wrapped lines top-down starting at (x0, y_top); returns the y
# coordinate immediately below the last line drawn.
.pdfreport_draw_lines <- function(lines, x0, y_top, cex, line_h, x_scale,
                                  col = PDFREPORT_COL_TEXT, align = "left", width = NULL) {
  y <- y_top
  for (ln in lines) {
    line_w <- 0
    if (align != "left" && length(ln$tokens)) {
      for (tok in ln$tokens) {
        fnt <- if (isTRUE(tok$bold)) 2 else 1
        line_w <- line_w + strwidth(tok$text, units = "inches", cex = cex, font = fnt) * x_scale
      }
      if (length(ln$tokens) > 1)
        line_w <- line_w + (length(ln$tokens) - 1) * strwidth(" ", units = "inches", cex = cex) * x_scale
    }
    x <- x0 + ln$indent
    if (align == "center" && !is.null(width)) x <- x0 + max(0, (width - line_w) / 2)
    ty <- y - line_h * 0.5
    if (isTRUE(ln$bullet)) text(x0, ty, "•", cex = cex, adj = c(0, 0.5), col = col)
    for (tok in ln$tokens) {
      fnt <- if (isTRUE(tok$bold)) 2 else 1
      text(x, ty, tok$text, cex = cex, font = fnt, adj = c(0, 0.5), col = col)
      tw <- strwidth(tok$text, units = "inches", cex = cex, font = fnt) * x_scale
      if (isTRUE(tok$underline))
        segments(x, ty - line_h * 0.32, x + tw, ty - line_h * 0.32, col = col, lwd = 0.6)
      x <- x + tw + strwidth(" ", units = "inches", cex = cex) * x_scale
    }
    y <- y - line_h
  }
  y
}

# ---- Shared page chrome --------------------------------------------------
# Every report renderer calls these same three functions, so every PDF the
# app produces shares the exact same brand header bar, meta block, footer
# and cover/TOC page - "same format and colour" by construction rather than
# by each renderer copying the look separately.
#
# .pdfreport_draw_header() draws (or, with dry_run = TRUE, just measures)
# the top brand bar + meta lines + optional section heading, and returns the
# y coordinate where the page's own content should start - so pagination
# math (Step 1, below) and actual drawing (Step 2) always agree, even
# though the header's height varies with how many meta_lines are passed.
.pdfreport_draw_header <- function(title, report_type, meta_lines, heading = NULL, dry_run = FALSE) {
  if (!dry_run) {
    rect(0, 94, 100, 100, col = PDFREPORT_COL_PRIMARY, border = NA)
    text(2, 97.4, toupper(title), col = "white", cex = 1.15, font = 2, adj = c(0, 0.5))
    if (nzchar(report_type))
      text(98, 97.4, report_type, col = "white", cex = 0.72, adj = c(1, 0.5))
  }
  
  y <- 91.5
  for (ml in meta_lines) {
    if (!dry_run) text(2, y, ml, cex = 0.62, adj = c(0, 0.5), col = PDFREPORT_COL_MUTED)
    y <- y - 1.7
  }
  divider_y <- y + 0.9
  if (!dry_run) segments(2, divider_y, 98, divider_y, col = PDFREPORT_COL_ACCENT, lwd = 1.2)
  
  heading_y <- divider_y - 2.3
  if (!is.null(heading) && nzchar(heading)) {
    if (!dry_run) text(2, heading_y, heading, cex = 0.95, font = 2, col = PDFREPORT_COL_PRIMARY, adj = c(0, 0.5))
    return(invisible(heading_y - 3))
  }
  invisible(divider_y - 2)
}

.pdfreport_draw_footer <- function(footer_text, pg_idx, n_pages, x_left = 4, x_right = 96) {
  if (nzchar(footer_text)) text(x_left, 1.4, footer_text, cex = 0.5, adj = c(0, 0.5), col = PDFREPORT_COL_MUTED)
  text(x_right, 1.4, sprintf("Page %d of %d", pg_idx, n_pages), cex = 0.5, adj = c(1, 0.5), col = PDFREPORT_COL_MUTED)
}

# Draws the report's page 1: project title, an optional confidentiality/
# consent paragraph, and an "Inhalt" table of contents (heading + page
# number per section) - built from the page plan every renderer already
# computes, so the page numbers are always exactly right.

# ---- Cover / "Inhalt" (table of contents) pages --------------------------
# Packs the title, the confidentiality/consent paragraph and a (possibly
# multi-level, dot-leadered) table of contents into one or more cover
# pages - a long or nested TOC now flows onto extra pages instead of
# running off the bottom of a single one. `toc_entries` is a list of
# list(text = "...", page = <int>, level = 1|2|3); level 1 is bold with no
# leader (a top-level "N. Heading"), levels 2/3 are indented with a dotted
# leader to the page number (matching a normal LaTeX/Word table of
# contents).
.pdfreport_build_cover_pages <- function(project_title, intro_lines, toc_entries,
                                         cex_body, line_h, x_scale, avail_h,
                                         x_left = 4, x_right = 96) {
  content_w <- x_right - x_left
  units <- list()
  
  if (nzchar(project_title))
    units[[length(units) + 1]] <- list(kind = "cover_title", height = line_h * 2.2)
  
  for (para in intro_lines) {
    lines <- .pdfreport_layout_text(para, content_w, cex_body, x_scale, plain = TRUE)
    for (ln in lines) units[[length(units) + 1]] <- list(kind = "text_line", line = ln, height = line_h)
    units[[length(units) + 1]] <- list(kind = "gap", height = line_h * 0.5)
  }
  if (length(intro_lines)) units[[length(units) + 1]] <- list(kind = "gap", height = line_h * 1.4)
  
  if (length(toc_entries)) {
    units[[length(units) + 1]] <- list(kind = "toc_heading", height = line_h * 2.2)
    for (entry in toc_entries) {
      lvl <- if (!is.null(entry$level)) entry$level else 1
      h <- if (lvl >= 3) line_h * 1.5 else if (lvl == 2) line_h * 1.6 else line_h * 1.9
      units[[length(units) + 1]] <- list(kind = "toc_entry", entry = entry, height = h)
    }
  }
  
  pages <- list(); cur <- list(); cur_h <- 0
  flush <- function() {
    if (!length(cur)) return(invisible(NULL))
    pages[[length(pages) + 1]] <<- cur
    cur <<- list(); cur_h <<- 0
  }
  for (u in units) {
    if (length(cur) && cur_h + u$height > avail_h) flush()
    cur[[length(cur) + 1]] <- u
    cur_h <- cur_h + u$height
  }
  flush()
  if (!length(pages)) pages <- list(list())
  pages
}

.pdfreport_draw_cover_page <- function(items, project_title, y_start, cex_body, line_h, x_scale,
                                       x_left = 4, x_right = 96) {
  y <- y_start
  for (item in items) {
    if (identical(item$kind, "cover_title")) {
      text(50, y - line_h, project_title, cex = 1.25, font = 2, col = PDFREPORT_COL_PRIMARY, adj = c(0.5, 0.5))
      y <- y - item$height
    } else if (identical(item$kind, "text_line")) {
      y <- .pdfreport_draw_lines(list(item$line), x_left, y, cex_body, line_h, x_scale)
    } else if (identical(item$kind, "gap")) {
      y <- y - item$height
    } else if (identical(item$kind, "toc_heading")) {
      text(x_left, y - line_h * 0.9, "Contents", cex = 1.0, font = 2, col = PDFREPORT_COL_PRIMARY, adj = c(0, 0.5))
      segments(x_left, y - line_h * 1.7, x_right, y - line_h * 1.7, col = PDFREPORT_COL_ACCENT, lwd = 0.8)
      y <- y - item$height
    } else if (identical(item$kind, "toc_entry")) {
      entry <- item$entry
      lvl <- if (!is.null(entry$level)) entry$level else 1
      indent <- (lvl - 1) * 3.5
      cex_e <- if (lvl == 1) 0.78 else if (lvl == 2) 0.72 else 0.66
      font_e <- if (lvl == 1) 2 else 1
      col_e <- if (lvl == 1) PDFREPORT_COL_PRIMARY else PDFREPORT_COL_TEXT
      ty <- y - item$height * 0.55
      text(x_left + indent, ty, entry$text, cex = cex_e, font = font_e, col = col_e, adj = c(0, 0.5))
      page_str <- as.character(entry$page)
      pw <- strwidth(page_str, units = "inches", cex = cex_e) * x_scale
      text(x_right, ty, page_str, cex = cex_e, adj = c(1, 0.5), col = PDFREPORT_COL_MUTED)
      if (lvl > 1) {
        text_w <- strwidth(entry$text, units = "inches", cex = cex_e, font = font_e) * x_scale
        x1 <- x_left + indent + text_w + 1
        x2 <- x_right - pw - 1.5
        if (x2 > x1) segments(x1, ty - 0.1, x2, ty - 0.1, col = PDFREPORT_COL_BORDER, lwd = 0.5, lty = 3)
      }
      y <- y - item$height
    }
  }
  invisible(y)
}

# Lays out one N-column table's rows (generalizes the Frage/Antwort/
# Begruendung row model in render_qa_report_pdf to arbitrary columns).
.pdfreport_layout_table <- function(columns, data, cex, x_scale, x_left, x_right, line_h, row_pad) {
  content_w <- x_right - x_left
  gap <- 1.2
  x <- x_left
  for (i in seq_along(columns)) {
    columns[[i]]$x <- x
    columns[[i]]$w <- content_w * columns[[i]]$width
    x <- x + columns[[i]]$w
  }
  
  n <- if (is.null(data) || !nrow(data)) 0 else nrow(data)
  rows <- lapply(seq_len(n), function(i) {
    cells <- lapply(columns, function(col) {
      val <- if (col$field %in% names(data)) data[[col$field]][i] else ""
      .pdfreport_layout_text(val, col$w - gap, cex, x_scale, plain = !isTRUE(col$richtext))
    })
    n_lines <- max(1, vapply(cells, length, integer(1)))
    list(cells = cells, height = n_lines * line_h + row_pad)
  })
  
  list(columns = columns, header_h = line_h + row_pad * 1.4, rows = rows, gap = gap)
}

# Splits ONE table row that is taller than a whole page into several chunks
# (line by line, all cells in parallel), so long free-text cells continue on the
# next page instead of being drawn past the bottom edge and cut off.
# first_avail = height available for the first chunk on the current page,
# page_avail  = height available for each following chunk (a fresh page).
.pdfreport_split_row <- function(row, line_h, row_pad, first_avail, page_avail) {
  n_total <- max(1L, vapply(row$cells, length, integer(1)))
  chunks <- list(); pos <- 0L; avail <- first_avail
  while (pos < n_total) {
    cap  <- max(1L, floor((avail - row_pad) / line_h))
    take <- min(cap, n_total - pos)
    cells <- lapply(row$cells, function(cl) {
      if (length(cl) > pos) cl[(pos + 1L):min(length(cl), pos + take)] else cl[0]
    })
    chunks[[length(chunks) + 1L]] <- list(cells = cells, height = take * line_h + row_pad)
    pos   <- pos + take
    avail <- page_avail
  }
  chunks
}

.pdfreport_draw_table_header <- function(tbl, x_left, x_right, y_cursor, cex_hdr = 0.66) {
  rect(x_left, y_cursor - tbl$header_h, x_right, y_cursor, col = PDFREPORT_COL_PRIMARY, border = NA)
  for (col in tbl$columns) {
    adj <- if (identical(col$align, "center")) c(0.5, 0.5) else if (identical(col$align, "right")) c(1, 0.5) else c(0, 0.5)
    tx <- if (identical(col$align, "center")) col$x + col$w / 2
    else if (identical(col$align, "right")) col$x + col$w - tbl$gap / 2
    else col$x + tbl$gap / 2
    text(tx, y_cursor - tbl$header_h / 2, col$label, col = "white", cex = cex_hdr, font = 2, adj = adj)
  }
  y_cursor - tbl$header_h
}

.pdfreport_draw_table_row <- function(tbl, row, x_left, x_right, y_cursor, cex, line_h, x_scale, shade) {
  yb <- y_cursor - row$height
  fill <- if (shade) PDFREPORT_COL_ROW_ALT else "white"
  rect(x_left, yb, x_right, y_cursor, col = fill, border = PDFREPORT_COL_BORDER, lwd = 0.35)
  # Vertically center each cell's wrapped lines within the row's full
  # height (row$height already includes the generous row_pad margin), so
  # text starts with real breathing room instead of right against the top
  # border - and a short cell (e.g. "Antwort") sits centered rather than
  # stuck to the top next to a much taller neighboring cell.
  for (i in seq_along(tbl$columns)) {
    col <- tbl$columns[[i]]
    if (i > 1) segments(col$x, yb, col$x, y_cursor, col = PDFREPORT_COL_BORDER, lwd = 0.3)
    align <- if (identical(col$align, "center")) "center" else "left"
    x0 <- if (align == "center") col$x else col$x + tbl$gap / 2
    n_lines_i <- max(1, length(row$cells[[i]]))
    cell_top <- y_cursor - max(0, (row$height - n_lines_i * line_h) / 2)
    .pdfreport_draw_lines(row$cells[[i]], x0, cell_top, cex, line_h, x_scale,
                          align = align, width = col$w)
  }
  yb
}

render_flex_report_pdf <- function(file, title, report_type = "", project_title = "",
                                   meta_lines = character(0), sections, footer_text = "",
                                   intro_lines = character(0)) {
  
  old_lc <- Sys.getlocale("LC_CTYPE")
  suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", "de_DE.UTF-8"), error = function(e) NULL))
  if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))
    suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", "C.UTF-8"), error = function(e) NULL))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_CTYPE", old_lc), silent = TRUE)), add = TRUE)
  
  pw <- 8.27; ph <- 11.69  # A4 portrait, inches
  .pdfreport_open_device(file, pw, ph)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  
  x_scale <- 100 / pw
  y_scale <- 100 / ph
  png_available <- requireNamespace("png", quietly = TRUE)
  all_meta <- if (nzchar(project_title)) c(paste0("Project: ", project_title), meta_lines) else meta_lines
  
  x_left <- 4; x_right <- 96
  content_w <- x_right - x_left
  
  cex_body <- 0.68
  # ✅ Same generous spacing as render_qa_report_pdf() - a tight line_h let
  # the last line of a wrapped cell run past its row's bottom edge, where
  # the next row's/item's background painted over it ("cut off" text).
  line_h <- strheight("Mg", units = "inches", cex = cex_body) * y_scale * 2.4
  row_pad <- line_h * 0.8
  
  y_top <- .pdfreport_draw_header(title, report_type, all_meta, heading = "X", dry_run = TRUE)
  foot_reserve <- 7
  avail_h <- y_top - foot_reserve
  
  # ---- Step 1: turn every section's pieces into a flat sequence of
  # "flow units" (a whole block, or one table header / one table row), then
  # greedily pack them into pages. ----
  page_plan <- list()
  for (sec in sections) {
    # A section with reserve_pages = k only reserves k blank placeholder pages
    # (own heading + entry in the Inhalt). The caller later swaps them for the
    # pages of an external PDF (e.g. the Organigramm) with
    # pdfreport_splice_pdf(), so page numbers in the Inhalt and in the footers
    # stay correct. The finished file carries attr "reserved_pages".
    if (!is.null(sec$reserve_pages) && sec$reserve_pages > 0) {
      for (j in seq_len(sec$reserve_pages)) {
        page_plan[[length(page_plan) + 1]] <- list(
          heading = sec$heading, continuation = j > 1, items = list(),
          toc_level = if (!is.null(sec$toc_level)) sec$toc_level else 1,
          toc_parent = sec$toc_parent, reserved = TRUE
        )
      }
      next
    }
    flow <- list()
    for (piece in sec$pieces) {
      if (identical(piece$type, "kv")) {
        for (item in piece$items) {
          txt <- paste0("**", item$label, ":** ", if (nzchar(trimws(as.character(item$value)))) item$value else "–")
          lines <- .pdfreport_layout_text(txt, content_w, cex_body, x_scale, plain = FALSE)
          flow[[length(flow) + 1]] <- list(kind = "text_para", lines = lines)
        }
      } else if (identical(piece$type, "text")) {
        for (para in piece$text) {
          if (!nzchar(trimws(para))) next
          lines <- .pdfreport_layout_text(para, content_w, cex_body, x_scale, plain = FALSE)
          flow[[length(flow) + 1]] <- list(kind = "text_para", lines = lines)
        }
      } else if (identical(piece$type, "image")) {
        files <- Filter(function(f) !is.null(f) && file.exists(f), piece$files)
        if (length(files) && png_available)
          flow[[length(flow) + 1]] <- list(kind = "image", files = files, height = 42)
      } else if (identical(piece$type, "table")) {
        tbl <- .pdfreport_layout_table(piece$columns, piece$data, cex_body, x_scale,
                                       x_left, x_right, line_h, row_pad)
        flow[[length(flow) + 1]] <- list(kind = "table", tbl = tbl)
      } else if (identical(piece$type, "heading")) {
        # A sub-heading *within* a section's flowing content (does not
        # force a page break by itself) - e.g. "Grundlagen und
        # Organisation" under "3. Laborinformationen", or "5.1.1 Staerken"
        # under "5. Auswertungen". Recorded into the Inhalt page as a
        # nested (levels 2/3) entry at whatever page it actually lands on.
        lvl <- if (!is.null(piece$level)) piece$level else 2
        flow[[length(flow) + 1]] <- list(kind = "heading_block", text = piece$text, level = lvl,
                                         height = line_h * 1.9)
      }
    }
    
    cur_items <- list(); cur_h <- 0; first_chunk <- TRUE; produced_any_page <- FALSE
    flush_page <- function(force_empty = FALSE) {
      if (!length(cur_items) && !force_empty) return(invisible(NULL))
      page_plan[[length(page_plan) + 1]] <<- list(
        heading = sec$heading, continuation = !first_chunk, items = cur_items,
        toc_level = if (!is.null(sec$toc_level)) sec$toc_level else 1,
        toc_parent = sec$toc_parent
      )
      produced_any_page <<- TRUE
      first_chunk <<- FALSE
      cur_items <<- list(); cur_h <<- 0
    }
    
    for (unit in flow) {
      if (unit$kind == "text_para") {
        # Split long paragraphs (Best Practices, Massnahmen, ...) line by
        # line, the same way table rows already flow across pages - a
        # paragraph taller than one page used to just get drawn past the
        # bottom margin instead of continuing onto the next page.
        for (ln in unit$lines) {
          if (length(cur_items) && cur_h + line_h > avail_h) flush_page()
          cur_items[[length(cur_items) + 1]] <- list(kind = "text_line", line = ln)
          cur_h <- cur_h + line_h
        }
        gap_h <- row_pad * 0.5
        cur_items[[length(cur_items) + 1]] <- list(kind = "gap", height = gap_h)
        cur_h <- cur_h + gap_h
      } else if (unit$kind %in% c("image", "heading_block")) {
        if (length(cur_items) && cur_h + unit$height > avail_h) flush_page()
        cur_items[[length(cur_items) + 1]] <- unit
        cur_h <- cur_h + unit$height
      } else if (unit$kind == "table") {
        tbl <- unit$tbl
        if (length(cur_items) && cur_h + tbl$header_h > avail_h) flush_page()
        cur_items[[length(cur_items) + 1]] <- list(kind = "table_header", tbl = tbl)
        cur_h <- cur_h + tbl$header_h
        if (!length(tbl$rows)) {
          cur_items[[length(cur_items) + 1]] <- list(kind = "table_empty", tbl = tbl, height = line_h + row_pad)
          cur_h <- cur_h + line_h + row_pad
        }
        for (ri in seq_along(tbl$rows)) {
          rp <- tbl$rows[[ri]]
          page_cap <- avail_h - tbl$header_h

          if (rp$height > page_cap) {
            # This single row is taller than a whole page (e.g. a question with
            # many long comments in one cell). Drawn as one block it ran past
            # the bottom margin and the end of the text was cut off, so it is
            # split line by line across as many pages as needed - the table
            # header is repeated on every page.
            first_avail <- avail_h - cur_h
            if (first_avail < row_pad + 4 * line_h) {
              # too little room left for even a few lines: start on a fresh page
              if (length(cur_items) && identical(cur_items[[length(cur_items)]]$kind, "table_header")) {
                cur_items[[length(cur_items)]] <- NULL   # don't leave a header alone
                cur_h <- cur_h - tbl$header_h
              }
              if (length(cur_items)) flush_page()
              cur_items[[length(cur_items) + 1]] <- list(kind = "table_header", tbl = tbl)
              cur_h <- tbl$header_h
              first_avail <- avail_h - cur_h
            }
            chunks <- .pdfreport_split_row(rp, line_h, row_pad, first_avail, page_cap)
            for (ci in seq_along(chunks)) {
              if (ci > 1) {
                flush_page()
                cur_items[[length(cur_items) + 1]] <- list(kind = "table_header", tbl = tbl)
                cur_h <- tbl$header_h
              }
              cur_items[[length(cur_items) + 1]] <- list(kind = "table_row", tbl = tbl, row = chunks[[ci]], shade = (ri %% 2 == 0))
              cur_h <- cur_h + chunks[[ci]]$height
            }
            next
          }

          if (cur_h + rp$height > avail_h) {
            flush_page()
            cur_items[[length(cur_items) + 1]] <- list(kind = "table_header", tbl = tbl)
            cur_h <- tbl$header_h
          }
          cur_items[[length(cur_items) + 1]] <- list(kind = "table_row", tbl = tbl, row = rp, shade = (ri %% 2 == 0))
          cur_h <- cur_h + rp$height
        }
      }
    }
    flush_page(force_empty = !produced_any_page)
  }
  
  # Build the (possibly 3-level) Inhalt list: a real entry for every
  # section's own heading (at that section's toc_level), a synthetic
  # "virtual parent" entry the first time a toc_parent label is seen
  # (sharing its child's page number - it has no page of its own, e.g.
  # "4. Vergleich Selbst- und Fremdbewertung" pointing at the same page as
  # "4.1 Fuehrung"), and one nested entry per "heading" piece encountered
  # on that page (in document order).
  toc_entries <- list()
  seen_parents <- character(0)
  for (idx in seq_along(page_plan)) {
    pg <- page_plan[[idx]]
    if (!isTRUE(pg$continuation)) {
      if (!is.null(pg$toc_parent) && !(pg$toc_parent %in% seen_parents)) {
        toc_entries[[length(toc_entries) + 1]] <- list(text = pg$toc_parent, page = idx, level = 1)
        seen_parents <- c(seen_parents, pg$toc_parent)
      }
      lvl <- if (!is.null(pg$toc_level)) pg$toc_level else 1
      toc_entries[[length(toc_entries) + 1]] <- list(text = pg$heading, page = idx, level = lvl)
    }
    for (item in pg$items) {
      if (identical(item$kind, "heading_block"))
        toc_entries[[length(toc_entries) + 1]] <- list(text = item$text, page = idx, level = item$level)
    }
  }
  n_cover <- length(.pdfreport_build_cover_pages(project_title, intro_lines, toc_entries, cex_body, line_h, x_scale, avail_h))
  toc_entries <- lapply(toc_entries, function(e) { e$page <- e$page + n_cover; e })
  cover_pages <- .pdfreport_build_cover_pages(project_title, intro_lines, toc_entries, cex_body, line_h, x_scale, avail_h)
  page_plan <- c(lapply(cover_pages, function(items) list(type = "cover", items = items)), page_plan)
  n_pages <- length(page_plan)
  
  # ---- Step 2: draw every planned page. ----
  for (pg_idx in seq_along(page_plan)) {
    pg <- page_plan[[pg_idx]]
    par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
    plot.new(); plot.window(xlim = c(0, 100), ylim = c(0, 100), xaxs = "i", yaxs = "i")
    
    if (identical(pg$type, "cover")) {
      y0 <- .pdfreport_draw_header(title, report_type, all_meta, heading = NULL)
      .pdfreport_draw_cover_page(pg$items, project_title, y0, cex_body, line_h, x_scale)
      .pdfreport_draw_footer(footer_text, pg_idx, n_pages)
      next
    }
    
    hd <- pg$heading
    if (isTRUE(pg$continuation)) hd <- paste0(hd, " (continued)")
    .pdfreport_draw_header(title, report_type, all_meta, heading = hd)

    if (isTRUE(pg$reserved)) {
      # Placeholder for an external PDF; normally replaced afterwards. Only
      # visible if that replacement failed.
      text(50, 55, "The file could not be attached.", cex = 0.8, font = 3, col = PDFREPORT_COL_MUTED)
      .pdfreport_draw_footer(footer_text, pg_idx, n_pages)
      next
    }

    y_cursor <- y_top
    for (item in pg$items) {
      if (identical(item$kind, "text_line")) {
        y_cursor <- .pdfreport_draw_lines(list(item$line), x_left, y_cursor, cex_body, line_h, x_scale)
      } else if (identical(item$kind, "gap")) {
        y_cursor <- y_cursor - item$height
      } else if (identical(item$kind, "heading_block")) {
        indent <- (item$level - 2) * 2
        cex_h <- if (item$level >= 3) 0.72 else 0.82
        col_h <- if (item$level >= 3) PDFREPORT_COL_ACCENT else PDFREPORT_COL_PRIMARY
        text(x_left + indent, y_cursor - item$height * 0.55, item$text, cex = cex_h, font = 2, col = col_h, adj = c(0, 0.5))
        y_cursor <- y_cursor - item$height
      } else if (identical(item$kind, "image")) {
        n_img <- length(item$files)
        slot_w <- (content_w - (n_img - 1) * 2) / n_img
        for (i in seq_along(item$files)) {
          img <- tryCatch(png::readPNG(item$files[i]), error = function(e) NULL)
          if (is.null(img)) next
          img_dim <- dim(img)
          asp <- img_dim[1] / img_dim[2]
          img_w <- min(slot_w, 55)
          img_h <- min(img_w * asp * (y_scale / x_scale), 40)
          x0 <- x_left + (i - 1) * (slot_w + 2) + (slot_w - img_w) / 2
          rasterImage(img, x0, y_cursor - img_h - 1, x0 + img_w, y_cursor - 1)
        }
        y_cursor <- y_cursor - item$height
      } else if (identical(item$kind, "table_header")) {
        y_cursor <- .pdfreport_draw_table_header(item$tbl, x_left, x_right, y_cursor)
      } else if (identical(item$kind, "table_empty")) {
        rect(x_left, y_cursor - item$height, x_right, y_cursor, col = "white", border = PDFREPORT_COL_BORDER, lwd = 0.4)
        text((x_left + x_right) / 2, y_cursor - item$height / 2, "No data available.",
             cex = 0.65, font = 3, col = PDFREPORT_COL_MUTED)
        y_cursor <- y_cursor - item$height
      } else if (identical(item$kind, "table_row")) {
        y_cursor <- .pdfreport_draw_table_row(item$tbl, item$row, x_left, x_right, y_cursor,
                                              cex_body, line_h, x_scale, item$shade)
      }
    }
    
    .pdfreport_draw_footer(footer_text, pg_idx, n_pages)
  }

  # Which finished pages are placeholders (see sec$reserve_pages above).
  out_file <- file
  attr(out_file, "reserved_pages") <- which(vapply(page_plan, function(p) isTRUE(p$reserved), logical(1)))
  invisible(out_file)
}

# Replaces the placeholder pages `reserved` (consecutive page numbers) of
# `main_pdf` with ALL pages of `insert_pdf`, writing `out_pdf`. The engine
# reserves exactly as many placeholders as insert_pdf has pages, so the total
# page count - and therefore the Inhalt and the "Seite x von N" footers - stay
# correct. Returns out_pdf, or main_pdf unchanged if anything goes wrong.
pdfreport_splice_pdf <- function(main_pdf, insert_pdf, reserved, out_pdf) {
  tryCatch({
    reserved <- sort(unique(as.integer(reserved)))
    n <- pdftools::pdf_length(main_pdf)
    first <- min(reserved); last <- max(reserved)
    parts <- character(0)
    if (first > 1) {
      f1 <- tempfile(fileext = ".pdf")
      pdftools::pdf_subset(main_pdf, pages = seq_len(first - 1), output = f1)
      parts <- c(parts, f1)
    }
    parts <- c(parts, insert_pdf)
    if (last < n) {
      f3 <- tempfile(fileext = ".pdf")
      pdftools::pdf_subset(main_pdf, pages = (last + 1):n, output = f3)
      parts <- c(parts, f3)
    }
    pdftools::pdf_combine(input = parts, output = out_pdf)
    if (file.exists(out_pdf) && file.size(out_pdf) > 0) out_pdf else main_pdf
  }, error = function(e) {
    message("Attaching the PDF failed: ", conditionMessage(e))
    main_pdf
  })
}

#-----------------------------------------------------------------------------------
# IVDCheckR report content (drawn by the report engine above)

# Free text for a "label: value" line; empty fields are marked
report_value <- function(x, empty = "Not provided") {
  x <- paste(as.character(x), collapse = ", ")
  x <- gsub("\t", " ", gsub("\r", "", x, fixed = TRUE), fixed = TRUE)
  if (is.na(x) || !nzchar(trimws(x))) empty else trimws(x)
}

report_kv <- function(...) list(type = "kv", items = list(...))
report_field <- function(label, value, empty = "Not provided") list(label = label, value = report_value(value, empty))

# Table piece; `widths` are fractions of the page width and should sum to 1
report_table <- function(d, widths = rep(1 / ncol(d), ncol(d)), align = "center") {
  align <- rep_len(align, ncol(d))
  list(type = "table", data = d, columns = lapply(seq_along(d), function(i) {
    list(field = names(d)[i], label = strtrim(names(d)[i], 40), width = widths[i], align = align[i])
  }))
}

# Format numbers for the report (no NaN/Inf from empty values or a target of 0)
report_num <- function(x, digits = 2) {
  vapply(x, function(v) if (is.finite(v)) format(round(v, digits), nsmall = digits, trim = TRUE) else "-", "", USE.NAMES = FALSE)
}

report_sections <- function(r) {
  docs <- lapply(r$docs, function(d) {
    value <- if (!d$applicable) "Not applicable" else c(
      if (nzchar(trimws(report_value(d$text, "")))) report_value(d$text),
      if (!is.null(d$file)) paste0("Attached PDF: ", d$file$name),
      if (!is.null(d$unreadable)) paste0("The uploaded file ", d$unreadable, " could not be read and is not attached."),
      if (!d$provided && is.null(d$unreadable)) "Marked as applicable, but nothing was provided")
    list(label = d$title, value = paste(value, collapse = "\n"))
  })

  mc_pieces <- if (!r$mc$ok) list(list(type = "text", text = r$mc$msg)) else list(
    do.call(report_kv, r$mc$fields),
    report_table(r$mc$summary, c(.28, .24, .24, .24)),
    list(type = "image", files = r$mc$plots),
    list(type = "heading", text = "Raw data", level = 2),
    report_table(r$mc$raw, c(.2, .4, .4)))

  qc_pieces <- if (!r$qc$ok) list(list(type = "text", text = "No data entered (optional step).")) else c(
    list(do.call(report_kv, r$qc$fields),
         list(type = "heading", text = "Results", level = 2),
         report_table(r$qc$results)),
    if (r$qc$no_target) list(list(type = "text", text = "Bias and RMSD need a target value above 0.")),
    list(list(type = "heading", text = "Raw data", level = 2),
         report_table(r$qc$raw)))

  attachments <- Filter(function(d) !is.null(d$file), r$docs)

  c(list(list(heading = "Project and measurand", pieces = list(do.call(report_kv, r$items))),
         list(heading = "IVDR documentation", pieces = list(
           do.call(report_kv, c(docs, list(report_field("Further comments", r$extra, "None")))))),
         list(heading = "Method comparison", pieces = mc_pieces),
         list(heading = "Quality control", pieces = qc_pieces),
         list(heading = "Chemicals and software", pieces = list(
           report_table(r$chem, c(.26, .18, .14, .26, .16), "left")))),
    # placeholder pages, replaced by the uploaded PDFs after rendering
    lapply(attachments, function(d) {
      list(heading = paste0("Attachment: ", d$title, " (", d$file$name, ")"), reserve_pages = d$pages, pieces = list())
    }))
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
        pages <- if (!is.null(upload)) tryCatch(pdf_length(upload$datapath), error = function(e) NA_integer_)
        unreadable <- if (!is.null(upload) && is.na(pages)) upload$name
        if (!is.null(unreadable)) upload <- NULL
        text <- input[[paste0("text", i)]]
        list(title = doc_items[[i]]$title, applicable = applicable, text = text, file = upload, pages = pages, unreadable = unreadable,
             provided = applicable && (!is.null(upload) || nzchar(report_value(text, ""))))
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

          # The plots are embedded as images, as in the other report engines
          plots <- file.path(work, c("regression.png", "difference.png"))
          for (i in 1:2) {
            png(plots[i], width = 5, height = 5, units = "in", res = 240)
            try(if (i == 1) plot1(mc_raw) else plot2(mc_raw), silent = TRUE)
            dev.off()
          }

          mc <- list(
            ok = TRUE, plots = plots,
            fields = list(report_field("Method x", input$descx), report_field("Method y", input$descy),
                          report_field("Reference range x", ref_range(input$refllx, input$refulx)),
                          report_field("Reference range y", ref_range(input$reflly, input$refuly)),
                          report_field("Regression function", sprintf("y = %s + %s * x", report_num(co[1, "EST"], 3), report_num(co[2, "EST"], 3))),
                          report_field("Spearman correlation", report_num(cor(mc_raw$x, mc_raw$y, method = "spearman"), 3)),
                          report_field("Value pairs", nrow(mc_raw))),
            summary = data.frame(Coefficient = c("Intercept", "Slope"), Estimate = report_num(co[, "EST"], 3),
                                 `95% CI, lower` = report_num(co[, "LCI"], 3), `95% CI, upper` = report_num(co[, "UCI"], 3), check.names = FALSE),
            raw = setNames(data.frame(seq_len(nrow(mc_raw)), mc_raw$x, mc_raw$y), make.unique(c("No.", input$descx, input$descy))))
          mc$raw[] <- lapply(mc$raw, as.character)
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
        qc$fields <- list(report_field("Analyte", input$analyte), report_field("Description", input$description),
                          report_field("QC type", if (identical(input$withbetw, "betweenrun")) "Between-run" else "Within-run"))
        qc$results <- cbind(Metric = rownames(res), as.data.frame(lapply(res, report_num)), stringsAsFactors = FALSE)
        qc$no_target <- any(unlist(res["Target value", ]) <= 0, na.rm = TRUE)
        qc$raw <- setNames(cbind(seq_len(nrow(rows)), rows), c("No.", toupper(levels)))
        qc$raw[] <- lapply(qc$raw, function(v) ifelse(is.na(v), "", as.character(v)))
      }

      # List of chemicals and software: only filled rows
      chem <- table_data()
      chem <- chem[Reduce(`|`, lapply(chem, function(v) nzchar(trimws(ifelse(is.na(v), "", as.character(v)))))), , drop = FALSE]

      sections <- report_sections(list(
        items = list(
          report_field("1. Measurand description", input$ParameterInfo),
          report_field("2. Intended use of the measurand", input$ParameterUse),
          report_field("3. Indication", input$Indication),
          report_field("4. Scientific validity", input$ScientificValidity),
          report_field("5. Measurement scale", input$MeasurementScale),
          report_field("6. Sample material", samples),
          report_field("7. Risk classification", paste("Class", input$RiskClass)),
          report_field("Risk classification details", input$RiskClassDetails),
          report_field("8. Modification of an existing product (standard) OR new product/development", input$ModificationProduct)),
        docs = docs, extra = input$extraText, mc = mc, qc = qc, chem = chem))

      raw_pdf <- file.path(work, "report.pdf")
      rendered <- render_flex_report_pdf(
        file = raw_pdf,
        title = "IVDCheckR Report",
        report_type = "LDT documentation",
        project_title = report_value(input$projectInput, "Untitled project"),
        meta_lines = c(paste0("Evaluation period: ", input$dates[1], " to ", input$dates[2]),
                       paste0("Created: ", format(Sys.Date(), "%Y-%m-%d"))),
        sections = sections,
        footer_text = "Created with IVDCheckR",
        intro_lines = "Documentation of a laboratory developed test (LDT) according to Article 5 (5) of the In Vitro Diagnostic Medical Device Regulation (EU) 2017/746 (EU-IVDR)."
      )

      # Swap the reserved placeholder pages for the uploaded PDFs
      final_pdf <- raw_pdf
      uploads <- unlist(lapply(docs, function(d) d$file$datapath))
      if (length(uploads) > 0) {
        inserted <- file.path(work, "attachments.pdf")
        pdf_combine(uploads, inserted)
        final_pdf <- pdfreport_splice_pdf(raw_pdf, inserted, attr(rendered, "reserved_pages"), file.path(work, "final.pdf"))
        if (identical(final_pdf, raw_pdf)) {
          showNotification("The uploaded PDFs could not be attached.", type = "warning", duration = 15)
        }
      }
      file.copy(final_pdf, file, overwrite = TRUE)
    }
  )

  for (id in c("dl_header", "dl_overview", paste0("dl_", names(STEPS)))) {
    output[[id]] <- report_download()
  }

}



shinyApp(ui, server)
