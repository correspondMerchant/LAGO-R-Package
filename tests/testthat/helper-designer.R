# The scale and step a cost designer slider row has in its UI. A row shows its
# coefficient times its scale, so a simulated slider report is in those units.
designer_row <- function(ui, id) {
  tag <- htmltools::tagQuery(ui)$find(paste0("#", id))$selectedTags()[[1]]
  list(
    scale = as.numeric(htmltools::tagGetAttribute(tag, "data-lago-scale")),
    step = as.numeric(htmltools::tagGetAttribute(tag, "data-step"))
  )
}

# What the slider of row `id` reports for a coefficient, rounded to its step.
slider_reading <- function(ui, id, value) {
  r <- designer_row(ui, id)
  round(value * r$scale / r$step) * r$step
}

# The last value, min, max and step the server sent to an input.
capture_updates <- function(session) {
  msgs <- list()
  session$sendInputMessage <- function(inputId, message) {
    msgs[[length(msgs) + 1]] <<- list(id = inputId, msg = message)
  }
  function(id) {
    hits <- Filter(function(m) identical(m$id, id), msgs)
    if (length(hits)) lapply(hits[[length(hits)]]$msg, as.numeric)
  }
}
