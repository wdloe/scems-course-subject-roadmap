library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(visNetwork)
library(DT)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------

# Keep deployment data inside the application bundle. Relative paths work both
# locally and on shinyapps.io/Posit Connect.
DATA_FILE <- file.path("data", "handbook-data.json")

if (!file.exists(DATA_FILE)) {
  stop("Dataset not found: ", DATA_FILE)
}

dataset <- fromJSON(DATA_FILE, simplifyVector = FALSE)
courses <- dataset$courses
areas_of_study <- dataset$majors
subjects <- dataset$subjects

# Application identity. Update APP_CREATOR if the published attribution should
# use a team, unit or different author name. To replace the visible logo
# placeholder, add a PNG file at www/ltu-logo.png.
APP_TITLE <- "Course and Subject Roadmap"
SCHOOL_NAME <- "School of Computing, Engineering and Mathematical Sciences (SCEMS)"
APP_CREATOR <- "W.Lukito@latrobe.edu.au"
APP_VERSION <- "1.0"
HANDBOOK_URL <- "https://handbook.latrobe.edu.au"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

text_value <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  value <- as.character(x[[1]])
  if (is.na(value) || !nzchar(value)) default else value
}

subject_index <- setNames(subjects, map_chr(subjects, ~ text_value(.x$code)))
aos_index <- setNames(areas_of_study, map_chr(areas_of_study, ~ text_value(.x$code)))

course_choices <- setNames(
  map_chr(courses, ~ text_value(.x$code)),
  map_chr(courses, ~ paste0(text_value(.x$name), " (", text_value(.x$code), ")"))
)

subject_catalogue <- map_dfr(subjects, function(subject) {
  tibble(
    `Code` = text_value(subject$code),
    `Subject name` = text_value(subject$name, "Not listed"),
    `Year` = text_value(subject$academic_year, "2026"),
    `Credit points` = text_value(subject$credit_points, "Not listed"),
    `Subject type` = text_value(subject$subject_type, "Not listed"),
    `Year level` = text_value(subject$year_level, "Not listed"),
    `AQF level` = text_value(subject$aqf_level, "Not listed"),
    `Coordinator` = text_value(subject$coordinator, "Not listed"),
    `Elective` = text_value(subject$available_as_elective, "Not listed"),
    `Exchange` = text_value(subject$available_to_exchange_students, "Not listed"),
    `Capstone` = text_value(subject$capstone, "Not listed"),
    `Academic progress review` = text_value(
      subject$academic_progress_review, "Not listed"
    ),
    `Requisites` = text_value(subject$requisite_rule_text, "None listed"),
    `Handbook URL` = text_value(subject$source_url)
  )
}) %>%
  arrange(`Code`)

find_course <- function(code) {
  hits <- keep(courses, ~ identical(text_value(.x$code), code))
  if (length(hits)) hits[[1]] else NULL
}

# -----------------------------------------------------------------------------
# Curriculum parsing
# -----------------------------------------------------------------------------

year_number <- function(label) {
  lookup <- c(
    "level one" = 1L,
    "level two" = 2L,
    "level three" = 3L,
    "level four" = 4L
  )
  result <- unname(lookup[tolower(trimws(label))])
  if (!length(result) || is.na(result)) NA_integer_ else as.integer(result)
}

role_from_label <- function(label, current_role) {
  label <- tolower(trimws(label))
  if (grepl("elective|choice", label)) return("elective")
  if (grepl("core|capstone", label)) return("core")
  current_role
}

contains_subject_descendant <- function(node) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (
    identical(text_value(node$type), "subject") &&
    nzchar(text_value(node$code))
  ) {
    return(TRUE)
  }
  children <- node$children %||% list()
  any(vapply(children, contains_subject_descendant, logical(1)))
}

collect_curriculum_subjects <- function(
    node,
    current_year = NA_integer_,
    current_role = "core",
    source = "course",
    rows = list()
) {
  if (is.null(node) || !is.list(node)) return(rows)
  
  label <- text_value(node$label, text_value(node$name))
  candidate_year <- year_number(label)
  if (!is.na(candidate_year)) current_year <- candidate_year
  current_role <- role_from_label(label, current_role)
  children <- node$children %||% list()
  
  # Some handbook structures specify only an elective credit-point allocation,
  # without naming any subjects. Preserve that requirement as a roadmap node.
  is_generic_elective <-
    identical(current_role, "elective") &&
    grepl("elective", label, ignore.case = TRUE) &&
    !contains_subject_descendant(node) &&
    nzchar(text_value(node$credit_points))
  
  if (is_generic_elective) {
    placeholder_year <- ifelse(is.na(current_year), 1L, current_year)
    placeholder_cp <- text_value(node$credit_points)
    placeholder_code <- paste0(
      "ELECTIVE-Y", placeholder_year, "-", gsub("[^0-9A-Za-z]", "", placeholder_cp), "CP"
    )
    
    rows[[length(rows) + 1L]] <- tibble(
      code = placeholder_code,
      name = paste0("Generic elective requirement (", placeholder_cp, " credit points)"),
      year = placeholder_year,
      role = "elective",
      source = source,
      credit_points = placeholder_cp,
      coordinator = "Not applicable",
      year_level = paste("Year", placeholder_year),
      aqf_level = "Not applicable",
      available_as_elective = "Yes — elective placeholder",
      available_to_exchange_students = "Not applicable",
      capstone = "No",
      url = "",
      is_placeholder = TRUE
    )
  }
  
  if (identical(text_value(node$type), "subject") && nzchar(text_value(node$code))) {
    code <- text_value(node$code)
    detail <- subject_index[[code]]
    subject_name <- text_value(detail$name, text_value(node$name, code))
    capstone_value <- text_value(detail$capstone, "Not listed")
    role <- current_role
    # The handbook has no standalone "fundamental" flag. Use Year 1 core as
    # the roadmap's fundamental category.
    if (identical(role, "core") && identical(current_year, 1L)) role <- "fundamental"
    
    rows[[length(rows) + 1L]] <- tibble(
      code = code,
      name = subject_name,
      year = ifelse(is.na(current_year), 1L, current_year),
      role = role,
      source = source,
      credit_points = text_value(detail$credit_points, text_value(node$credit_points)),
      coordinator = text_value(detail$coordinator, "Not listed"),
      year_level = text_value(detail$year_level, paste("Year", current_year)),
      aqf_level = text_value(detail$aqf_level, "Not listed"),
      available_as_elective = text_value(detail$available_as_elective, "Not listed"),
      available_to_exchange_students = text_value(
        detail$available_to_exchange_students,
        "Not listed"
      ),
      capstone = capstone_value,
      url = text_value(detail$source_url, text_value(node$url)),
      is_placeholder = FALSE
    )
  }
  
  for (child in children) {
    rows <- collect_curriculum_subjects(
      child,
      current_year,
      current_role,
      source,
      rows
    )
  }
  rows
}

collect_aos_codes <- function(node, result = character()) {
  if (is.null(node) || !is.list(node)) return(result)
  label <- tolower(text_value(node$label, text_value(node$name)))
  if (grepl("suspended|closed|phasing out|discontinued", label)) return(result)
  if (text_value(node$type) %in% c("major", "minor", "specialisation")) {
    code <- text_value(node$code)
    if (nzchar(code) && code %in% names(aos_index)) result <- c(result, code)
  }
  for (child in node$children %||% list()) {
    result <- collect_aos_codes(child, result)
  }
  unique(result)
}

subjects_for_selection <- function(course, aos_code = "") {
  rows <- collect_curriculum_subjects(course$structure, source = "course")
  if (nzchar(aos_code) && aos_code %in% names(aos_index)) {
    rows <- c(
      rows,
      collect_curriculum_subjects(
        aos_index[[aos_code]]$structure,
        source = aos_code
      )
    )
  }
  if (!length(rows)) return(tibble())
  
  bind_rows(rows) %>%
    arrange(year, code) %>%
    group_by(code) %>%
    summarise(
      name = first(name),
      year = min(year),
      role = if ("fundamental" %in% role) {
        "fundamental"
      } else if ("core" %in% role) {
        "core"
      } else {
        "elective"
      },
      source = paste(unique(source), collapse = ", "),
      credit_points = first(credit_points),
      coordinator = first(coordinator),
      year_level = first(year_level),
      aqf_level = first(aqf_level),
      available_as_elective = first(available_as_elective),
      available_to_exchange_students = first(available_to_exchange_students),
      capstone = first(capstone),
      url = first(url),
      is_placeholder = any(is_placeholder),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# Prerequisite parsing
# -----------------------------------------------------------------------------

collect_relationship_codes <- function(value, result = character()) {
  if (is.null(value)) return(result)
  if (is.list(value)) {
    if (
      identical(text_value(value$academic_item_type$value), "subject") &&
      nzchar(text_value(value$academic_item_code))
    ) {
      result <- c(result, text_value(value$academic_item_code))
    }
    for (item in value) result <- collect_relationship_codes(item, result)
  }
  unique(result)
}

prerequisite_codes <- function(subject) {
  rules <- subject$requisites_raw %||% list()
  result <- character()
  for (rule in rules) {
    if (identical(text_value(rule$requisite_type$value), "prerequisite")) {
      result <- c(result, collect_relationship_codes(rule$containers))
    }
  }
  setdiff(unique(result), text_value(subject$code))
}

build_edges <- function(subject_table) {
  if (!nrow(subject_table)) return(tibble(from = character(), to = character()))
  included <- subject_table$code
  edges <- map_dfr(included, function(target) {
    detail <- subject_index[[target]]
    parents <- intersect(prerequisite_codes(detail), included)
    tibble(from = parents, to = target)
  })
  distinct(edges, from, to)
}

# -----------------------------------------------------------------------------
# Matrix graph
# -----------------------------------------------------------------------------

role_colours <- c(
  fundamental = "#86D0C4",
  core = "#78A9D1",
  elective = "#F8B25C"
)

build_graph <- function(subject_table, transpose = FALSE) {
  if (!nrow(subject_table)) {
    return(list(nodes = tibble(), edges = tibble()))
  }
  
  max_year <- max(4L, subject_table$year, na.rm = TRUE)
  relation_edges <- build_edges(subject_table)
  positioned <- subject_table %>%
    group_by(year) %>%
    arrange(
      is_placeholder,
      factor(role, levels = c("fundamental", "core", "elective")),
      code,
      .by_group = TRUE
    ) %>%
    mutate(
      row = row_number(),
      x = if (transpose) (row - (n() + 1) / 2) * 145 else (year - 1) * 280,
      y = if (transpose) (year - 1) * 175 else (row - (n() + 1) / 2) * 78
    ) %>%
    ungroup()
  
  nodes <- positioned %>%
    transmute(
      id = code,
      label = ifelse(
        is_placeholder,
        paste0("ELECTIVE\n", credit_points, " CP"),
        code
      ),
      title = paste0(
        "<b>", code, "</b><br>", name,
        "<br>Year: ", year,
        "<br>Role: ", tools::toTitleCase(role),
        "<br>Curriculum source: ",
        ifelse(grepl("(^|, )course($|, )", source), "Course", "Major / specialisation only"),
        "<br>Credit points: ", credit_points,
        "<br>Coordinator: ", coordinator
      ),
      x = as.numeric(x),
      y = as.numeric(y),
      color.background = unname(role_colours[role]),
      color.border = ifelse(grepl("(^|, )course($|, )", source), "#59636E", "#D62828"),
      borderWidth = ifelse(grepl("(^|, )course($|, )", source), 1, 3),
      shape = "box",
      font.size = 20,
      url = url,
      fixed = TRUE,
      relation_from = NA_character_,
      relation_to = NA_character_
    )
  
  # Layout nodes and dashed edges live in the same coordinate system as the
  # subjects. This keeps headings and separators aligned at every window size.
  if (transpose) {
    left_x <- min(positioned$x) - 150
    right_x <- max(positioned$x) + 100
    year_y <- seq(0, 525, 175)
    separator_y <- year_y[1:3] + 87.5
    separator_from <- paste0(".layout-sep-left-", 1:3)
    separator_to <- paste0(".layout-sep-right-", 1:3)
    anchor_x <- c(
      left_x - 20, right_x + 20,
      rep(left_x, 4), rep(left_x + 70, 3), rep(right_x, 3)
    )
    anchor_y <- c(
      mean(year_y), mean(year_y),
      year_y, separator_y, separator_y
    )
  } else {
    top_y <- min(positioned$y) - 90
    bottom_y <- max(positioned$y) + 70
    separator_from <- paste0(".layout-sep-top-", 1:3)
    separator_to <- paste0(".layout-sep-bottom-", 1:3)
    anchor_x <- c(
      -140, 980,
      seq(0, 840, 280), rep(c(140, 420, 700), 2)
    )
    anchor_y <- c(
      0, 0,
      rep(top_y, 4), rep(top_y + 35, 3), rep(bottom_y, 3)
    )
  }
  
  layout_anchors <- tibble(
    id = c(
      ".layout-left", ".layout-right",
      paste0(".layout-year-", 1:4), separator_from, separator_to
    ),
    label = c("", "", paste0("<b>Year ", 1:4, "</b>"), rep("", 6)),
    title = rep("", 12),
    x = anchor_x,
    y = anchor_y,
    color.background = rep("rgba(0,0,0,0)", 12),
    color.border = rep("rgba(0,0,0,0)", 12),
    borderWidth = rep(0, 12),
    shape = c(rep("dot", 2), rep("text", 4), rep("dot", 6)),
    url = rep("", 12),
    size = rep(1, 12),
    fixed = rep(TRUE, 12),
    font.size = c(rep(1, 2), rep(24, 4), rep(1, 6)),
    font.multi = rep("html", 12),
    chosen = rep(FALSE, 12)
  )
  nodes <- bind_rows(nodes, layout_anchors)
  
  position_year <- setNames(positioned$year, positioned$code)
  position_x <- setNames(positioned$x, positioned$code)
  position_y <- setNames(positioned$y, positioned$code)
  same_year_relations <- relation_edges %>%
    mutate(year = unname(position_year[from])) %>%
    filter(year == unname(position_year[to])) %>%
    group_by(year) %>%
    arrange(from, to, .by_group = TRUE) %>%
    mutate(
      route_number = row_number(),
      route_offset = 40 + route_number * 4
    ) %>%
    ungroup() %>%
    rename(parent = from, child = to)
  
  cross_year_edges <- relation_edges %>%
    filter(unname(position_year[from]) != unname(position_year[to])) %>%
    mutate(
      arrows = "to",
      color = "#59636E",
      width = 1.2,
      dashes = FALSE,
      smooth.enabled = TRUE,
      smooth.type = "cubicBezier",
      smooth.forceDirection = if (transpose) "vertical" else "horizontal",
      smooth.roundness = 0.35,
      layout_edge = FALSE,
      route_edge = FALSE,
      custom_route = FALSE,
      route_offset = 0,
      hidden = FALSE,
      relation_from = from,
      relation_to = to
    )
  
  if (nrow(same_year_relations)) {
    same_year_edges <- same_year_relations %>%
      transmute(
        from = parent,
        to = child,
        arrows = "to",
        relation_from = parent, relation_to = child,
        # Curve same-year connections away from their row or column.
        smooth.type = ifelse(
          if (transpose) {
            unname(position_x[child]) > unname(position_x[parent])
          } else {
            unname(position_y[child]) < unname(position_y[parent])
          },
          "curvedCW",
          "curvedCCW"
        ),
        smooth.roundness = pmin(0.55, 0.18 + route_number * 0.035)
      ) %>%
      mutate(
        color = "#59636E", width = 1.2, dashes = FALSE,
        smooth.enabled = TRUE,
        smooth.forceDirection = "none",
        layout_edge = FALSE, route_edge = FALSE,
        custom_route = FALSE, route_offset = 0, hidden = FALSE
      )
  } else {
    same_year_edges <- tibble(
      from = character(), to = character(), arrows = character(),
      relation_from = character(), relation_to = character(),
      color = character(), width = numeric(), dashes = logical(),
      smooth.enabled = logical(), smooth.type = character(),
      smooth.forceDirection = character(), smooth.roundness = numeric(),
      layout_edge = logical(), route_edge = logical(),
      custom_route = logical(), route_offset = numeric(), hidden = logical()
    )
  }
  
  separator_edges <- tibble(
    from = separator_from,
    to = separator_to,
    arrows = "",
    color = "#AEB7BF",
    width = 1,
    dashes = TRUE,
    smooth.enabled = FALSE,
    smooth.type = "continuous",
    smooth.forceDirection = "none",
    smooth.roundness = 0,
    layout_edge = TRUE,
    route_edge = FALSE,
    custom_route = FALSE,
    route_offset = 0,
    hidden = FALSE,
    relation_from = "",
    relation_to = "",
    chosen = FALSE,
    hoverWidth = 0,
    selectionWidth = 0
  )
  edges <- bind_rows(cross_year_edges, same_year_edges, separator_edges)
  
  list(nodes = nodes, edges = edges, relations = relation_edges, max_year = max_year)
}

directed_related_codes <- function(start, edges) {
  if (!nzchar(start) || !nrow(edges)) return(start)
  if ("layout_edge" %in% names(edges)) edges <- filter(edges, !layout_edge)
  
  walk <- function(seed, direction) {
    seen <- seed
    frontier <- seed
    while (length(frontier)) {
      next_codes <- if (identical(direction, "prerequisite")) {
        edges$from[edges$to %in% frontier]
      } else {
        edges$to[edges$from %in% frontier]
      }
      next_codes <- setdiff(unique(next_codes), seen)
      seen <- unique(c(seen, next_codes))
      frontier <- next_codes
    }
    seen
  }
  
  unique(c(
    walk(start, "prerequisite"),
    walk(start, "postrequisite")
  ))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$title(paste(APP_TITLE, "| SCEMS")),
    tags$script(HTML("
      $(document).on('change', '#subject', function() {
        var code = this.value || '';
        if (window.Shiny) {
          Shiny.setInputValue('detail_subject', code, {priority: 'event'});
        }
        window.focusRoadmapSubject(code);
      });

      window.applyRoadmapFocus = function(network, start) {
        if (!start || /^\\.(layout|route)-/.test(String(start))) return;
        var edges = network.body.data.edges.get();
        var nodes = network.body.data.nodes.get();
        var incoming = {}; var outgoing = {};
        nodes.forEach(function(n) { incoming[n.id] = []; outgoing[n.id] = []; });
        edges.forEach(function(e) {
          if (e.layout_edge) return;
          var parent = e.relation_from || e.from;
          var child = e.relation_to || e.to;
          if (outgoing[parent]) outgoing[parent].push(child);
          if (incoming[child]) incoming[child].push(parent);
        });
        function walk(adjacency) {
          var found = {}; var queue = [start]; found[start] = true;
          while (queue.length) {
            var current = queue.shift();
            (adjacency[current] || []).forEach(function(next) {
              if (!found[next]) { found[next] = true; queue.push(next); }
            });
          }
          return found;
        }
        var prerequisites = walk(incoming);
        var postrequisites = walk(outgoing);
        var seen = {};
        Object.keys(prerequisites).forEach(function(id) { seen[id] = true; });
        Object.keys(postrequisites).forEach(function(id) { seen[id] = true; });
        network.body.data.nodes.update(nodes.map(function(n) {
          var isLayout = /^\\.(layout|route)-/.test(String(n.id));
          return {id: n.id, opacity: (isLayout || seen[n.id]) ? 1 : 0.18};
        }));
        network.body.data.edges.update(edges.map(function(e) {
          if (e.layout_edge) return {id: e.id, color: {color: '#AEB7BF', opacity: 1}, width: 1};
          var parent = e.relation_from || e.from;
          var child = e.relation_to || e.to;
          var related = !!seen[parent] && !!seen[child];
          return {
            id: e.id,
            color: {color: related ? '#59636E' : '#B8C1C8', opacity: related ? 1 : 0.18},
            width: related ? 2.4 : 1
          };
        }));
      };

      window.resetRoadmapFocus = function(network) {
        var nodes = network.body.data.nodes.get();
        var edges = network.body.data.edges.get();
        network.body.data.nodes.update(nodes.map(function(n) {
          return {id: n.id, opacity: 1};
        }));
        network.body.data.edges.update(edges.map(function(e) {
          return e.layout_edge
            ? {id: e.id, color: {color: '#AEB7BF', opacity: 1}, width: 1}
            : {id: e.id, color: {color: '#59636E', opacity: 1}, width: 1.2};
        }));
      };

      window.focusRoadmapSubject = function(code) {
        var graphElement = document.getElementById('graphroadmap');
        var network = graphElement && graphElement.chart;
        if (!network) return;
        if (code && network.body.nodes[code]) {
          network.pinnedSubject = code;
          window.applyRoadmapFocus(network, code);
          network.selectNodes([code], false);
        } else {
          network.pinnedSubject = null;
          network.unselectAll();
          window.resetRoadmapFocus(network);
        }
      };
    ")),
    tags$style(HTML(" 
      :root {
        --latrobe-red: #e1231d;
        --latrobe-red-dark: #a91a16;
        --text-primary: #263238;
        --text-secondary: #53616c;
        --border: #dfe5ea;
      }
      body { background: #f7f9fb; color: var(--text-primary); }
      .container-fluid { padding: 0; }
      .site-header {
        color: white;
        background: var(--latrobe-red);
        border-bottom: 5px solid var(--latrobe-red-dark);
        padding: 22px 28px;
      }
      .site-header-inner {
        max-width: 1680px; margin: 0 auto; display: flex; align-items: center;
        gap: 20px;
      }
      .school-logo {
        width: 92px; height: 92px; flex: 0 0 92px; object-fit: contain;
        background: white; border-radius: 8px; padding: 8px;
      }
      .logo-placeholder {
        width: 92px; height: 92px; flex: 0 0 92px; border-radius: 8px;
        border: 1px dashed rgba(255,255,255,.72); background: rgba(255,255,255,.1);
        display: flex; flex-direction: column; align-items: center;
        justify-content: center; text-align: center; letter-spacing: .08em;
        font-size: 15px; font-weight: 700;
      }
      .logo-placeholder small { font-size: 10px; font-weight: 500; opacity: .8; margin-top: 3px; }
      .header-copy { min-width: 0; flex: 1; }
      .university-name { font-size: 15px; font-weight: 700; letter-spacing: .09em; text-transform: uppercase; }
      .school-name { font-size: 16px; opacity: .92; margin-top: 2px; }
      .app-title { font-size: 32px; line-height: 1.15; font-weight: 700; margin: 10px 0 5px; }
      .app-subtitle { font-size: 15px; line-height: 1.45; opacity: .88; margin: 0; max-width: 850px; }
      .header-actions { flex: 0 0 auto; text-align: right; }
      .scope-badge {
        display: inline-block; background: rgba(255,255,255,.14); border: 1px solid rgba(255,255,255,.35);
        border-radius: 999px; padding: 6px 11px; font-size: 13px; font-weight: 600; margin-bottom: 10px;
      }
      .official-link { color: white; font-weight: 600; text-decoration: underline; text-underline-offset: 3px; }
      .official-link:hover, .official-link:focus { color: white; opacity: .82; }
      .app-content { max-width: 1680px; margin: 0 auto; padding: 22px 24px 10px; }
      .main-tabs .nav-tabs {
        border-bottom: 2px solid var(--border); margin-bottom: 0;
      }
      .main-tabs .tab-content { padding-top: 20px; }
      .main-tabs .nav-tabs > li > a {
        color: var(--text-secondary); border: 0; border-bottom: 3px solid transparent;
        border-radius: 0; padding: 11px 22px; font-size: 16px; font-weight: 700;
      }
      .main-tabs .nav-tabs > li > a:hover,
      .main-tabs .nav-tabs > li > a:focus { background: #fff1f0; color: var(--latrobe-red-dark); }
      .main-tabs .nav-tabs > li.active > a,
      .main-tabs .nav-tabs > li.active > a:hover,
      .main-tabs .nav-tabs > li.active > a:focus {
        color: var(--latrobe-red-dark); background: white; border: 0;
        border-bottom: 3px solid var(--latrobe-red);
      }
      .control-card, .detail-card, .graph-card {
        background: white; border: 1px solid var(--border); border-radius: 10px;
        box-shadow: 0 2px 7px rgba(28, 47, 61, 0.07); padding: 16px;
      }
      .roadmap-layout {
        display: grid;
        grid-template-columns: minmax(260px, 300px) minmax(620px, 1fr) minmax(260px, 300px);
        gap: 16px;
        align-items: start;
      }
      .roadmap-controls-panel, .roadmap-tree-panel, .roadmap-details-panel { min-width: 0; }
      .control-card { position: sticky; top: 12px; }
      .roadmap-details-panel .detail-card { position: sticky; top: 12px; }
      .panel-title { font-size: 19px; font-weight: 700; margin: 0 0 4px; }
      .panel-intro { color: var(--text-secondary); font-size: 13px; margin: 0 0 16px; }
      .legend-row { display: flex; align-items: center; margin: 7px 0; }
      .legend-swatch { width: 24px; height: 16px; border-radius: 4px; margin-right: 9px; }
      .selectize-control.single .selectize-input { cursor: text; }
      .transpose-toggle { margin: 9px 0 4px; }
      .transpose-toggle .form-group, .transpose-toggle .checkbox { margin: 0; }
      .transpose-toggle input[type='checkbox'] {
        -webkit-appearance: none; appearance: none; width: 38px; height: 21px;
        margin: 0 8px 0 0; vertical-align: -5px; border: 0; border-radius: 999px;
        background: #b9c2c9; cursor: pointer; position: relative; transition: background .18s ease;
      }
      .transpose-toggle input[type='checkbox']::before {
        content: ''; position: absolute; width: 17px; height: 17px; left: 2px; top: 2px;
        border-radius: 50%; background: white; box-shadow: 0 1px 3px rgba(0,0,0,.28);
        transition: transform .18s ease;
      }
      .transpose-toggle input[type='checkbox']:checked { background: var(--latrobe-red); }
      .transpose-toggle input[type='checkbox']:checked::before { transform: translateX(17px); }
      .transpose-toggle input[type='checkbox']:focus-visible {
        outline: 3px solid rgba(225,35,29,.28); outline-offset: 2px;
      }
      .transpose-toggle label { font-weight: 600; cursor: pointer; }
      .graph-shell { position: relative; min-height: 680px; overflow: hidden; }
      #roadmap { position: relative; z-index: 2; }
      .subject-name { font-size: 18px; font-weight: 700; margin-bottom: 5px; }
      .detail-grid {
        display: grid; grid-template-columns: minmax(90px, 115px) minmax(0, 1fr);
        gap: 7px 10px; overflow-wrap: anywhere;
      }
      .detail-label { font-weight: 600; color: #53616c; }
      .handbook-link { display: inline-block; margin-top: 10px; }
      .catalogue-card {
        background: white; border: 1px solid var(--border); border-radius: 10px;
        box-shadow: 0 2px 7px rgba(28, 47, 61, 0.07); padding: 20px;
      }
      .catalogue-title { font-size: 24px; font-weight: 700; margin: 0 0 5px; }
      .catalogue-intro { color: var(--text-secondary); margin: 0 0 18px; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border: 1px solid #b8c2ca; border-radius: 5px; padding: 5px 8px; background: white;
      }
      table.dataTable thead th { white-space: nowrap; }
      .dt-button.btn-default { border-color: #b8c2ca; }
      .site-footer {
        margin-top: 22px; color: #fff1f0; background: var(--latrobe-red-dark);
        border-top: 4px solid var(--latrobe-red); padding: 25px 28px 18px;
      }
      .footer-inner { max-width: 1680px; margin: 0 auto; }
      .footer-grid { display: grid; grid-template-columns: 1.3fr 1fr 1fr; gap: 36px; }
      .footer-heading { color: white; font-size: 15px; font-weight: 700; margin: 0 0 7px; }
      .site-footer p { font-size: 13px; line-height: 1.5; margin: 0 0 6px; }
      .site-footer a { color: white; text-decoration: underline; text-underline-offset: 2px; }
      .footer-bottom {
        border-top: 1px solid rgba(255,255,255,.18); margin-top: 18px; padding-top: 12px;
        display: flex; justify-content: space-between; gap: 18px; font-size: 12px; opacity: .82;
      }
      @media (max-width: 1200px) {
        .roadmap-layout { grid-template-columns: minmax(190px, 220px) minmax(620px, 1fr); }
        .roadmap-details-panel { grid-column: 1 / -1; }
        .roadmap-details-panel .detail-card { position: static; }
      }
      @media (max-width: 900px) {
        .site-header { padding: 18px; }
        .site-header-inner { align-items: flex-start; flex-wrap: wrap; }
        .school-logo, .logo-placeholder { width: 72px; height: 72px; flex-basis: 72px; }
        .app-title { font-size: 25px; }
        .header-actions { width: 100%; text-align: left; padding-left: 92px; }
        .app-content { padding: 16px 12px 8px; }
        .roadmap-layout { grid-template-columns: minmax(0, 1fr); }
        .roadmap-details-panel { grid-column: auto; }
        .control-card { position: static; margin-bottom: 14px; }
        .graph-shell { min-height: 580px; overflow-x: auto; }
        #roadmap { min-width: 850px; }
        .footer-grid { grid-template-columns: 1fr; gap: 18px; }
        .footer-bottom { flex-direction: column; }
      }
      @media (max-width: 520px) {
        .school-logo, .logo-placeholder { width: 62px; height: 62px; flex-basis: 62px; }
        .university-name { font-size: 12px; }
        .school-name { font-size: 13px; }
        .app-title { font-size: 22px; }
        .header-actions { padding-left: 0; }
      }
    "))
  ),
  
  tags$header(
    class = "site-header",
    div(
      class = "site-header-inner",
      if (file.exists(file.path("www", "ltu-logo.png"))) {
        tags$img(class = "school-logo", src = "ltu-logo.png", alt = "La Trobe University logo")
      } else {
        div(class = "logo-placeholder", "SCEMS", tags$small("LOGO PLACEHOLDER"))
      },
      div(
        class = "header-copy",
        div(class = "university-name", "La Trobe University"),
        div(class = "school-name", SCHOOL_NAME),
        h1(class = "app-title", APP_TITLE),
        p(
          class = "app-subtitle",
          "Explore course structures, major and specialisation requirements, and subject prerequisite pathways."
        )
      ),
      div(
        class = "header-actions",
        div(class = "scope-badge", "2026"),
        br(),
        tags$a(
          class = "official-link", href = HANDBOOK_URL, target = "_blank",
          rel = "noopener noreferrer", "View the official Handbook ↗"
        )
      )
    )
  ),
  
  div(
    class = "app-content",
    div(
      class = "main-tabs",
      tabsetPanel(
        id = "main_menu",
        selected = "tree",
        tabPanel(
          "Roadmap",
          value = "tree",
          div(
            class = "roadmap-layout",
            div(
              class = "roadmap-controls-panel",
              div(
                class = "control-card",
                div(class = "panel-title", "Roadmap controls"),
                p(class = "panel-intro", "Select a course, then refine the roadmap by major or specialisation."),
                selectizeInput(
                  "course", "Course",
                  choices = c("Select or search for a course" = "", course_choices),
                  selected = "",
                  options = list(
                    plugins = list("select_on_focus"),
                    placeholder = "Type a course name or code",
                    searchField = c("text", "value"),
                    openOnFocus = TRUE,
                    closeAfterSelect = TRUE,
                    allowEmptyOption = TRUE
                  )
                ),
                selectizeInput(
                  "major", "Major / specialisation", choices = c("None" = ""), selected = "",
                  options = list(
                    plugins = list("select_on_focus"),
                    placeholder = "Type a major name or code",
                    searchField = c("text", "value"),
                    openOnFocus = TRUE,
                    closeAfterSelect = TRUE,
                    allowEmptyOption = TRUE
                  )
                ),
                selectizeInput(
                  "subject", "Subject", choices = c("Select a subject" = ""), selected = "",
                  options = list(
                    plugins = list("select_on_focus"),
                    placeholder = "Type a subject name or code",
                    searchField = c("text", "value"),
                    openOnFocus = TRUE,
                    closeAfterSelect = TRUE,
                    allowEmptyOption = TRUE
                  )
                ),
                checkboxInput("isolate", "Show only related subjects", value = FALSE),
                div(
                  class = "transpose-toggle",
                  checkboxInput("transpose", "Years as rows", value = FALSE)
                ),
                hr(),
                h4("Legend"),
                div(class = "legend-row", div(class = "legend-swatch", style = "background:#86D0C4"), "Fundamental (Year 1 core)"),
                div(class = "legend-row", div(class = "legend-swatch", style = "background:#78A9D1"), "Core"),
                div(class = "legend-row", div(class = "legend-swatch", style = "background:#F8B25C"), "Elective / choice"),
                div(class = "legend-row", div(class = "legend-swatch", style = "background:white;border:3px solid #D62828"), "Major / specialisation only")
              )
            ),
            div(
              class = "roadmap-tree-panel",
              div(
                class = "graph-card",
                div(
                  class = "graph-shell",
                  visNetworkOutput("roadmap", height = "630px")
                )
              )
            ),
            div(
              class = "roadmap-details-panel",
              uiOutput("subject_detail")
            )
          )
        ),
        tabPanel(
          "Subject Catalogue",
          value = "subject_catalogue",
          div(
            class = "catalogue-card",
            h2(class = "catalogue-title", "2026 Subject Catalogue"),
            p(
              class = "catalogue-intro",
              paste0(
                "Browse all ", nrow(subject_catalogue),
                " subjects in the active IT and Engineering dataset. Use the main search box, ",
                "filters beneath each column heading, or click a heading to sort."
              )
            ),
            DTOutput("subject_catalogue_table")
          )
        )
      )
    )
  ),
  
  tags$footer(
    class = "site-footer",
    div(
      class = "footer-inner",
      div(
        class = "footer-grid",
        tags$section(
          h4(class = "footer-heading", "About this roadmap"),
          p(
            "An interactive curriculum-planning aid for exploring subject sequencing, prerequisites, " ,
            "course requirements, and major or specialisation pathways."
          )
        ),
        tags$section(
          h4(class = "footer-heading", "Data source and reference"),
          p("Based on the active 2026 La Trobe University Handbook dataset."),
          p(
            tags$a(
              href = HANDBOOK_URL, target = "_blank", rel = "noopener noreferrer",
              "La Trobe University Handbook ↗"
            )
          )
        ),
        tags$section(
          h4(class = "footer-heading", "Credits"),
          p(paste("Developed by", APP_CREATOR)),
          p(paste("Prepared for", SCHOOL_NAME))
        )
      ),
      div(
        class = "footer-bottom",
        span(
          "This visualisation is a planning aid. The official Handbook remains the authoritative source " ,
          "for course rules, subject requirements, availability, and enrolment advice."
        ),
        span(paste0("Version ", APP_VERSION, " · 2026 dataset"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  output$subject_catalogue_table <- renderDT({
    catalogue <- subject_catalogue
    handbook_urls <- catalogue$`Handbook URL`
    catalogue$`Handbook URL` <- NULL
    catalogue$Handbook <- map_chr(handbook_urls, function(url) {
      if (!nzchar(url)) return("Not available")
      safe_url <- htmltools::htmlEscape(url)
      paste0(
        '<a href="', safe_url,
        '" target="_blank" rel="noopener noreferrer">Open ↗</a>'
      )
    })
    
    datatable(
      catalogue,
      rownames = FALSE,
      filter = "top",
      escape = setdiff(names(catalogue), "Handbook"),
      extensions = c("Buttons"),
      class = "stripe hover compact",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c(10, 25, 50, 100, "All")),
        scrollX = TRUE,
        autoWidth = TRUE,
        searchHighlight = TRUE,
        language = list(
          search = "Search all subjects:",
          searchPlaceholder = "Code, name, coordinator…"
        )
      )
    )
  }, server = TRUE)
  
  selected_course <- reactive({
    req(input$course)
    find_course(input$course)
  })
  
  observeEvent(selected_course(), {
    codes <- collect_aos_codes(selected_course()$structure)
    choices <- c("None" = "")
    if (length(codes)) {
      labels <- map_chr(codes, function(code) {
        paste0(text_value(aos_index[[code]]$name), " (", code, ")")
      })
      choices <- c(choices, setNames(codes, labels))
    }
    updateSelectizeInput(
      session, "major", choices = choices, selected = "", server = FALSE
    )
  }, ignoreInit = FALSE)
  
  roadmap_subjects <- reactive({
    course <- selected_course()
    req(course)
    subjects_for_selection(course, input$major %||% "")
  })
  
  observeEvent(roadmap_subjects(), {
    table <- roadmap_subjects()
    choices <- c("Select a subject" = "")
    if (nrow(table)) {
      labels <- ifelse(
        table$is_placeholder,
        paste0("Elective placeholder — Year ", table$year, " — ", table$credit_points, " CP"),
        paste0(table$code, " — ", table$name)
      )
      choices <- c(choices, setNames(table$code, labels))
    }
    current_subject <- input$subject %||% ""
    selected <- if (current_subject %in% table$code) current_subject else ""
    updateSelectizeInput(
      session, "subject", choices = choices, selected = selected, server = FALSE
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$clicked_subject, {
    code <- input$clicked_subject$code %||% ""
    if (code %in% roadmap_subjects()$code) {
      updateSelectizeInput(session, "subject", selected = code, server = FALSE)
    }
  })
  
  output$roadmap <- renderVisNetwork({
    table <- roadmap_subjects()
    req(nrow(table) > 0)
    graph <- build_graph(table, transpose = isTRUE(input$transpose))
    
    if (isTRUE(input$isolate)) {
      selected_code <- input$subject %||% ""
      if (nzchar(selected_code)) {
        keep_codes <- directed_related_codes(selected_code, graph$relations)
        graph$nodes <- filter(
          graph$nodes,
          id %in% keep_codes |
            grepl("^\\.layout-", id) |
            (grepl("^\\.route-", id) & relation_from %in% keep_codes & relation_to %in% keep_codes)
        )
        graph$edges <- filter(
          graph$edges,
          layout_edge | (relation_from %in% keep_codes & relation_to %in% keep_codes)
        )
      }
    }
    
    widget <- visNetwork(graph$nodes, graph$edges, width = "100%", height = "630px") %>%
      visNodes(
        shape = "box",
        margin = 7,
        widthConstraint = list(minimum = 108, maximum = 108),
        heightConstraint = list(minimum = 34)
      ) %>%
      visEdges(
        arrows = list(
          to = list(enabled = TRUE, scaleFactor = 0.9, type = "arrow")
        ),
        smooth = list(
          enabled = TRUE,
          type = "cubicBezier",
          forceDirection = "horizontal",
          roundness = 0.35
        )
      ) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(
        hover = TRUE,
        navigationButtons = FALSE,
        zoomView = FALSE,
        dragNodes = FALSE,
        dragView = FALSE
      ) %>%
      visOptions(highlightNearest = FALSE) %>%
      visEvents(
        hoverNode = "function(params) {
          window.applyRoadmapFocus(this, params.node);
        }",
        blurNode = "function(params) {
          if (this.pinnedSubject) {
            window.applyRoadmapFocus(this, this.pinnedSubject);
          } else {
            window.resetRoadmapFocus(this);
          }
        }",
        selectNode = "function(params) {
          if (params.nodes.length && /^\\.(layout|route)-/.test(String(params.nodes[0]))) {
            this.unselectAll();
          }
        }",
        selectEdge = "function(params) {
          if (params.edges.length) {
            var selectedEdge = this.body.data.edges.get(params.edges[0]);
            if (selectedEdge && selectedEdge.layout_edge) this.unselectAll();
          }
        }",
        click = "function(params) {
          if (params.nodes.length) {
            var code = params.nodes[0];
            if (/^\\.(layout|route)-/.test(String(code))) {
              this.unselectAll();
              return;
            }
            this.pinnedSubject = code;
            window.applyRoadmapFocus(this, code);
            var subjectSelect = document.getElementById('subject');
            if (subjectSelect && subjectSelect.selectize) {
              subjectSelect.selectize.setValue(code);
            }
            Shiny.setInputValue('detail_subject', code, {priority: 'event'});
            Shiny.setInputValue(
              'clicked_subject',
              {code: code, stamp: Date.now()},
              {priority: 'event'}
            );
          } else if (params.edges.length) {
            var clickedEdge = this.body.data.edges.get(params.edges[0]);
            if (clickedEdge && clickedEdge.layout_edge) {
              this.unselectAll();
              return;
            }
          } else {
            this.pinnedSubject = null;
            window.resetRoadmapFocus(this);
          }
        }"
      )
    
    widget
  })
  
  output$subject_detail <- bindEvent(renderUI({
    table <- roadmap_subjects()
    detail_code <- input$detail_subject %||% ""
    dropdown_code <- input$subject %||% ""
    code <- if (detail_code %in% table$code) detail_code else dropdown_code
    if (!nzchar(code)) {
      return(div(class = "detail-card", "Select or click a subject to view details."))
    }
    row <- table %>% filter(.data$code == .env$code)
    if (!nrow(row)) return(NULL)
    row <- row[1, ]
    
    div(
      class = "detail-card",
      div(
        class = "subject-name",
        if (isTRUE(row$is_placeholder)) {
          paste0("Elective placeholder — ", row$credit_points, " credit points")
        } else {
          paste0(row$code, " — ", row$name)
        }
      ),
      if (isTRUE(row$is_placeholder)) {
        tags$p(
          "This is a generic elective requirement. It represents credit points ",
          "to be completed, not a specific La Trobe subject."
        )
      },
      div(
        class = "detail-grid",
        div(class = "detail-label", "Year"), div(row$year),
        div(class = "detail-label", "Role"), div(tools::toTitleCase(row$role)),
        div(class = "detail-label", "Curriculum source"),
        div(if (grepl("(^|, )course($|, )", row$source)) "Course" else "Major / specialisation only"),
        div(class = "detail-label", "Credit points"), div(row$credit_points),
        div(class = "detail-label", "Coordinator"), div(row$coordinator),
        div(class = "detail-label", "AQF level"), div(row$aqf_level),
        div(class = "detail-label", "Elective"), div(row$available_as_elective),
        div(class = "detail-label", "Exchange"), div(row$available_to_exchange_students),
        div(class = "detail-label", "Capstone"), div(row$capstone)
      ),
      if (nzchar(row$url)) {
        tags$a(
          class = "handbook-link btn btn-primary",
          href = row$url,
          target = "_blank",
          rel = "noopener noreferrer",
          "Open handbook page"
        )
      }
    )
  }), input$detail_subject, input$subject, roadmap_subjects(), ignoreInit = FALSE)
}

shinyApp(ui, server)