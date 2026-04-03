library(readxl)
library(dplyr)
library(knitr)
library(scales)
library(ggplot2)
library(tidyr)
# ---- LOAD DATA ----

plan <- read_excel("data/training_plan.xlsx", sheet = 1)
plan$Startdato_date <- as.Date(plan$Startdato)
plan$Startdato_vis <- format(plan$Startdato_date, "%d-%m-%Y")

plan$phase_color <- case_when(
  grepl("Base", plan$Fase) ~ "#2ecc71",
  grepl("Opbygning", plan$Fase) ~ "#3498db",
  grepl("Specifik|Peak", plan$Fase) ~ "#9b59b6",
  grepl("Recovery", plan$Fase) ~ "#f39c12",
  grepl("Taper", plan$Fase) ~ "#e67e22",
  grepl("Race", plan$Fase) ~ "#e74c3c",
  TRUE ~ "#7f8c8d"
)

week <- read_excel(
  "data/training_plan.xlsx",
  sheet = 2,
  range = "A1:F8"
)

week$Type <- case_when(
  week$Type == "Easy" ~ "Easy run",
  week$Type == "Key1" ~ "Kvalitet",
  week$Type == "Key2" ~ "Kvalitet",
  week$Type == "Long" ~ "Langtur",
  week$Type == "B2B" ~ "Back-to-back",
  TRUE ~ week$Type
)

week$effort_color <- case_when(
  week$Type == "Easy run" ~ "#2ecc71",     # grøn
  week$Type == "Kvalitet" ~ "#c0392b",     # rød
  week$Type == "Langtur" ~ "#3498db",      # blå
  week$Type == "Back-to-back" ~ "#3498db", # blå
  TRUE ~ "#7f8c8d"                         # fallback grå
)

# ---- FIND AKTUEL UGE ----
today <- Sys.Date()

plan <- plan %>%
  mutate(
    current = today >= Startdato_date & today < (Startdato_date + 7)
  )

today_name <- weekdays(Sys.Date())

week$Today <- ifelse(week$Dag == today_name, TRUE, FALSE)

# ---- FILTER VISNING ----

current_week <- which(plan$current)

if (length(current_week) == 0) {
  if (today < min(plan$Startdato_date)) {
    # Før planen starter
    current_week <- 1
  } else {
    # Efter planen er slut
    current_week <- nrow(plan)
  }
}

# 1. Vælg relevante rækker først
plan_view <- plan[idx_start:idx_end, ]
plan_view$Aktuel <- ""

# 2. Tilføj aktuel markering
plan_view$Aktuel <- ifelse(
  (idx_start:idx_end) == current_week,
  "➡️",
  ""
)

# 3. Vælg kun de kolonner du vil vise
plan_view <- plan_view %>%
  select(
    Aktuel,
    Uge,
    Startdato_vis,
    Fase,
    `Planlagt km`,
    KS1,
    KS2,
    Long,
    `Fast finish`
  )

# 4. Rename
colnames(plan_view) <- c(
  "Aktuel",
  "Uge",
  "Start",
  "Fase",
  "Km",
  "KS1",
  "KS2",
  "Long/B2B",
  "Fast"
)

# ---- CALCULATIONS ----

week_km <- sum(week$`Plan km`, na.rm = TRUE)
actual_km <- sum(week$`Faktisk km`, na.rm = TRUE)

target_km <- plan$`Planlagt km`[1]

status <- case_when(
  week_km > target_km + 10 ~ "⚠️ Overload",
  week_km < target_km - 15 ~ "⚠️ For lav",
  TRUE ~ "✅ OK"
)

week$cum_km <- cumsum(replace_na(week$`Faktisk km`, 0))
week$Dag <- factor(
  week$Dag,
  levels = c("Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag","Søndag")
)
#progress <- week_km / target_km
progress <- ifelse(target_km > 0, actual_km / target_km, 0)
progress <- min(progress, 1.2)




# ---- TYPE COLORS ----

color_type <- function(type) {
  case_when(
    type %in% c("Key1", "Key2") ~ "#e74c3c",
    type == "Long" ~ "#2980b9",
    type == "B2B" ~ "#8e44ad",
    TRUE ~ "#2ecc71"
  )
}

week$color <- color_type(week$Type)
week_display <- week %>%
  select(Dag, `Plan km`, `Faktisk km`, Type, Styrke, Mobilitet)
# ---- GRAF -----

# ---- DATA ----
week <- week %>%
  mutate(
    faktisk_km = replace_na(`Faktisk km`, 0),
    plan_km = replace_na(`Plan km`, 0),
    cum_faktisk = cumsum(faktisk_km),
    cum_plan = cumsum(plan_km)
  )

# rækkefølge
week$Dag <- factor(
  week$Dag,
  levels = c("Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag","Søndag")
)

# ---- FIX: kun vurder status op til i dag ----
today_idx <- max(which(week$Today), 1)

week <- week %>%
  mutate(
    status = case_when(
      row_number() > today_idx ~ "future",
      cum_faktisk >= cum_plan ~ "ahead",
      TRUE ~ "behind"
    )
  )

# ---- PLOT ----
p_week <- ggplot(week, aes(x = Dag)) +
  
  # plan (stiplet)
  geom_line(
    aes(y = cum_plan, group=1),
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  
  # faktisk (farvet)
  geom_line(
    aes(y = cum_faktisk, color = status, group = 1),
    linewidth = 1.5
  ) +
  
  geom_point(
    aes(y = cum_faktisk, color = status, group = 1),
    size = 2.5
  ) +
  
  # mål-linje
  geom_hline(
    yintercept = target_km,
    linetype = "dotted"
  ) +
  
  # i dag
  geom_point(
    data = subset(week, Today == TRUE),
    aes(y = cum_faktisk,group = 1),
    size = 4,
    color = "black"
  ) +
  
  scale_color_manual(
    values = c(
      "ahead" = "#2ecc71",
      "behind" = "#e74c3c",
      "future" = "#bdc3c7"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "Ugens progression",
    x = "",
    y = "Km"
  )

ggsave(
  "docs/week_progress.svg",
  p_week,
  width = 8,
  height = 4
)

# ---- BUILD TABLE ----

week_html <- "<table><tr>"
#cols <- colnames(week_display)
cols <- c("Dag", "Plan km", "Faktisk km", "Type", "Styrke", "Mobilitet")

for (c in cols) {
  week_html <- paste0(week_html, "<th>", c, "</th>")
}
  week_html <- paste0(week_html, "</tr>")

for (i in 1:nrow(week)) {
  row_style <- ifelse(
    week$Today[i],
    "style='background:#ffeaa7; font-weight:600;'",
    ""
  )
  week_html <- paste0(week_html, "<tr ", row_style, ">")
  
  for (c in cols) {
    value <- week_display[i, c]
    
    if (c == "Type") {
      week_html <- paste0(
        week_html,
        "<td><span style='
      background:", week$effort_color[i], ";
      color:white;
      padding:4px 10px;
      border-radius:12px;
      font-size:12px;
    '>", value, "</span></td>"
      )
    } else {
      week_html <- paste0(week_html, "<td>", value, "</td>")
    }
  }
  
  week_html <- paste0(week_html, "</tr>")
}

week_html <- paste0(week_html, "</table>")


#----
plan_html <- "<table><tr>"
cols_plan <- colnames(plan_view)

for (c in cols_plan) {
  plan_html <- paste0(plan_html, "<th>", c, "</th>")
}
plan_html <- paste0(plan_html, "</tr>")
for (i in 1:nrow(plan_view)) {
  
  # Highlight aktuel uge
  row_style <- ifelse(
    plan_view$Aktuel[i] == "⬅️",
    "style='background:#e8f6ff; font-weight:600;'",
    ""
  )
  
  plan_html <- paste0(plan_html, "<tr ", row_style, ">")
  
  for (c in cols_plan) {
    value <- plan_view[[c]][i]
    
    if (c == "Fase") {
      plan_html <- paste0(
        plan_html,
        "<td><span style='
          background:", plan$phase_color[idx_start + i - 1], ";
          color:white;
          padding:4px 10px;
          border-radius:12px;
          font-size:12px;
        '>", value, "</span></td>"
      )
    } else {
      plan_html <- paste0(plan_html, "<td>", value, "</td>")
    }
  }
  
  plan_html <- paste0(plan_html, "</tr>")
}
plan_html <- paste0(plan_html, "</table>")

# ---- PROGRESS BAR ----

progress_bar <- paste0(
  "<div style='background:#ddd; width:300px; height:20px; border-radius:10px;'>",
  "<div style='background:#27ae60; width:", percent(progress),
  "; height:20px; border-radius:10px;'></div>",
  "</div>"
)

# ---- HTML ----

html <- paste0(
  "
<!DOCTYPE html>
<html>
<head>
<meta charset='UTF-8'>
<title>RohdeRunning's Dashboard</title>

<style>
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto;
  background: #f5f7fa;
  margin: 0;
  padding: 20px;
}

h1 {
  margin-bottom: 10px;
}

.container {
  max-width: 1100px;
  margin: auto;
}

.grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 20px;
}

.card {
  background: white;
  padding: 20px;
  border-radius: 12px;
  box-shadow: 0 4px 10px rgba(0,0,0,0.08);
}

.big {
  font-size: 28px;
  font-weight: bold;
}

.status-ok { color: #27ae60; }
.status-warn { color: #e67e22; }
.status-bad { color: #e74c3c; }

table {
  border-collapse: collapse;
  width: 100%;
  margin-top: 10px;
}

th, td {
  padding: 8px;
  border-bottom: 1px solid #eee;
  text-align: center;
}

th {
  background: #fafafa;
}

.progress {
  background: #eee;
  border-radius: 10px;
  height: 10px;
  margin-top: 10px;
}

.progress-bar {
  background: #27ae60;
  height: 10px;
  border-radius: 10px;
}

table {
  border-collapse: collapse;
  width: 100%;
  margin-top: 10px;
  border-radius: 8px;
  overflow: hidden;
}

tr:nth-child(even) {
  background-color: #fafafa;
}

tr:hover {
  background-color: #f1f1f1;
}

td {
  padding: 10px;
}

th {
  padding: 10px;
  background: #f8f9fb;
  font-weight: 600;
}

.table-container {
  overflow-x: auto;
  margin-top: 10px;
}

.table-container table {
  min-width: 900px;
}

table {
  font-size: 13px;
}

th {
  position: sticky;
  top: 0;
  background: #f8f9fb;
  z-index: 1;
}
</style>

</head>
<body>

<div class='container'>

<h1>RohdeRunning Træning</h1>

<div class='grid'>
  <div class='card'>
    <div>Planlagt km</div>
    <div class='big'>", round(week_km,1), "</div>
  </div>

  <div class='card'>
    <div>Mål</div>
    <div class='big'>", target_km, "</div>
  </div>

<div class='card'>
  <div>Faktisk km</div>
  <div class='big'>", round(actual_km,1), "</div>
</div>

  <div class='card'>
    <div>Status</div>
    <div class='big'>", status, "</div>
  </div>
</div>

<div class='card'>
  <div>Progress</div>
  <div class='progress'>
    <div class='progress-bar' style='width:", percent(progress), ";'></div>
  </div>
</div>


<div class='card'>
<h2>Aktuel uge</h2>
", week_html, "
</div>



<div class='card'>
<h2>Overordnet plan</h2>
", knitr::kable(plan_view, format = "html"), "
</div>

<div class='card'>
  <h3>Ugens progression</h3>
  <img src='week_progress.svg' style='width:100%'>
</div>

</div>

</body>
</html>
"
)

writeLines(html, "docs/index.html")