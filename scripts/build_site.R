library(readxl)
library(dplyr)
library(knitr)
library(scales)

# ---- LOAD DATA ----

plan <- read_excel("data/training_plan.xlsx", sheet = 1)

week <- read_excel(
  "data/training_plan.xlsx",
  sheet = 2,
  range = "A1:F8"
)

# ---- CALCULATIONS ----

week_km <- sum(week$`Plan km`, na.rm = TRUE)

target_km <- plan$`Planlagt km`[1]

status <- case_when(
  week_km > target_km + 10 ~ "⚠️ Overload",
  week_km < target_km - 15 ~ "⚠️ For lav",
  TRUE ~ "✅ OK"
)

progress <- week_km / target_km

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

# ---- BUILD TABLE ----

week_html <- "<table><tr>"
cols <- colnames(week)

for (c in cols) {
  week_html <- paste0(week_html, "<th>", c, "</th>")
}
week_html <- paste0(week_html, "</tr>")

for (i in 1:nrow(week)) {
  week_html <- paste0(week_html, "<tr>")
  
  for (c in cols) {
    value <- week[i, c]
    
    if (c == "Type") {
      week_html <- paste0(
        week_html,
        "<td style='background:", week$color[i], "; color:white;'>",
        value,
        "</td>"
      )
    } else {
      week_html <- paste0(week_html, "<td>", value, "</td>")
    }
  }
  
  week_html <- paste0(week_html, "</tr>")
}

week_html <- paste0(week_html, "</table>")

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
<title>Mors 100 Miles Dashboard</title>

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
  grid-template-columns: 1fr 1fr 1fr;
  gap: 20px;
  margin-bottom: 20px;
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
</style>

</head>
<body>

<div class='container'>

<h1>🏃‍♂️ Mors 100 Miles</h1>

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
", knitr::kable(plan, format = "html"), "
</div>

</div>

</body>
</html>
"
)

writeLines(html, "docs/index.html")