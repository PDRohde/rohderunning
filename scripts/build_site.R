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
body { font-family: Arial; margin: 40px; background:#f9f9f9; }
.card {
  background:white;
  padding:20px;
  margin-bottom:20px;
  border-radius:10px;
  box-shadow:0px 2px 6px rgba(0,0,0,0.1);
}
table { border-collapse: collapse; width:100%; }
th, td { border: 1px solid #ddd; padding: 6px; text-align:center; }
th { background: #f4f4f4; }
</style>
</head>
<body>

<h1>🏃‍♂️ Mors 100 Miles – Dashboard</h1>

<div class='card'>
<h2>📊 Uge status</h2>
<p><b>Planlagt km:</b> ", round(week_km,1), "</p>
<p><b>Mål:</b> ", target_km, "</p>
<p><b>Status:</b> ", status, "</p>
", progress_bar, "
</div>

<div class='card'>
<h2>📅 Aktuel uge</h2>
", week_html, "
</div>

<div class='card'>
<h2>📈 Overordnet plan</h2>
", knitr::kable(plan, format = "html"), "
</div>

</body>
</html>
"
)

writeLines(html, "docs/index.html")