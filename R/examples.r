# In this file we provide some example cases for the given functions of our package.


require(httr)
require(purr)
require(tidyr)

# Find indicator code by accessing KPI
url <- "http://api.kolada.se/v2/kpi?"

res <- content(GET(url))
dfmeta <- map_df(.x = res$values, ~flatten(.x))
dfmeta

# Download metadata for a specific KPI ID#N07402

url <- "http://api.kolada.se/v2/kpi/N07402" # alternatively the user can type: "http://api.kolada.se/v2/kpi?title=kvinnofridskränkning"

res <- content(GET(url))
dfmeta <- map_df(.x = res$values, ~flatten(.x))

dfmeta

# Search organizational units in a specific municipality

url <- "http://api.kolada.se/v2/ou?municipality=1280"

res <- content(GET(url))

trans <- transpose(res$values)

dfob <- map_df(.x = trans, .f = simplify)

dfob

# Get data for KPIs in a given municipality for a specific year
url <- "http://api.kolada.se/v2/data/kpi/N07402/municipality/1280,1281"
res <- content(GET(url))

trans <- transpose(res$values)

df <- map_df(.x = ltrans, .f = simplify)

df <- df %>%
  unnest(values) %>%
  mutate(values = map(values, ~flatten(.x) %>% bind_rows() ) ) %>%
  unnest(values)

df