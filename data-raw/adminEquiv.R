
adminAbbrevs <- c(
  "Newfoundland"          = "NL",
  "Labrador"              = "NL",
  "Nova Scotia"           = "NS",
  "Prince Edward Island"  = "PE",
  "New Brunswick"         = "NB",
  "Quebec"                = "QC",
  "Ontario"               = "ON",
  "Manitoba"              = "MB",
  "Saskatchewan"          = "SK",
  "Alberta"               = "AB",
  "British Columbia"      = "BC",
  "Yukon Territory"       = "YT",
  "Northwest Territories" = "NT",
  "Nunavut"               = "NU"
)

adminEquiv <- data.table::data.table(
  admin_boundary = factor(names(adminAbbrevs)),
  admin_abbrev   = factor(adminAbbrevs)
)

usethis::use_data(adminEquiv, internal = TRUE, overwrite = TRUE)

