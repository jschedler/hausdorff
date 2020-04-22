#' Hurricane Harvey registry data by uper neighborhood
#'
#' This data was downloaded from the Urban Data Platform (\url{kinderudp.org}). Please see the page for this dataset for full documentation.
#'
#' @format A shapefile containing the 88 super neighborhoods in the city of Houston, as well as summary data from the hurricane Harvey registry.
#' \describe{
#'   \item{sbname}{The name of the super neighborhood.}
#'   \item{url}{Link to the the website for the super neighborhood.}
#'   \item{freq}{The total number of respondents in the superneighborhood. A value of -1 indicates the data were suppressed for privacy reasons if 17 or fewer residents responded to the survey.}
#'   \item{p_flooded}{Percentage of respondents reporting a flooded home}
#'   \item{p_damaged}{Percentage of respondents reporting a damaged home}
#'   \item{p_displace}{Percentage of respondents who had to leave their home because of Hurricane Harvey}
#'   \item{p_trash}{Percentage of respondents reporting piles of trash or debris on their block}
#'   \item{p_sleep}{Percentage of respondents who answered "sometimes" or "often" to "I had trouble falling asleep or staying asleep, because of pictures or thoughts about it that came into my mind." Note that "it" refers to Hurricane Harvey and that the time frame for this question is the seven days preceding the moment the respondent answered the survey.}
#'   \item{p_runnynos}{Percentage of respondents reporting a runny nose, cough, postnasal drip, itchy eyes, or dry/scaly skin in the year following Hurricane Harvey.}
#'   \item{p_concentr}{Percentage of respondents reporting problems concentrating in the year following Hurricane Harvey.}
#'   \item{p_headache}{Percentage of respondents reporting headaches/migraines in the year following Hurricane Harvey}
#'   \item{p_shortbre}{Percentage of respondents reporting shortness of breath, chest tightness or pain, whistling or wheezing sound when exhaling, coughing or wheezing attacks by the cold or the flu, or trouble sleeping because of these respiratory symptoms in the year following Hurricane Harvey.}
#' }
#' @source \url{https://www.kinderudp.org/#/datasetCatalog/ep8kjgygg00d}
"hhr"