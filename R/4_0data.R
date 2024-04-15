#' ACCES Program
#'
#' A dataset comprising 8245 applicants to the ACCES Program across 23 different
#' departments in Colombia, including eligibility for the ACCES Program,
#' position scores of the SABER 11, cutoffs of each department, and the names of each department.
#'
#' @format A data frame with 8245 rows and 4 columns: \describe{
#'   \item{elig}{ligibility for the ACCES Program.
#'   1: eligible; 0: not eligible}
#'   \item{saber11}{position scores of the SABER 11.
#'   We multiply the position score by −1 so that the values of the running variable
#'   above a cutoff lead to the program eligibility.}
#'   \item{cutoff}{cutoffs of each department.}
#'   \item{department}{the names of each department.}
#' @references Melguizo, T., F. Sanchez, and T. Velasco (2016). Credit for low-income students and access to and academic performance in higher education in colombia: A regression discontinuity approach. World development 80, 61–77.
"acces"

