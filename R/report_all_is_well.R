#' Generate Random Cowsay Praise
#'
#' Generate random praise from a cow.
#'
#' @export
#' @examples
#' report_all_is_well()
report_all_is_well <- function(){
	message <- paste0(
	    sprintf(cow, sample(adj, 1)),
        "\n\n\n\n"
	)
	class(message) <- c("all_good", "character")
	message
}


cow <- "\n ------- \nYou are %s! \n -------- \n    \\   ^__^ \n     \\  (oo)\\ ________ \n        (__)\\         )\\ /\\ \n             ||------w|\n             ||      ||"

adj <- c("outstanding", "astounding", "staggering", "kryptonian*", "breathtaking",
"stunning", "prodigious", "stupendous", "righteous", "wickedly awesome",
"superb", "sublime", "indomitable", "transcendent", "marvelous",
"resplendent", "phenomenal", "remarkable", "funkadelic", "magnificent",
"virtuosic", "rapturous", "flawless", "majestic", "splendiferous",
"legendary")
