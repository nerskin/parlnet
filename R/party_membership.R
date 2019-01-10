#' @export

party_membership <- function(chamber = 'HoR',start_date = '1998-01-01',end_date = '2100-01-01'){
	start_date <- lubridate::ymd(start_date)
	end_date <- lubridate::ymd(end_date)
	relevant_speeches <- dplyr::filter(parlnet::speeches,date >= start_date,date <= end_date,chamber == chamber)
	dplyr::summarise(dplyr::group_by(dplyr::count(relevant_speeches,name,party),name),party = party[which.max(n)])
}
