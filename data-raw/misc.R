library(readr)
library(usethis)

HoR_election_dates <- read_csv('HoR_election_dates.csv')
dd_election_dates <- read_csv('dd_election_dates.csv')

use_data(HoR_election_dates,overwrite=TRUE)

## Write Senate Term Ends

## Rules: half of the senate expires on the 30th of June every three years
## If there is a dd election, half the senate expires on the thirtieth of June between two and three years from the election, and then the usual pattern resumes

## See section 13 of the constitution:

"As soon as may be after the Senate first meets, and after each first meeting of the Senate following a dissolution thereof, the Senate shall divide the senators chosen for each State into two classes, as nearly equal in number as practicable; and the places of the senators of the first class shall become vacant at the expiration of three years, and the places of those of the second class at the expiration of six years, from the beginning of their term of service; and afterwards the places of senators shall become vacant at the expiration of six years from the beginning of their term of service.

The election to fill vacant places shall be made within one year before the places are to become vacant.

For the purposes of this section the term of service of a senator shall be taken to begin on the first day of July following the day of his election, except in the cases of the first election and of the election next after any dissolution of the Senate, when it shall be taken to begin on the first day of July preceding the day of his election."

## nb none of this applies to the four territory senators, whose terms coincide with HoR terms

senate_expiry_dates <- list()

first_election <- HoR_election_dates$date[1]
index <- 1

start_dates <- list()#in the sense of section 13 - not necessarily the actual 'start_date'
end_dates <- list()#

library(lubridate)

# calculate start dates from dd elections and the first election
first_july_first_before <- function(date){
	if (month(date) >= 7){
		month(date) <- 7
		day(date) <- 1
		return(date)
	}
	else{
		year(date) <- year(date) - 1
		month(date) <- 7
		day(date) <- 1
		return(date)
	}
}

start_dates[[1]] <- first_july_first_before(first_election)

dissolution_dates <- dd_election_dates$election_date

while (TRUE){

	ordinary_end_date <- start_dates[[index]] + years(3) - days(1)
	if (any( dissolution_dates > start_dates[[index]] & dissolution_dates < ordinary_end_date)){
		dissolution_date <- dissolution_dates[dissolution_dates > start_dates[[index]] & dissolution_dates < ordinary_end_date][1]
		dissolution_dates <- dissolution_dates[dissolution_dates != dissolution_date]
		end_dates[[index]] <- dissolution_date
		index <- index + 1
		start_dates[[index]] <- first_july_first_before(end_dates[[index - 1]])
	} else{
		end_dates[[index]] <- start_dates[[index]] + years(3) - days(1)
		index <- index + 1
		start_dates[[index]] <- end_dates[[index-1]] + days(1)
		
	}
	if (end_dates[[index-1]] > Sys.Date()){
		break
	}
}

start_dates <- start_dates[1:length(start_dates) - 1]

start_dates <- do.call(c,start_dates)
end_dates <- do.call(c,end_dates)

library(tibble)

senate_dates <- tibble(start_date = start_dates,end_date = end_dates)

use_data(senate_dates,overwrite=TRUE)
