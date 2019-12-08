A GitHub flavored Markdown textfile documenting a dataset.

Generated using docdata package on 2019-12-08 14:02:03.
To cite this package, type citations("docdata") in console.

## Data source

ratings.csv

## About this file

* What (is the data): Ratings/phenomenology dataset with boundary and drift rate parameters
* Who (generated this documentation): Hause Lin
* Who (collected the data): Hause Lin
* When (was the data collected): between 2016 to 2018
* Where (was the data collected): University of Toronto and Amazon MTurk
* How (was the data collected): 
* Why (was the data collected): Experimental study of effort exertion, fatigue, "ego depletion"

## Additional information

* Contact: hauselin@gmail.com
* Registration: see https://github.com/hauselin/depletion_bayes

## Columns

* Rows: 1352
* Columns: 11

| Column       | Type      | Description                                                  |
| ------------ | --------- | ------------------------------------------------------------ |
| pNo          | character | participant id                                               |
| condition    | character | experimental condition (control/low demand, deplete/high demand) |
| study        | integer   | study number (1, 2, 3, 4)                                    |
| a            | numeric   | EZ-diffusion boundary parameter                              |
| v            | numeric   | EZ-diffusion drift rate parameter                            |
| bored        | numeric   | boredom rating                                               |
| effort       | numeric   | effort rating                                                |
| fatigue      | numeric   | fatigue rating                                               |
| frustrate    | numeric   | frustration rating                                           |
| mentaldemand | numeric   | mental demand rating                                         |
| session      | numeric   | session number (1, 2)                                        |

End of documentation.

