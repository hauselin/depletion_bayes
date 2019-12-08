A GitHub flavored Markdown textfile documenting a dataset.

Generated using docdata package on 2019-12-08 14:02:22.
To cite this package, type citations("docdata") in console.

## Data source

stroop.csv

## About this file

* What (is the data): Stroop task data
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

* Rows: 2744
* Columns: 11

| Column       | Type      | Description                                                  |
| ------------ | --------- | ------------------------------------------------------------ |
| pNo          | character | participant id                                               |
| condition    | character | experimental condition (control/low demand, deplete/high demand) |
| study        | character | study number (1, 2, 3, 4)                                    |
| sessionOrder | character | order of experimental condition assignment                   |
| congruency   | character | Stroop congruency (congruent, incongruent)                   |
| rtOverall    | character | reaction time (seconds) collapsed across Stroop trial types  |
| accOverall   | character | accuracy (proportion correct) collapsed across Stroop trial types |
| rt           | character | reaction time (seconds)                                      |
| acc          | character | accuracy (proprotion correct)                                |
| rtCorrect    | character | reaction time (seconds) for correct trials                   |
| session      | character | session number (1, 2)                                        |

End of documentation.

