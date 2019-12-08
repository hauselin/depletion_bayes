A GitHub flavored Markdown textfile documenting a dataset.

Generated using docdata package on 2019-12-08 14:02:14.
To cite this package, type citations("docdata") in console.

## Data source

stroop_single_trial.csv

## About this file

* What (is the data): Stroop data (single-trial)
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

* Rows: 246600
* Columns: 7

| Column     | Type      | Description                                                  |
| ---------- | --------- | ------------------------------------------------------------ |
| pNo        | character | participant id                                               |
| condition  | character | experimental condition (control/low demand, deplete/high demand) |
| study      | integer   | study number (1, 2, 3, 4)                                    |
| trialnum   | integer   | trial number                                                 |
| congruency | character | Stroop congruency (congruent, incongruent)                   |
| acc        | numeric   | accuracy (0: error, 1: correct)                              |
| rt         | numeric   | reaction time (seconds)                                      |

End of documentation.

