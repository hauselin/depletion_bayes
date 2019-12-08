A GitHub flavored Markdown textfile documenting a dataset.

Generated using docdata package on 2019-12-08 17:55:21.
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

| Column        | Type       | Description                                                  |
| ------------- | ---------- | ------------------------------------------------------------ |
| pNo           | character  | participantid                                                |
| condition     | character  | experimentalcondition(control/lowdemand,deplete/highdemand)  |
| study         | character  | studynumber(1,2,3,4)                                         |
| sessionOrder  | character  | orderofexperimentalconditionassignment                       |
| congruency    | character  | Stroopcongruency(congruent,incongruent)                      |
| rtOverall     | character  | reactiontime(seconds)collapsedacrossStrooptrialtypes         |
| accOverall    | character  | accuracy(proportioncorrect)collapsedacrossStrooptrialtypes   |
| rt            | character  | reactiontime(seconds)                                        |
| acc           | character  | accuracy(proprotioncorrect)                                  |
| rtCorrect     | character  | reactiontime(seconds)forcorrecttrials                        |
| session       | character  | sessionnumber(1,2)                                           |

End of documentation.

