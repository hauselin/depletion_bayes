A GitHub flavored Markdown textfile documenting a dataset.

Generated using docdata package on 2019-12-08 17:55:00.
To cite this package, type citations("docdata") in console.

## Data source

ddm.csv

## About this file

* What (is the data): EZ-ddm (drift-diffusion model) dataset
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
* Columns: 9

| Column        | Type       | Description                                                  |
| ------------- | ---------- | ------------------------------------------------------------ |
| study         | character  | studynumber(1,2,3,4)                                         |
| session       | character  | sessionnumber(1,2)                                           |
| pNo           | character  | participantid                                                |
| condition     | character  | experimentalcondition(control/lowdemand,deplete/highdemand)  |
| congruency    | character  | Stroopcongruency(congruent,incongruent)                      |
| sessionOrder  | character  | orderofexperimentalconditionassignment                       |
| a             | character  | EZ-diffusionboundaryparameter                                |
| v             | character  | EZ-diffusiondriftrateparameter                               |
| t0_Ter        | character  | EZ-diffusionnon-decisiontimeparameter                        |

End of documentation.

