A GitHub flavored Markdown textfile documenting a dataset.

Generated using [docdata package](https://hauselin.github.io/docdata/) on 2019-12-08 18:16:46.
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

| Column       | Type      | Description                                                  |
| ------------ | --------- | ------------------------------------------------------------ |
| study        | integer   | study number (1, 2, 3, 4)                                    |
| session      | integer   | session number (1, 2)                                        |
| pNo          | character | participant id                                               |
| condition    | character | experimental condition (control/low demand, deplete/high demand) |
| congruency   | character | Stroop congruency (congruent, incongruent)                   |
| sessionOrder | character | order of experimental condition assignment                   |
| a            | numeric   | EZ-diffusion boundary parameter                              |
| v            | numeric   | EZ-diffusion drift rate parameter                            |
| t0_Ter       | numeric   | EZ-diffusion non-decision time parameter                     |

End of documentation.

