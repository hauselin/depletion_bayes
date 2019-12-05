# Dataset variables

The `pNo` variable in all dataset is the participant ID. For studies 3 and 4 (`study == 3` or `study == 4`), the participant IDs have been "encrypted"/anonymized (i.e., the IDs look like Amazon MTurk Worker IDs, but they aren't the original IDs.)

## ddm.csv

EZ-ddm (EZ-drift-diffusion model) dataset**

study: study number (1, 2, 3, 4)

session: session number (1, 2)

pNo: participant id

condition: experimental condition

* control: low demand condition
* deplete: high demand condition

congruency: stroop trial type congruency

* congruent: congruent stroop trials
* incongruent: incongruent stroop trials

sessionOrder: order of experimental condition assignment for participant

* control-deplete: control (low demand) then deplete (high demand)
* deplete-control: deplete (high demand) then control (low demand) 

a: EZ-diffusion boundary parameter 

v: EZ-diffusion drift rate parameter

t0_Ter: EZ-diffusion non-decision time parameter

## ratings.csv

**Ratings/phenomenology dataset with boundary and drift rate parameters**

pNo: participant id

condition: experimental condition

* control: low demand condition
* deplete: high demand condition

study: study number (1, 2, 3, 4)

a: EZ-diffusion boundary parameter 

v: EZ-diffusion drift rate parameter

bored: boredom rating

effort: effort rating

fatigue: fatigue rating

frustrate: frustration rating

mentaldemand: mental demand rating

session: session number (1, 2)

## stroop.csv

**Stroop task data**

pNo: participant id

condition: experimental condition

* control: low demand condition
* deplete: high demand condition

study: study number (1, 2, 3, 4)  

sessionOrder: order of experimental condition assignment for participant

* control-deplete: control (low demand) then deplete (high demand)
* deplete-control: deplete (high demand) then control (low demand) 

congruency: stroop trial type congruency

* congruent: congruent stroop trials
* incongruent: incongruent stroop trials

rtOverall: reaction time (seconds) collapsed across Stroop trial types

accOverall: accuracy (proportion correct) collapsed across Stroop trial types

rt: reaction time (seconds)

acc: accuracy (proprotion correct)

rtCorrect: reaction time (seconds) for correct trials

session: session number (1, 2)

## stroop_single_trial.csv

**Single-trial stroop task data**

pNo: participant id

condition: experimental condition

* control: low demand condition
* deplete: high demand condition

study: study number (1, 2, 3, 4)  

trialnum: trial number

congruency: stroop trial type congruency

* congruent: congruent stroop trials
* incongruent: incongruent stroop trials

rt: reaction time (seconds)

acc: accuracy (proprotion correct)

rtCorrect: reaction time (seconds) for correct trials