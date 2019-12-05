#!/bin/bash
#SBATCH --job-name="R"
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --ntasks=40 # number of cpus
#SBATCH --cpus-per-task=1
#SBATCH --mem=30000
#SBATCH --output=output.out
##SBATCH --mail-user=hauselin@gmail.com
#SBATCH --mail-type=FAIL
##SBATCH --time=40:00:00
##SBATCH --requeue
##SBATCH --checkpoint=1:0:0

# begin
hostname
pwd
date +'%y-%m-%d %H:%M:%S'

# load modules
module load R/3.5.3

# runscripts
Rscript m_a_condition_congruency.R
Rscript m_v_condition_congruency.R
# Rscript m_a_phenom.R
# Rscript m_v_phenom.R

# finish
date +'%y-%m-%d %H:%M:%S'