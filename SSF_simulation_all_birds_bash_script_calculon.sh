#!/bin/bash

#SBATCH --partition=generalq
#SBATCH --job-name=simulation.bird.emigration.test.1
#SBATCH --mail-type=ALL
#SBATCH --mail-user=luis.arias@mail.utoronto.ca
#SBATCH --ntasks=40
#SBATCH --mem=15gb
#SBATCH --output=test1.out
#SBATCH --error=test1.err

module load R/3.6.0
module load gcc/8.3.0

Rscript "SSF_simulation_all_birds_R_file_calculon.R"

echo Done

