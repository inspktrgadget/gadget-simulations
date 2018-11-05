#!/bin/bash

#SBATCH --job-name="model.spp.scenario"
#SBATCH --time=100:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=10
#SBATCH --partition=omnip,long,normal
#SBATCH --mail-user=pnfrater@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=run-%j.o
#SBATCH --error=run-%j.e

module purge
module load gnu
module load R_base


R CMD BATCH /users/work/pnf1/gadget/simulations/age_data_var_rec/model/spp/scenario/run.R 
