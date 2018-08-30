#!/bin/bash

#SBATCH --job-name="model_type.spp_type"
#SBATCH --time=300:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=25
#SBATCH --partition=omnip
#SBATCH --mail-user=pnfrater@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=run-%j.o
#SBATCH --error=run-%j.e

module purge
module load gnu
module load R_base


R CMD BATCH /users/work/pnf1/gadget/simulations/age_data_var_rec/model_type/spp_type/run.R 
