PARTITION=$1
NSEEDS=$2
NUISANCE=$3

sbatch --array=1-$NSEEDS \
       --partition=$PARTITION \
       -n 1 \
       --output=/projects/dbenkes/allison/drotr_example_analysis/scratch/%a_%J.out \
       --job-name=drotr \
       --wrap "Rscript run_analysis.R \$SLURM_ARRAY_TASK_ID $NUISANCE"
