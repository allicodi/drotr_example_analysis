# RSPH HPC Analysis Example
## OTR Project

This is an example of how to use RSPH HPC for basic data analysis. Requires working knowledge of RSPH HPC coverered in RSPH HPC Cheat Sheet. 

### Initial setup

Follow these steps to set up your environment on RSPH HPC

1. **Log into RSPH HPC**

See RSPH HPC Cheat sheet for instructions

2. **Clone repository into home directory**

```bash
git clone git@github.com:allicodi/drotr_example_analysis
```

3. **Create directory for results and scratch files in the project space**

```bash

# PREREQ: If you don't already have a folder for your projects within the project space, create this first
cd /projects/entericsepi
mkdir <your_name>         # or other organization system that is used in the project space

# Create project folder
cd <your_name>
mkdir drotr_example_analysis

cd drotr_example_analysis
mkdir scratch
mkdir results
mkdir nuisance

```

4. **Update output paths in scripts to match directories created**

- Change the output path on line 8 of `sub_jobs.sh` to match the path to your scratch folder created in step 3. This can be done using vim or nano text editors on HPC directly, or make changes locally then push to GitHub. To use vim:

 ```bash
cd ~/drotr_example_analysis
vi sub_jobs.sh

# Use keypad to move cursor down to output line
# Click 'i' to enter insert mode
# Swap 'dbenkes' for 'entericsepi' (or other project space folder) and 'allison' for 'your_name'
# Click 'ESC'
# Click ':wq' for write (save) and quit

```

- Repeat for paths on lines 77, 80, 107, and 110 of `run_analysis.R`

5. Create personal R library and install required packages

```bash
   # Create a directory for R libraries
   mkdir ~/Rlibs

   # Start an interactive session (do not do this on the login node)
   srun --pty --partition=interactive-cpu --nodes=1 --ntasks-per-node=1 --mem-per-cpu=8G --time=02:00:00 bash

   # Open R console
   R

   # Set library path and install packages
   .libPaths("~/Rlibs")

   # Install drotr package from GitHub
   devtools::install_github("allicodi/drotr")

   # Install other packages from CRAN
   install.packages(c("here"))

   ```

## Running the analysis

1. Navigate to repository with data analysis code and submit a job using `sub_jobs.sh`

```bash

cd ~/drotr_example_analysis

# Argument 1: Partition 
# Argument 2: Number of seeds
# Argument 3: Fit nuisance models (TRUE) or load existing nuisance models (FALSE)

# Example running on empire partition for 3 seeds and fitting nuisance models
./sub_jobs.sh empire 3 TRUE

```

You can check on the progress of your jobs using `squeue -u <user_id>`. You will see something similar to this:

```bash
[acodi@node23 drotr_example_analysis]$ squeue -u acodi
             JOBID PARTITION     NAME     USER ST       TIME  NODES NODELIST(REASON)
        28705647_1    empire    drotr    acodi  R       0:02      1 node52
        28705647_2    empire    drotr    acodi  R       0:02      1 node52
        28705647_3    empire    drotr    acodi  R       0:02      1 node52
```

2. When your job finishes running, results will be saved in the `drotr_example_analysis/results` folder you made within the project space. You can transfer them to your local machine using SCP/SFTP, or begin an interactive R session to view them interactively.