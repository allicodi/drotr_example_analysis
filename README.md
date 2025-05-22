# RSPH HPC Analysis Example
## OTR Project

This is an example of how to use RSPH HPC for basic data analysis. Requires working knowledge of RSPH HPC coverered in RSPH HPC Cheat Sheet. 

### Initial setup

Follow these steps to get your environment set up on RSPH HPC

1. **Log into RSPH HPC**

See RSPH HPC Cheat sheet for instructions

2. **Clone repository into home directory**

```bash
git clone git@github.com:allicodi/drotr_example_analysis
```

3. **Create directory for results and scratch files in the project space**

```bash

# PREREQ: If you don't already have a folder for your projects within the project space, create this first
cd /projects/dbenkes
mkdir <your_name>

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
# Swap 'allison' for 'your_name'
# Click 'ESC'
# Click ':wq' for write (save) and quit

```

- Repeat for paths on lines 77, 80, 107, and 110 of `run_analysis.R`

5. Create personal R library and install required packages

## Running the analysis

1. ./run_simulation.sh arg1 arg2 arg3