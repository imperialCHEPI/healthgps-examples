#!/bin/bash
#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=128:mem=180gb
#PBS -J 1-10

# If the user hasn't set $HGPS_ROOT in their .profile, then assume it's in their home
# directory
if [ -z "$HGPS_ROOT" ]; then
    HGPS_ROOT=~/healthgps
fi

# Important: if you want to change the number of threads, you need to change this
# variable *AND* the ncpus option above
num_threads=128

# Model config is in same dir as this script
config_dir="$(dirname "$0")"

# NB: If you want the config to be configurable with a command-line argument, you can do
# this instead:
#   config_dir="$1"

# Load correct modules (be careful when changing).
module purge
module load tools/dev
module load GCCcore/12.3.0

# Run HGPS.
hgps="$HGPS_ROOT/out/install/linux-release/bin/HealthGPS.Console"
"$hgps" -c "$config_dir" -T $num_threads -j $PBS_ARRAY_INDEX
