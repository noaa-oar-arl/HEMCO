#!/bin/bash
#
# runHEMCO_NUOPC.sh: Script to run HEMCO in standalone mode using the NUOPC interface
#
# This script runs HEMCO using the NUOPC interface with a standard HEMCO configuration file.
#
# Usage: ./runHEMCO_NUOPC.sh [config_file]
#
# Created: April 23, 2025

#SBATCH -c 8
#SBATCH -N 1
#SBATCH -t 0-12:00
#SBATCH -p sapphire,huce_cascade,seas_compute,shared
#SBATCH --mem=15000
#SBATCH --mail-type=END

# Set the proper # of threads for OpenMP
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-1}

# Define paths
HEMCO_NUOPC_BIN="../build_nuopc/bin/hemco_nuopc_standalone"
DEFAULT_CONFIG="./HEMCO_Config.rc"
LOG_FILE="HEMCO_NUOPC.log"

# Check if a config file was provided
if [ $# -ge 1 ]; then
    CONFIG_FILE="$1"
else
    CONFIG_FILE="$DEFAULT_CONFIG"
fi

# Check that HEMCO NUOPC executable exists
if [ ! -f "$HEMCO_NUOPC_BIN" ]; then
    echo "ERROR: HEMCO NUOPC standalone executable not found at $HEMCO_NUOPC_BIN"
    echo "Please build HEMCO with NUOPC support first."
    exit 1
fi

# Check that configuration file exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "ERROR: HEMCO configuration file not found at $CONFIG_FILE"
    exit 1
fi

# Print info
echo "Running HEMCO with NUOPC interface..."
echo "Executable: $HEMCO_NUOPC_BIN"
echo "Config:     $CONFIG_FILE"
echo "Log file:   $LOG_FILE"

# Run HEMCO with NUOPC. The "time" command will return CPU and wall times.
# Stdout and stderr will be directed to the log file.
time $HEMCO_NUOPC_BIN --config "$CONFIG_FILE" > $LOG_FILE 2>&1
exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo "HEMCO NUOPC run completed successfully!"
else
    echo "ERROR: HEMCO NUOPC run failed with exit code $exit_code"
    echo "Check the log file at $LOG_FILE for details."
fi

# Exit with the same code as the HEMCO run
exit $exit_code