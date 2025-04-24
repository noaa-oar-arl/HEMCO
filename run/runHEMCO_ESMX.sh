#!/bin/bash
#
# runHEMCO_ESMX.sh: Script to run HEMCO in standalone mode using the ESMX interface
#
# This script runs HEMCO using the ESMX interface with a YAML configuration file
# instead of the traditional text-based configuration files.
#
# Usage: ./runHEMCO_ESMX.sh
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
HEMCO_ESMX_BIN="../build_esmf/bin/hemco_esmx"
HEMCO_ESMX_CONFIG="./esmx/hemco_esmx.yaml"
LOG_FILE="HEMCO_ESMX.log"

# Check that HEMCO ESMX executable exists
if [ ! -f "$HEMCO_ESMX_BIN" ]; then
    echo "ERROR: HEMCO ESMX executable not found at $HEMCO_ESMX_BIN"
    echo "Please build HEMCO with ESMX support first."
    exit 1
fi

# Check that configuration file exists
if [ ! -f "$HEMCO_ESMX_CONFIG" ]; then
    echo "ERROR: HEMCO ESMX configuration file not found at $HEMCO_ESMX_CONFIG"
    exit 1
fi

# Print info
echo "Running HEMCO with ESMX interface..."
echo "Executable: $HEMCO_ESMX_BIN"
echo "Config:     $HEMCO_ESMX_CONFIG"
echo "Log file:   $LOG_FILE"

# Run HEMCO with ESMX. The "time" command will return CPU and wall times.
# Stdout and stderr will be directed to the log file.
time $HEMCO_ESMX_BIN --config $HEMCO_ESMX_CONFIG > $LOG_FILE 2>&1
exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo "HEMCO ESMX run completed successfully!"
else
    echo "ERROR: HEMCO ESMX run failed with exit code $exit_code"
    echo "Check the log file at $LOG_FILE for details."
fi

# Exit with the same code as the HEMCO run
exit $exit_code