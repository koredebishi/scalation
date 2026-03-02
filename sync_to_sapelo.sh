#!/bin/bash
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# sync_to_sapelo.sh - Sync local code to Sapelo HPC
#
# Usage:
#   ./sync_to_sapelo.sh
#
# This script syncs the scalation_2.0 source code to Sapelo, excluding
# build artifacts and logs that don't need to be transferred.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Configuration
REMOTE_USER="krb84578"
REMOTE_HOST="sapelo2.gacrc.uga.edu"  # Login node (use xfer.gacrc.uga.edu for large transfers)
REMOTE_PATH="/scratch/krb84578/workDir/scalation_2.0"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}======================================${NC}"
echo -e "${GREEN}  Syncing scalation_2.0 to Sapelo    ${NC}"
echo -e "${GREEN}======================================${NC}"
echo ""

# Files/directories to exclude from sync
EXCLUDES=(
    "target/"
    "project/target/"
    "project/project/target/"
    ".bloop/"
    ".metals/"
    ".idea/"
    "*.class"
    "*.log"
    "log/experiments/"
    "log/outfiles/"
    "log/recorder/"
    "log/scalation/"
    "log/simulation/"
    "*.ser"
    ".git/"
    "hs_err_*.log"
)

# Build exclude arguments for rsync
EXCLUDE_ARGS=""
for item in "${EXCLUDES[@]}"; do
    EXCLUDE_ARGS="$EXCLUDE_ARGS --exclude='$item'"
done

echo -e "${YELLOW}Excluded patterns:${NC}"
for item in "${EXCLUDES[@]}"; do
    echo "  - $item"
done
echo ""

# Dry run first to show what will be transferred
echo -e "${YELLOW}Performing dry run...${NC}"
eval rsync -avzn --delete $EXCLUDE_ARGS ./ ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/

echo ""
echo -e "${YELLOW}The above shows what WILL be transferred.${NC}"
read -p "Proceed with actual sync? (y/n): " confirm

if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
    echo ""
    echo -e "${GREEN}Starting sync...${NC}"
    eval rsync -avz --progress --delete $EXCLUDE_ARGS ./ ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/

    echo ""
    echo -e "${GREEN}======================================${NC}"
    echo -e "${GREEN}  Sync complete!                     ${NC}"
    echo -e "${GREEN}======================================${NC}"
    echo ""
    echo -e "Next steps on Sapelo:"
    echo -e "  1. ssh ${REMOTE_USER}@sapelo2.gacrc.uga.edu"
    echo -e "  2. cd ${REMOTE_PATH}"
    echo -e "  3. ml Java/21.0.5"
    echo -e "  4. sbt assembly"
    echo -e "  5. sbatch run_CalibrationArray.sbatch"
else
    echo -e "${RED}Sync cancelled.${NC}"
fi

