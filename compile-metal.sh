#!/bin/bash

set -euo pipefail

METAL_FILE="glitch_effect.metal"
AIR_FILE="glitch_effect.air"
METALLIB_FILE="glitch_effect.metallib"

xcrun -sdk macosx metal -c "${METAL_FILE}" -o "${AIR_FILE}" -mmacos-version-min=14.1

xcrun -sdk macosx metallib "${AIR_FILE}" -o "${METALLIB_FILE}"

rm -f "${AIR_FILE}"
