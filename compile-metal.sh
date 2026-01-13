#!/bin/bash

set -euo pipefail

xcrun -sdk macosx metal -c glitch-effect.metal -o glitch-effect.air
xcrun -sdk macosx metallib glitch-effect.air -o glitch-effect.metallib

xcrun -sdk macosx metal -c shift-glitch.metal -o shift-glitch.air
xcrun -sdk macosx metallib shift-glitch.air -o shift-glitch.metallib

xcrun -sdk macosx metal -c bloom.metal -o bloom.air
xcrun -sdk macosx metallib bloom.air -o bloom.metallib

rm *.air
