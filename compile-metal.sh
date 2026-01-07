#!/bin/bash

set -euo pipefail

xcrun -sdk macosx metal -c glitch_effect.metal -o glitch_effect.air
xcrun -sdk macosx metallib glitch_effect.air -o glitch_effect.metallib

xcrun -sdk macosx metal -c shift-glitch.metal -o shift-glitch.air
xcrun -sdk macosx metallib shift-glitch.air -o shift-glitch.metallib

xcrun -sdk macosx metal -c glow-text.metal -o glow-text.air
xcrun -sdk macosx metallib glow-text.air -o glow-text.metallib
