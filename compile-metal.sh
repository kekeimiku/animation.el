#!/bin/bash

set -euo pipefail

xcrun -sdk macosx metal -c glitch_effect.metal -o glitch_effect.air
xcrun -sdk macosx metallib glitch_effect.air -o glitch_effect.metallib

xcrun -sdk macosx metal -c shift-glitch.metal -o shift-glitch.air
xcrun -sdk macosx metallib shift-glitch.air -o shift-glitch.metallib

xcrun -sdk macosx metal -c bloom.metal -o bloom.air
xcrun -sdk macosx metallib bloom.air -o bloom.metallib

xcrun -sdk macosx metal -c glow-rgbsplit-twitchy.metal -o glow-rgbsplit-twitchy.air
xcrun -sdk macosx metallib glow-rgbsplit-twitchy.air -o glow-rgbsplit-twitchy.metallib
