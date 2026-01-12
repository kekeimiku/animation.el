#!/bin/bash

set -euo pipefail

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    particle-cursor.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework QuartzCore \
    -o particle-cursor-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    buffer-transition.m \
    -framework AppKit \
    -framework QuartzCore \
    -o buffer-transition-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    ripple-click.m \
    -framework AppKit \
    -framework QuartzCore \
    -o ripple-click-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    neon-text.m \
    -framework AppKit \
    -framework QuartzCore \
    -o neon-text-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    window-shake.m \
    -framework AppKit \
    -framework QuartzCore \
    -o window-shake-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    draw-lightning.m \
    -framework AppKit \
    -framework QuartzCore \
    -o bracket-lightning-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    glitch-effect.m \
    -framework AppKit \
    -framework Metal \
    -framework MetalKit \
    -framework QuartzCore \
    -o glitch-effect-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    shift-glitch.m \
    -framework AppKit \
    -framework Metal \
    -framework MetalKit \
    -framework QuartzCore \
    -o shift-glitch-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    bloom.m \
    -framework AppKit \
    -framework Metal \
    -framework MetalKit \
    -framework QuartzCore \
    -o bloom-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    glow-rgbsplit-twitchy.m \
    -framework AppKit \
    -framework Metal \
    -framework MetalKit \
    -framework QuartzCore \
    -o glow-rgbsplit-twitchy-core.dylib

clang -shared \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework CoreGraphics \
    fishhook.c text-glow.c \
    -o text-glow-core.dylib
