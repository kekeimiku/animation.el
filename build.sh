#!/bin/bash

set -euo pipefail

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
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
    -Wl,-x \
    buffer-transition.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework QuartzCore \
    -o buffer-transition-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    ripple-click.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework QuartzCore \
    -o ripple-click-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    window-shake.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework QuartzCore \
    -o window-shake-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    draw-lightning.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework QuartzCore \
    -o bracket-lightning-core.dylib

clang -shared \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    fishhook.c text-glow.c \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework CoreGraphics \
    -o text-glow-core.dylib

clang -shared \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    fishhook.c text-rainbow.c \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework CoreGraphics \
    -o text-rainbow-core.dylib

clang -shared \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -Wl,-x \
    metal-loader.m \
    -I/Applications/Emacs.app/Contents/Resources/include \
    -framework AppKit \
    -framework Metal \
    -framework MetalKit \
    -framework QuartzCore \
    -o metal-loader-core.dylib
