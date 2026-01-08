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
    -target arm64-apple-macos14.1 \
    -fobjc-arc \
    -O3 \
    -flto \
    -fvisibility=hidden \
    -I/Applications/Emacs.app/Contents/Resources/include \
    bracket-content-flash.m \
    -framework AppKit \
    -framework QuartzCore \
    -o bracket-content-flash-core.dylib