#include "fishhook.h"
#include <CoreGraphics/CoreGraphics.h>
#include <emacs-module.h>
#include <math.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static bool glow_enabled = false;
static CGColorRef last_fill_color = NULL;
static void (*original_CGContextShowGlyphsAtPositions)(CGContextRef, const CGGlyph *, const CGPoint *, size_t) = NULL;
static void (*original_CGContextSetFillColorWithColor)(CGContextRef, CGColorRef) = NULL;

static CGColorRef create_rainbow_color(float x_position) {
  float hue = fmodf(x_position / 200.0f, 1.0f);
  float saturation = 1.0f;
  float brightness = 1.0f;

  float r, g, b;
  int i = (int)(hue * 6.0f);
  float f = hue * 6.0f - i;
  float p = brightness * (1.0f - saturation);
  float q = brightness * (1.0f - f * saturation);
  float t = brightness * (1.0f - (1.0f - f) * saturation);

  // clang-format off
  switch (i % 6) {
    case 0: r = brightness; g = t; b = p; break;
    case 1: r = q; g = brightness; b = p; break;
    case 2: r = p; g = brightness; b = t; break;
    case 3: r = p; g = q; b = brightness; break;
    case 4: r = t; g = p; b = brightness; break;
    case 5: r = brightness; g = p; b = q; break;
    default: r = g = b = 0;
  }
  // clang-format on

  return CGColorCreateGenericRGB(r, g, b, 1.0);
}

static void hooked_CGContextSetFillColorWithColor(CGContextRef c, CGColorRef color) {
  if (last_fill_color) CGColorRelease(last_fill_color);

  last_fill_color = color ? CGColorRetain(color) : NULL;

  if (original_CGContextSetFillColorWithColor) original_CGContextSetFillColorWithColor(c, color);
}

static void hooked_CGContextShowGlyphsAtPositions(CGContextRef c, const CGGlyph *glyphs, const CGPoint *positions, size_t count) {
  if (glow_enabled && c) {
    CGContextSaveGState(c);

    for (size_t i = 0; i < count; i++) {
      CGColorRef rainbow_color = create_rainbow_color(positions[i].x);
      CGContextSetFillColorWithColor(c, rainbow_color);

      if (original_CGContextShowGlyphsAtPositions) original_CGContextShowGlyphsAtPositions(c, &glyphs[i], &positions[i], 1);

      CGColorRelease(rainbow_color);
    }

    CGContextRestoreGState(c);
  } else {
    if (original_CGContextShowGlyphsAtPositions) original_CGContextShowGlyphsAtPositions(c, glyphs, positions, count);
  }
}

static void install_hooks() {
  if (original_CGContextShowGlyphsAtPositions) return;

  struct rebinding rebindings[2];

  rebindings[0].name = "CGContextShowGlyphsAtPositions";
  rebindings[0].replacement = hooked_CGContextShowGlyphsAtPositions;
  rebindings[0].replaced = (void **)&original_CGContextShowGlyphsAtPositions;

  rebindings[1].name = "CGContextSetFillColorWithColor";
  rebindings[1].replacement = hooked_CGContextSetFillColorWithColor;
  rebindings[1].replaced = (void **)&original_CGContextSetFillColorWithColor;

  rebind_symbols(rebindings, 2);
}

static void enable_text_glow() {
  if (!original_CGContextShowGlyphsAtPositions) install_hooks();

  if (glow_enabled) return;

  glow_enabled = true;
}

static void disable_text_glow() {
  if (!glow_enabled) return;

  glow_enabled = false;
}

static emacs_value text_glow_render_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  enable_text_glow();
  return env->intern(env, "nil");
}

static emacs_value text_glow_cleanup_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  disable_text_glow();
  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  emacs_value defalias = env->intern(env, "defalias");

  emacs_value render_args[] = {
      env->intern(env, "text-rainbow-render"),
      env->make_function(env, 0, 0, text_glow_render_emacs, R"(Render text rainbow effect.)", NULL)};
  env->funcall(env, defalias, 2, render_args);

  emacs_value cleanup_args[] = {
      env->intern(env, "text-rainbow-cleanup"),
      env->make_function(env, 0, 0, text_glow_cleanup_emacs, R"(Stop text rainbow effect.)", NULL)};
  env->funcall(env, defalias, 2, cleanup_args);

  return 0;
}
