#include "fishhook.h"
#include <CoreGraphics/CoreGraphics.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static bool glow_enabled = false;

static void (*original_CGContextShowGlyphsAtPositions)(CGContextRef, const CGGlyph *, const CGPoint *, size_t) = NULL;

static void hooked_CGContextShowGlyphsAtPositions(CGContextRef c, const CGGlyph *glyphs, const CGPoint *positions, size_t count) {
  if (glow_enabled && c) {
    CGContextSaveGState(c);

    CGSize offset = CGSizeMake(0, 0);

    CGColorRef shadowColor = CGColorCreateGenericRGB(0.2, 0.7, 1.0, 0.9);
    CGContextSetShadowWithColor(c, offset, 15.0, shadowColor);
    CGColorRelease(shadowColor);

    if (original_CGContextShowGlyphsAtPositions) {
      original_CGContextShowGlyphsAtPositions(c, glyphs, positions, count);
    }

    CGContextRestoreGState(c);
  } else {
    if (original_CGContextShowGlyphsAtPositions) original_CGContextShowGlyphsAtPositions(c, glyphs, positions, count);
  }
}

static void install_hooks() {
  if (original_CGContextShowGlyphsAtPositions) return;

  struct rebinding rebindings[1];

  rebindings[0].name = "CGContextShowGlyphsAtPositions";
  rebindings[0].replacement = hooked_CGContextShowGlyphsAtPositions;
  rebindings[0].replaced = (void **)&original_CGContextShowGlyphsAtPositions;

  rebind_symbols(rebindings, 1);
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
      env->intern(env, "text-glow-render"),
      env->make_function(env, 0, 0, text_glow_render_emacs, R"(Render text glow effect.)", NULL)};
  env->funcall(env, defalias, 2, render_args);

  emacs_value cleanup_args[] = {
      env->intern(env, "text-glow-cleanup"),
      env->make_function(env, 0, 0, text_glow_cleanup_emacs, R"(Stop text glow effect.)", NULL)};
  env->funcall(env, defalias, 2, cleanup_args);

  return 0;
}
