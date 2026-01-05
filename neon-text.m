#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static CALayer *neonLayer = nil;
static NSTimer *glowTimer = nil;
static CGFloat glowPhase = 0.0;

static void animate_glow(NSTimer *timer) {
  glowPhase += 0.05;
  CGFloat intensity = 0.3 + 0.2 * sin(glowPhase);
  neonLayer.shadowOpacity = intensity;
  neonLayer.shadowRadius = 12.0 + 6.0 * sin(glowPhase * 0.7);
  CGFloat angle = glowPhase * 0.3;
  ((CAGradientLayer *)neonLayer).startPoint = CGPointMake(0.5 + 0.5 * cos(angle), 0.5 + 0.5 * sin(angle));
  ((CAGradientLayer *)neonLayer).endPoint = CGPointMake(0.5 - 0.5 * cos(angle), 0.5 - 0.5 * sin(angle));
}

static void create_neon_overlay() {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  if (neonLayer) {
    [neonLayer removeFromSuperlayer];
    neonLayer = nil;
  }

  CALayer *glowLayer = [CALayer layer];
  glowLayer.frame = view.bounds;
  glowLayer.backgroundColor = [[NSColor clearColor] CGColor];

  glowLayer.shadowColor = [[NSColor colorWithRed:0.5 green:0.5 blue:1.0 alpha:1.0] CGColor];
  glowLayer.shadowRadius = 10.0;
  glowLayer.shadowOpacity = 0.15;
  glowLayer.shadowOffset = CGSizeMake(0, 0);

  neonLayer = glowLayer;
  [view.layer addSublayer:neonLayer];

  if (!glowTimer) {
    glowTimer = [NSTimer scheduledTimerWithTimeInterval:0.05 repeats:YES block:^(NSTimer *timer) { animate_glow(timer); }];
  }
}

static void remove_neon_overlay() {
  if (glowTimer) {
    [glowTimer invalidate];
    glowTimer = nil;
  }

  if (neonLayer) {
    [neonLayer removeFromSuperlayer];
    neonLayer = nil;
  }

  glowPhase = 0.0;
}

static emacs_value neon_text_render_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  dispatch_async(dispatch_get_main_queue(), ^{
    create_neon_overlay();
  });

  return env->intern(env, "nil");
}

static emacs_value neon_text_stop_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  dispatch_async(dispatch_get_main_queue(), ^{
    remove_neon_overlay();
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  emacs_value render_func = env->make_function(env, 0, 0, neon_text_render_emacs, R"(Render neon glow effect for text.)", NULL);
  emacs_value render_symbol = env->intern(env, "neon-text-render");
  emacs_value render_args[] = {render_symbol, render_func};
  env->funcall(env, env->intern(env, "defalias"), 2, render_args);

  emacs_value stop_func = env->make_function(env, 0, 0, neon_text_stop_emacs, R"(Stop neon glow effect.)", NULL);
  emacs_value stop_symbol = env->intern(env, "neon-text-stop");
  emacs_value stop_args[] = {stop_symbol, stop_func};
  env->funcall(env, env->intern(env, "defalias"), 2, stop_args);

  return 0;
}
