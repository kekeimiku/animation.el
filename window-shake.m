#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static void shake_window(CGFloat intensity, CGFloat duration) {
  NSWindow *window = [NSApp mainWindow];

  NSView *contentView = window.contentView;
  if (!contentView.layer) contentView.wantsLayer = YES;

  CAKeyframeAnimation *shakeAnimation = [CAKeyframeAnimation animationWithKeyPath:@"transform"];

  int steps = 8;
  NSMutableArray *values = [NSMutableArray arrayWithCapacity:steps + 2];

  [values addObject:[NSValue valueWithCATransform3D:CATransform3DIdentity]];

  for (int i = 0; i < steps; i++) {
    CGFloat progress = (CGFloat)i / steps;
    CGFloat damping = 1.0 - progress;

    CGFloat offsetX = (arc4random_uniform(2) ? 1 : -1) * intensity * damping;
    CGFloat offsetY = (arc4random_uniform(2) ? 1 : -1) * intensity * damping;

    CATransform3D transform = CATransform3DMakeTranslation(offsetX, offsetY, 0);
    [values addObject:[NSValue valueWithCATransform3D:transform]];
  }

  [values addObject:[NSValue valueWithCATransform3D:CATransform3DIdentity]];

  shakeAnimation.values = values;
  shakeAnimation.duration = duration;
  shakeAnimation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionLinear];
  shakeAnimation.removedOnCompletion = YES;

  [contentView.layer addAnimation:shakeAnimation forKey:@"shake"];
}

static emacs_value window_shake_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  int intensity = env->extract_integer(env, args[0]);
  double duration = env->extract_float(env, args[1]);

  dispatch_async(dispatch_get_main_queue(), ^{
    shake_window((CGFloat)intensity, (CGFloat)duration);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  emacs_value function = env->make_function(env, 2, 2, window_shake_emacs, R"(Shake window with given intensity and duration.)", NULL);
  emacs_value symbol = env->intern(env, "window-shake");
  emacs_value args[] = {symbol, function};
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
