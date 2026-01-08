#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static void flash_rect(CGFloat x, CGFloat y, CGFloat w, CGFloat h, NSColor *color, double duration) {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  CGFloat viewHeight = view.bounds.size.height;
  // Emacs coordinate (0,0) is top-left. Cocoa is bottom-left.
  // Convert Y:
  CGFloat cocoaY = viewHeight - (y + h);

  CALayer *layer = [CALayer layer];
  layer.frame = CGRectMake(x, cocoaY, w, h);
  layer.backgroundColor = color.CGColor;
  layer.opacity = 0.0; // Initially invisible, driven by animation

  // Animation configuration
  CABasicAnimation *anim = [CABasicAnimation animationWithKeyPath:@"opacity"];
  anim.fromValue = @(0.4); // Starting opacity (semi-transparent)
  anim.toValue = @(0.0);   // Fade to transparent
  anim.duration = duration;
  anim.removedOnCompletion = NO;
  anim.fillMode = kCAFillModeForwards;
  anim.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseOut];
  
  [layer addAnimation:anim forKey:@"flashFade"];
  
  [view.layer addSublayer:layer];

  // Cleanup after animation
  dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(duration * NSEC_PER_SEC)),
                 dispatch_get_main_queue(), ^{
                   [layer removeFromSuperlayer];
                 });
}

static emacs_value flash_rect_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  // Arguments: x, y, w, h, r, g, b, duration
  int x = env->extract_integer(env, args[0]);
  int y = env->extract_integer(env, args[1]);
  int w = env->extract_integer(env, args[2]);
  int h = env->extract_integer(env, args[3]);
  
  double r = env->extract_float(env, args[4]);
  double g = env->extract_float(env, args[5]);
  double b = env->extract_float(env, args[6]);
  double duration = env->extract_float(env, args[7]);

  NSColor *color = [NSColor colorWithRed:r green:g blue:b alpha:1.0];

  // Execute UI operations on the main thread
  dispatch_async(dispatch_get_main_queue(), ^{
    flash_rect((CGFloat)x, (CGFloat)y, (CGFloat)w, (CGFloat)h, color, duration);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  
  // Register bracket-content-flash-draw-rect function
  emacs_value func = env->make_function(env, 8, 8, flash_rect_emacs, "Draw a flashing rectangle.", NULL);
  emacs_value symbol = env->intern(env, "bracket-content-flash-draw-rect");
  emacs_value args[] = {symbol, func};
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
