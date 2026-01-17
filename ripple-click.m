#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static void animation(CGFloat x, CGFloat y, NSColor *color) {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  CGFloat centerX = x;
  CGFloat centerY = view.bounds.size.height - y;

  CAShapeLayer *rippleLayer = [CAShapeLayer layer];

  CGFloat initialRadius = 10.0;
  CGFloat finalRadius = 50.0;

  CGRect initialRect = CGRectMake(centerX - initialRadius, centerY - initialRadius, initialRadius * 2, initialRadius * 2);
  NSBezierPath *initialPath = [NSBezierPath bezierPathWithOvalInRect:initialRect];

  CGRect finalRect = CGRectMake(centerX - finalRadius, centerY - finalRadius, finalRadius * 2, finalRadius * 2);
  NSBezierPath *finalPath = [NSBezierPath bezierPathWithOvalInRect:finalRect];

  rippleLayer.path = initialPath.CGPath;
  rippleLayer.fillColor = [NSColor clearColor].CGColor;
  rippleLayer.strokeColor = color.CGColor;
  rippleLayer.lineWidth = 3.0;
  rippleLayer.opacity = 1.0;

  [view.layer addSublayer:rippleLayer];

  CAAnimationGroup *animationGroup = [CAAnimationGroup animation];
  animationGroup.duration = 0.6;
  animationGroup.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseOut];
  animationGroup.removedOnCompletion = NO;
  animationGroup.fillMode = kCAFillModeForwards;

  CABasicAnimation *scaleAnimation = [CABasicAnimation animationWithKeyPath:@"path"];
  scaleAnimation.fromValue = (__bridge id)initialPath.CGPath;
  scaleAnimation.toValue = (__bridge id)finalPath.CGPath;

  CABasicAnimation *fadeAnimation = [CABasicAnimation animationWithKeyPath:@"opacity"];
  fadeAnimation.fromValue = @1.0;
  fadeAnimation.toValue = @0.0;

  CABasicAnimation *lineWidthAnimation = [CABasicAnimation animationWithKeyPath:@"lineWidth"];
  lineWidthAnimation.fromValue = @3.0;
  lineWidthAnimation.toValue = @1.5;

  animationGroup.animations = @[ scaleAnimation, fadeAnimation, lineWidthAnimation ];

  [rippleLayer addAnimation:animationGroup forKey:@"ripple"];

  dispatch_after(
      dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.6 * NSEC_PER_SEC)),
      dispatch_get_main_queue(), ^{
        [rippleLayer removeFromSuperlayer];
      });
}

static emacs_value ripple_click_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  int x = env->extract_integer(env, args[0]);
  int y = env->extract_integer(env, args[1]);

  double red = env->extract_float(env, args[2]);
  double green = env->extract_float(env, args[3]);
  double blue = env->extract_float(env, args[4]);

  NSColor *color = [NSColor colorWithRed:red green:green blue:blue alpha:1];

  dispatch_async(dispatch_get_main_queue(), ^{
    animation((CGFloat)x, (CGFloat)y, color);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value function = env->make_function(env, 5, 5, ripple_click_emacs, R"(Show ripple effect at mouse click position.)", NULL);
  emacs_value symbol = env->intern(env, "ripple-click");
  emacs_value args[] = { symbol, function };
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
