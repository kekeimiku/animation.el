#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static void animation(CGFloat x, CGFloat y, CGFloat width, CGFloat height, NSColor *color) {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  CAEmitterLayer *emitter = [CAEmitterLayer layer];

  CGFloat centerX = x + width / 2;
  CGFloat centerY = view.bounds.size.height - y - height / 2;
  emitter.emitterPosition = CGPointMake(centerX, centerY);
  emitter.emitterSize = CGSizeMake(width, height);
  emitter.emitterShape = kCAEmitterLayerRectangle;
  emitter.renderMode = kCAEmitterLayerAdditive;

  CAEmitterCell *particle = [CAEmitterCell emitterCell];

  static CGImageRef particleImage = NULL;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    NSSize size = NSMakeSize(10, 10);
    NSImage *image = [[NSImage alloc] initWithSize:size];
    [image lockFocus];
    NSBezierPath *circle = [NSBezierPath bezierPathWithOvalInRect:NSMakeRect(0, 0, 10, 10)];
    [[NSColor whiteColor] setFill];
    [circle fill];
    [image unlockFocus];

    NSRect imageRect = NSMakeRect(0, 0, size.width, size.height);
    particleImage = [image CGImageForProposedRect:&imageRect context:nil hints:nil];
    CGImageRetain(particleImage);
  });

  particle.contents = (__bridge id)particleImage;
  particle.color = [NSColor colorWithRed:0.7 green:0.7 blue:0.7 alpha:1.0].CGColor;
  particle.redRange = 1.0;
  particle.greenRange = 1.0;
  particle.blueRange = 1.0;

  particle.lifetime = 0.8;
  particle.lifetimeRange = 0.4;
  particle.birthRate = 50;

  particle.velocity = 100;
  particle.velocityRange = 50;
  particle.emissionRange = M_PI * 2;

  particle.scale = 0.3;
  particle.scaleRange = 0.2;
  particle.scaleSpeed = -0.3;

  particle.alphaSpeed = -1.0;

  particle.spin = 2;
  particle.spinRange = 4;

  emitter.emitterCells = @[ particle ];
  [view.layer addSublayer:emitter];

  dispatch_after(
      dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.1 * NSEC_PER_SEC)),
      dispatch_get_main_queue(), ^{
        emitter.birthRate = 0;
        dispatch_after(
            dispatch_time(DISPATCH_TIME_NOW, (int64_t)(1.0 * NSEC_PER_SEC)),
            dispatch_get_main_queue(), ^{
              [emitter removeFromSuperlayer];
            });
      });
}

static emacs_value particle_cursor_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  int x = env->extract_integer(env, args[0]);
  int y = env->extract_integer(env, args[1]);
  int width = env->extract_integer(env, args[2]);
  int height = env->extract_integer(env, args[3]);

  double red = env->extract_float(env, args[4]);
  double green = env->extract_float(env, args[5]);
  double blue = env->extract_float(env, args[6]);

  NSColor *color = [NSColor colorWithRed:red green:green blue:blue alpha:1];

  dispatch_async(dispatch_get_main_queue(), ^{
    animation((CGFloat)x, (CGFloat)y, (CGFloat)width, (CGFloat)height, color);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value function = env->make_function(env, 7, 7, particle_cursor_emacs, R"(Emit particle effect at cursor position)", NULL);
  emacs_value symbol = env->intern(env, "particle-cursor");
  emacs_value args[] = {symbol, function};
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
