#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static void draw_lightning(CGFloat x1, CGFloat y1, CGFloat x2, CGFloat y2, NSColor *color) {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  CGFloat viewHeight = view.bounds.size.height;
  CGFloat startX = x1;
  CGFloat startY = viewHeight - y1;
  CGFloat endX = x2;
  CGFloat endY = viewHeight - y2;

  NSBezierPath *lightningPath = [NSBezierPath bezierPath];
  [lightningPath moveToPoint:NSMakePoint(startX, startY)];

  CGFloat distance = sqrt(pow(endX - startX, 2) + pow(endY - startY, 2));
  int segments = (int)(distance / 20.0) + 2;

  for (int i = 1; i < segments; i++) {
    CGFloat t = (CGFloat)i / segments;
    CGFloat baseX = startX + (endX - startX) * t;
    CGFloat baseY = startY + (endY - startY) * t;

    CGFloat angle = atan2(endY - startY, endX - startX);
    CGFloat perpAngle = angle + M_PI / 2.0;
    CGFloat maxOffset = 15.0 * (1.0 - fabs(t - 0.5) * 2.0);
    CGFloat offset = ((CGFloat)rand() / RAND_MAX - 0.5) * maxOffset;

    CGFloat x = baseX + cos(perpAngle) * offset;
    CGFloat y = baseY + sin(perpAngle) * offset;

    [lightningPath lineToPoint:NSMakePoint(x, y)];
  }

  [lightningPath lineToPoint:NSMakePoint(endX, endY)];

  CAShapeLayer *lightningLayer = [CAShapeLayer layer];
  lightningLayer.path = lightningPath.CGPath;
  lightningLayer.strokeColor = color.CGColor;
  lightningLayer.fillColor = [NSColor clearColor].CGColor;
  lightningLayer.lineWidth = 2.5;
  lightningLayer.lineCap = kCALineCapRound;
  lightningLayer.lineJoin = kCALineJoinRound;
  lightningLayer.opacity = 1.0;

  lightningLayer.shadowColor = color.CGColor;
  lightningLayer.shadowRadius = 8.0;
  lightningLayer.shadowOpacity = 0.8;
  lightningLayer.shadowOffset = CGSizeMake(0, 0);

  [view.layer addSublayer:lightningLayer];

  CAShapeLayer *glowLayer = [CAShapeLayer layer];
  glowLayer.path = lightningPath.CGPath;
  glowLayer.strokeColor = color.CGColor;
  glowLayer.fillColor = [NSColor clearColor].CGColor;
  glowLayer.lineWidth = 6.0;
  glowLayer.lineCap = kCALineCapRound;
  glowLayer.lineJoin = kCALineJoinRound;
  glowLayer.opacity = 0.4;
  [view.layer addSublayer:glowLayer];

  for (int i = 0; i < 2; i++) {
    CGFloat px = (i == 0) ? startX : endX;
    CGFloat py = (i == 0) ? startY : endY;

    CAShapeLayer *flashLayer = [CAShapeLayer layer];
    CGFloat radius = 8.0;
    NSBezierPath *circlePath = [NSBezierPath bezierPathWithOvalInRect:CGRectMake(px - radius, py - radius, radius * 2, radius * 2)];
    flashLayer.path = circlePath.CGPath;
    flashLayer.fillColor = color.CGColor;
    flashLayer.opacity = 1.0;
    flashLayer.shadowColor = color.CGColor;
    flashLayer.shadowRadius = 12.0;
    flashLayer.shadowOpacity = 1.0;
    [view.layer addSublayer:flashLayer];

    CAKeyframeAnimation *flashAnim = [CAKeyframeAnimation animationWithKeyPath:@"opacity"];
    flashAnim.values = @[ @1.0, @0.3, @1.0, @0.5, @0.0 ];
    flashAnim.duration = 0.3;
    [flashLayer addAnimation:flashAnim forKey:@"flash"];

    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.3 * NSEC_PER_SEC)),
                   dispatch_get_main_queue(), ^{
                     [flashLayer removeFromSuperlayer];
                   });
  }

  CABasicAnimation *strokeAnim = [CABasicAnimation animationWithKeyPath:@"strokeEnd"];
  strokeAnim.fromValue = @0.0;
  strokeAnim.toValue = @1.0;
  strokeAnim.duration = 0.15;
  strokeAnim.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseIn];
  [lightningLayer addAnimation:strokeAnim forKey:@"stroke"];
  [glowLayer addAnimation:strokeAnim forKey:@"stroke"];

  CAKeyframeAnimation *flickerAnim = [CAKeyframeAnimation animationWithKeyPath:@"opacity"];
  flickerAnim.values = @[ @1.0, @0.6, @1.0, @0.4, @0.8, @0.0 ];
  flickerAnim.duration = 0.4;
  flickerAnim.beginTime = CACurrentMediaTime() + 0.15;
  [lightningLayer addAnimation:flickerAnim forKey:@"flicker"];

  CAKeyframeAnimation *glowFlickerAnim = [CAKeyframeAnimation animationWithKeyPath:@"opacity"];
  glowFlickerAnim.values = @[ @0.4, @0.2, @0.4, @0.1, @0.3, @0.0 ];
  glowFlickerAnim.duration = 0.4;
  glowFlickerAnim.beginTime = CACurrentMediaTime() + 0.15;
  [glowLayer addAnimation:glowFlickerAnim forKey:@"flicker"];

  dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(0.6 * NSEC_PER_SEC)),
                 dispatch_get_main_queue(), ^{
                   [lightningLayer removeFromSuperlayer];
                   [glowLayer removeFromSuperlayer];
                 });
}

static emacs_value draw_lightning_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  int x1 = env->extract_integer(env, args[0]);
  int y1 = env->extract_integer(env, args[1]);
  int x2 = env->extract_integer(env, args[2]);
  int y2 = env->extract_integer(env, args[3]);

  double red = env->extract_float(env, args[4]);
  double green = env->extract_float(env, args[5]);
  double blue = env->extract_float(env, args[6]);

  NSColor *color = [NSColor colorWithRed:red green:green blue:blue alpha:1.0];

  dispatch_async(dispatch_get_main_queue(), ^{
    draw_lightning((CGFloat)x1, (CGFloat)y1, (CGFloat)x2, (CGFloat)y2, color);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value function = env->make_function(env, 7, 7, draw_lightning_emacs, R"(Draw lightning between matching brackets.)", NULL);
  emacs_value symbol = env->intern(env, "draw-lightning");
  emacs_value args[] = { symbol, function };
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
