#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

typedef enum {
  TRANSITION_FADE = 0,
  TRANSITION_PUSH_LEFT,
  TRANSITION_PUSH_RIGHT
} TransitionType;

static void buffer_transition(TransitionType type, CGFloat duration) {
  NSWindow *window = [NSApp mainWindow];
  NSView *view = window.contentView;

  if (!view.layer) view.wantsLayer = YES;

  CATransition *transition = [CATransition animation];
  transition.duration = duration;
  transition.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];

  switch (type) {
  case TRANSITION_FADE:
    transition.type = kCATransitionFade;
    break;

  case TRANSITION_PUSH_LEFT:
    transition.type = kCATransitionPush;
    transition.subtype = kCATransitionFromLeft;
    break;

  case TRANSITION_PUSH_RIGHT:
    transition.type = kCATransitionPush;
    transition.subtype = kCATransitionFromRight;
    break;
  }

  [view.layer addAnimation:transition forKey:@"buffer-transition"];
}

static emacs_value buffer_transition_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  int type_int = env->extract_integer(env, args[0]);
  double duration = env->extract_float(env, args[1]);

  TransitionType type = (TransitionType)type_int;

  dispatch_async(dispatch_get_main_queue(), ^{
    buffer_transition(type, (CGFloat)duration);
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value function = env->make_function(env, 2, 2, buffer_transition_emacs, R"(Apply transition animation to current buffer view.)", NULL);
  emacs_value symbol = env->intern(env, "buffer-transition");
  emacs_value args[] = { symbol, function };
  env->funcall(env, env->intern(env, "defalias"), 2, args);

  return 0;
}
