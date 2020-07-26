#include <emacs-module.h>

#include "tagger.h"

int plugin_is_GPL_compatible;

/* mecabel_impl functions. */

emacs_value mecabel_impl_test (
  emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) noexcept {
  return env->make_integer(env, 123);
}

/* Utility functions. */

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

static void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall(env, Qprovide, 1, args);
}

/* Module init function. */

int emacs_module_init (struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);

  if (ert->size < sizeof(*ert))
    return 1;

  if (env->size < sizeof(*env))
    return 2;

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function(env, lsym, \
                env->make_function (env, amin, amax, csym, doc, data))

  DEFUN("mecabel-impl-test", mecabel_impl_test, 0, 0, NULL, NULL);

  provide(env, "mecabel-impl");

  return 0;
}
