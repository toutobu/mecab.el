#include <assert.h>
#include <emacs-module.h>

#include "tagger.h"

int plugin_is_GPL_compatible;

using namespace std;
using namespace mecabel;

/* mecabel_impl functions. */

emacs_value mecabel_impl_create_tagger(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void *data) noexcept {
  assert(nargs == 2);
  auto size = env->extract_integer(env, args[1]) + 1;
  char mecab_arg[size];
  env->copy_string_contents(env, args[0], mecab_arg, &size);
  return env->make_user_ptr(env, NULL, tagger::create_tagger(mecab_arg));
}

emacs_value mecabel_impl_parse_to_node(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void *data) noexcept {
  assert(nargs == 3);
  const auto t = reinterpret_cast<tagger*>(env->get_user_ptr(env, args[0]));
  auto size = env->extract_integer(env, args[2]) + 1;
  char sentence[size];
  env->copy_string_contents(env, args[1], sentence, &size);
  return env->make_user_ptr(env, NULL, (void*) t->parse_to_node(sentence));
}

emacs_value mecabel_impl_next_node(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void *data) noexcept {
  assert(nargs == 1);
  emacs_value nil = env->intern(env, "nil");
  const auto n = reinterpret_cast<MeCab::Node*>(env->get_user_ptr(env, args[0]));
  return n->next ? env->make_user_ptr(env, NULL, n->next) : nil;
}

emacs_value mecabel_impl_get_node_value(
  emacs_env* env, ptrdiff_t nargs, emacs_value* args, void *data) noexcept {
  assert(nargs == 3);
  const auto n = reinterpret_cast<MeCab::Node*>(env->get_user_ptr(env, args[0]));
  auto size = env->extract_integer(env, args[2]) + 1;
  char key[size];
  env->copy_string_contents(env, args[1], key, &size);
  const string value = tagger::get_node_value(n, key);
  return env->make_string(env, value.c_str(), value.size());
}

/* Utility functions. */

static void bind_function(emacs_env* env, const char* name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);
  emacs_value args[] = { Qsym, Sfun };
  env->funcall(env, Qfset, 2, args);
}

static void provide(emacs_env* env, const char* feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = { Qfeat };
  env->funcall(env, Qprovide, 1, args);
}

/* Module init function. */

int emacs_module_init (struct emacs_runtime* ert) {
  emacs_env* env = ert->get_environment(ert);

  if (ert->size < sizeof(*ert))
    return 1;

  if (env->size < sizeof(*env))
    return 2;

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function(env, lsym, \
                env->make_function (env, amin, amax, csym, doc, data))

  DEFUN("mecabel-impl-create-tagger",
        mecabel_impl_create_tagger,
        2, 2, NULL, NULL);
  DEFUN("mecabel-impl-parse-to-node",
        mecabel_impl_parse_to_node,
        3, 3, NULL, NULL);
  DEFUN("mecabel-impl-next-node",
        mecabel_impl_next_node,
        1, 1, NULL, NULL);
  DEFUN("mecabel-impl-get-node-value",
        mecabel_impl_get_node_value,
        3, 3, NULL, NULL);

  provide(env, "mecabel-impl");

  return 0;
}
