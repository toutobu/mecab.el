#include <mecab.h>
#include <string>

#include <iostream>

#include "gtest/gtest.h"

#include "tagger.h"

using namespace std;
using namespace mecabel;

TEST(mecabel_test, create_tagger) {
  tagger* t = tagger::create_tagger("");
  EXPECT_TRUE(t != nullptr);
}

TEST(mecabel_test, parse_to_node) {
  tagger* t = tagger::create_tagger("");
  const string sentence = "こう云う風にして歩くのだ";

  const MeCab::Node* n = t->parse_to_node(sentence);
  ASSERT_EQ(string(n->surface, n->length), "");
  ASSERT_EQ(string(n->feature), "BOS/EOS,*,*,*,*,*,*,*,*");

  n = n->next;
  ASSERT_EQ(string(n->surface, n->length), "こう");
  ASSERT_EQ(string(n->feature), "副詞,助詞類接続,*,*,*,*,こう,コウ,コー");

  n = n->next;
  ASSERT_EQ(string(n->surface, n->length), "云う");
  ASSERT_EQ(string(n->feature), "動詞,自立,*,*,五段・ワ行促音便,基本形,云う,イウ,イウ");
}

TEST(mecabel_test, get_node_value) {
  tagger* t = tagger::create_tagger("");
  const string sentence = "こう云う風にして歩くのだ";

  const MeCab::Node* n = t->parse_to_node(sentence)->next;

  const string surface = tagger::get_node_value(n, "surface");
  ASSERT_EQ(surface, "こう");
  const string feature = tagger::get_node_value(n, "feature");
  ASSERT_EQ(feature, "副詞,助詞類接続,*,*,*,*,こう,コウ,コー");
}
