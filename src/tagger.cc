#include <exception>
#include <mecab.h>
#include <string>

#include <iostream>

#include "tagger.h"

using namespace std;
using namespace mecabel;

const MeCab::Node* tagger::parse_to_node(const string& sentence) {
  _sentence = new string(sentence);
  return _native_tagger->parseToNode(_sentence->c_str());
}

const string tagger::get_node_value(
  const MeCab::Node* node, const string& key) {
  if (key == "surface") {
    return node->length ? string(node->surface, node->length) : "";
  } else if (key == "feature") {
    return string(node->feature);
  }
  throw std::invalid_argument("Unknown key.");
}
