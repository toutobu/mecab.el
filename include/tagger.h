#ifndef __MECABEL_TAGGER_H_
#define __MECABEL_TAGGER_H_

#include <mecab.h>
#include <string>

namespace mecabel {

struct tagger {
  public:
    tagger(const char *arg)
      : _native_tagger(MeCab::createTagger(arg)) {}

    tagger(const std::string& arg)
      : _native_tagger(MeCab::createTagger(arg.c_str())) {}

    ~tagger() {
      delete _native_tagger;
      delete _sentence;
    }

    static tagger* create_tagger(const char* arg) {
      return new tagger(arg);
    }

    static tagger* create_tagger(const std::string& arg) {
      return new tagger(arg);
    }

    static const std::string
    get_node_value(const MeCab::Node*, const std::string&);

    const MeCab::Node* parse_to_node(const std::string&);

  private:
    MeCab::Tagger* _native_tagger;
    std::string* _sentence;
};

} // mecabel

#endif // __MECABEL_TAGGER_H_
