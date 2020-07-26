#ifndef __MECABEL_TAGGER_H_
#define __MECABEL_TAGGER_H_

#include <boost/algorithm/string.hpp>
#include <mecab.h>

namespace mecabel {

struct tagger {
public:
  static tagger* create_tagger() {
    return new tagger;
  }
};

} // mecabel

#endif // __MECABEL_TAGGER_H_
