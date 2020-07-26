#include "gtest/gtest.h"

#include "tagger.h"

using namespace mecabel;

TEST(mecabelTest, create_taggerTest) {
  tagger* t = tagger::create_tagger();
  EXPECT_TRUE(t != nullptr);
}
