#pragma once

#include <string.h>

#define UNIT_NAME_PREFIX "play-haskell-sandbox-u"


static bool unit_name_valid(const char *unit_name) {
  const size_t prefix_len = strlen(UNIT_NAME_PREFIX);

  const size_t len = strlen(unit_name);
  if (len < prefix_len || memcmp(unit_name, UNIT_NAME_PREFIX, prefix_len) != 0) return false;
  for (size_t i = prefix_len; i < len; i++) {
    const char c = unit_name[i];
    // Avoid isalnum() and friends because those are locale-dependent
    if (!('a' <= c && c <= 'z') && !('A' <= c && c <= 'Z') &&
          !('0' <= c && c <= '9') && c != '-')
      return false;
  }
  return true;
}
