#include <stdio.h>
#include "CHandle.h"

void id(Handle *hnd, int i) {
  intptr_t x = i;
  SNetOut(hnd, 0, x);
}
