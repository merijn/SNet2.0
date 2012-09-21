#include <stdarg.h>

#include "CHandle.h"

void SNetOut(Handle *hnd, int variant, ...)
{
  va_list args;
  va_start(args, variant);

  for (int i = 0; i < hnd->lengths[variant]; i++) {
    hnd->values[i] = va_arg(args, intptr_t);
  }

  hnd->snetOut(hnd->record, variant, hnd->values);
}
