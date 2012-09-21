#ifndef __CHANDLE_H__
#define __CHANDLE_H__
#include <stdint.h>

typedef struct {
  int *lengths;
  intptr_t *values;
  void *record;
  void (*snetOut)(void *, int, intptr_t *);
} Handle;

void SNetOut(Handle *hnd, int variant, ...);
#endif
