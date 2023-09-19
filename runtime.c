#include <alloca.h>
#include <stdlib.h>
#include <stdio.h>

enum rinha_gc_tag_t {
  STRING = 1,
  TUPLE = 2,
  CLOSURE = 3
};

struct rinha_string_t {
  size_t length;
  char contents[];
};

#define ARENA_SIZE 1024 * 1024

struct rinha_memory_state_t {
  void* heap_start;
  void* heap_end;
  void* heap_current;
};

static struct rinha_memory_state_t rinha_memory_state = {
  .heap_start = NULL,
  .heap_end = NULL,
  .heap_current = NULL,
};

void rinha_init_memory() {
  rinha_memory_state.heap_start = rinha_memory_state.heap_current = malloc(ARENA_SIZE);
  rinha_memory_state.heap_end = rinha_memory_state.heap_start + ARENA_SIZE;
}

void* rinha_memcpy(char* dst, char* src, size_t n) {
    for (; n > 0; n--) { *dst++ = *src++; }
    return dst;
}

void rinha_run_gc() {
  // RUN GC
}

void* rinha_alloc(int words, enum rinha_gc_tag_t tag) {
  size_t size = (words + 1) * 8;
  if (rinha_memory_state.heap_current + size > rinha_memory_state.heap_end) {
    rinha_run_gc();
  }

  void* mem = rinha_memory_state.heap_current;
  rinha_memory_state.heap_current += size;

  // Garbage collector info.
  *((int64_t *) mem) = words << 4 | tag;

  return mem + 8;
}

struct rinha_string_t* rinha_int_to_string(int value) {
  /* Maximum amount of characters needed for representing
   * a 32 bit integer as a string in decimal format.
   * ceil(log10 (2^32 - 1)) = 10
   * + 1 for minus sign
   * + 1 for null byte
   * + 4 for word alignment
   */
  struct rinha_string_t* str = rinha_alloc(sizeof(size_t) + 16, STRING);
  str->length = 0;
  char* p = str->contents + 16;
  *--p = 0;
  do {
    *--p = '0' + value % 10;
    value /= 10;
    str->length++;
  } while (value);
  

  /* Move string to previously allocated position, this should keep it aligned. */
  rinha_memcpy(str->contents, p, str->length);
  return str;
}

struct rinha_string_t* rinha_strcat(
  struct rinha_string_t* s1,
  struct rinha_string_t* s2
) {
  struct rinha_string_t* result = rinha_alloc(
    sizeof(size_t) + s1->length + s2->length,
    STRING
  );
  result->length = s1->length + s2->length;
  
  void* ptr = rinha_memcpy(result->contents, s1->contents, s1->length);
  rinha_memcpy(ptr, s2->contents, s2->length);

  return result;
}

void rinha_write(int fd, char* buffer, size_t count) {
  // TODO: Clang attribute to make sure this is using System V calling convention.
  // Assume parameters are already in %rdi, %rsi and %rdx
  // FIXME: Assumption breaks when using -O3
  long int ret = 0;
  __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(1) );
}

void rinha_print(struct rinha_string_t* str) __attribute__((optnone)) {
  // TODO: Add line break?
  // TODO: When using -O3, fields arent properly accessed.
  rinha_write(1, str->contents, str->length);
}
