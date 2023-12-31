#include <alloca.h>
#include <stdlib.h>
#include <stdio.h>

#define RINHA_GC_ROOT_LIST_LENGTH 1024
#define RINHA_GC_ARENA_SIZE 1024 * 1024
#define RINHA_GC_MAXIMUM_ALLOCATIONS_BEFORE_GC 100000

enum rinha_gc_tag_t {
  INTEGER = 0,
  STRING = 1,
  TUPLE = 2,
  CLOSURE = 3
};

enum rinha_gc_object_state_t {
  WHITE, // Recently allocated value
  RED,   // Marked as used
  GREEN, // Marked as collectable value ???
  BLUE,  // Root value
};

struct rinha_object_t {
  enum rinha_gc_object_state_t state; 
  enum rinha_gc_tag_t tag;
  int16_t words;
  struct rinha_object_t *next;
};

#define OBJECT_ADDR(x) (void*) x - sizeof(struct rinha_object_t)

struct rinha_gc_root_list_t {
  size_t size;
  struct rinha_object_t* values[RINHA_GC_ROOT_LIST_LENGTH];
};

struct rinha_memory_state_t {
  void* heap_start;
  void* heap_end;
  void* heap_current;
  struct rinha_gc_root_list_t roots;
};

static struct rinha_memory_state_t rinha_memory_state = {
  .heap_start = NULL,
  .heap_end = NULL,
  .heap_current = NULL,
  .roots = { .size = 0, .values = {} },
};

static struct rinha_object_t *object_list_head = NULL;
static size_t rinha_allocated_objects = 0;

struct rinha_string_t {
  size_t length;
  char contents[];
};

struct rinha_tuple_t {
  void *first;
  void *second;
};

struct rinha_closure_t {
  void* function;
  void* arguments[];
};

static struct rinha_string_t CLOSURE_STR = {
  .length = 10,
  .contents = "<#closure>",
};

// TODO: Replace malloc/free with increment/compact (linear allocator + mark and compact gc)

void rinha_init_memory() {
  // rinha_memory_state.heap_start = rinha_memory_state.heap_current = malloc(RINHA_GC_ARENA_SIZE);
  // rinha_memory_state.heap_end = rinha_memory_state.heap_start + RINHA_GC_ARENA_SIZE;
}

// Now every kid with a Commodore 64 can hack into NASA
static int rinha_is_integer(int64_t value) {
  // Since we only used 32 bit integers, we can assume this is a pointer.
  // I'm not sure how much one can assume virtual addresses are on the
  // top half of the 64 bit range, but it'll do for now.
  // My initial idea was to use the same technique OCaml uses, which marks
  // the first bit as one and has to do shifts all the time in order to
  // make it work. But since we don't have the same limitions and needs,
  // like supporting some 32 bit architectures we can leverage this fact
  // to avoid adding a few extra instructions for 32 bit arithmetic.
  return value <= 0xffffffff;
}

void rinha_gc_add_root(void* ptr) {
  if (rinha_is_integer((int64_t) ptr)) {
    return;
  }

  struct rinha_object_t* object = ptr - sizeof(struct rinha_object_t);
  object->state = BLUE;
  
  rinha_memory_state.roots.values[rinha_memory_state.roots.size] = object;
  rinha_memory_state.roots.size++;
}

void* rinha_memcpy(char* dst, char* src, size_t n) {
    for (; n > 0; n--) { *dst++ = *src++; }
    return dst;
}

struct queue_node_t {
  void* value;
  struct queue_node_t *next;
};

struct queue_t {
  size_t length;
  struct queue_node_t *first;
  struct queue_node_t *last;
};

// TODO: Remove malloc here and use preallocated space for this.
// Idea:
// Since every value can add two more items in average to the queue
// RINHA_GC_ROOT_LIST_LENGTH ^ 2 slots for queue items should do.

void queue_add(struct queue_t *queue, void* element) {
  struct queue_node_t *node = malloc(sizeof(struct queue_node_t));
  node->value = element;

  if (queue->last == NULL) {
    queue->length = 1;
    queue->last = queue->first = node;
    node->next = NULL;
  } else {
    queue->length++;
    queue->last->next = node;
    queue->last = node;
  }
}

int queue_is_empty(struct queue_t *queue) {
  return queue->length == 0;
}

void* queue_pop(struct queue_t *queue) {
  struct queue_node_t *top = queue->first;

  if (top == NULL) {
    return (void *) 0xd3adbeef; // It'll make it easier to find if it causes a segfault.
  }

  if (top->next == NULL) {
    void* value = top->value;
    free(top);
    queue->length = 0;
    queue->first = queue->last = NULL;
    return value;
  }

  queue->length--;
  queue->first = top->next;
  void* value = top->value;
  free(top);
  return value;
}
void rinha_gc_mark() {
  struct queue_t work_queue = {
    .length = 0,
    .last = NULL,
    .first = NULL,
  };

#ifdef DEBUG
  printf("Scanning through %d roots\n", rinha_memory_state.roots.size);
#endif

  for (int i = 0; i < rinha_memory_state.roots.size; i++) {
    queue_add(&work_queue, rinha_memory_state.roots.values[i]);
  }

  for (struct rinha_object_t *obj = object_list_head; obj != NULL; obj = obj->next) {
    // printf("%x\n", obj);
    // printf("%x %d %x\n", obj, obj->tag, obj->next);

    /* Let objets live for at least one collecting cycle before they're collected.
       This is definitely naive, but should work for some cases. */
    if (obj->state == WHITE) {
      obj->state = RED;
      continue;
    }
    if (obj->state != BLUE) {
      obj->state = GREEN;
    }
  }

  int scanned_objects = 0;

  while (!queue_is_empty(&work_queue)) {
    // printf("Queue size %d - last %llx\n", work_queue.length, work_queue.last);
    struct rinha_object_t* object = queue_pop(&work_queue);
    scanned_objects++;

    if (object->state != BLUE) {
      object->state = RED;
    }

    switch (object->tag) {
      case TUPLE: {
          struct rinha_tuple_t *tuple = (void*) &object[1];
          // printf("%llx %llx\n", tuple->first, tuple->second);

          if (!rinha_is_integer((int64_t) tuple->first)) {
            queue_add(&work_queue, OBJECT_ADDR(tuple->first));
          }
          if (!rinha_is_integer((int64_t) tuple->second)) {
            queue_add(&work_queue, OBJECT_ADDR(tuple->second));
          }
        }
        break;

      case CLOSURE: {
          struct rinha_closure_t *closure = (void*) &object[1];
          // TODO: Should function pointer be scanned?
          // I can only observe static pointers being there.
          for (int i = 0; i < object->words - 1; i++) {
            if (!rinha_is_integer((int64_t) closure->arguments[i])) {
              queue_add(&work_queue, OBJECT_ADDR(closure->arguments[i]));
            }
          }
        }
        break;

      case STRING:
        // TODO: How to deal with statically allocated strings?
        // Depending on the control over address space, if it could be
        // allocated within the 32 bit range it would bypass collection.

      default:
        break;
    }
  }

#ifdef DEBUG
  printf("Scanned through %d objects\n", scanned_objects);
#endif
}

void rinha_gc_sweep() {
  struct rinha_object_t* object = object_list_head;
  struct rinha_object_t** previous = &object_list_head;

  int freed_objects = 0;
  rinha_allocated_objects = 0;

  while (object != NULL) {
    // printf("%llx %llx %d %d\n", object, object->next, object->tag, object->state);
    void* next = object->next;

    if (object->state == GREEN) {
      *previous = next;
      free(object);
      freed_objects++;
    } else {
      //rinha_allocated_objects++;
      previous = &object->next;
    }

    object = next;
  }

#ifdef DEBUG
  printf("%d freed objects\n", freed_objects);
#endif
}

void rinha_run_gc() {
#ifdef DEBUG
  printf("RUNNING GC\n");
#endif
  rinha_gc_mark();
  rinha_gc_sweep();
}

void rinha_require_allocations(int allocations) {
  if (rinha_allocated_objects + allocations > RINHA_GC_MAXIMUM_ALLOCATIONS_BEFORE_GC) {
    rinha_run_gc();
  }
}

void* rinha_alloc(int words, enum rinha_gc_tag_t tag) {
  // size_t size = words * 8 + sizeof(struct rinha_object_t);

  // if (rinha_memory_state.heap_current + size > rinha_memory_state.heap_end) {
  //   rinha_run_gc();
  // }

  // struct rinha_object_t* object = rinha_memory_state.heap_current;
  // rinha_memory_state.heap_current += size;

  // Only whe we build gc
  struct rinha_object_t* object = calloc(words * 8 + sizeof(struct rinha_object_t), 1);

  if (object == NULL) {
    // Probably a memory leak somewhere
    exit(157);
  }

  if (rinha_allocated_objects > RINHA_GC_MAXIMUM_ALLOCATIONS_BEFORE_GC) {
    rinha_run_gc();
    return rinha_alloc(words, tag);
  }

  rinha_allocated_objects++;
  object->state = WHITE;
  object->tag = tag;
  object->words = words;
  object->next = object_list_head;
  object_list_head = object;

  return &object[1];
}

struct rinha_string_t* rinha_int_to_string(int value) {
  /* Maximum amount of characters needed for representing
   * a 32 bit integer as a string in decimal format.
   * ceil(log10 (2^32 - 1)) = 10
   * + 1 for minus sign
   * + 1 for null byte
   * + 4 for word alignment
   */
  struct rinha_string_t* str = rinha_alloc(3, STRING);

  int is_negative = 0;
  if (value < 0) {
    value = -value;
    is_negative = 1;
  }

  str->length = 0;
  char* p = str->contents + 16;
  *--p = 0;
  do {
    *--p = '0' + value % 10;
    value /= 10;
    str->length++;
  } while (value);

  if (is_negative) {
    *--p = '-';
    str->length++;
  }

  /* Move string to previously allocated position, this should keep it aligned. */
  rinha_memcpy(str->contents, p, str->length);
  return str;
}

int32_t rinha_unbox_integer(void* ptr) {
  if (rinha_is_integer((int64_t) ptr)) {
    return (int32_t) ptr;
  }

  return (int32_t) *((int64_t *) ptr);
}

/*
 * TODO: Use separate field on rinha_object_t to tell which
 * string conversion implementation to use instead, then this
 * should look up for it and call with the object pointer.
 * Probably could use it for other stuff as well, if it needs
 * more fields it should probably be rethinked in order to
 * not affect locality.
 */
struct rinha_string_t* rinha_dyn_to_string(void* ptr) {
  struct rinha_object_t* object = ptr - sizeof(struct rinha_object_t);

  if (rinha_is_integer((int64_t) ptr)) {
    return rinha_int_to_string((int32_t) ptr);
  }

  switch (object->tag) {
    case INTEGER:
      return rinha_int_to_string((int32_t) *((int64_t *) ptr));
    case STRING:
      return ptr;
    case TUPLE: {
      char* str;
      struct rinha_tuple_t* tuple = ptr;
      struct rinha_string_t* first = rinha_dyn_to_string(tuple->first);
      struct rinha_string_t* second = rinha_dyn_to_string(tuple->second);

      size_t string_length = first->length + second->length + 4 + sizeof(size_t);
      struct rinha_string_t* result = rinha_alloc(
        string_length / 8 + 1,
        STRING
      );
      result->length = string_length;
      str = result->contents;
      str[0] = '(';
      str[first->length + 1] = ',';
      str[first->length + 2] = ' ';
      str[string_length - 1] = ')';
      rinha_memcpy(&str[1], first->contents, first->length);
      rinha_memcpy(&str[first->length + 3], second->contents, second->length);

      return result;
    }
    case CLOSURE: {
      struct rinha_string_t* closure_str = rinha_alloc(sizeof(CLOSURE_STR) / 8 + 1, STRING);
      rinha_memcpy(closure_str, &CLOSURE_STR, sizeof(CLOSURE_STR));
      return closure_str;
    }
  }
}

struct rinha_string_t* rinha_strcat(
  struct rinha_string_t* s1,
  struct rinha_string_t* s2
) {
  struct rinha_string_t* result = rinha_alloc(
    (sizeof(size_t) + s1->length + s2->length) / 8 + 1,
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
  __asm__ __volatile__ ("syscall" : "=a"(ret) : "a"(1));
}

void rinha_print(struct rinha_string_t* str) __attribute__((optnone)) {
  // TODO: Add line break?
  // TODO: When using -O3, fields arent properly accessed.
  rinha_write(1, str->contents, str->length);
  // TODO: buffering everything before context switch is a lot faster.
  rinha_write(1, "\n", 1);
}
