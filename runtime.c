#include <stdio.h>

void int_to_string(int64_t value, char* buffer) {
  int i = 0;
  while (value != 0) {
    char digit = value % 10 + '0';
    buffer[i] = digit;
    value = value / 10;
    i++;
  }
  buffer[i] = '\0';
}

void print(int64_t value) {
  char buffer[1024];
  int_to_string(value, buffer);
  puts(buffer);
}
