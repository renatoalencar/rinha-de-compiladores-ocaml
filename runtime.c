#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* int_to_string(int32_t value) {
  char* buffer = malloc(1024);
  sprintf(buffer, "%d", value);
  return buffer;
}

char* rinha_strcat(char* s1, char* s2) {
  char* result = malloc(strlen(s1) + strlen(s2));
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

void print(char* buffer) {
  puts(buffer);
}
