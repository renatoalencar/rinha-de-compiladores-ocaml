#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* int_to_string(int value) {
  /* Maximum amount of characters needed for representing
   * a 32 bit integer as a string in decimal format.
   * ceil(log10 (2^32 - 1)) = 10
   * + 1 for minus sign
   * + 1 for null byte
   * + 4 for word alignment
   */
  char* buffer = malloc(16);
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
