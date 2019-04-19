#ifndef SRC_UTILS_H
#define SRC_UTILS_H
#include <stdbool.h>

#define HashSize 317
int hash(char *str);
char *concat(const char *s1, const char *s2);
char *fmt_String(const char *format, char **s2);
bool isBlankIdentifier(char* identifier);
#endif
