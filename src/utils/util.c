#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../ast/tree.h"
#include "util.h"


int hash(char *str)
{
  unsigned int hash = 0;
  while (*str)
    hash = (hash << 1) + *str++;
  return hash % HashSize;
}

char* concat(const char *s1, const char *s2){
  char *result = malloc(strlen(s1) + strlen(s2) + 1);
  strcpy(result, s1);
  strcat(result, s2);
  return result;
}

char * fmt_String(const char *format, char **args){
  int i   = 0;
  int arg = 0;
  char *result = "";
  while(format[i] != '\0'){
    if(format[i] == '%'){
      i++;
      if(format[i] == '\0') return result;

      switch(format[i]){
      case 's':
        {
          if(!args[arg]){
            fprintf(stderr, "fmtString cannot be called with this argument");
            exit(1);
          }
          
          result = concat(result, args[arg]);
          arg++;
        }
        break;
      }   
    }
    else{
      result = concat(result, (char[2]){format[i], '\0'}); 
    }
    i++;
  }
  return result;
}

bool isBlankIdentifier(char* identifier) {
  if (strcmp(identifier, "_") == 0) {
    return true;
  } 
  return false;
}
