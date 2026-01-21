
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

//#include <stddef.h>
//#include <string.h>

typedef unsigned char u8;

typedef enum {
  TBool,
  TNumber,
  TNil,
  TString,
} Typ;

typedef struct {
  int obj_size;
} Obj;

typedef struct {
  Obj obj;
  int string_size;
  char chars[];
} ObjString;

typedef struct {
  Typ typ;
  union {
    double number;
    bool boolean;
    Obj* obj_pointer;
  } as;
} Value;

#define ValueOfDouble(x) ((Value){TNumber, {.number = (x)}})

#define AsDouble(v) ((v).as.number)

void debug_sizes() {
  printf("**sizeof(Typ)=%ld\n",sizeof(Typ));
  printf("**sizeof(Obj)=%ld\n",sizeof(Obj));
  printf("**sizeof(ObjString)=%ld\n",sizeof(ObjString));
  printf("**sizeof(Value)=%ld\n",sizeof(Value));
}

char* readFile(const char* path, size_t* fileSize) {
  FILE* file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  *fileSize = ftell(file);
  rewind(file);

  char* buffer = (char*)malloc(*fileSize + 1);
  if (buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(74);
  }

  size_t bytesRead = fread(buffer, sizeof(char), *fileSize, file);
  if (bytesRead < *fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(74);
  }

  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

void printValue(Value v) {
  double d = AsDouble(v);
  printf("%g\n", d);
}

void debug(char* contents, size_t size) {
  printf("**contents[0..6]] (marker)   = %.7s\n",contents);
  printf("**contents[7]]    (#doubles) = %d\n",contents[7]);
  u8 num_doubles = contents[7];
  double* doubles = (double*)&contents[8];
  for (int i=0; i<num_doubles; i++) {
    double d = doubles[i];
    printf("**double[%d] = %g\n",i,d);
  }
  size_t bytecode_offset = 8*(num_doubles+1);
  printf("**bytecode:");
  for (size_t i = bytecode_offset; i < size; i++) {
    char c = contents[i];
    printf(" %02x",c);
  }
  printf("\n");
}

void run_bytecode(char* contents) {

  u8 num_doubles = contents[7];
  double* doubles = (double*)&contents[8];
  size_t bytecode_offset = 8*(num_doubles+1);

  int stack_size = 100; //TODO: check overflow
  Value stack[stack_size];
  Value* sp = stack;

#define PUSH(d) (*sp++ = (d))
#define POP (*--sp)

  char* ip = &contents[bytecode_offset];

  int step = 0;
  printf("**EXECUTE...\n");
  for (;;) {
    //u8 stack_depth = sp-stack;
    //printf("**stack(#%d):", stack_depth);
    //for (int j=0; j < stack_depth; j++) printf(" %g",stack[j]);
    //printf("\n");
    step++;
    char c = *ip++;
    //printf("%03d: %02x\n",step,c);
    switch (c) {
    case '\n': {
      return;
    }
    case 'c': {
      u8 i = *ip++;
      double d = doubles[i];
      //printf("**constant(%d) load -> %g\n",i,d);
      PUSH(ValueOfDouble(d));
      break;
    }
    case 'p': {
      printValue(POP);
      break;
    }
    case '~': {
      sp[-1] = ValueOfDouble(AsDouble(sp[-1]));
      break;
    }
    default:
      printf("unknown op: %02x ('%c')\n",c,c);
      exit(1);
    }
  }
}

int main(int argc, char* argv[]) {

  //printf("**argc=%d\n",argc);
  //for (int i=0; i<argc; i++) printf("**argv[%d]=%s\n",i,argv[i]);
  //debug_sizes();

  char* lox_file = argv[1];
  size_t lox_file_size;
  char* contents = readFile(lox_file,&lox_file_size);

  //debug(contents,lox_file_size);
  run_bytecode(contents);
  //printf("**done\n");
}
