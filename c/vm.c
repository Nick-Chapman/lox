
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

//#include <stddef.h>
//#include <string.h>

#include <assert.h>

typedef unsigned char u8;

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

typedef enum {
  TBool,
  TNumber, // TODO: be consistent: double/number
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
#define ValueOfBool(x) ((Value){TBool, {.boolean = (x)}})

//TODO: these should assert of the type is wrong!
//#define AsDouble(v) ((v).as.number)
#define AsBool(v) ((v).as.boolean)

bool IsDouble(Value v) {
  return v.typ == TNumber;
}
bool IsBool(Value v) {
  return v.typ == TBool;
}

double AsDouble(Value v) {
  assert(IsDouble(v));
  return v.as.number;
}

void printValue(Value v) {
  if (IsDouble(v)) {
    double d = AsDouble(v);
    printf("%g", d);
  } else if (IsBool(v)) {
    printf("%s", AsBool(v) ? "true" : "false");
  } else {
    printf("<unknown-value-type>");
  }
}

bool eqValue(Value a, Value b) {
  if (IsDouble(a) && IsDouble(b)) {
    return AsDouble(a) == AsDouble(b);
  }
  if (IsBool(a) && IsBool(b)) {
    return AsBool(a) == AsBool(b);
  }
  return false;
}


//////////////////////////////////////////////////////////////////////

void debug_sizes() {
  printf("**sizeof(Typ)=%ld\n",sizeof(Typ));
  printf("**sizeof(Obj)=%ld\n",sizeof(Obj));
  printf("**sizeof(ObjString)=%ld\n",sizeof(ObjString));
  printf("**sizeof(Value)=%ld\n",sizeof(Value));
}

void debug(char* contents, size_t size) {
  //printf("**contents[0..6]] (marker)   = %.7s\n",contents);
  printf("**contents[7]]    (#doubles) = %d\n",contents[7]);
  u8 num_doubles = contents[7];
  double* doubles = (double*)&contents[8];
  for (int i=0; i<num_doubles; i++) printf("**double[%d] = %g\n",i,doubles[i]);
  size_t bytecode_offset = 8*(num_doubles+1);
  printf("**bytecode:");
  for (size_t i = bytecode_offset; i < size; i++) {
    char c = contents[i];
    printf(" %02x",c);
  }
  printf("\n");
}

void printStack(Value* base, Value* top) {
  int depth = top - base;
  printf("**stack(#%d):", depth);
  for (int i = 0; i < depth; i++) {
    putchar(' ');
    printValue(base[i]);
  }
  printf("\n");
}

//////////////////////////////////////////////////////////////////////
// TODO: vm state in struct?
// so can have sep functions for push/pop

void run_bytecode(char* contents) {

  u8 num_doubles = contents[7];
  double* doubles = (double*)&contents[8];
  size_t bytecode_offset = 8*(num_doubles+1);

  int stack_size = 100; //TODO: check overflow
  Value stack[stack_size];
  Value* sp = stack;

#define PUSH(d) (*sp++ = (d))
#define POP (*--sp)

  char* ip_start = &contents[bytecode_offset];
  char* ip = ip_start;

  int step = 0;
  //printf("**EXECUTE...\n");
  for (;;step++) {
    //printStack(stack,sp);
    char c = *ip++;
    //printf("%d (%ld) %02x '%c'\n",step,ip-ip_start,c,c);
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
      putchar('\n');
      break;
    }
    case '~': {
      sp[-1] = ValueOfDouble(- AsDouble(sp[-1]));
      break;
    }
    case '!': {
      sp[-1] = ValueOfBool(! AsBool(sp[-1]));
      break;
    }

    case '*': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfDouble (AsDouble(a) * AsDouble(b));
      PUSH(res);
      break;
    }
    case '+': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfDouble (AsDouble(a) + AsDouble(b));
      PUSH(res);
      break;
    }
    case '-': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfDouble (AsDouble(a) - AsDouble(b));
      PUSH(res);
      break;
    }
    case '/': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfDouble (AsDouble(a) / AsDouble(b));
      PUSH(res);
      break;
    }
    case '<': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfBool (AsDouble(a) < AsDouble(b));
      PUSH(res);
      break;
    }
    case '>': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfBool (AsDouble(a) > AsDouble(b));
      PUSH(res);
      break;
    }
    case '=': {
      Value b = POP;
      Value a = POP;
      //if (a.typ) {}
      //if (b.typ) {}
      Value res = ValueOfBool (eqValue(a,b));
      //Value res = ValueOfBool (false);
      PUSH(res);
      break;
    }
    case 'f': {
      Value res = ValueOfBool(false);
      PUSH(res);
      break;
    }
    default:
      printf("unknown op: %02x ('%c')\n",c,c);
      exit(1);
    }
  }
}

//////////////////////////////////////////////////////////////////////

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
