
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

//#include <stddef.h>
#include <string.h>

#include <assert.h>

typedef unsigned char u8;
typedef unsigned short u16;

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

//////////////////////////////////////////////////////////////////////
// (Obj) String

//typedef struct {
//  int obj_size;
//} Obj;

typedef struct {
  //Obj obj;
  u16 length;
  char payload[];
} ObjString; // TODO: Name is String (or is this name in use?)

ObjString* makeString(u16 length, char* payload) {
  //printf("makeString(%d,\"%s\")\n",length,payload);
  ObjString* str = malloc(sizeof(u16) + length + 1); // TODO: leaks
  str->length = length;
  memcpy(str->payload,payload,length+1);
  return str;
}

bool eqString(ObjString* s1, ObjString* s2) {
  if (s1->length != s2->length) return false;
  return 0 == memcmp(s1->payload,s2->payload,s1->length);
}

ObjString* concatString(ObjString* str1, ObjString* str2) {
  ObjString* str = malloc(sizeof(u16) + str1->length + str2->length + 1); // TODO: leaks
  str->length = str1->length + str2->length;
  memcpy(str->payload,               str1->payload,str1->length);
  memcpy(str->payload + str1->length,str2->payload,str2->length + 1);
  return str;
}

//////////////////////////////////////////////////////////////////////
// Value representation

typedef enum {
  TBool = 71, // nothing should depend on details of runtime tag values
  TDouble,
  TNil,
  TString,
} Typ;

typedef struct {
  Typ typ;
  union {
    double number;
    bool boolean;
    ObjString* string;
  } as;
} Value;


Value ValueOfDouble(double number) {
  return (Value) { .typ = TDouble, .as = { .number = number } };
}
Value ValueOfBool(bool boolean) {
  return (Value) { .typ = TBool, .as = { .boolean = boolean } };
}
Value ValueOfString(ObjString* string) {
  return (Value) { .typ = TString, .as = { .string = string } };
}


Value ValueNil = { .typ = TNil, .as = { .number = 999 } }; //never look at number;


bool IsNil(Value v) {
  return v.typ == TNil;
}
bool IsDouble(Value v) {
  return v.typ == TDouble;
}
bool IsBool(Value v) {
  return v.typ == TBool;
}
bool IsString(Value v) {
  return v.typ == TString;
}


double AsDouble(Value v) {
  assert(IsDouble(v));
  return v.as.number;
}
bool AsBool(Value v) {
  assert(IsBool(v));
  return v.as.boolean;
}
ObjString* AsString(Value v) {
  assert(IsString(v));
  return v.as.string;
}

//////////////////////////////////////////////////////////////////////
// Value operations

void printValue(Value v) {
  if (IsNil(v)) {
    printf("nil");
  } else if (IsDouble(v)) {
    double d = AsDouble(v);
    printf("%g", d);
  } else if (IsBool(v)) {
    printf("%s", AsBool(v) ? "true" : "false");
  } else if (IsString(v)) {
    ObjString* str = AsString(v);
    printf("%.*s",str->length,str->payload);
  } else {
    printf("<unknown-value-type>");
  }
}

bool eqValue(Value a, Value b) {
  if (IsNil(a) && IsNil(b)) {
    return true;
  }
  if (IsDouble(a) && IsDouble(b)) {
    return AsDouble(a) == AsDouble(b);
  }
  if (IsBool(a) && IsBool(b)) {
    return AsBool(a) == AsBool(b);
  }
  if (IsString(a) && IsString(b)) {
    return eqString(AsString(a),AsString(b));
  }
  return false;
}

//////////////////////////////////////////////////////////////////////
// decode static program text: constants and bytecode

typedef struct {
  u8 num_doubles;
  u8 num_strings;
  double* doubles;
  u16* string_length;
  char** string_payload;
  char* ip_start;
  char* ip_end;
} Code;

void show_code(Code code) {
  printf("**#doubles = %d\n",code.num_doubles);
  for (int i=0; i<code.num_doubles; i++) {
    printf("**double[%d] = %g\n",i,code.doubles[i]);
  }
  printf("**#strings = %d\n",code.num_strings);
  for (int i=0; i<code.num_strings; i++) {
    printf("**string_length[%d] = %d\n",i,code.string_length[i]);
  }
  for (int i=0; i<code.num_strings; i++) {
    printf("**string_payload[%d] = %s\n",i,code.string_payload[i]);
  }
  printf("**bytecode:");
  for (char* ip = code.ip_start; ip < code.ip_end; ip++) printf(" %02x",*ip);
  printf("\n");
}

Code decode(char* contents, size_t size) {
  u8 num_doubles = contents[6];
  u8 num_strings = contents[7];
  size_t off = 8;
  double* doubles = (double*)&contents[off];
  off += 8*num_doubles;
  u16* string_length = (u16*)&contents[off];
  off += 2*num_strings;
  char** string_payload = (char**)malloc(sizeof(char*)*num_strings);
  for (int i = 0; i<num_strings; i++) {
    string_payload[i] = &contents[off];
    off += (string_length[i] + 1);
  }
  return (Code) {
    .num_doubles = num_doubles,
    .num_strings = num_strings,
    .doubles = doubles,
    .string_length = string_length,
    .string_payload = string_payload,
    .ip_start = &contents[off],
    .ip_end = &contents[size],
  };
}

//////////////////////////////////////////////////////////////////////

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

void runtime_error(char* mes) {
  printf("%s\n[line 1] in script\n", mes); // TODO: print real backtrace
  exit(1); // TODO: what code?
}

void run_code(Code code) {

  int stack_size = 100; //TODO: check overflow
  Value stack[stack_size];
  Value* sp = stack;

#define PUSH(d) (*sp++ = (d))
#define POP (*--sp)

  char* ip = code.ip_start;

  int step = 0;
  //printf("**EXECUTE...\n");
  for (;;step++) {

    //printStack(stack,sp);
    char c = *ip++;
    //printf("%d (%ld) %02x '%c'\n",step,ip-code.ip_start,c,c);

    switch (c) {
    case '\n': {
      return;
    }
    case 'c': {
      u8 i = *ip++;
      double d = code.doubles[i];
      Value res = ValueOfDouble(d);
      PUSH(res);
      break;
    }
    case '"': {
      u8 i = *ip++;
      u16 length = code.string_length[i];
      char* payload = code.string_payload[i];
      ObjString* string = makeString(length,payload);
      Value res = ValueOfString(string);
      PUSH(res);
      break;
    }

    case 'g': {
      u8 i = *ip++;
      Value res = stack[i];
      PUSH(res);
      break;
    }
    case 's': {
      u8 i = *ip++;
      Value res = sp[-1]; // peek
      stack[i] = res;
      break;
    }

    case 'p': {
      printValue(POP);
      putchar('\n');
      break;
    }
    case 'd': {
      --sp;
      break;
    }
    case '~': {
      Value a = sp[-1];
      if (!IsDouble(a)) {
        runtime_error("Operand must be a number.");
      }
      sp[-1] = ValueOfDouble(- AsDouble(a));
      break;
    }
    case '!': {
      Value a = sp[-1];
      if (!IsBool(a)) {
        runtime_error("xxx TODO Operand must be a number.");
      }
      sp[-1] = ValueOfBool(! AsBool(a));
      break;
    }
    case '+': {
      Value b = POP;
      Value a = POP;
      if (IsDouble(a) && IsDouble(b)) {
        Value res = ValueOfDouble (AsDouble(a) + AsDouble(b));
        PUSH(res);
      } else if (IsString(a) && IsString(b)) {
        Value res = ValueOfString (concatString(AsString(a),AsString(b)));
        PUSH(res);
      } else {
        runtime_error("Operands must be two numbers or two strings.");
      }
      break;
    }

#define BIN(op) { \
      Value b = POP; \
      Value a = POP; \
      if (!(IsDouble(a) && IsDouble(b))) { \
        runtime_error("Operands must be numbers."); \
      } \
      Value res = ValueOfDouble (AsDouble(a) op AsDouble(b)); \
      PUSH(res); \
    }

    case '-': { BIN(-); break; }
    case '*': { BIN(*); break; }
    case '/': { BIN(/); break; }

#define COMPARE(op) { \
      Value b = POP; \
      Value a = POP; \
      if (!(IsDouble(a) && IsDouble(b))) { \
        runtime_error("Operands must be numbers."); \
      } \
      Value res = ValueOfBool (AsDouble(a) op AsDouble(b)); \
      PUSH(res); \
    }

    case '<': { COMPARE (<) break; }
    case '>': { COMPARE (>) break; }

    case '=': {
      Value b = POP;
      Value a = POP;
      Value res = ValueOfBool (eqValue(a,b));
      PUSH(res);
      break;
    }
    case 'f': {
      Value res = ValueOfBool(false);
      PUSH(res);
      break;
    }
    case 't': {
      Value res = ValueOfBool(true);
      PUSH(res);
      break;
    }
    case 'n': {
      Value res = ValueNil;
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

void debug_sizes() {
  printf("**sizeof(u8)=%ld\n",sizeof(u8));
  printf("**sizeof(u16)=%ld\n",sizeof(u16));
  printf("**sizeof(Typ)=%ld\n",sizeof(Typ));
  //printf("**sizeof(Obj)=%ld\n",sizeof(Obj));
  printf("**sizeof(ObjString)=%ld\n",sizeof(ObjString));
  printf("**sizeof(Value)=%ld\n",sizeof(Value));
}

int main(int argc, char* argv[]) {

  //printf("**argc=%d\n",argc);
  //for (int i=0; i<argc; i++) printf("**argv[%d]=%s\n",i,argv[i]);
  //debug_sizes();

  char* lox_file = argv[1];
  size_t lox_file_size;
  char* contents = readFile(lox_file,&lox_file_size);

  Code code = decode(contents,lox_file_size);
  //show_code(code);
  run_code(code);
  //printf("**done\n");
}
