
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
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
// op code

typedef enum {

  OP_HALT               = '\0', // TODO avoid

  OP_NUMBER             = 'c',
  OP_STRING             = '"',
  OP_NIL                = 'n',
  OP_TRUE               = 't',
  OP_FALSE              = 'f',

  OP_POP                = 'd',
  OP_GET_LOCAL          = 'g',
  OP_SET_LOCAL          = 's',
  OP_GET_UPVALUE        = 'G',
  OP_SET_UPVALUE        = 'S',

  OP_EQUAL              = '=',
  OP_GREATER            = '>',
  OP_LESS               = '<',
  OP_ADD                = '+',
  OP_SUBTRACT           = '-',
  OP_MULTIPLY           = '*',
  OP_DIVIDE             = '/',
  OP_NOT                = '!',
  OP_NEGATE             = '~',

  OP_PRINT              = 'p',
  OP_JUMP               = 'j',
  OP_JUMP_IF_FALSE      = 'b',
  //OP_LOOP

  OP_CALL               = 'C',
  OP_CLOSURE            = 'F',
  OP_INDIRECT           = 'I',
  OP_RETURN             = 'R',

} OpCode;

//////////////////////////////////////////////////////////////////////
// Value representation

typedef enum {
  TNil,
  TDouble,
  TBool,
  TString,
  TFunc,
  TIndirection,
} Typ;

typedef struct value {
  Typ typ;
  union {
    double number;
    bool boolean;
    struct ObjString* string;
    struct ObjFunc* func;
    struct ObjIndirection* indirection;
  } as;
} Value;


//////////////////////////////////////////////////////////////////////
// (Obj) Indirection

typedef struct ObjIndirection {
  Value value;
} ObjIndirection;

ObjIndirection* makeIndirection(Value value) {
  ObjIndirection* indirection = malloc(sizeof(ObjIndirection)); // TODO: leak
  indirection->value = value;
  return indirection;
}

//////////////////////////////////////////////////////////////////////
// (Obj) Func

typedef struct ObjFunc {
  u8 arity;
  u8* code;
  Value up[];
} ObjFunc;

ObjFunc* makeFunc(u8 arity, u8* code, u8 num_up_values) {
  ObjFunc* func = malloc(sizeof(ObjFunc) + num_up_values * sizeof(Value)); // TODO: leak
  func->arity = arity;
  func->code = code;
  return func;
}

//////////////////////////////////////////////////////////////////////
// (Obj) String

//typedef struct {
//  int obj_size;
//} Obj;

typedef struct ObjString {
  //Obj obj;
  u16 length;
  char payload[];
} ObjString;

ObjString* makeString(u16 length, char* payload) {
  //printf("makeString(%d,\"%s\")\n",length,payload);
  ObjString* str = malloc(sizeof(u16) + length + 1); // TODO: leak
  str->length = length;
  memcpy(str->payload,payload,length+1);
  return str;
}

bool eqString(ObjString* s1, ObjString* s2) {
  if (s1->length != s2->length) return false;
  return 0 == memcmp(s1->payload,s2->payload,s1->length);
}

ObjString* concatString(ObjString* str1, ObjString* str2) {
  ObjString* str = malloc(sizeof(u16) + str1->length + str2->length + 1); // TODO: leak
  str->length = str1->length + str2->length;
  memcpy(str->payload,               str1->payload,str1->length);
  memcpy(str->payload + str1->length,str2->payload,str2->length + 1);
  return str;
}

//////////////////////////////////////////////////////////////////////
// Value constructors, discriminators and assessors.

Value ValueNil = { .typ = TNil, .as = { .number = 999 } }; //never look at number;

Value ValueOfDouble(double number) {
  return (Value) { .typ = TDouble, .as = { .number = number } };
}
Value ValueOfBool(bool boolean) {
  return (Value) { .typ = TBool, .as = { .boolean = boolean } };
}
Value ValueOfString(ObjString* string) {
  return (Value) { .typ = TString, .as = { .string = string } };
}
Value ValueOfFunc(ObjFunc* func) {
  return (Value) { .typ = TFunc, .as = { .func = func } };
}
Value ValueOfIndirection(ObjIndirection* indirection) {
  return (Value) { .typ = TIndirection, .as = { .indirection = indirection } };
}


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
bool IsFunc(Value v) {
  return v.typ == TFunc;
}
bool IsIndirection(Value v) {
  return v.typ == TIndirection;
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
ObjFunc* AsFunc(Value v) {
  assert(IsFunc(v));
  return v.as.func;
}
ObjIndirection* AsIndirection(Value v) {
  assert(IsIndirection(v));
  return v.as.indirection;
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
  } else if (IsFunc(v)) {
    printf("<func>");
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

bool isTruthy(Value a) { // everything except nil/false
  if (IsNil(a)) {
    return false;
  }
  else if (IsBool(a)) {
    return AsBool(a);
  }
  return true;
}

//////////////////////////////////////////////////////////////////////
// decode static program text: constants and bytecode

typedef struct {
  u8 num_doubles;
  u8 num_strings;
  double* doubles;
  u16* string_length;
  char** string_payload;
  u8* ip_start;
  u8* ip_end;
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
  for (u8* ip = code.ip_start; ip < code.ip_end; ip++) printf(" %02x",*ip);
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
    .ip_start = (u8*)&contents[off],
    .ip_end = (u8*)&contents[size],
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
// Call frame

typedef struct {
  Value* base;
  u8* ip;
  Value* up_values;
} CallFrame;

//////////////////////////////////////////////////////////////////////
// TODO: vm state in struct?
// so can have sep functions for push/pop

void runtime_error(char* mes) {
  printf("%s\n[line 1] in script\n", mes); // TODO: print real backtrace
  exit(1); // TODO: what exit code?
}

void run_code(Code code) {

  int stack_size = 100; //TODO: check overflow
  Value stack[stack_size];
  Value* base = stack;
  Value* sp = stack; //stack depth

  CallFrame frames[100]; //TODO: check overflow
  int frame_depth = 0;

  Value* up_values = 0;

#define PUSH(d) (*sp++ = (d))
#define POP (*--sp)

  u8* ip = code.ip_start;

  int step = 0;
  //printf("**EXECUTE...\n");
  for (;;step++) {

    //printStack(stack,sp);
    OpCode c = *ip;
    //printf("%d (%ld) %02x '%c'\n",step,ip-code.ip_start,c,c); fflush(stdout);
    ip++;

    switch (c) {
    case OP_HALT: { // TOOD: better to test ip has reached a point past the last instruction
      return;
    }
    case OP_NUMBER: {
      u8 i = *ip++;
      double d = code.doubles[i];
      Value res = ValueOfDouble(d);
      PUSH(res);
      break;
    }
    case OP_STRING: {
      u8 i = *ip++;
      u16 length = code.string_length[i];
      char* payload = code.string_payload[i];
      ObjString* string = makeString(length,payload);
      Value res = ValueOfString(string);
      PUSH(res);
      break;
    }
    case OP_NIL: {
      Value res = ValueNil;
      PUSH(res);
      break;
    }
    case OP_TRUE: {
      Value res = ValueOfBool(true);
      PUSH(res);
      break;
    }
    case OP_FALSE: {
      Value res = ValueOfBool(false);
      PUSH(res);
      break;
    }
    case OP_POP: {
      --sp;
      break;
    }
    case OP_GET_LOCAL: {
      u8 arg = *ip++;
      Value value = AsIndirection(base[arg])->value;
      PUSH(value);
      break;
    }
    case OP_SET_LOCAL: {
      u8 arg = *ip++;
      Value value = sp[-1]; //peek
      AsIndirection(base[arg])->value = value;
      break;
    }
    case OP_GET_UPVALUE: {
      u8 arg = *ip++;
      Value value = AsIndirection(up_values[arg])->value;
      PUSH(value);
      break;
    }
    case OP_SET_UPVALUE: {
      u8 arg = *ip++;
      Value value = sp[-1]; //peek
      AsIndirection(up_values[arg])->value = value;
      break;
    }
    case OP_EQUAL: {
      Value b = POP; // TODO: avoid pop/pop/push
      Value a = POP;
      Value res = ValueOfBool (eqValue(a,b));
      PUSH(res);
      break;
    }
#define COMPARE(op) { \
      Value b = POP; \
      Value a = POP; \
      if (!(IsDouble(a) && IsDouble(b))) { \
        runtime_error("Operands must be numbers."); \
      } \
      Value res = ValueOfBool (AsDouble(a) op AsDouble(b)); \
      PUSH(res); \
    }
    case OP_GREATER: { COMPARE (>) break; }
    case OP_LESS: { COMPARE (<) break; }
#undef COMPARE
    case OP_ADD: {
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
    case OP_SUBTRACT: { BIN(-); break; }
    case OP_MULTIPLY: { BIN(*); break; }
    case OP_DIVIDE: { BIN(/); break; }
#undef BIN
    case OP_NOT: {
      Value a = sp[-1];
      sp[-1] = ValueOfBool(! isTruthy(a));
      break;
    }
    case OP_NEGATE: {
      Value a = sp[-1];
      if (!IsDouble(a)) {
        runtime_error("Operand must be a number.");
      }
      sp[-1] = ValueOfDouble(- AsDouble(a));
      break;
    }
    case OP_PRINT: {
      printValue(POP);
      putchar('\n');
      break;
    }
    case OP_JUMP: {
      u8 arg = *ip++;
      int dist = (int)arg - 128;
      ip+=dist;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      u8 arg = *ip++;
      int dist = (int)arg - 128;
      Value v = sp[-1]; //peek
      bool taken = !isTruthy(v);
      if (taken) ip+=dist;
      break;
    }
    case OP_CALL: {
      u8 nActuals = *ip++;
      Value callee0 = sp[-1-nActuals];
      Value callee = AsIndirection(callee0)->value;
      if (!IsFunc(callee)) {
        runtime_error("Can only call functions and classes.");
      }
      ObjFunc* func = AsFunc(callee);
      int nFormals = func->arity;
      if (nActuals != nFormals) {
        char buf[80];
        sprintf(buf,"Expected %d arguments but got %d.",nFormals,nActuals);
        runtime_error(buf);
      }
      CallFrame cf = { .ip = ip, .base = base, .up_values = up_values };
      frames[frame_depth++] = cf;
      base = sp - nActuals - 1;
      ip = func->code;
      up_values = func->up;
      break;
    }
    case OP_CLOSURE: {
      u8 arity = *ip++;
      u8 num_up_values = *ip++;
      int dist = (int)(*ip++) - 128;
      u8* code = ip + dist;
      ObjFunc* func = makeFunc(arity,code,num_up_values);
      for (int u = 0; u < num_up_values; u++) {
        u8 mode = *ip++;
        u8 arg = *ip++;
        switch (mode) {
        case 1: {
          Value value = base[arg];
          func->up[u] = value;
          break;
        }
        case 2: {
          Value value = up_values[arg];
          func->up[u] = value;
          break;
        }
        default: { printf("unknown closure mode: %d\n",mode); exit(1); }
        }
      }
      Value res = ValueOfFunc(func);
      PUSH(res);
      break;
    }
    case OP_INDIRECT: {
      Value value = sp[-1];
      Value res = ValueOfIndirection(makeIndirection(value));
      sp[-1] = res;
      break;
    }
    case OP_RETURN: {
      Value res = POP;
      CallFrame cf = frames[--frame_depth];
      sp = base;
      ip = cf.ip;
      base = cf.base;
      up_values = cf.up_values;
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
