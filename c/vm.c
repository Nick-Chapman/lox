
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef unsigned char u8;
typedef unsigned short u16;

//////////////////////////////////////////////////////////////////////
// OpCode

typedef enum {

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
// Typ, Value

typedef enum {
  TNil,
  TDouble,
  TBool,
  TString,
  TClosure,
  TIndirection,
} Typ;

typedef struct value {
  Typ typ;
  union {
    double number;
    bool boolean;
    struct ObjString* string;
    struct ObjClosure* closure;
    struct ObjIndirection* indirection;
  } as;
} Value;

//////////////////////////////////////////////////////////////////////
// ObjIndirection

typedef struct ObjIndirection {
  Value value;
} ObjIndirection;

ObjIndirection* makeIndirection(Value value) {
  ObjIndirection* indirection = malloc(sizeof(ObjIndirection)); // TODO: leak
  indirection->value = value;
  return indirection;
}

//////////////////////////////////////////////////////////////////////
// ObjClosure

typedef struct ObjClosure {
  u8 arity; // TODO: could be inlined in the code
  u8* code;
  Value ups[]; // "up values"
} ObjClosure;

ObjClosure* makeClosure(u8 arity, u8* code, u8 num_ups) {
  ObjClosure* closure = malloc(sizeof(ObjClosure) + num_ups * sizeof(Value)); // TODO: leak
  closure->arity = arity;
  closure->code = code;
  return closure;
}

//////////////////////////////////////////////////////////////////////
// ObjString

typedef struct ObjString {
  u16 length;
  char payload[];
} ObjString;

ObjString* makeString(u16 length, char* payload) {
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
// Value construction

Value ValueNil = { .typ = TNil, .as = { .number = 999 } }; //never look at number;

Value ValueOfDouble(double number) {
  return (Value) { .typ = TDouble, .as = { .number = number } };
}
Value ValueOfBool(bool boolean) {
  return (Value) { .typ = TBool, .as = { .boolean = boolean } };
}
Value ValueOfString(struct ObjString* string) {
  return (Value) { .typ = TString, .as = { .string = string } };
}
Value ValueOfClosure(struct ObjClosure* closure) {
  return (Value) { .typ = TClosure, .as = { .closure = closure } };
}
Value ValueOfIndirection(struct ObjIndirection* indirection) {
  return (Value) { .typ = TIndirection, .as = { .indirection = indirection } };
}

//////////////////////////////////////////////////////////////////////
// Value discrimination by type ("Is")

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
bool IsClosure(Value v) {
  return v.typ == TClosure;
}
bool IsIndirection(Value v) {
  return v.typ == TIndirection;
}

//////////////////////////////////////////////////////////////////////
// Value deconstruction by type ("As")

double AsDouble(Value v) {
  assert(IsDouble(v));
  return v.as.number;
}
bool AsBool(Value v) {
  assert(IsBool(v));
  return v.as.boolean;
}
struct ObjString* AsString(Value v) {
  assert(IsString(v));
  return v.as.string;
}
struct ObjClosure* AsClosure(Value v) {
  assert(IsClosure(v));
  return v.as.closure;
}
struct ObjIndirection* AsIndirection(Value v) {
  assert(IsIndirection(v));
  return v.as.indirection;
}

//////////////////////////////////////////////////////////////////////
// Value operations: print_value(), equal_value(), is_falsey()

void print_value(Value v) {
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
  } else if (IsClosure(v)) {
    printf("<func>");
  } else if (IsIndirection(v)) {
    printf("&");
    print_value(AsIndirection(v)->value);
  } else {
    printf("<unknown-value-type>");
  }
}

bool equal_value(Value a, Value b) {
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

bool is_falsey(Value a) { // just nil/false
  if (IsNil(a)) {
    return true;
  }
  else if (IsBool(a)) {
    return !AsBool(a);
  }
  return false;
}

//////////////////////////////////////////////////////////////////////
// Decode static program text: constants and bytecode

typedef struct {
  u8 num_doubles;
  u8 num_strings;
  double* doubles;
  u16* string_length;
  char** string_payload;
  u8* ip_start;
  u8* ip_end;
} Code;

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
// DEBUG: print_stack()

void print_stack(Value* base, Value* top) {
  int depth = top - base;
  printf("**stack(#%d):", depth);
  for (int i = 0; i < depth; i++) {
    putchar(' ');
    print_value(base[i]);
  }
  printf("\n");
}

//////////////////////////////////////////////////////////////////////
// runtime_error()

void runtime_error(char* mes) {
  printf("%s\n[line 1] in script\n", mes); // TODO: print real backtrace
  exit(1); // TODO: what exit code?
}

//////////////////////////////////////////////////////////////////////
// CallFrame

typedef struct {
  Value* base;
  u8* ip;
  Value* ups;
} CallFrame;

//////////////////////////////////////////////////////////////////////
// run_code()

void run_code(Code code) {

  // TODO: move VM state into a struct

  int stack_size = 100; //TODO: check overflow
  Value stack[stack_size];
  Value* base = stack;
  Value* sp = stack; //stack depth

  CallFrame frames[100]; //TODO: check overflow
  int frame_depth = 0;

  Value* ups = 0;

#define PUSH(d) (*sp++ = (d))
#define POP (*--sp)

  u8* ip = code.ip_start;
  int step = 0;

  for (;;step++) {

    if (ip == code.ip_end) return; // halt

    //print_stack(stack,sp);
    //printf("%d (%ld) %02x '%c'\n",step,ip-code.ip_start,*ip,*ip); fflush(stdout);
    OpCode op_code = *ip++;

    switch (op_code) {

    case OP_NUMBER: {
      u8 arg = *ip++;
      double d = code.doubles[arg];
      Value value = ValueOfDouble(d);
      PUSH(value);
      break;
    }
    case OP_STRING: {
      u8 arg = *ip++;
      u16 length = code.string_length[arg];
      char* payload = code.string_payload[arg];
      ObjString* string = makeString(length,payload);
      Value value = ValueOfString(string);
      PUSH(value);
      break;
    }
    case OP_NIL: {
      Value value = ValueNil;
      PUSH(value);
      break;
    }
    case OP_TRUE: {
      Value value = ValueOfBool(true);
      PUSH(value);
      break;
    }
    case OP_FALSE: {
      Value value = ValueOfBool(false);
      PUSH(value);
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
      Value value = AsIndirection(ups[arg])->value;
      PUSH(value);
      break;
    }
    case OP_SET_UPVALUE: {
      u8 arg = *ip++;
      Value value = sp[-1]; //peek
      AsIndirection(ups[arg])->value = value;
      break;
    }
    case OP_EQUAL: {
      Value v2 = POP; // TODO: avoid pop/pop/push
      Value v1 = POP;
      Value value = ValueOfBool (equal_value(v1,v2));
      PUSH(value);
      break;
    }
#define COMPARE(op) { \
      Value v2 = POP; \
      Value v1 = POP; \
      if (!(IsDouble(v1) && IsDouble(v2))) { \
        runtime_error("Operands must be numbers."); \
      } \
      Value value = ValueOfBool (AsDouble(v1) op AsDouble(v2)); \
      PUSH(value); \
    }
    case OP_GREATER: { COMPARE (>) break; }
    case OP_LESS: { COMPARE (<) break; }
#undef COMPARE
    case OP_ADD: {
      Value v2 = POP;
      Value v1 = POP;
      if (IsDouble(v1) && IsDouble(v2)) {
        Value value = ValueOfDouble (AsDouble(v1) + AsDouble(v2));
        PUSH(value);
      } else if (IsString(v1) && IsString(v2)) {
        Value value = ValueOfString (concatString(AsString(v1),AsString(v2)));
        PUSH(value);
      } else {
        runtime_error("Operands must be two numbers or two strings.");
      }
      break;
    }
#define BIN(op) { \
      Value v2 = POP; \
      Value v1 = POP; \
      if (!(IsDouble(v1) && IsDouble(v2))) { \
        runtime_error("Operands must be numbers."); \
      } \
      Value value = ValueOfDouble (AsDouble(v1) op AsDouble(v2)); \
      PUSH(value); \
    }
    case OP_SUBTRACT: { BIN(-); break; }
    case OP_MULTIPLY: { BIN(*); break; }
    case OP_DIVIDE: { BIN(/); break; }
#undef BIN
    case OP_NOT: {
      Value v1 = sp[-1];
      sp[-1] = ValueOfBool(is_falsey(v1));
      break;
    }
    case OP_NEGATE: {
      Value v1 = sp[-1];
      if (!IsDouble(v1)) {
        runtime_error("Operand must be a number.");
      }
      sp[-1] = ValueOfDouble(- AsDouble(v1));
      break;
    }
    case OP_PRINT: {
      print_value(POP);
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
      Value value = sp[-1]; //peek
      bool taken = is_falsey(value);
      if (taken) ip+=dist;
      break;
    }
    case OP_CALL: {
      u8 num_actuals = *ip++;
      Value callee = AsIndirection(sp[-1-num_actuals])->value;
      if (!IsClosure(callee)) {
        runtime_error("Can only call functions and classes.");
      }
      ObjClosure* closure = AsClosure(callee);
      int arity = closure->arity;
      if (num_actuals != arity) {
        char buf[80];
        sprintf(buf,"Expected %d arguments but got %d.",arity,num_actuals);
        runtime_error(buf);
      }
      CallFrame cf = { .ip = ip, .base = base, .ups = ups };
      frames[frame_depth++] = cf;
      base = sp - num_actuals - 1;
      ip = closure->code;
      ups = closure->ups;
      break;
    }
    case OP_CLOSURE: {
      u8 arity = *ip++;
      u8 num_ups = *ip++;
      int dist = (int)(*ip++) - 128;
      u8* code = ip + dist;
      ObjClosure* closure = makeClosure(arity,code,num_ups);
      for (int u = 0; u < num_ups; u++) {
        u8 mode = *ip++;
        u8 arg = *ip++;
        switch (mode) {
        case 1: {
          Value value = base[arg];
          closure->ups[u] = value;
          break;
        }
        case 2: {
          Value value = ups[arg];
          closure->ups[u] = value;
          break;
        }
        default: { printf("unknown closure mode: %d\n",mode); exit(1); }
        }
      }
      Value value = ValueOfClosure(closure);
      PUSH(value);
      break;
    }
    case OP_INDIRECT: {
      Value v1 = sp[-1];
      Value value = ValueOfIndirection(makeIndirection(v1));
      sp[-1] = value;
      break;
    }
    case OP_RETURN: {
      Value value = POP;
      CallFrame cf = frames[--frame_depth];
      sp = base;
      ip = cf.ip;
      base = cf.base;
      ups = cf.ups;
      PUSH(value);
      break;
    }
    default:
      printf("unknown op: %02x ('%c')\n",op_code,op_code);
      exit(1);
    }
  }
}

//////////////////////////////////////////////////////////////////////
// read_file()

char* read_file(const char* path, size_t* fileSize) {
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
// main()

int main(int argc, char* argv[]) {

  char* lox_file = argv[1];
  size_t lox_file_size;
  char* contents = read_file(lox_file,&lox_file_size);

  Code code = decode(contents,lox_file_size);
  run_code(code);
}
