# Before running this file, you will need to have installed the thrift compiler
# into /usr/local/bin.

/**
 * The first thing to know about are types. The available types in Thrift are:
 *
 *  bool        Boolean, one byte
 *  i8 (byte)   Signed 8-bit integer
 *  i16         Signed 16-bit integer
 *  i32         Signed 32-bit integer
 *  i64         Signed 64-bit integer
 *  double      64-bit floating point value
 *  string      String
 *  binary      Blob (byte array)
 *  map<t1,t2>  Map from one type to another
 *  list<t1>    Ordered list of one type
 *  set<t1>     Set of unique elements of one type
 *
 * Did you also notice that Thrift supports C style comments?
 */

 namespace java zio.schema.codec.generated

struct BasicInt {
  1: i32 value,
}

struct BasicString {
  1: string value,
}

struct BasicDouble {
  1: double value,
}

struct Embedded {
  1: BasicInt embedded,
}

struct IntList {
  1: list<i32> items,
}

struct StringList {
  1: list<string> items,
}

struct Record {
  1: string name,
  2: i32 value,
}

struct HighArity {
  1: i32 f1,
  2: i32 f2,
  3: i32 f3,
  4: i32 f4,
  5: i32 f5,
  6: i32 f6,
  7: i32 f7,
  8: i32 f8,
  9: i32 f9,
  10: i32 f10,
  11: i32 f11,
  12: i32 f12,
  13: i32 f13,
  14: i32 f14,
  15: i32 f15,
  16: i32 f16,
  17: i32 f17,
  18: i32 f18,
  19: i32 f19,
  20: i32 f20,
  21: i32 f21,
  22: i32 f22,
  23: i32 f23,
  24: i32 f24,
}

struct StringValue {
  1: string value,
}

struct IntValue {
  1: i32 value,
}

struct BoolValue {
  1: bool value,
}

union OneOf {
  1: StringValue stringValue
  2: IntValue intValue
  3: BoolValue boolValue
}

struct Enumeration {
  1: OneOf value,
}

struct SetValue {
  1: set<Record> value,
}

struct MapValue {
  1: map<string, Record> value,
}

enum Color {
  RED,
  GREEN,
  BLUE
}

struct EnumValue {
  1: Color value
}