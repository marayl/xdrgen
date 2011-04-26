\title{XDR: External Data Representation Standard}

\documentclass[12pt]{article}
%include polycode.fmt

\begin{document}
\maketitle

\begin{abstract}
This document describes the External Data Representation Standard (XDR)
protocol as it is currently deployed and accepted.
\end{abstract}

\section{INTRODUCTION}

XDR is a standard for the description and encoding of data.  It is useful for
transferring data between different computer architectures, and has been used
to communicate data between such diverse machines as the SUN WORKSTATION*,
VAX*, IBM-PC*, and Cray*.  XDR fits into the ISO presentation layer, and is
roughly analogous in purpose to X.409, ISO Abstract Syntax Notation.  The
major difference between these two is that XDR uses implicit typing, while
X.409 uses explicit typing.

XDR uses a language to describe data formats.  The language can only be used
only to describe data; it is not a programming language.  This language allows
one to describe intricate data formats in a concise manner. The alternative of
using graphical representations (itself an informal language) quickly becomes
incomprehensible when faced with complexity.  The XDR language itself is
similar to the C language [1], just as Courier [4] is similar to
Mesa. Protocols such as ONC RPC (Remote Procedure Call) and the NFS* (Network
File System) use XDR to describe the format of their data.

The XDR standard makes the following assumption: that bytes (or octets) are
portable, where a byte is defined to be 8 bits of data.  A given hardware
device should encode the bytes onto the various media in such a way that other
hardware devices may decode the bytes without loss of meaning.  For example,
the Ethernet* standard suggests that bytes be encoded in ``little-endian''
style [2], or least significant bit first.

\subsection{Test Harness}

Several imports are required for use by the test harness.

\begin{code}
import Control.Monad
import Data.ByteString.UTF8
import Data.XDR.AST
import Data.XDR.Parser
import Test.HUnit
\end{code}

Parse a string literal to produce either a module specification or one or more
errors.

\begin{code}
parse :: String -> Either [ParseError] ModuleSpec
parse s = parseString [] (fromString s) "test"
\end{code}

Assert that evaluated integer expression matches expected value.

\begin{code}
assertEval :: Integer -> String -> Assertion
assertEval expected s =
    case parseConstExpr (fromString s) "test" of
      Left err -> assertFailure . show $ err
      Right c -> assertEqual "" expected (evalConstExpr c)
\end{code}

Assert that module specification matches expected definition.

\begin{code}
assertDef :: Definition -> Either [ParseError] ModuleSpec -> Assertion
assertDef _ (Left err) =
    assertFailure . show $ err
assertDef expected (Right (ModuleSpec _ _ [])) =
    assertFailure "no definitions"
assertDef expected (Right (ModuleSpec _ _ actual)) =
    assertEqual "" expected (last actual)
\end{code}

Assert that an error occurred.

\begin{code}
assertErr :: Either [ParseError] ModuleSpec -> Assertion
assertErr r =
    unless (either (\x -> True) (\x -> False) r)
               (assertFailure "expected error")
\end{code}

The test cases covered in this document.

\begin{code}
tests =
    TestList [ TestLabel "integer" testInteger
             , TestLabel "unsigned integer" testUnsignedInteger
             , TestLabel "enumeration" testEnumeration
             , TestLabel "boolean" testBoolean
             , TestLabel "hyper" testHyper
             , TestLabel "unsigned hyper" testUnsignedHyper
             , TestLabel "floating point" testFloatingPoint
             , TestLabel "double precision" testDoublePrecision
             , TestLabel "quadruple" testQuadruple
             , TestLabel "fixed opaque" testFixedOpaque
             , TestLabel "variable opaque" testVariableOpaque
             , TestLabel "string" testString
             , TestLabel "fixed array" testFixedArray
             , TestLabel "variable array" testVariableArray
             , TestLabel "structure" testStructure
             , TestLabel "discriminated union" testDiscriminatedUnion
             , TestLabel "void" testVoid
             , TestLabel "constant" testConstant
             , TestLabel "typedef" testTypedef
             , TestLabel "optional" testOptional
             , TestLabel "comment" testComment
             , TestLabel "whitespace" testWhitespace
             , TestLabel "identifier" testIdentifier
             , TestLabel "decimal" testDecimal
             , TestLabel "hexadecimal" testHexadecimal
             , TestLabel "multiplication expression" testMultiplication
             , TestLabel "addition expression" testAddition
             , TestLabel "shift expression" testShift
             , TestLabel "logical expression" testLogical
             , TestLabel "unary expression" testUnary
             , TestLabel "distributive expression" testDistributive
             , TestLabel "empty body" testEmptyBody
            ]

main =
    runTestTT tests
\end{code}

\section{BASIC BLOCK SIZE}

The representation of all items requires a multiple of four bytes (or 32 bits)
of data.  The bytes are numbered 0 through n-1.  The bytes are read or written
to some byte stream such that byte m always precedes byte m+1.  If the n bytes
needed to contain the data are not a multiple of four, then the n bytes are
followed by enough (0 to 3) residual zero bytes, r, to make the total byte
count a multiple of 4.

We include the familiar graphic box notation for illustration and comparison.
In most illustrations, each box (delimited by a plus sign at the 4 corners and
vertical bars and dashes) depicts a byte.  Ellipses (...) between boxes show
zero or more additional bytes where required.

\begin{verbatim}
+--------+--------+...+--------+--------+...+--------+
| byte 0 | byte 1 |...|byte n-1|    0   |...|    0   |   BLOCK
+--------+--------+...+--------+--------+...+--------+
|<-----------n bytes---------->|<------r bytes------>|
|<-----------n+r (where (n+r) mod 4 = 0)>----------->|
\end{verbatim}

\section{XDR DATA TYPES}

Each of the sections that follow describes a data type defined in the XDR
standard, shows how it is declared in the language, and includes a graphic
illustration of its encoding.

For each data type in the language we show a general paradigm declaration.
Note that angle brackets ($\langle$ and $\rangle$) denote variablelength
sequences of data and square brackets ([ and ]) denote fixed-length sequences
of data.  ``n'', ``m'' and ``r'' denote integers.  For the full language
specification and more formal definitions of terms such as ``identifier'' and
``declaration'', refer to section 5: ``The XDR Language Specification''.

For some data types, more specific examples are included.  A more extensive
example of a data description is in section 6: ``An Example of an XDR Data
Description''.

\subsection{Integer}

An XDR signed integer is a 32-bit datum that encodes an integer in the range
[-2147483648,2147483647].  The integer is represented in two's complement
notation.  The most and least significant bytes are 0 and 3, respectively.
Integers are declared as follows:

\begin{verbatim}
int identifier;

  (MSB)                   (LSB)
+-------+-------+-------+-------+
|byte 0 |byte 1 |byte 2 |byte 3 |                      INTEGER
+-------+-------+-------+-------+
<------------32 bits------------>
\end{verbatim}

\begin{code}
testInteger =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "i" (PlainSpec IntSpec))
              ) (parse "typedef int i;")
             )
\end{code}

\subsection{Unsigned Integer}

An XDR unsigned integer is a 32-bit datum that encodes a nonnegative integer
in the range [0,4294967295].  It is represented by an unsigned binary number
whose most and least significant bytes are 0 and 3, respectively.  An unsigned
integer is declared as follows:

\begin{verbatim}
unsigned int identifier;

  (MSB)                   (LSB)
+-------+-------+-------+-------+
|byte 0 |byte 1 |byte 2 |byte 3 |             UNSIGNED INTEGER
+-------+-------+-------+-------+
<------------32 bits------------>
\end{verbatim}

\begin{code}
testUnsignedInteger =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "ui" (PlainSpec UIntSpec))
              ) (parse "typedef unsigned int ui;")
             )
\end{code}

\subsection{Enumeration}

Enumerations have the same representation as signed integers.  Enumerations
are handy for describing subsets of the integers.  Enumerated data is declared
as follows:

\begin{verbatim}
enum { name-identifier = constant, ... } identifier;
\end{verbatim}

For example, the three colors red, yellow, and blue could be described by an
enumerated type:

\begin{verbatim}
enum { RED = 2, YELLOW = 3, BLUE = 5 } colors;
\end{verbatim}

It is an error to encode as an enum any other integer than those that have
been given assignments in the enum declaration.

\begin{code}
testEnumeration =
    TestCase (assertDef
              (TypeDef (EnumDecl "colors"
                        (EnumSpec [ ConstDecl "RED" (LitExpr 2)
                                  , ConstDecl "YELLOW" (LitExpr 3)
                                  , ConstDecl "BLUE" (LitExpr 5)
                                  ]
                        )
                       )
              ) (parse "enum colors { RED = 2, YELLOW = 3, BLUE = 5 };")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (LitExpr 0)
                                  , ConstDecl "B" (LitExpr 1)
                                  , ConstDecl "C" (LitExpr 2)
                                  ]
                        )
                       )
              ) (parse "enum x { A, B, C };")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (UnExpr NEG (LitExpr 1))
                                  , ConstDecl "B" (LitExpr 0)
                                  , ConstDecl "C" (LitExpr 1)
                                  ]
                        )
                       )
              ) (parse "enum x { A = -1, B, C };")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A"
                                    (NamedExpr (ConstDecl "X" (LitExpr 1)))
                                  , ConstDecl "B" (LitExpr 2)
                                  ]
                        )
                       )
              ) (parse "const X = 1; typedef enum { A = X, B } x;")
             )
\end{code}

\subsection{Boolean}

Booleans are important enough and occur frequently enough to warrant their own
explicit type in the standard.  Booleans are declared as follows:

\begin{verbatim}
bool identifier;
\end{verbatim}

This is equivalent to:

\begin{verbatim}
enum { FALSE = 0, TRUE = 1 } identifier;
\end{verbatim}

\begin{code}
testBoolean =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "b" (PlainSpec BoolSpec))
              ) (parse "typedef bool b;")
             )
\end{code}

\subsection{Hyper Integer and Unsigned Hyper Integer}

The standard also defines 64-bit (8-byte) numbers called hyper integer and
unsigned hyper integer.  Their representations are the obvious extensions of
integer and unsigned integer defined above.

They are represented in two's complement notation.  The most and least
significant bytes are 0 and 7, respectively.  Their declarations:

\begin{verbatim}
hyper identifier; unsigned hyper identifier;

  (MSB)                                                   (LSB)
+-------+-------+-------+-------+-------+-------+-------+-------+
|byte 0 |byte 1 |byte 2 |byte 3 |byte 4 |byte 5 |byte 6 |byte 7 |
+-------+-------+-------+-------+-------+-------+-------+-------+
<----------------------------64 bits---------------------------->
                                           HYPER INTEGER
                                           UNSIGNED HYPER INTEGER
\end{verbatim}

\begin{code}
testHyper =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "l" (PlainSpec HyperSpec))
              ) (parse "typedef hyper l;")
             )
\end{code}

\begin{code}
testUnsignedHyper =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "ul" (PlainSpec UHyperSpec))
              ) (parse "typedef unsigned hyper ul;")
             )
\end{code}

\subsection{Floating-point}

The standard defines the floating-point data type ``float'' (32 bits or 4
bytes).  The encoding used is the IEEE standard for normalized
single-precision floating-point numbers [3].  The following three fields
describe the single-precision floating-point number:

S: The sign of the number.  Values 0 and 1 represent positive and negative,
   respectively.  One bit.

E: The exponent of the number, base 2.  8 bits are devoted to this field.  The
   exponent is biased by 127.

F: The fractional part of the number's mantissa, base 2.  23 bits are devoted
   to this field.

Therefore, the floating-point number is described by:

\begin{verbatim}
(-1)**S * 2**(E-Bias) * 1.F
\end{verbatim}

It is declared as follows:

\begin{verbatim}
float identifier;

+-------+-------+-------+-------+
|byte 0 |byte 1 |byte 2 |byte 3 |              SINGLE-PRECISION
S|   E   |           F          |         FLOATING-POINT NUMBER
+-------+-------+-------+-------+
1|<- 8 ->|<-------23 bits------>|
<------------32 bits------------>
\end{verbatim}

Just as the most and least significant bytes of a number are 0 and 3, the most
and least significant bits of a single-precision floating- point number are 0
and 31.  The beginning bit (and most significant bit) offsets of S, E, and F
are 0, 1, and 9, respectively.  Note that these numbers refer to the
mathematical positions of the bits, and NOT to their actual physical locations
(which vary from medium to medium).

The IEEE specifications should be consulted concerning the encoding for signed
zero, signed infinity (overflow), and denormalized numbers (underflow) [3].
According to IEEE specifications, the ``NaN'' (not a number) is system
dependent and should not be interpreted within XDR as anything other than
``NaN''.

\begin{code}
testFloatingPoint =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "f" (PlainSpec FloatSpec))
              ) (parse "typedef float f;")
             )
\end{code}

\subsection{Double-precision Floating-point}

The standard defines the encoding for the double-precision floating- point
data type ``double'' (64 bits or 8 bytes).  The encoding used is the IEEE
standard for normalized double-precision floating-point numbers [3].  The
standard encodes the following three fields, which describe the
double-precision floating-point number:

S: The sign of the number.  Values 0 and 1 represent positive and negative,
   respectively.  One bit.

E: The exponent of the number, base 2.  11 bits are devoted to this field.
   The exponent is biased by 1023.

F: The fractional part of the number's mantissa, base 2.  52 bits are devoted
   to this field.

Therefore, the floating-point number is described by:

\begin{verbatim}
(-1)**S * 2**(E-Bias) * 1.F
\end{verbatim}

It is declared as follows:

\begin{verbatim}
double identifier;

+------+------+------+------+------+------+------+------+
|byte 0|byte 1|byte 2|byte 3|byte 4|byte 5|byte 6|byte 7|
S|    E   |                    F                        |
+------+------+------+------+------+------+------+------+
1|<--11-->|<-----------------52 bits------------------->|
<-----------------------64 bits------------------------->
                               DOUBLE-PRECISION FLOATING-POINT
\end{verbatim}

Just as the most and least significant bytes of a number are 0 and 3, the most
and least significant bits of a double-precision floating- point number are 0
and 63.  The beginning bit (and most significant bit) offsets of S, E , and F
are 0, 1, and 12, respectively.  Note that these numbers refer to the
mathematical positions of the bits, and NOT to their actual physical locations
(which vary from medium to medium).

The IEEE specifications should be consulted concerning the encoding for signed
zero, signed infinity (overflow), and denormalized numbers (underflow) [3].
According to IEEE specifications, the ``NaN'' (not a number) is system
dependent and should not be interpreted within XDR as anything other than
``NaN''.

\begin{code}
testDoublePrecision =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "d" (PlainSpec DoubleSpec))
              ) (parse "typedef double d;")
             )
\end{code}

\subsection{Quadruple-precision Floating-point}

The standard defines the encoding for the quadruple-precision floating-point
data type ``quadruple'' (128 bits or 16 bytes).  The encoding used is designed
to be a simple analog of of the encoding used for single and double-precision
floating-point numbers using one form of IEEE double extended precision. The
standard encodes the following three fields, which describe the
quadruple-precision floating-point number:

S: The sign of the number.  Values 0 and 1 represent positive and negative,
   respectively.  One bit.

E: The exponent of the number, base 2.  15 bits are devoted to this field.
   The exponent is biased by 16383.

F: The fractional part of the number's mantissa, base 2.  112 bits are devoted
   to this field.

Therefore, the floating-point number is described by:

\begin{verbatim}
(-1)**S * 2**(E-Bias) * 1.F
\end{verbatim}

It is declared as follows:

\begin{verbatim}
quadruple identifier;

+------+------+------+------+------+------+-...--+------+
|byte 0|byte 1|byte 2|byte 3|byte 4|byte 5| ...  |byte15|
S|    E       |                  F                      |
+------+------+------+------+------+------+-...--+------+
1|<----15---->|<-------------112 bits------------------>|
<-----------------------128 bits------------------------>
                             QUADRUPLE-PRECISION FLOATING-POINT
\end{verbatim}

Just as the most and least significant bytes of a number are 0 and 3, the most
and least significant bits of a quadruple-precision floating-point number are
0 and 127.  The beginning bit (and most significant bit) offsets of S, E , and
F are 0, 1, and 16, respectively.  Note that these numbers refer to the
mathematical positions of the bits, and NOT to their actual physical locations
(which vary from medium to medium).

The encoding for signed zero, signed infinity (overflow), and denormalized
numbers are analogs of the corresponding encodings for single and
double-precision floating-point numbers [5], [6].  The ``NaN'' encoding as it
applies to quadruple-precision floating-point numbers is system dependent and
should not be interpreted within XDR as anything other than ``NaN''.

\begin{code}
testQuadruple =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "q" (PlainSpec QuadrupleSpec))
              ) (parse "typedef quadruple q;")
             )
\end{code}

\subsection{Fixed-length Opaque Data}

At times, fixed-length uninterpreted data needs to be passed among machines.
This data is called ``opaque'' and is declared as follows:

\begin{verbatim}
opaque identifier[n];
\end{verbatim}

where the constant n is the (static) number of bytes necessary to contain the
opaque data.  If n is not a multiple of four, then the n bytes are followed by
enough (0 to 3) residual zero bytes, r, to make the total byte count of the
opaque object a multiple of four.

\begin{verbatim}
    0        1     ...
+--------+--------+...+--------+--------+...+--------+
| byte 0 | byte 1 |...|byte n-1|    0   |...|    0   |
+--------+--------+...+--------+--------+...+--------+
|<-----------n bytes---------->|<------r bytes------>|
|<-----------n+r (where (n+r) mod 4 = 0)------------>|
                                                FIXED-LENGTH OPAQUE
\end{verbatim}

\begin{code}
testFixedOpaque =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "o" (OpaqueSpec (LitExpr 8192)))
              ) (parse "typedef opaque o[8192];")
             )
\end{code}

\subsection{Variable-length Opaque Data}

The standard also provides for variable-length (counted) opaque data, defined
as a sequence of n (numbered 0 through n-1) arbitrary bytes to be the number n
encoded as an unsigned integer (as described below), and followed by the n
bytes of the sequence.

Byte m of the sequence always precedes byte m+1 of the sequence, and byte 0 of
the sequence always follows the sequence's length (count).  If n is not a
multiple of four, then the n bytes are followed by enough (0 to 3) residual
zero bytes, r, to make the total byte count a multiple of four.
Variable-length opaque data is declared in the following way:

\begin{verbatim}
opaque identifier<m>;
\end{verbatim}
or
\begin{verbatim}
opaque identifier<>;
\end{verbatim}

The constant m denotes an upper bound of the number of bytes that the sequence
may contain.  If m is not specified, as in the second declaration, it is
assumed to be (2**32) - 1, the maximum length.  The constant m would normally
be found in a protocol specification.  For example, a filing protocol may
state that the maximum data transfer size is 8192 bytes, as follows:

\begin{verbatim}
opaque filedata<8192>;

   0     1     2     3     4     5   ...
+-----+-----+-----+-----+-----+-----+...+-----+-----+...+-----+
|        length n       |byte0|byte1|...| n-1 |  0  |...|  0  |
+-----+-----+-----+-----+-----+-----+...+-----+-----+...+-----+
|<-------4 bytes------->|<------n bytes------>|<---r bytes--->|
                        |<----n+r (where (n+r) mod 4 = 0)---->|
                                         VARIABLE-LENGTH OPAQUE
\end{verbatim}

It is an error to encode a length greater than the maximum described in the
specification.

\begin{code}
testVariableOpaque =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "o" (VarOpaqueSpec Nothing))
              ) (parse "typedef opaque o<>;")

              >> assertDef
              (TypeDef
               (SimpleDecl "o" (VarOpaqueSpec (Just (LitExpr 8192))))
              ) (parse "typedef opaque o<8192>;")
             )
\end{code}

\subsection{String}

The standard defines a string of n (numbered 0 through n-1) ASCII bytes to be
the number n encoded as an unsigned integer (as described above), and followed
by the n bytes of the string.  Byte m of the string always precedes byte m+1
of the string, and byte 0 of the string always follows the string's length.
If n is not a multiple of four, then the n bytes are followed by enough (0 to
3) residual zero bytes, r, to make the total byte count a multiple of four.
Counted byte strings are declared as follows:

\begin{verbatim}
string object<m>;
\end{verbatim}
or
\begin{verbatim}
string object<>;
\end{verbatim}

The constant m denotes an upper bound of the number of bytes that a string may
contain.  If m is not specified, as in the second declaration, it is assumed
to be (2**32) - 1, the maximum length.  The constant m would normally be found
in a protocol specification.  For example, a filing protocol may state that a
file name can be no longer than 255 bytes, as follows:

\begin{verbatim}
string filename<255>;

   0     1     2     3     4     5   ...
+-----+-----+-----+-----+-----+-----+...+-----+-----+...+-----+
|        length n       |byte0|byte1|...| n-1 |  0  |...|  0  |
+-----+-----+-----+-----+-----+-----+...+-----+-----+...+-----+
|<-------4 bytes------->|<------n bytes------>|<---r bytes--->|
                        |<----n+r (where (n+r) mod 4 = 0)---->|
                                                         STRING
\end{verbatim}

It is an error to encode a length greater than the maximum described in the
specification.

\begin{code}
testString =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "s" (StringSpec Nothing))
              ) (parse "typedef string s<>;")

              >> assertDef
              (TypeDef
               (SimpleDecl "s" (StringSpec (Just (LitExpr 255))))
              ) (parse "typedef string s<255>;")
             )
\end{code}

subsection{Fixed-length Array}

Declarations for fixed-length arrays of homogeneous elements are in the
following form:

\begin{verbatim}
type-name identifier[n];
\end{verbatim}

Fixed-length arrays of elements numbered 0 through n-1 are encoded by
individually encoding the elements of the array in their natural order, 0
through n-1.  Each element's size is a multiple of four bytes. Though all
elements are of the same type, the elements may have different sizes.  For
example, in a fixed-length array of strings, all elements are of type
``string'', yet each element will vary in its length.

\begin{verbatim}
+---+---+---+---+---+---+---+---+...+---+---+---+---+
|   element 0   |   element 1   |...|  element n-1  |
+---+---+---+---+---+---+---+---+...+---+---+---+---+
|<--------------------n elements------------------->|

                                      FIXED-LENGTH ARRAY
\end{verbatim}

\begin{code}
testFixedArray =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "a" (ArraySpec IntSpec (LitExpr 255)))
              ) (parse "typedef int a[255];")
             )
\end{code}

\subsection{Variable-length Array}

Counted arrays provide the ability to encode variable-length arrays of
homogeneous elements.  The array is encoded as the element count n (an
unsigned integer) followed by the encoding of each of the array's elements,
starting with element 0 and progressing through element n- 1.  The declaration
for variable-length arrays follows this form:

\begin{verbatim}
type-name identifier<m>;
\end{verbatim}
or
\begin{verbatim}
type-name identifier<>;
\end{verbatim}

The constant m specifies the maximum acceptable element count of an array; if
m is not specified, as in the second declaration, it is assumed to be (2**32)
- 1.

\begin{verbatim}
  0  1  2  3
+--+--+--+--+--+--+--+--+--+--+--+--+...+--+--+--+--+
|     n     | element 0 | element 1 |...|element n-1|
+--+--+--+--+--+--+--+--+--+--+--+--+...+--+--+--+--+
|<-4 bytes->|<--------------n elements------------->|
                                                COUNTED ARRAY
\end{verbatim}

It is an error to encode a value of n that is greater than the maximum
described in the specification.

\begin{code}
testVariableArray =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "a" (VarArraySpec IntSpec Nothing))
              ) (parse "typedef int a<>;")

              >> assertDef
              (TypeDef
               (SimpleDecl "a" (VarArraySpec IntSpec (Just (LitExpr 255))))
              ) (parse "typedef int a<255>;")
             )
\end{code}

\subsection{Structure}

Structures are declared as follows:

\begin{verbatim}
struct {
   component-declaration-A;
   component-declaration-B;
   ...
} identifier;
\end{verbatim}

The components of the structure are encoded in the order of their declaration
in the structure.  Each component's size is a multiple of four bytes, though
the components may be different sizes.

\begin{verbatim}
+-------------+-------------+...
| component A | component B |...                      STRUCTURE
+-------------+-------------+...
\end{verbatim}n

\begin{code}
testStructure =
    TestCase (assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)
                                    , SimpleDecl "j" (PlainSpec IntSpec)
                                    ]
                        )
                       )
              ) (parse "struct x { int i; int j; };")
             )
\end{code}

\subsection{Discriminated Union}

A discriminated union is a type composed of a discriminant followed by a type
selected from a set of prearranged types according to the value of the
discriminant.  The type of discriminant is either ``int'', ``unsigned int'',
or an enumerated type, such as ``bool''.  The component types are called
``arms'' of the union, and are preceded by the value of the discriminant which
implies their encoding.  Discriminated unions are declared as follows:

\begin{verbatim}
union switch (discriminant-declaration) {
case discriminant-value-A:
   arm-declaration-A;
case discriminant-value-B:
   arm-declaration-B;
...
default: default-declaration;
} identifier;
\end{verbatim}

Each ``case'' keyword is followed by a legal value of the discriminant.  The
default arm is optional.  If it is not specified, then a valid encoding of the
union cannot take on unspecified discriminant values.  The size of the implied
arm is always a multiple of four bytes.

The discriminated union is encoded as its discriminant followed by the
encoding of the implied arm.

\begin{verbatim}
  0   1   2   3
+---+---+---+---+---+---+---+---+
|  discriminant |  implied arm  |          DISCRIMINATED UNION
+---+---+---+---+---+---+---+---+
|<---4 bytes--->|
\end{verbatim}

\begin{code}
testDiscriminatedUnion =
    TestCase (assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         ] Nothing
                        )
                       )
              ) (parse "union x switch (bool opt) { case TRUE: int i; };")

              >>assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         ] (Just
                            (DeclArm (SimpleDecl "s" (StringSpec Nothing))))
                        )
                       )
              ) (parse "union x switch (bool opt) { case TRUE: int i; default: string s<>; };")
             )
\end{code}

\subsection{Void}

An XDR void is a 0-byte quantity.  Voids are useful for describing operations
that take no data as input or no data as output. They are also useful in
unions, where some arms may contain data and others do not.  The declaration
is simply as follows:

\begin{verbatim}
void;
\end{verbatim}

Voids are illustrated as follows:

\begin{verbatim}
  ++
  ||                                                     VOID
  ++
--><-- 0 bytes
\end{verbatim}

\begin{code}
testVoid =
    TestCase (assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         , (LitExpr 0, VoidArm)
                         ] Nothing
                        )
                       )
              ) (parse "union x switch (bool opt) { case TRUE: int i; case FALSE: void; };")
             )
\end{code}

\subsection{Constant}

The data declaration for a constant follows this form:

\begin{verbatim}
const name-identifier = n;
\end{verbatim}

``const'' is used to define a symbolic name for a constant; it does not
declare any data.  The symbolic constant may be used anywhere a regular
constant may be used.  For example, the following defines a symbolic constant
DOZEN, equal to 12.

\begin{verbatim}
const DOZEN = 12;
\end{verbatim}

\begin{code}
testConstant =
    TestCase (assertDef
              (ConstDef (ConstDecl "DOZEN" (LitExpr 12)))
              (parse "const DOZEN = 12;")
             )
\end{code}

\subsection{Typedef}

``typedef'' does not declare any data either, but serves to define new
identifiers for declaring data. The syntax is:

\begin{verbatim}
typedef declaration;
\end{verbatim}

The new type name is actually the variable name in the declaration part of the
typedef.  For example, the following defines a new type called ``eggbox''
using an existing type called ``egg'':

\begin{verbatim}
typedef egg eggbox[DOZEN];
\end{verbatim}

Variables declared using the new type name have the same type as the new type
name would have in the typedef, if it was considered a variable.  For example,
the following two declarations are equivalent in declaring the variable
``fresheggs'':

\begin{verbatim}
eggbox  fresheggs; egg     fresheggs[DOZEN];
\end{verbatim}

When a typedef involves a struct, enum, or union definition, there is another
(preferred) syntax that may be used to define the same type.  In general, a
typedef of the following form:

\begin{verbatim}
typedef <<struct, union, or enum definition>> identifier;
\end{verbatim}

may be converted to the alternative form by removing the ``typedef'' part and
placing the identifier after the ``struct'', ``union'', or ``enum'' keyword, instead
of at the end.  For example, here are the two ways to define the type ``bool'':

\begin{verbatim}
typedef enum {    /* using typedef */
   FALSE = 0,
   TRUE = 1
} bool;

enum bool {       /* preferred alternative */
   FALSE = 0,
   TRUE = 1
};
\end{verbatim}

The reason this syntax is preferred is one does not have to wait until the end
of a declaration to figure out the name of the new type.

\begin{code}
testTypedef =
    TestCase (assertDef
              (TypeDef (SimpleDecl "eggbox"
                        (ArraySpec (NamedSpec "egg")
                         (NamedExpr (ConstDecl "DOZEN" (LitExpr 12)))
                        )
                       )
              ) (parse "const DOZEN = 12; typedef egg eggbox[DOZEN];")

              >> assertDef
              (TypeDef (EnumDecl "x"
                        (EnumSpec [ ConstDecl "A" (LitExpr 255)
                                  , ConstDecl "B" (LitExpr 256)
                                  ]
                        )
                       )
              ) (parse "typedef enum { A = 0xff, B } x;")

              >> assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)])
                       )
              ) (parse "typedef struct { int i; } x;")

              >> assertDef
              (TypeDef (UnionDecl "x"
                        (UnionSpec (UnionDis "opt" BoolSpec)
                         [ (LitExpr 1,
                            DeclArm (SimpleDecl "i" (PlainSpec IntSpec)))
                         ] Nothing
                        )
                       )
              ) (parse "typedef union switch (bool opt) { case TRUE: int i; } x;")
             )
\end{code}

\subsection{Optional-data}

Optional-data is one kind of union that occurs so frequently that we give it a
special syntax of its own for declaring it.  It is declared as follows:

\begin{verbatim}
type-name *identifier;
\end{verbatim}

This is equivalent to the following union:

\begin{verbatim}
union switch (bool opted) {
case TRUE:
   type-name element;
case FALSE:
   void;
} identifier;
\end{verbatim}

It is also equivalent to the following variable-length array declaration,
since the boolean ``opted'' can be interpreted as the length of the array:

\begin{verbatim}
type-name identifier<1>;
\end{verbatim}

Optional-data is not so interesting in itself, but it is very useful for
describing recursive data-structures such as linked-lists and trees.  For
example, the following defines a type ``stringlist'' that encodes lists of
arbitrary length strings:

\begin{verbatim}
struct *stringlist {
   string item<>;
   stringlist next;
};
\end{verbatim}

It could have been equivalently declared as the following union:

\begin{verbatim}
union stringlist switch (bool opted) {
case TRUE:
   struct {
      string item<>;
      stringlist next;
   } element;
case FALSE:
   void;
};
\end{verbatim}

or as a variable-length array:

\begin{verbatim}
struct stringlist<1> {
   string item<>;
   stringlist next;
};
\end{verbatim}

Both of these declarations obscure the intention of the stringlist type, so
the optional-data declaration is preferred over both of them.  The
optional-data type also has a close correlation to how recursive data
structures are represented in high-level languages such as Pascal or C by use
of pointers. In fact, the syntax is the same as that of the C language for
pointers.

\begin{code}
testOptional =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "p" (PointerSpec IntSpec))
              ) (parse "typedef int* p;")
             )
\end{code}

\subsection{Areas for Future Enhancement}

The XDR standard lacks representations for bit fields and bitmaps, since the
standard is based on bytes.  Also missing are packed (or binary-coded)
decimals.

The intent of the XDR standard was not to describe every kind of data that
people have ever sent or will ever want to send from machine to
machine. Rather, it only describes the most commonly used data-types of
high-level languages such as Pascal or C so that applications written in these
languages will be able to communicate easily over some medium.

One could imagine extensions to XDR that would let it describe almost any
existing protocol, such as TCP.  The minimum necessary for this are support
for different block sizes and byte-orders.  The XDR discussed here could then
be considered the 4-byte big-endian member of a larger XDR family.

\section{DISCUSSION}

(1) Why use a language for describing data?  What's wrong with diagrams?

There are many advantages in using a data-description language such as XDR
versus using diagrams.  Languages are more formal than diagrams and lead to
less ambiguous descriptions of data.  Languages are also easier to understand
and allow one to think of other issues instead of the low-level details of
bit-encoding.  Also, there is a close analogy between the types of XDR and a
high-level language such as C or Pascal.  This makes the implementation of XDR
encoding and decoding modules an easier task.  Finally, the language
specification itself is an ASCII string that can be passed from machine to
machine to perform on-the-fly data interpretation.

(2) Why is there only one byte-order for an XDR unit?

Supporting two byte-orderings requires a higher level protocol for determining
in which byte-order the data is encoded.  Since XDR is not a protocol, this
can't be done.  The advantage of this, though, is that data in XDR format can
be written to a magnetic tape, for example, and any machine will be able to
interpret it, since no higher level protocol is necessary for determining the
byte-order.

(3) Why is the XDR byte-order big-endian instead of little-endian?  Isn't this
unfair to little-endian machines such as the VAX(r), which has to convert from
one form to the other?

Yes, it is unfair, but having only one byte-order means you have to be unfair
to somebody.  Many architectures, such as the Motorola 68000* and IBM 370*,
support the big-endian byte-order.

(4) Why is the XDR unit four bytes wide?

There is a tradeoff in choosing the XDR unit size.  Choosing a small size such
as two makes the encoded data small, but causes alignment problems for
machines that aren't aligned on these boundaries.  A large size such as eight
means the data will be aligned on virtually every machine, but causes the
encoded data to grow too big.  We chose four as a compromise.  Four is big
enough to support most architectures efficiently, except for rare machines
such as the eight-byte aligned Cray*.  Four is also small enough to keep the
encoded data restricted to a reasonable size.

(5) Why must variable-length data be padded with zeros?

It is desirable that the same data encode into the same thing on all machines,
so that encoded data can be meaningfully compared or checksummed.  Forcing the
padded bytes to be zero ensures this.

(6) Why is there no explicit data-typing?

Data-typing has a relatively high cost for what small advantages it may have.
One cost is the expansion of data due to the inserted type fields.  Another is
the added cost of interpreting these type fields and acting accordingly.  And
most protocols already know what type they expect, so data-typing supplies
only redundant information.  However, one can still get the benefits of
data-typing using XDR. One way is to encode two things: first a string which
is the XDR data description of the encoded data, and then the encoded data
itself.  Another way is to assign a value to all the types in XDR, and then
define a universal type which takes this value as its discriminant and for
each value, describes the corresponding data type.

\section{THE XDR LANGUAGE SPECIFICATION}

\subsection{Notational Conventions}

This specification uses an extended Back-Naur Form notation for describing the
XDR language.  Here is a brief description of the notation:

(1) The characters `$\mid$', `(', `)', `[', `]', `"', and `*' are special.
(2) Terminal symbols are strings of any characters surrounded by double
quotes.  (3) Non-terminal symbols are strings of non-special characters.  (4)
Alternative items are separated by a vertical bar (`$\mid$').  (5) Optional
items are enclosed in brackets.  (6) Items are grouped together by enclosing
them in parentheses.  (7) A `*' following an item means 0 or more occurrences
of that item.

For example, consider the following pattern:

\begin{verbatim}
"a " "very" (", " "very")* [" cold " "and "]  " rainy "
("day" | "night")
\end{verbatim}

An infinite number of strings match this pattern. A few of them are:

\begin{verbatim}
"a very rainy day"
"a very, very rainy day"
"a very cold and  rainy day"
"a very, very, very cold and  rainy night"
\end{verbatim}

\subsection{Lexical Notes}

(1) Comments begin with `/*' and terminate with `*/'.

\begin{code}
testComment =
    TestCase (assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)])
                       )
              ) (parse "/*1*/ struct /*2*/ x /*3*/ { /*4*/ int /*5*/ i; /*6*/ }; /*7*/")
             )
\end{code}

(2) White space serves to separate items and is otherwise ignored.

\begin{code}
testWhitespace =
    TestCase (assertDef
              (TypeDef (StructDecl "x"
                        (StructSpec [ SimpleDecl "i" (PlainSpec IntSpec)])
                       )
              ) (parse "struct x{int i;};")
             )
\end{code}

(3) An identifier is a letter followed by an optional sequence of letters,
digits or underbar (`\_'). The case of identifiers is not ignored.

\begin{code}
testIdentifier =
    TestCase (assertDef
              (TypeDef
               (SimpleDecl "a1_" (PlainSpec IntSpec))
              ) (parse "typedef int a1_;")

              >> assertErr (parse "typedef int _a1;")
              >> assertErr (parse "typedef int 1_a;")
             )
\end{code}

(4) A constant is a sequence of one or more decimal digits, optionally
preceded by a minus-sign (`-').

\begin{code}
testDecimal =
    TestCase (assertDef
              (ConstDef (ConstDecl "DOZEN" (LitExpr 12)))
              (parse "const DOZEN = 12;")

              >> assertDef
              (ConstDef (ConstDecl "X" (LitExpr 255)))
              (parse "const X = 255;")
             )
\end{code}

\begin{code}
testHexadecimal =
    TestCase (assertDef
              (ConstDef (ConstDecl "X" (LitExpr 255)))
              (parse "const X = 0xff;")
             )
\end{code}

\begin{code}
testMultiplication =
    TestCase (assertEval 77 "7 * 11"
              >> assertEval 3 "33 / 11"
              >> assertEval 4 "11 % 7"
             )
\end{code}

\begin{code}
testAddition =
    TestCase (assertEval 18 "7 + 11"
              >> assertEval (-6) "7 - 13"
             )
\end{code}

\begin{code}
testShift =
    TestCase (assertEval 64 "16 << 2"
              >> assertEval 4 "16 >> 2"
             )
\end{code}

\begin{code}
testLogical =
    TestCase (assertEval 15 "0xfff & 0x00f"
              >> assertEval 15 "0xff0 ^ 0xfff"
              >> assertEval 15 "0x00f | 0x00f"
             )
\end{code}

\begin{code}
testUnary =
    TestCase (assertEval (-1) "-1"
              >> assertEval (-1) "~0"
             )
\end{code}

\begin{code}
testDistributive =
    TestCase (assertEval 14 "2 * (3 + 4)"
              >> assertEval 14 "2 * 3 + 2 * 4"
             )
\end{code}

\subsection{Syntax Information}

\begin{verbatim}
declaration:
     type-specifier identifier
   | type-specifier identifier "[" value "]"
   | type-specifier identifier "<" [ value ] ">"
   | "opaque" identifier "[" value "]"
   | "opaque" identifier "<" [ value ] ">"
   | "string" identifier "<" [ value ] ">"
   | type-specifier "*" identifier
   | "void"
\end{verbatim}

\begin{verbatim}
value:
     constant
   | identifier
\end{verbatim}

\begin{verbatim}
type-specifier:
     [ "unsigned" ] "int"
   | [ "unsigned" ] "hyper"
   | "float"
   | "double"
   | "quadruple"
   | "bool"
   | enum-type-spec
   | struct-type-spec
   | union-type-spec
   | identifier
\end{verbatim}

\begin{verbatim}
enum-type-spec:
   "enum" enum-body
\end{verbatim}

\begin{verbatim}
enum-body:
   "{"
      ( identifier "=" value )
      ( "," identifier "=" value )*
   "}"
\end{verbatim}

\begin{verbatim}
struct-type-spec:
   "struct" struct-body
\end{verbatim}

\begin{verbatim}
struct-body:
   "{"
      ( declaration ";" )
      ( declaration ";" )*
   "}"
\end{verbatim}

\begin{verbatim}
union-type-spec:
   "union" union-body
\end{verbatim}

\begin{verbatim}
union-body:
   "switch" "(" declaration ")" "{"
      ( "case" value ":" declaration ";" )
      ( "case" value ":" declaration ";" )*
      [ "default" ":" declaration ";" ]
   "}"
\end{verbatim}

\begin{verbatim}
constant-def:
   "const" identifier "=" constant ";"
\end{verbatim}

\begin{verbatim}
type-def:
     "typedef" declaration ";"
   | "enum" identifier enum-body ";"
   | "struct" identifier struct-body ";"
   | "union" identifier union-body ";"
\end{verbatim}

\begin{verbatim}
definition:
     type-def
   | constant-def
\end{verbatim}

\begin{verbatim}
specification:
     definition *
\end{verbatim}

\begin{code}
testEmptyBody =
    TestCase (assertErr (parse "enum x { };")
              >> assertErr (parse "struct x { };")
              >> assertErr (parse "union x switch (bool opt) { };")
             )
\end{code}

\subsection{Syntax Notes}

(1) The following are keywords and cannot be used as identifiers: ``bool'',
``case'', ``const'', ``default'', ``double'', ``quadruple'', ``enum'',
``float'', ``hyper'', ``opaque'', ``string'', ``struct'', ``switch'',
``typedef'', ``union'', ``unsigned'' and ``void''.

(2) Only unsigned constants may be used as size specifications for arrays.  If
an identifier is used, it must have been declared previously as an unsigned
constant in a ``const'' definition.

(3) Constant and type identifiers within the scope of a specification are in
the same name space and must be declared uniquely within this scope.

(4) Similarly, variable names must be unique within the scope of struct and
union declarations. Nested struct and union declarations create new scopes.

(5) The discriminant of a union must be of a type that evaluates to an
integer. That is, ``int'', ``unsigned int'', ``bool'', an enumerated type or
any typedefed type that evaluates to one of these is legal.  Also, the case
values must be one of the legal values of the discriminant.  Finally, a case
value may not be specified more than once within the scope of a union
declaration.

\section{AN EXAMPLE OF AN XDR DATA DESCRIPTION}

Here is a short XDR data description of a thing called a ``file'', which might
be used to transfer files from one machine to another.

\begin{verbatim}
const MAXUSERNAME = 32;     /* max length of a user name */
const MAXFILELEN = 65535;   /* max length of a file      */
const MAXNAMELEN = 255;     /* max length of a file name */

/*
 * Types of files:
 */
enum filekind {
   TEXT = 0,       /* ascii data */
   DATA = 1,       /* raw data   */
   EXEC = 2        /* executable */
};

/*
 * File information, per kind of file:
 */
union filetype switch (filekind kind) {
case TEXT:
   void;                           /* no extra information */
case DATA:
   string creator<MAXNAMELEN>;     /* data creator         */
case EXEC:
   string interpretor<MAXNAMELEN>; /* program interpretor  */
};

/*
 * A complete file:
 */
struct file {
   string filename<MAXNAMELEN>; /* name of file    */
   filetype type;               /* info about file */
   string owner<MAXUSERNAME>;   /* owner of file   */
   opaque data<MAXFILELEN>;     /* file data       */
};
\end{verbatim}

Suppose now that there is a user named ``john" who wants to store his lisp
program ``sillyprog" that contains just the data ``(quit)".  His file would be
encoded as follows:

\begin{verbatim}
OFFSET  HEX BYTES       ASCII    COMMENTS
------  ---------       -----    --------
 0      00 00 00 09     ....     -- length of filename = 9
 4      73 69 6c 6c     sill     -- filename characters
 8      79 70 72 6f     ypro     -- ... and more characters ...
12      67 00 00 00     g...     -- ... and 3 zero-bytes of fill
16      00 00 00 02     ....     -- filekind is EXEC = 2
20      00 00 00 04     ....     -- length of interpretor = 4
24      6c 69 73 70     lisp     -- interpretor characters
28      00 00 00 04     ....     -- length of owner = 4
32      6a 6f 68 6e     john     -- owner characters
36      00 00 00 06     ....     -- length of file data = 6
40      28 71 75 69     (qui     -- file data bytes ...
44      74 29 00 00     t)..     -- ... and 2 zero-bytes of fill
\end{verbatim}

\end{document}
