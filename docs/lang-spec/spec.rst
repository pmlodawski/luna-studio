*******************************************
The Luna programming language specification
*******************************************

:revision: 1
:date: 2014/04/16

:copyright: Flowbox sp. z o.o. All rights reserved.


.. contents:: Table of Contents


Introduction
============

Welcome to the Luna language reference guide.

Luna is a general-purpose language designed with efficient and easy data processing in mind. It is compiled, statically-typed and multi-paradigm. Luna is designed to fully utilize the power of modern hardware thanks to its concurrent nature and lightweight threading model. Type inference and concise syntax allow writing clean and elegant programs, while maintaining all the benefits of the static type system, like compile-time error detection and optimization opportunities.

The distinctive feature of Luna is its dual textual & visual representation model. Every program in the text form has its equivalent representation as a visual dataflow graph and vice-versa. Both forms are interchangeable and can be developed simultaneously. The visual dataflow graph bridges the communication gap between technical and non-technical team members.

The grammar of Luna is compact and regular, allowing for easy analysis by automatic tools such as integrated development environments.

For more information – and other documents – visit http://flowbox.io.



Notation
========

The syntax specified using Extended Backus-Naur Form (EBNF):

.. _EBNF Production:
.. _EBNF Expression:
.. _EBNF Alternative:
.. _EBNF Term:
.. _EBNF Group:
.. _EBNF Option:
.. _EBNF Repetition:

.. code-block:: EBNF

    Production  = Production-Name "=" [ Expression ] "." ;
    Expression  = Alternative { "|" Alternative } ;
    Alternative = Term { Term } ;
    Term        = Production-Name | Token [ "…" Token ] | Group | Option | Repetition ;
    Group       = "(" Expression ")" ;
    Option      = "[" Expression "]" ;
    Repetition  = "{" Expression "}" ;

Productions are expressions constructed from terms and the following operators, in increasing precedence:

.. code-block:: text

    |   alternation
    ()  grouping
    []  option (0 or 1 times)
    {}  repetition (0 to n times)

Lower-case production names are used to identify lexical tokens. Non-terminals are in CamelCase. Lexical tokens are enclosed in double quotes ``""`` or back quotes ``````.

The form "``a … b``" represents the set of characters from ``a`` through ``b`` as alternatives. To conform with EBNF, this is written formally as "``a ?…? b``". The horizontal ellipsis "``…``" is also used elsewhere in the specification to informally denote various enumerations or code snippets that are not further specified. The character "``…``" (as opposed to the three characters "``...``") is not a token of the Luna language.



Source code representation
==========================

Source code is Unicode text encoded in UTF-8_. The text is not canonicalized, so a single accented code point is distinct from the same character constructed from combining an accent and a letter; those are treated as two code points. For simplicity, this document will use the unqualified term character to refer to a Unicode code point in the source text.

.. _UTF-8: http://en.wikipedia.org/wiki/UTF-8

Each code point is distinct; for instance, upper and lower case letters are different characters.

The compiler disallows the NUL character (``U+0000``) in the source text. Moreover, `byte-order mark`_ (``U+FEFF``) at the beginning of a file are ignored and should not appear elsewhere.

.. _byte-order mark: https://en.wikipedia.org/wiki/Byte_order_mark


Characters, letters and digits
------------------------------

The following terms are used to denote specific Unicode character classes and numbers. Take a note that the underscore character ``_`` (``U+005F``) is considered a lowercase letter.

.. _EBNF Newline:
.. _EBNF Char:
.. _EBNF Digit:
.. _EBNF Letter-Uppercase:
.. _EBNF Letter-Lowercase:
.. _EBNF Letter:
.. _EBNF Decimal-Digit:
.. _EBNF Binary-Digit:
.. _EBNF Octal-Digit:
.. _EBNF Hex-Digit:

.. code-block:: EBNF

    Newline          = (* the Unicode code point U+000A *) ;
    Char             = (* an arbitrary Unicode code point except newline *) ;
    Digit            = (* a Unicode code point classified as "Decimal Digit" *) ;
    Letter-Uppercase = (* a Unicode code point classified as "Uppercase Letter" *) ;
    Letter-Lowercase = "_" | (* a Unicode code point classified as "Lowercase Letter" *) ;
    Letter           = Letter-Uppercase | Letter-Lowercase ;
    Decimal-Digit    = "0" ?…? "9" ;
    Binary-Digit     = "0"  |  "1" ;
    Octal-Digit      = "0" ?…? "7" ;
    Hex-Digit        = "0" ?…? "9" | "A" ?…? "F" | "a" ?…? "f" ;



Lexical elements
================

Comments
--------

There are two forms of comments:

line comments
    start with the character ``#`` and stop at the end of the line. A line comment acts like a newline.

general comment
    start with the general comment begin character sequence ``#[`` and continue through the general comment end character sequence ``#]``. A general comment containing one or more newlines acts like a newline, otherwise it acts like a space. They can be nested.

Note that both of them start with the same character: ``#``. If ``#[`` cannot be matched with appropriate ``#]``, it is considered a *line comment*.

.. _EBNF Comment:

.. code-block:: EBNF

    Comment = "#"  [ Letter | "#"     ] Newline
            | "#[" [ Letter | Newline ] "#]" ;


Tokens
------

Tokens form the vocabulary of the Luna language. There are four classes: *identifiers*, *keywords*, *operators & delimiters*, and *literals*.

White space, formed from spaces (``U+0020``), horizontal tabs (``U+0009``), carriage returns (``U+000D``), and newlines (``U+000A``), serves two purposes: to mark code blocks via same-indent (like in Python), and to separate tokens that would otherwise combine into a single token.


Semicolons
----------

The formal grammar uses semicolons ``;`` as terminators in a number of productions. Luna programs may omit all of these semicolons using the following two rules:

1. When the input is broken into tokens, a semicolon is automatically inserted into the token stream at the end of a non-blank line if the line's final token is:

    * an identifier
    * an integer, floating-point, imaginary, rune, or string literal
    * one of the keywords break, continue, fallthrough, or return
    * one of the operators and delimiters ``++``, ``--``, ``)``, ``]``, or ``}``
      
2. To allow complex statements to occupy a single line, a semicolon may be omitted before a closing "``)``" or "``}``".

To reflect idiomatic use, code examples in this document elide semicolons using these rules.


Values and types
----------------

An expression evaluates to a value and has a static type. Values and types are separated in Luna. Every program must be well-typed to compile successfully.


Identifiers
-----------

Identifiers name program entities such as variables and functions. An identifier is a sequence of one or more letters and digits.

.. _EBNF Identifier:

.. code-block:: EBNF

    Identifier = Letter-Lowercase { Letter | Digit } ;


Type identifiers
----------------

Type identifiers name such entities as classes and modules. All user-defined types must begin with an uppercase letter.

.. _EBNF Type-Identifier:

.. code-block:: EBNF

    Type-Identifier = Letter-Uppercase { Letter | Digit } ;


Keywords
--------

The following keywords are reserved and may not be used as identifiers.

.. code-block:: text

    case
    class
    def
    else
    if
    interface
    import


Operators and Delimiters
------------------------

Any sequence of following characters is considered an operator in Luna. Operators act just like normal functions, but they are implicitly infix.

.. code-block:: text

    + & ! - | < > * / % ^ $


Integer literals
----------------

An integer literal is a sequence of digits representing an integer constant. An optional prefix sets a non-decimal base: ``0`` for octal, ``0x`` or ``0X`` for hexadecimal. In hexadecimal literals, letters ``a…f`` and ``A…F`` represent values ``10`` through ``15``.

.. _EBNF Int-Lit:
.. _EBNF Bin-Lit:
.. _EBNF Decimal-Lit:
.. _EBNF Octal-Lit:
.. _EBNF Hex-Lit:

.. code-block:: EBNF

    Int-Lit     = Decimal-Lit | Octal-Lit | Hex-Lit ;
    Bin-Lit     = "0" ( "b" | "B" ) Bin-digit { Binary-Digit } ;
    Decimal-Lit = ( "1" ?…? "9" ) { Decimal-Digit } ;
    Octal-Lit   = "0" { Octal-Digit } ;
    Hex-Lit     = "0" ( "x" | "X" ) Hex-Digit { Hex-Digit } ;


Floating-point literals
-----------------------

A floating-point literal is a decimal representation of a floating-point constant. It has an integer part, a decimal point, a fractional part, and an exponent part. The integer and fractional part comprise decimal digits; the exponent part is an ``e`` or ``E`` followed by an optionally signed decimal exponent. One of the integer part or the fractional part may be elided; one of the decimal point or the exponent may be elided.

.. _EBNF Float-Lit:
.. _EBNF Decimals:
.. _EBNF Exponent:

.. code-block:: EBNF

    Float-Lit = Decimals "." [ Decimals ] [ Exponent ] |
                Decimals Exponent | "." Decimals [ Exponent ] ;
    Decimals  = Decimal-Digit { Decimal-Digit } ;
    Exponent  = ( "e" | "E" ) [ "+" | "-" ] Decimals ;

.. warning:: Czy osobno double/float?


Character literals
------------------

A character literal represents a Unicode code point. A *rune literal* is expressed as one or more characters enclosed in single quotes. Within the quotes, any character may appear except single quote and newline. A single quoted character represents the Unicode value of the character itself, while multi-character sequences beginning with a backslash encode values in various formats.

The simplest form represents the single character within the quotes; since Luna source text is Unicode characters encoded in UTF-8, multiple UTF-8-encoded bytes may represent a single integer value. For instance, the literal ``'a'`` holds a single byte representing a literal ``a``, Unicode ``U+0061``, value ``0x61``, while ``'ä'`` holds two bytes (``0xc3`` ``0xa4``) representing a literal *a-dieresis*, ``U+00E4``, value ``0xe4``.

Several backslash escapes allow arbitrary values to be encoded as ASCII text. There are four ways to represent the integer value as a numeric constant: ``\x`` followed by exactly two hexadecimal digits; ``\u`` followed by exactly four hexadecimal digits; ``\U`` followed by exactly eight hexadecimal digits, and a plain backslash ``\`` followed by exactly three octal digits. In each case the value of the literal is the value represented by the digits in the corresponding base.

Although these representations all result in an integer, they have different valid ranges. Octal escapes must represent a value between 0 and 255 inclusive. Hexadecimal escapes satisfy this condition by construction. The escapes ``\u`` and ``\U`` represent Unicode code points so within them some values are illegal, in particular those above ``0x10FFFF`` and surrogate halves.

After a backslash, certain single-character escapes represent special values:


String literals
---------------

A string literal represents a string constant obtained from concatenating a sequence of characters. Interpreted string literals are character sequences between double quotes ``""``. The text between the quotes, which may not contain newlines, forms the value of the literal, with backslash escapes interpreted as they are in rune literals (except that ``\'`` is illegal and ``\"`` is legal), with the same restrictions. The three-digit octal (``\nnn``) and two-digit hexadecimal (``\xnn``) escapes represent individual bytes of the resulting string; all other escapes represent the (possibly multi-byte) UTF-8 encoding of individual characters. Thus inside a string literal ``\377`` and ``\xFF`` represent a single byte of value ``0xFF`` = ``255``, while ``ÿ``, ``\u00FF``, ``\U000000FF`` and ``\xc3\xbf`` represent the two bytes ``0xc3 0xbf`` of the UTF-8 encoding of character ``U+00FF``.

.. _EBNF String-Lit:

.. code-block:: EBNF

    String-Lit = '"' { Value  | Byte-Value } '"' ;


Tuples
------

.. code-block:: EBNF

    Tuple-Lit = "{" [ Token { "," Token } ] "}";

.. warning:: TODO????

.. warning:: Token???

.. warning:: Niezdefiniowany ``Block`` jest jeszcze.



Constants
=========

.. warning:: ??


Types
=====

A type determines the set of values and operations specific to values of that type. A type may be specified by a (possibly qualified) type name or a type literal, which composes a new type from previously declared types.

.. _EBNF Type:
.. _EBNF TypeName:
.. _EBNF TypeLit:

.. code-block:: EBNF

    Type     = TypeName | TypeLit | "(" Type ")" ;
    TypeName = type-identifier| QualifiedIdent ;
    TypeLit  = ArrayType | StructType | PointerType | FunctionType | InterfaceType
             | SliceType | MapType | ChannelType ;

.. warning:: [TODO przerobic; czy w ogole potrzebne?]

Named instances of the types are predeclared. Composite types – list, class, module, function, interface, tuple – may be constructed using type literals.

.. warning:: [todo lista typów]

.. warning:: linki

The type of a variable is the type defined by or inferred from its declaration.


List type
---------

A list a sequence of elements of a single type, called the element type.

List is  a parameterized type, meaning that it accepts a type parameter. The name of list type is pair of brackets []. It is followed by a type elements.

.. _EBNF ListType:

.. code-block:: EBNF

    ListType = "[]" Type ;


Tuple type
----------

A ``n``-tuple is an ordered sequence of length ``n``. Contrary to lists, they can contain elements of several different types and it contains a fixed number of elements (ie. tuple length is a part of its type).

Tuples' types match iff the length of a tuples is the same and corresponding element types match.

.. _EBNF TupleType:

.. code-block:: EBNF

    TupleType = "{" [ Type { "," Type } ] "}" ;

.. warning:: TUPLETYPE????


Function types
--------------

A function type denotes the set of all functions with the same parameter and result types.

.. _EBNF FunctionType:
.. _EBNF Result:
.. _EBNF Parameters:
.. _EBNF ParameterList:
.. _EBNF ParameterDecl:

.. code-block:: EBNF

    FunctionType  = Parameters ["->" Result] ;
    Result        = Type ;
    Parameters    = Type | ParameterList ;
    ParameterList = "(" [ Type { "," Type } ] ")" ;
    ParameterDecl = [ IdentifierList ] [ "..." ] Type ;

If the function has more than one parameter, they must be written as comma-separated list surrounded with parentheses.


User-defined types
------------------

There are two kinds of user defined types: modules and classes. Their names can be used whenever a type is expected.

Type is visible within a whole file where it is defined, disregarding the definition order. All other types need to be imported using the import directives. See `Importing`_ chapter for information how to bring into scope types from other modules. See `Classes`_ chapter for details on how to define classes.


Parametric types
----------------

Instead of giving a particular (imported or defined within module) typename, it is also possible to give a type parameter. The type parameters names start with a lowercase character. Type parameters are “replaced” with the appropriate types during the compilation.
If the type is omitted, it is usually the same as if it would be explicitly written as type parameter.


Properties of types and values
==============================

.. warning:: ??



Blocks
======

List literal
------------

The number of elements is called the length. It is representable by a value of type ``Int`` and is never negative. Lists can be either finite or infinite. Currently it is not allowed to call ``length`` method on an infinite list.

Luna provides support for the list literals – they have a form of a comma-separated list enclosed with brackets.

.. _EBNF List:
.. _EBNF ListElem:
.. _EBNF ListSeq:

.. code-block:: EBNF

    List     = "[" [ ListElem | { "," ListElem } ] "]" ;
    ListElem = ( ListSeq | (* thingy *)) ;
    ListSeq  = SomethingOrderable ".." [ SomethingOrderable ] ;

The binary pipe operator ``|`` allows prepending element to the list:

.. code-block:: ruby

    a = 0 | [1,2,3]

The lists can be concatonated using the binary plus operator ``+``:

.. code-block:: ruby

    a = [0,1] + [2,3]

The element under ``i``-th index can be accessed using ``list[i]`` notation. The elements in list are indexed from ``0`` (first element). Index has to be from range ``-list.length <= i < list.length`` – accessing list item with index out of bounds is illegal. If index is negative, it counts elements from the last: ``-1`` is the last element, ``-2`` is the element before the last and ``-list.length`` is the first element.


Tuple literal
-------------

Tuples are denoted by a comma separated list of types that is enclosed with braces.

.. code-block:: EBNF

    Tuple = "{" [ Expression { "," Expression } ] "}" ;



Function declarations
---------------------

A function declaration binds an identifier, the *function name*, to a function body. Function name starts always with a lower-case character.

.. _EBNF FunctionDecl:
.. _EBNF FunctionName:
.. _EBNF Function:
.. _EBNF FunctionBody:

.. code-block:: EBNF

    FunctionDecl = "def" FunctionName ( Function | FunctionType ) ;
    FunctionName = Identifier ;
    Function     = FunctionType FunctionBody ;
    FunctionBody = Block ;

The general syntax for defining function is following:

.. code-block:: ruby

    def functionName Arguments -> ReturnType :
        function body

If the ``-> Return Type`` is ommitted, it will replaced with an anonymous parameter type, which will be inferred by the compiler.

Arguments are a list of identifiers separated by space. When identifiers are explicitly typed, they need to enclosed within the parentheses (). Alternatively, the whole parameter list can be enclosed with parentheses, with arguments separated by a comma.

.. code-block:: ruby

    def add self x y : x+ y
        # the simple notation, works when arguments are not explicitly typed
    
    def add self (x::Int) (y::Int) : x + y
        # typed arguments need to be enclosed in parentheses
    
    def add (self, x::Int, y::Int) : x + y
        # alternative argument list style: parentheses enclosed list with arguments separated by comma.

The function body consists of a series expressions. The last one will be interpreted as an return value. No braces are required, all indented code under the function header will be treated as its body. The first expression can be on the same line as function header.

.. code-block:: ruby

    def add a b: a+b
        # function “foo” returns the sum of its parameters

    def add a::Int b::Int -> Int: a + b
        # this function will work only with integer parameters

Function literal
----------------

Lambdas are anonymous functions, defined within an expression. Lambdas are introduced by a colon ``:`` that separated the argument list and the function body.

.. code-block:: ruby

    square = x : x*x  # define lambda that squares its parameter and bound x to it
    i = square 4      # i is 16
    f = square 5.0    # 25.0 #f is 25.0

In the example above we can use the lambda with any type – it is possible because its argument wasn’t explicitly typed. Compiler inferences the parameter type at every usage place and deduces appropriate return type. If we wanted our lambda to work only with ``Int`` parameters we could’ve written it as:

.. code-block:: ruby

    square = x::Int : x * x   
    i = square 4              # ok, 4 is Int
    f = square 5.0            # compilation error, Int expected, but Float received

Lambda body can access the variables from the surrounding scope. It should be noted that lambda "*captures*" the variables from the outer scope at the moment of definition (ie. Luna is lexically scoped). If the variable is rebound to another value, the lambda won’t track this change. The following example shows that:

.. code-block:: ruby

    def foo:
    x = 10
        lambda = a : a + x
        x = "kkkk"     #x is now a String
        y = lambda 100 #y is 110

.. warning:: Używamy sformułowania „variable” ale to jest _ZŁE_ sformułowanie. Trzeba pójść podobnym tokiem jak Scala i używać terminu „values” / „binding to names”.




Declarations and scope
======================

.. warning:: Standardowy lexical scoping? Cokowleik ciekawego się dzieje?



Expressions
===========

.. warning:: ??



Statements
==========

.. warning:: ???

Pattern matching
----------------

.. warning:: ``_`` — nie matchowane.

Creating variables is straight-forward – just use them:

.. code-block:: ruby

    x = 10
    y = x + 5

Even though we haven’t used any types, all variables are statically typed. They receive a type inferred from the expression on the right-hand side of the ``=`` operator. If we want to be sure that variable is of a specific type, we can explicitly declare its type:

.. code-block:: ruby

    z::Int = y  # will compile iff type of y is Int

Actually, it is a part of a bigger mechanism. The left-hand side of the ``=`` operator is called pattern and can be composed of:

* identifier to be bound with a value from the right-hand side
* an underscore character _ (wildcard) to discard value
* tuple of patterns – to match tuple as possibly extract values from it
* type constructor with pattern for each parameter – to extract values of fields from class

Pattern matching matches each pattern on the left hand-side to a value on the right-hand side.

.. code-block:: ruby

    t = {1, 2, ”three”}
    {_, _, y} = x  # y is now String “three”

A bit more complex example:

.. code-block:: ruby

    class Point :
        x,y :: Int
      
    def exampleMousePosition:
        Point 50 75
      
    def main self:
        Point _ yPos  = self.exampleMousePosition
        # yPos is now 75


Function calls
--------------

The most common expression is a function call. Luna, being a function language, strives to make calling function as syntactically clean as possible. There are two calling conventions:

.. code-block:: ruby

    foo         # calls function foo with no arguments
    foo()       # calls function foo with no arguments

    bar 1 2 3   # calls function bar with three Int arguments
    bar(1,2,3)  # calls function bar with three Int arguments



Built-in functions
==================

.. warning:: Lista wbudowanych metod/procedur.



Modules
=======

Module is basically a file that contains:

* imports
* fields
* methods
* classes (with their own sub-members)

Modules are very similar to classes. Similary, every module introduces its own type which can be instantiated.

Programs are built from modules. Each module is defined in a separate file. There is no other way to create them other than by creating a dedicated file. In the same fashion as for types, module names (ie. filenames) must start with upper-case letter.


Importing
---------

To declare dependencies of the current module (file), use the ``import`` statement in a given file. You can either import the module as a whole (including all of its nested members) or import functions and classes selectively.

To import ``Std`` module as a whole, add the following statement:

.. code-block:: ruby

    import Std

From this line, it's possible to use any function or class defined in ``Std`` module:

.. code-block:: ruby

    a = Std.Vector 1.1 2.3 5.811
    b = Std.Console.print (2 + 3)

.. note:: ``Std`` module is a part of standard library and is imported by default.

To import selectively from a ``Std`` module, pass a list after a colon. You can pass a list separated by commas or by introducing a block. The following two versions are equal:

.. code-block:: ruby

    import Std:Vector,Console

    import Std:Vector
               Console

This allows to run the same previous example in slightly more succint way:

.. code-block:: ruby

    a = Std.Vector 1.1 2.3 5.811
    b = Std.Console.print (2 + 3)

You could also import everything from the module – just use the ``*``, e.g.:

.. code-block:: ruby

    import Std:*


Program entry point
-------------------

Each program has a module defined in the ``Main.luna`` file. This file contains the ``main`` method, which is the entry point of a program.


Modules hierarchy
-----------------

Modules can be gouped in a hierarchy, by placing them in a directory structure. To import a module ``Mod`` from directory ``DirA/DirB/DirC``:

.. code-block:: ruby

    import DirA.DirB.DirC.Mod


``self`` implicit parameter
---------------------------

Every method defined in a module gets an implicit ``self`` parameter, that allows treating the module in the same way as a class.



Classes
=======

Luna is *not* an object-oriented programming language in a strict sense, however it supports many of its useful aspects while providing a familiar syntax.

Classes consist of fields (member variables) and methods (member functions). Class definition is introduced by a class keyword:

.. warning:: static function????

.. warning:: function ≠ method ≠ procedure.

.. code-block:: ruby

    class Point :
        x,y :: Int  # two fields of type Int
        def print self:
            c = Console
            c.print self.x
            c.print self.x


Class fields
------------

Class object is a sequence of fields. They can be accessed using the dot operator ``.``, as shown in the snipped in previous section.


Methods
-------

Defining methods works exactly the same as defining function within the module. The only difference is that the first self parameter will be the class object, not the module.
Methods can be called, using dot operator ``.``,  on the object of the class. The class object on the left-hand side of dot will be passed to the function as the first, implicit parameter ``self``.

.. code-block:: ruby

    p = Point 5 10
    p.print


Type constructor
----------------

The class definition defines not only a type but also an entity known as "*type constructor*". It can be perceived as a special kind of function allowing creation of instances of the class, as well as pattern-matching on its fields.

Type constructor arguments are the type's fields in the order of definition. For the ``Point`` class from the snippet in previous section, the constructor is named ``Point`` and takes two ``Int`` parameters.

.. code-block:: ruby

    p = Point 5 10  # using constructor to create a class object
    x1 = p.x        # access class field via dot operator
    Point x2 _ = p  # use type constructor to pattern-match class fields
                    # so x2 becomes 5

Every class has exactly one type constructor and it is implicitly generated by the compiler.

Parametrized class
------------------

The class can be parametrized by another types. For example, we might want to write a ``Point`` class that stores its coordinates using any user-provided type.

.. code-block:: ruby

    class Point coordinate:
        x,y :: coordinate

Now the the ``Point`` type constructor is parametrized and takes two parameters of any type.

.. code-block:: ruby

    p1 = Point 5 10      # p1 is of type "Point Int"
    p2 = Point 5.0 10.0  # p2 is of type "Point Float"

Parametrized class typename can be followed by the typenames that will be used to substitute its parameters. The list can be viewed as an example of paramtrized class (with a unique name ``[]``), so we could write our own list type.

.. code-block:: ruby

    class OurList a:
        head :: a
        tail :: OurList a

        def prepend self (x::a):
            OurList x self

    def main:
        l = OurList 3 ()  # ???????????????????????????
        l = l.prepend 2
        l = l.prepend 1   # now l contains [1,2,3]

.. warning:: nil? nul? null? none?




Errors
======

.. warning:: Opis tego jak propagują się błędy i jak można je obsługiwać.



Visual representation
=====================

.. warning:: Obrazki prezentujące jak wyglądają różne odpowiadające sobie pary kod–graf.



Deployment guide
================

To build your program, pass the entry module of your program to the ``lunac`` compiler, ie. type the following in the shell in appropriate directorys:

.. code-block:: bash

    lunac Main.luna

The command above compiles Luna source code from *Main* module and links it with the standard library. The result is saved in a callable file ``out``.

Compiler supports a number of options, including:

.. code-block:: text

    -o, --output <OUTPUT>  -- save the output into <OUTPUT>
    -h, --help -- display compiler’s options and lunac compiler usage

To read more about compiler options, run ``lunac --help``.

.. warning:: Tutaj informacja o tym, jak się korzysta z systemu np. na AWS.
