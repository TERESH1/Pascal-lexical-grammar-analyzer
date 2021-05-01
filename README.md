# Pascal-lexical-grammar-analyzer
# EBNF definition
```
(* Lexical definition for tokenization *)
Letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z" | "_" ;
Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
Identifier = letter , { letter | digit } ;
IntegerNumber = digit, { digit } ;
ExpDesignator = "e" | "E" ;
Exponent = ExpDesignator, [ "+" | "-" ], IntegerNumber ;
RealNumber = IntegerNumber, ".", IntegerNumber, Exponent ;
Number = RealNumber | IntegerNumber ;
StringValue = '"', { all_characters - '"' }, '"' 
            | "'", { all_characters - ''' }, "'" ;
Reserved = "and" | "downto" | "if" | "or" | "then" | "array" | "else"
         | "in" | "packed" | "to" | "begin" | "end" | "label" | "procedure"
         | "type" | "case" | "file" | "mod" | "program" | "until" | "const"
         | "for" | "nil" | "record" | "var" | "div" | "function" | "not"
         | "repeat" | "while" | "do" | "goto" | "of" | "set" | "with"
         | "class" | "private" | "public" | "protected" | "internal"
         | "constructor" | "destructor" | "property" | "auto" | "sealed"
         | "abstract" | "forward" | "extensionmethod" | "default" | "try"
         | "except" | "finally" | "on" | "operator" | "static" ;
Symbol = ":=" | "+=" | "-=" | "*=" | "/=" | "<=" | "<>" | ">=" | ".." 
       | "," | "." | ";" | ":" | "^" | "(" | ")" | "[" | "]" | "<" 
       | ">" | "=" | "+" | "-" | "*" | "/" | "$" | "@" | "&" | "#" ;
OperatorForOverloading = "+" | "-" | "not" | "@" | "^" | "*" | "/" | "div" | "mod" | "and" | "shl" | "shr"
                       | "or" | "xor" | "=" | "<>" | "<" | ">" | "<=" | ">=" | "in" | "implicit" | "explicit" ;
UnaryOperator = "+" | "-" | "not" | "@" | "^" ;
MultOperator = "*" | "/" | "div" | "mod" | "and" | "shl" | "shr" ;
AddOperator = "+" | "-" | "or" | "xor" ;
Relation = "=" | "<>" | "<" | ">" | "<=" | ">=" | "in" | "is" ;
AssignmentOperator = ":=" | "+=" | "-=" | "*=" | "/=" ;

(* Grammar definition *)
OneFormalParam = [ "var" ], IdentList, ":", Type ;
FormalParameters = "(", OneFormalParam, { ";", OneFormalParam }, ")" ;
FunctionHeading = [ "static" ], "function", ( Designator | ( "operator", OperatorForOverloading ) ), [ FormalParameters ] ;
ProcedureHeading = [ "static" ], "procedure", ( Designator | ( "operator", AssignmentOperator ) ), [ FormalParameters ] ;
FunctionDecl = FunctionHeading, ":", Type, ";" ( "forward" | ( [ "extensionmethod", ";" ], Block ) ) ;
ProcedureDecl = ProcedureHeading, ";", ( "forward" | ( [ "extensionmethod", ";" ], Block ) ) ;
SubprogDeclList = ( ProcedureDecl | FunctionDecl ), ";", { ( ProcedureDecl | FunctionDecl ), ";" } ;
Element = Expression, [ "..", Expression ] ;
FunctionCall = Designator, ActualParameters
             | "new", Type, [ ActualParameters ] ;
SetValue = "[", [ Element, { ",", Element } ], "]" ;
Factor = ( FunctionCall | SetValue | Number | StringValue | "$", StringValue | "nil" | Identifier | "(", Expression, ")" ), [ DesignatorStuff ] ;
Term = Factor
     | Factor, MultOperator, Factor
     | Term, MultOperator, Factor ;
SimpleExpression = [ UnaryOperator ], Term
                 | [ UnaryOperator ], Term, AddOperator, Term
                 | SimpleExpression, AddOperator, Term ;
Expression = SimpleExpression, [ Relation, SimpleExpression ] ;
MemoryStatement = "new", "(", Identifier, ")"
                | "dispose", "(", Identifier, ")" ;
ExpList = Expression, { ",", Expression } ;
ActualParameters = "(", [ ExpList ], ")" ;
DesignatorStuff = ( ".", Identifier, [ ActualParameters ] | "[", ExpList, "]" | "^" ), [ DesignatorStuff ] ;
Designator = Identifier, [ DesignatorStuff ] ;
DesignatorList = Designator, { "," Designator } ;
IOStatement = "read", "(", DesignatorList, ")"
            | "write", "(", ExpList, ")"
            | "readln", [ "(", DesignatorList, ")" ]
            | "writeln", [ "(", ExpList, ")" ] ;
WhichWay = "to" | "downto" ;
ForStatement = "for", [ "var" ], Identifier, ":=", Expression, WhichWay, Expression, "do", Statement ;
RepeatStatement = "repeat", [ Statement, { ";", Statement }, [ ";" ] ], "until", Expression ;
WhileStatement = "while", Expression, "do", Statement ;
CaseLabelList = Expression, { ",", Expression } ;
Case = CaseLabelList, ":", Statement ;
CaseStatement = "case", Expression, "of", Case, { ";", Case }, [ ";" ], "end" ;
IfStatement = "if", Expression, "then", Statement, [ "else", Statement ] ;
ProcedureCall = Designator, [ ActualParameters ] ;
Assignment = Designator, AssignmentOperator, Expression ;
ExceptionProc = "on", [ Identifier, ":" ], Type, "do", Statement ;
ExceptionProcBlock = ExceptionProc, { ";", ExceptionProc }, [ [ ";" ] "else", Statement ]
                   | Statement, { ";", Statement } ;
TryStatement --> "try", [ Statement, { ";", Statement} ], [ ";" ], ( "except", ExceptionProcBlock | "finally", [ Statement, { ";", Statement } ] ), [ ";" ], "end" ;
Statement = Assignment
          | IOStatement
          | ProcedureCall
          | IfStatement
          | CaseStatement
          | WhileStatement
          | RepeatStatement
          | ForStatement
          | MemoryStatement
          | var VariableDecl
          | TryStatement
          | StatementSequence ;
StatementSequence = "begin", [ Statement, { ";", Statement }, [ ";" ] ], "end" ;
FieldList = IdentList, ":", Type ;
FieldListSequence = FieldList, { ";", FieldList } ;
PointerType = "^", Identifier ;
EnumType = "(", IdentList, ")" ;
SetType = "set", "of", Type ;
ClassPrefix = "auto", [ "sealed" ]
            | "sealed", [ "auto" ] ;
ClassConstructor = "constructor", [ Identifier ], [ FormalParameters ], ";", StatementSequence ;
ClassDestructor = "destructor", [ Identifier ], ";", StatementSequence ;
AccessModifiers =  "public" | "protected", "internal" | "protected" | "internal" | "private", "protected" | "private" ;
PropertyReader = Expression | Designator ;
PropertyWriter =  Assignment | Designator ;
Property =  "property", Identifier, [ "[", FieldList, "]" ], ":", Type, ( [ "read", PropertyReader ], "write", PropertyWriter | "read", PropertyReader, [ "write", PropertyWriter ] ), [ ";", "default" ] ;
ClassMember =  [ "static" ], ( FieldList | ProcedureDecl | FunctionDecl | Property | ClassConstructor | ClassDestructor ) ;
ClassSection =  [ AccessModifiers ], ClassMember, { ";", ClassMember } ;
ClassSectionSequence =  ClassSection, { ";", ClassSection } ;
RecordType = "record", [ ClassSectionSequence ], [ ";" ], "end" ;
ClassType = [ ClassPrefix ], "class", ( [ Parameters ], [ ClassSectionSequence ], [ ";" ], "end" | Parameters, [ [ ClassSectionSequence ], [ ";" ], "end" ] ) ;
Subrange = Factor, "..", Factor ;
ArrayType = "array", [ "[", ( Subrange, { ",", Subrange } | { "," } ), "]" ] "of", Type ;
TypeList = Type, { ",", Type } ;
Type = ArrayType | PointerType | RecordType | ClassType | SetType | EnumType | Designator, "<", TypeList, ">" | Designator ;
VariableDecl = Identifier, ":", Type, ":=", Expression
             | Identifier, ":=", Expression
             | IdentList, ":", Type ;
TypeDef = Identifier, "=", Type ;
ConstantDef = Identifier, [ ":", Type ], "=", Expression ;
VariableDeclBlock = "var", VariableDecl, ";", { VariableDecl, ";" } ;
TypeDefBlock = "type", TypeDef, ";", { TypeDef, ";" } ;
ConstantDefBlock = "const", ConstantDef, ";", { ConstantDef, ";" } ;
Declarations = ( ConstantDefBlock | TypeDefBlock | VariableDeclBlock | SubprogDeclList ), { ConstantDefBlock | TypeDefBlock | VariableDeclBlock | SubprogDeclList } ;
Block = [ Declarations ], StatementSequence ;
IdentList = Identifier, { ",", Identifier } ;
Parameters = "(", IdentList, ")" ;
ProgramModule = [ "program", Identifier, [ Parameters ], ";" ], Block, "." ;
CompilationUnit = ProgramModule ;
```