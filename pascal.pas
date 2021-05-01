type
  set_of_string = set of string;
  
  node = class
  private
    _nodes: list<node>;
    _value: string;
  public
    property Nodes[x: integer]: node
    read _nodes[x];default;
    property Value: string
    read _value;
    property Count: integer
    read _nodes.Count;
    
    constructor(v: string);
    begin
      _value := v;
      _nodes := new List<node>();
    end;
    
    function AddNode(v: string): node;
    begin
      var t := new node(v);
      _nodes.Add(t);
      Result := t;
    end;
    
    function AddNode(n: node): node;
    begin
      _nodes.Add(n);
      Result := n;
    end;
  end;
  
  Lexem = auto class
    start_index: integer;
    value: string;
  end;

procedure PrettyPrint(n: node; level: integer; pre_write: string);
const
  width = 3;
begin
  writeln(n.Value);
  for var i := 0 to n.Count - 2 do
  begin
    write(pre_write + '├' + '─' * (width - 1));
    PrettyPrint(n[i], level + 1, pre_write + '│' + ' ' * (width - 1));
  end;
  if n.Count > 0 then
  begin
    write(pre_write + '└' + '─' * (width - 1));
    PrettyPrint(n[n.Count - 1], level + 1, pre_write + ' ' * width);
  end;
end;

procedure PrintLexemes(lexemes: List<lexem>);
begin
  for var i := 0 to lexemes.Count - 1 do
    println($'[{i}] {lexemes[i].start_index}: "{lexemes[i].value}" ({lexemes[i].GetType.Name})');
end;

type
  LPascalIdentifier = class(Lexem);
  LPascalNumber = class(Lexem);
  LPascalNInteger = class(LPascalNumber);
  LPascalNReal = class(LPascalNumber);
  LPascalString = class(Lexem);
  LPascalReserved = class(Lexem);
  LPascalSymbol = class(Lexem);
  
  Relation_ = (Equal, NotEqual, InSet);

function SafeCharIn(str: string; cur_pos: integer; &set: set of char): boolean;
begin
  if cur_pos > str.Length then
    Result := false
  else
    Result := str[cur_pos] in &set;
end;

function SafeCharComp(str: string; cur_pos: integer; r: Relation_; c: char): boolean;
begin
  if cur_pos > str.Length then
    Result := false
  else
  begin
    if r = Relation_.Equal then
      Result := str[cur_pos] = c
    else
    if r = Relation_.NotEqual then
      Result := str[cur_pos] <> c;
  end;
end;

function SafeLastLexemValueComp(lexemes: List<Lexem>; r: Relation_; val: string): boolean;
begin
  var Count := lexemes.Count;
  if Count = 0 then
    Result := false
  else
  begin
    if r = Relation_.Equal then
      Result := lexemes[Count - 1].value = val
    else
    if r = Relation_.NotEqual then
      Result := lexemes[Count - 1].value <> val;
  end;
end;

function SafeLexemComp(lexemes: List<Lexem>; cur_pos: integer; r: Relation_; val: object): boolean;
begin
  if cur_pos >= lexemes.Count then
    Result := false
  else
  begin
    if r = Relation_.Equal then
      Result := lexemes[cur_pos].value = val.ToString
    else
    if r = Relation_.NotEqual then
      Result := lexemes[cur_pos].value <> val.ToString
    else
    if r = Relation_.InSet then
      Result := lexemes[cur_pos].value in set_of_string(val);
  end;
end;

type
  PascalLexicalAnalyzer = class
    static letters := ['a'..'z', 'A'..'Z', '_'];
    static digits := ['0'..'9'];
    static simblos := [',', '.', ';', ':', '^', '(', ')', '[', ']', '<', '>', '=', '+', '-', '*', '/', '$', '@', '&', '#'];
    static reserved_words := ['and', 'downto', 'if', 'or', 'then', 'array', 'else', 'in', 'packed', 'to', 'begin', 'end', 'label', 'procedure', 'type', 'case', 'file', 'mod', 'program', 'until', 'const', 'for', 'nil', 'record', 'var', 'div', 'function', 'not', 'repeat', 'while', 'do', 'goto', 'of', 'set', 'with', 'class', 'private', 'public', 'protected', 'internal', 'constructor', 'destructor', 'property', 'auto', 'sealed', 'abstract', 'forward', 'extensionmethod', 'default', 'try', 'except', 'finally', 'on', 'operator', 'static', 'new'];
    
    static function Analyze(str: string; var lexemes: List<lexem>): boolean;
    begin
      Result := true;
      var cur_index := 1;
      while cur_index <= str.Length do
      begin
        var l: lexem;
        var next_index: integer;
        var isNotFind := true;
        var cur_char := str[cur_index];
        //var cur_ord := ord(cur_char);
        
        while (cur_char = #9) or (cur_char = #10) or (cur_char = #13) or (cur_char = ' ') do
        begin
          cur_index += 1;
          cur_char := str[cur_index];
        end;
        
        if (cur_char = '(') and (str[cur_index + 1] = '*') then
        begin
          cur_index := Pos('*)', str, cur_index + 2) + 2;
          continue;
        end;
        if (cur_char = '{') then
        begin
          cur_index := Pos('}', str, cur_index) + 1;
          continue;
        end;
        if (cur_char = '/') and (str[cur_index + 1] = '/') then
        begin
          cur_index := Pos(#10, str, cur_index) + 1;
          continue;
        end;
        
        if Symbol(str, cur_index, next_index) then
        begin
          l := new LPascalSymbol();
          isNotFind := false;
        end;
        
        if isNotFind and Reserved(str, cur_index, next_index) then
        begin
          if SafeLastLexemValueComp(lexemes, Relation_.Equal, '&') then
          begin
            lexemes.RemoveAt(lexemes.Count - 1);
            l := new LPascalIdentifier();
            AddNewPascalLexem(lexemes, str, l, cur_index - 1, next_index);
            cur_index := next_index;
            continue;
          end else
            l := new LPascalReserved();
          isNotFind := false;
        end;
        
        if isNotFind and Identifier(str, cur_index, next_index) then
        begin
          if SafeLastLexemValueComp(lexemes, Relation_.Equal, '&') then
          begin
            lexemes.RemoveAt(lexemes.Count - 1);
            l := new LPascalIdentifier();
            AddNewPascalLexem(lexemes, str, l, cur_index - 1, next_index);
            cur_index := next_index;
            continue;
          end else
            l := new LPascalIdentifier();
          isNotFind := false;
        end;
        
        if isNotFind and RealNumber(str, cur_index, next_index) then
        begin
          l := new LPascalNReal();
          isNotFind := false;
        end;
        
        if isNotFind and IntegerNumber(str, cur_index, next_index) then
        begin
          if SafeLastLexemValueComp(lexemes, Relation_.Equal, '#') then
          begin
            lexemes.RemoveAt(lexemes.Count - 1);
            l := new LPascalString();
            AddNewPascalLexem(lexemes, str, l, cur_index - 1, next_index);
            cur_index := next_index;
            continue;
          end else
            l := new LPascalNInteger();
          isNotFind := false;
        end;
        
        if isNotFind and StringValue(str, cur_index, next_index) then
        begin
          l := new LPascalString();
          isNotFind := false;
        end;
        
        if isNotFind then
        begin
          writeln();
          writeln($'Error: [{cur_index}] {str.Substring(cur_index-1, min(20, str.Length))} ...');
          Result := false;
          exit;
        end
        else
        begin
          AddNewPascalLexem(lexemes, str, l, cur_index, next_index);
          cur_index := next_index;
        end;
      end;
    end;
    
    static procedure AddNewPascalLexem(lexemes: List<lexem>; str: string; l: lexem; cur_pos: integer; next_pos: integer);
    begin
      l.start_index := cur_pos;
      l.value := str.Substring(cur_pos - 1, next_pos - cur_pos);
      if not (l is LPascalString) then
        l.value := l.value.ToLower();
      lexemes.Add(l);
    end;
    
    /// Letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
    ///        | "H" | "I" | "J" | "K" | "L" | "M" | "N"
    ///        | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
    ///        | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
    ///        | "c" | "d" | "e" | "f" | "g" | "h" | "i"
    ///        | "j" | "k" | "l" | "m" | "n" | "o" | "p"
    ///        | "q" | "r" | "s" | "t" | "u" | "v" | "w"
    ///        | "x" | "y" | "z" | "_" ;
    static function Letter(str: string; cur_pos: integer): boolean;
    begin
      Result := SafeCharIn(str, cur_pos, letters);
    end;
    
    /// Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    static function Digit(str: string; cur_pos: integer): boolean;
    begin
      Result := SafeCharIn(str, cur_pos, digits);
    end;
    
    /// Identifier = letter , { letter | digit } ;
    static function Identifier(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      if Letter(str, cur_pos) then 
      begin
        Result := true;
        next_pos := cur_pos + 1;
        while(Letter(str, next_pos) or Digit(str, next_pos)) do
          next_pos += 1;
      end
      else Result := false;
    end;
    
    /// IntegerNumber = digit, { digit } ;
    static function IntegerNumber(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      if Digit(str, cur_pos) then 
      begin
        Result := true;
        next_pos := cur_pos + 1;
        while(Digit(str, next_pos)) do
          next_pos += 1;
      end
      else Result := false;
    end;
    
    /// ExpDesignator = "e" | "E" ;
    static function ExpDesignator(str: string; cur_pos: integer): boolean;
    begin
      var str_cur_pos := str[cur_pos];
      Result := (str_cur_pos = 'e') or (str_cur_pos = 'E');
    end;
    
    /// Exponent = ExpDesignator, [ "+" | "-" ], IntegerNumber ;
    static function Exponent(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      if ExpDesignator(str, cur_pos) then 
      begin
        var t := cur_pos + 1;
        var _chr := str[next_pos];
        if (_chr = '+') or (_chr = '-') then
          t += 1;
        Result := IntegerNumber(str, t, next_pos);
        exit;
      end;
      Result := false;
    end;
    
    /// RealNumber = IntegerNumber, ".", IntegerNumber, Exponent ;
    static function RealNumber(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      if IntegerNumber(str, cur_pos, next_pos) then 
        if SafeCharComp(str, next_pos, Relation_.Equal, '.') then
          if IntegerNumber(str, next_pos + 1, next_pos) then
          begin
            Result := true;
            Exponent(str, next_pos, next_pos);
            exit;
          end;
      Result := false;
    end;
    
    /// Number = RealNumber | IntegerNumber ;
    static function Number(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      Result := RealNumber(str, cur_pos, next_pos) or IntegerNumber(str, cur_pos, next_pos);
    end;
    
    /// StringValue = '"', { all_characters - '"' }, '"' 
    ///             | "'", { all_characters - ''' }, "'" ;
    static function StringValue(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      var ch := #39; //'
      if (str[cur_pos] <> ch) then
        ch := #34; //"
      if (str[cur_pos] = ch) then
      begin
        next_pos := str.IndexOf(ch, cur_pos) + 2;
        if next_pos <> 1 then
        begin
          Result := true;
          exit;
        end;
      end;
      Result := false;
    end;
    
    /// Reserved = "and" | "downto" | "if" | "or" | "then" | "array" | "else"
    ///          | "in" | "packed" | "to" | "begin" | "end" | "label" | "procedure"
    ///          | "type" | "case" | "file" | "mod" | "program" | "until" | "const"
    ///          | "for" | "nil" | "record" | "var" | "div" | "function" | "not"
    ///          | "repeat" | "while" | "do" | "goto" | "of" | "set" | "with"
    ///          | "class" | "private" | "public" | "protected" | "internal"
    ///          | "constructor" | "destructor" | "property" | "auto" | "sealed"
    ///          | "abstract" | "forward" | "extensionmethod" | "default" | "try"
    ///          | "except" | "finally" | "on" | "operator" | "static" ;
    static function Reserved(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      Result := false;
      if Identifier(str, cur_pos, next_pos) then
      begin
        Result := str.Substring(cur_pos - 1, next_pos - cur_pos).ToLower() in reserved_words;
      end;
    end;
    
    /// Symbol = ":=" | "+=" | "-=" | "*=" | "/=" | "<=" | "<>" | ">=" | ".." 
    ///        | "," | "." | ";" | ":" | "^" | "(" | ")" | "[" | "]" | "<" 
    ///        | ">" | "=" | "+" | "-" | "*" | "/" | "$" | "@" | "&" | "#" ;
    static function Symbol(str: string; cur_pos: integer; var next_pos: integer): boolean;
    begin
      var cur_char := str[cur_pos];
      Result := cur_char in simblos;
      if Result then
      begin
        next_pos := cur_pos + 1;
        if cur_char = ':' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '+' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '-' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '*' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '/' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '<' then
        begin
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '>') then
          begin
            next_pos += 1;
            exit;
          end;
        end;
        if cur_char = '>' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '=') then
          begin
            next_pos += 1;
            exit;
          end;
        if cur_char = '.' then
          if SafeCharComp(str, cur_pos + 1, Relation_.Equal, '.') then
          begin
            next_pos += 1;
            exit;
          end;
      end;
    end;
  end;
  
  
  
  
  
  
  
  
  
  
  
  
  
  PascalGrammarAnalyzer = class
    static Unary_Operator := ['+', '-', 'not', '@', '^'];
    static Mult_Operator := ['*', '/', 'div', 'mod', 'and', 'shl', 'shr'];
    static Add_Operator := ['+', '-', 'or', 'xor'];
    static _Relation_ := ['=', '<>', '<', '>', '<=', '>=', 'in', 'is'];
    static Assign_Operator := [':=', '+=', '-=', '*=', '/='];
    /// OperatorForOverloading = "+" | "-" | "not" | "@" | "^" | "*" | "/" | "div" | "mod" | "and" | "shl" | "shr"
    ///                        | "or" | "xor" | "=" | "<>" | "<" | ">" | "<=" | ">=" | "in" | "implicit" | "explicit" ;
    static OperatorForOverloading := Unary_Operator + Mult_Operator + Add_Operator + _Relation_ - ['is'] + ['implicit', 'explicit'];
    static class_prefix := ['auto', 'sealed', 'abstract'];
    
    static function AssociativeToLeft(n: node): node;
    begin
      Result := new node(n.value);
      if n.Count >= 3 then
      begin
        Result.AddNode(n[0]);
        Result.AddNode(n[1]);
        Result.AddNode(n[2]);
        for var i := 1 to ((n.Count - 1) div 2) - 1 do
        begin
          var t := new node(n.value);
          t.AddNode(Result);
          Result := t;
          Result.AddNode(n[i * 2 + 1]);
          Result.AddNode(n[(i + 1) * 2]);
        end;
      end else
        Result.AddNode(n[0]);
    end;
    
    /// UnaryOperator = "+" | "-" | "not" | "@" | "^" ;
    static function UnaryOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
    begin
      Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Unary_Operator);
      if Result then
        upper_command.AddNode('UnaryOperator').AddNode($' {lexemes[cur_pos].value}');
    end;
    
    /// MultOperator = "*" | "/" | "div" | "mod" | "and" | "shl" | "shr" ;
    static function MultOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
    begin
      Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Mult_Operator);
      if Result then
        upper_command.AddNode('MultOperator').AddNode($' {lexemes[cur_pos].value}');
    end;
    
    /// AddOperator = "+" | "-" | "or" | "xor" ;
    static function AddOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
    begin
      Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Add_Operator);
      if Result then
        upper_command.AddNode('AddOperator').AddNode($' {lexemes[cur_pos].value}');
    end;
    
    /// Relation = "=" | "<>" | "<" | ">" | "<=" | ">=" | "in" | "is" ;
    static function Relation(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
    begin
      Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, _Relation_);
      if Result then
        upper_command.AddNode('Relation').AddNode($' {lexemes[cur_pos].value}');
    end;
    
    /// AssignmentOperator = ":=" | "+=" | "-=" | "*=" | "/=" ;
    static function AssignmentOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
    begin
      Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Assign_Operator);
      if Result then
        upper_command.AddNode('AssignmentOperator').AddNode($' {lexemes[cur_pos].value}');
    end;
    
    /// OneFormalParam = [ "var" ], IdentList, ":", Type ;
    static function OneFormalParam(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('OneFormalParam');
      if (lexemes[cur_pos].value = 'var') then
      begin
        cur_pos += 1;
        cur_command.AddNode('var');
      end;
      Result := 
        IdentList(lexemes, cur_pos, next_pos, cur_command) and
              (lexemes[next_pos].value = ':') and
              &Type(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// FormalParameters = "(", OneFormalParam, { ";", OneFormalParam }, ")" ;
    static function FormalParameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FormalParameters');
      Result := 
        (lexemes[cur_pos].value = '(') and
              OneFormalParam(lexemes, cur_pos + 1, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and OneFormalParam(lexemes, cur_pos + 1, next_pos, cur_command));
        Result := (lexemes[cur_pos].value = ')');
        if Result then 
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// FunctionHeading = [ "static" ], "function", ( Designator | ( "operator", OperatorForOverloading ) ), [ FormalParameters ] ;
    static function FunctionHeading(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FunctionHeading');
      Result := false;
      if (lexemes[cur_pos].value = 'static') then
      begin
        cur_command.AddNode('static');
        cur_pos += 1;
      end;
      if (lexemes[cur_pos].value = 'function') then
      begin
        if Designator(lexemes, cur_pos + 1, next_pos, cur_command) then
        begin
          Result := true;
          cur_pos := next_pos;
        end else
        if (lexemes[cur_pos + 1].value = 'operator') and
          (lexemes[cur_pos + 2].value in OperatorForOverloading) then
        begin
          Result := true;
          cur_command.AddNode('operator').AddNode($' {lexemes[cur_pos + 2].value}');
          cur_pos += 3;
        end;
      end;
      if Result then
      begin
        if not FormalParameters(lexemes, cur_pos, next_pos, cur_command) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ProcedureHeading = [ "static" ], "procedure", ( Designator | ( "operator", AssignmentOperator ) ), [ FormalParameters ] ;
    static function ProcedureHeading(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ProcedureHeading');
      if (lexemes[cur_pos].value = 'static') then
      begin
        cur_command.AddNode('static');
        cur_pos += 1;
      end;
      Result := false;
      if (lexemes[cur_pos].value = 'procedure') then
      begin
        if Designator(lexemes, cur_pos + 1, next_pos, cur_command) then
        begin
          Result := true;
          cur_pos := next_pos;
        end else
        if (lexemes[cur_pos + 1].value = 'operator') and
          (lexemes[cur_pos + 2].value in Assign_Operator) then
        begin
          Result := true;
          cur_command.AddNode('operator').AddNode($' {lexemes[cur_pos + 2].value}');
          cur_pos += 3;
        end;
      end;
      if Result then
      begin
        if not FormalParameters(lexemes, cur_pos, next_pos, cur_command) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// FunctionDecl = FunctionHeading, ":", Type, ";" ( "forward" | ( [ "extensionmethod", ";" ], Block ) ) ;
    static function FunctionDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FunctionDecl');
      Result := FunctionHeading(lexemes, cur_pos, next_pos, cur_command) and 
              (lexemes[next_pos].value = ':') and 
              &Type(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        Result := (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value = 'forward');
        if Result then
        begin
          next_pos := cur_pos + 2;
          cur_command.AddNode(lexemes[cur_pos + 1].value);
        end
        else
        begin
          if (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value = 'extensionmethod') then
          begin
            cur_command.AddNode('extensionmethod');
            cur_pos += 2;
          end;
          Result := (lexemes[cur_pos].value = ';') and Block(lexemes, cur_pos + 1, next_pos, cur_command);
        end;
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ProcedureDecl = ProcedureHeading, ";", ( "forward" | ( [ "extensionmethod", ";" ], Block ) ) ;
    static function ProcedureDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ProcedureDecl');
      Result := ProcedureHeading(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        Result := (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value = 'forward');
        if Result then
        begin
          next_pos := cur_pos + 2;
          cur_command.AddNode(lexemes[cur_pos + 1].value);
        end
        else
        begin
          if (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value = 'extensionmethod') then
          begin
            cur_command.AddNode('extensionmethod');
            cur_pos += 2;
          end;
          Result := (lexemes[cur_pos].value = ';') and Block(lexemes, cur_pos + 1, next_pos, cur_command);
        end;
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// SubprogDeclList = ( ProcedureDecl | FunctionDecl ), ";", { ( ProcedureDecl | FunctionDecl ), ";" } ;
    static function SubprogDeclList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('SubprogDeclList');
      Result := false;
      while (ProcedureDecl(lexemes, cur_pos, next_pos, cur_command) or 
            FunctionDecl(lexemes, cur_pos, next_pos, cur_command)) 
            and
            (lexemes[next_pos].value = ';') do
      begin
        Result := true;
        next_pos += 1;
        cur_pos := next_pos;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// Element = Expression, [ "..", Expression ] ;
    static function Element(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Element');
      Result := Expression(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        if not ((lexemes[cur_pos].value = '..') and Expression(lexemes, cur_pos + 1, next_pos, cur_command)) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// FunctionCall = Designator, ActualParameters
    ///              | "new", Type, [ ActualParameters ] ;
    static function FunctionCall(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FunctionCall');
      Result := 
        (lexemes[cur_pos].value = 'new') and
              &Type(lexemes, cur_pos + 1, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        if not ActualParameters(lexemes, next_pos, next_pos, cur_command) then
          next_pos := cur_pos;
      end else
        Result :=
          Designator(lexemes, cur_pos, next_pos, cur_command) and
                  ActualParameters(lexemes, next_pos, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// SetValue = "[", [ Element, { ",", Element } ], "]" ;
    static function SetValue(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('SetValue');
      Result := (lexemes[cur_pos].value = '[');
      if Result then
      begin
        cur_pos += 1;
        if Element(lexemes, cur_pos, next_pos, cur_command) then
        begin
          repeat
            cur_pos := next_pos;
          until not ((lexemes[cur_pos].value = ',') and Element(lexemes, cur_pos + 1, next_pos, cur_command));
          next_pos := cur_pos;
        end;
        Result := (lexemes[cur_pos].value = ']');
        if Result then 
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// Factor = ( FunctionCall | SetValue | Number | StringValue | "$", StringValue | "nil" | Identifier | "(", Expression, ")" ), [ DesignatorStuff ] ;
    static function Factor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Factor');
      Result := FunctionCall(lexemes, cur_pos, next_pos, cur_command) or
              SetValue(lexemes, cur_pos, next_pos, cur_command);
      if not Result then
      begin
        Result := (lexemes[cur_pos] is LPascalNumber) or
                  (lexemes[cur_pos] is LPascalString) or 
                  (lexemes[cur_pos].value = 'nil') or
                  (lexemes[cur_pos] is LPascalIdentifier);
        if Result then
        begin
          next_pos := cur_pos + 1;
          cur_command.AddNode(lexemes[cur_pos].value)
        end else
        begin
          Result := 
            (lexemes[cur_pos].value = '$') and
                      (lexemes[cur_pos + 1] is LPascalString);
          if Result then
          begin
            cur_command.AddNode('$' + lexemes[cur_pos + 1].value);
            next_pos := cur_pos + 2;
          end
          else
          begin
            Result := (lexemes[cur_pos].value = '(') and 
                          Expression(lexemes, cur_pos + 1, next_pos, cur_command) and 
                          (lexemes[next_pos].value = ')');
            if Result then
              next_pos := next_pos + 1;
          end;
        end;
      end;
      if Result then
      begin
        cur_pos := next_pos;
        if not DesignatorStuff(lexemes, cur_pos, next_pos, cur_command) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Term = Factor
    ///      | Factor, MultOperator, Factor
    ///      | Term, MultOperator, Factor ;
    static function Term(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Term');
      Result := Factor(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not (MultOperator(lexemes, cur_pos, cur_command) and Factor(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// SimpleExpression = [ UnaryOperator ], Term
    ///                  | [ UnaryOperator ], Term, AddOperator, Term
    ///                  | SimpleExpression, AddOperator, Term ;
    static function SimpleExpression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('SimpleExpression');
      if UnaryOperator(lexemes, cur_pos, cur_command) then
        cur_pos += 1;
      var term_expr := new node('');
      Result := Term(lexemes, cur_pos, next_pos, term_expr);
      if Result then
      begin
        repeat
          cur_command.AddNode(AssociativeToLeft(term_expr[0]));
          cur_pos := next_pos;
          term_expr := new node('');
        until not (AddOperator(lexemes, cur_pos, cur_command) and Term(lexemes, cur_pos + 1, next_pos, term_expr));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Expression = SimpleExpression, [ Relation, SimpleExpression ] ;
    static function Expression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Expression');
      var simp_expr := new node('');
      Result := SimpleExpression(lexemes, cur_pos, next_pos, simp_expr);
      if Result then
      begin
        cur_command.AddNode(AssociativeToLeft(simp_expr[0]));
        cur_pos := next_pos;
        simp_expr := new node('');
        if not (Relation(lexemes, cur_pos, cur_command) and SimpleExpression(lexemes, cur_pos + 1, next_pos, simp_expr)) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// MemoryStatement = "new", "(", Identifier, ")"
    ///                 | "dispose", "(", Identifier, ")" ;
    static function MemoryStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('MemoryStatement');
      Result := ((lexemes[cur_pos].value = 'new') or 
              (lexemes[cur_pos].value = 'dispose'))
        and
              (lexemes[cur_pos + 1].value = '(') and 
              (lexemes[cur_pos + 2] is LPascalIdentifier) and 
              (lexemes[cur_pos + 3].value = ')');
      if Result then
      begin
        cur_command.AddNode(lexemes[cur_pos].value);
        cur_command.AddNode(lexemes[cur_pos + 2].value);
        upper_command.AddNode(cur_command);
        next_pos := cur_pos + 4;
      end;
    end;
    
    /// ExpList = Expression, { ",", Expression } ;
    static function ExpList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ExpList');
      Result := Expression(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ',') and Expression(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ActualParameters = "(", [ ExpList ], ")" ;
    static function ActualParameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ActualParameters');
      Result := (lexemes[cur_pos].value = '(');
      if Result then
      begin
        cur_pos += 1;
        if ExpList(lexemes, cur_pos, next_pos, cur_command) then
          cur_pos := next_pos;
        Result := SafeLexemComp(lexemes, cur_pos, Relation_.Equal, ')');
        if Result then
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// DesignatorStuff = ( ".", Identifier, [ ActualParameters ] | "[", ExpList, "]" | "^" ), [ DesignatorStuff ] ;
    static function DesignatorStuff(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('DesignatorStuff');
      Result := false;
      if SafeLexemComp(lexemes, cur_pos, Relation_.Equal, '.') and 
        (lexemes[cur_pos + 1] is LPascalIdentifier) then
      begin
        cur_command.AddNode(lexemes[cur_pos + 1].value);
        next_pos := cur_pos + 2;
        cur_pos := next_pos;
        if ActualParameters(lexemes, cur_pos, next_pos, cur_command) then
          cur_pos := next_pos;
        Result := true;
      end;
      if not Result and SafeLexemComp(lexemes, cur_pos, Relation_.Equal, '[') and 
              ExpList(lexemes, cur_pos + 1, next_pos, cur_command) and 
        (lexemes[next_pos].value = ']') then
      begin
        next_pos += 1;
        cur_pos := next_pos;
        Result := true;
      end;
      if not Result and SafeLexemComp(lexemes, cur_pos, Relation_.Equal, '^') then
      begin
        cur_command.AddNode(lexemes[cur_pos].value);
        next_pos := cur_pos + 1;
        cur_pos := next_pos;
        Result := true;
      end;
      if Result then
      begin
        DesignatorStuff(lexemes, cur_pos, next_pos, cur_command);
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Designator = Identifier, [ DesignatorStuff ] ;
    static function Designator(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Designator');
      Result := (lexemes[cur_pos] is LPascalIdentifier);
      if Result then
      begin
        cur_command.AddNode(lexemes[cur_pos].value);
        if not DesignatorStuff(lexemes, cur_pos + 1, next_pos, cur_command) then
          next_pos := cur_pos + 1;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// DesignatorList = Designator, { "," Designator } ;
    static function DesignatorList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('DesignatorList');
      Result := Designator(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ',') and Designator(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// IOStatement = "read", "(", DesignatorList, ")"
    ///             | "write", "(", ExpList, ")"
    ///             | "readln", [ "(", DesignatorList, ")" ]
    ///             | "writeln", [ "(", ExpList, ")" ] ;
    static function IOStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result := false;
      var cur_command := new node('IOStatement');
      if (lexemes[cur_pos].value = 'read') or
            (lexemes[cur_pos].value = 'write') or
            (lexemes[cur_pos].value = 'readln') or
      (lexemes[cur_pos].value = 'writeln') then
        cur_command.AddNode(lexemes[cur_pos].value)
      else
        exit;
      Result := (lexemes[cur_pos].value = 'read') and 
              (lexemes[cur_pos + 1].value = '(') and 
              DesignatorList(lexemes, cur_pos + 2, next_pos, cur_command) and 
              (lexemes[next_pos].value = ')') 
        or
              (lexemes[cur_pos].value = 'write') and 
              (lexemes[cur_pos + 1].value = '(') and 
              ExpList(lexemes, cur_pos + 2, next_pos, cur_command) and 
              (lexemes[next_pos].value = ')');
      if Result then
        next_pos += 1
      else
      begin
        if (lexemes[cur_pos].value = 'readln') then
        begin
          Result := true;
          if (lexemes[cur_pos + 1].value = '(') and 
                    DesignatorList(lexemes, cur_pos + 2, next_pos, cur_command) and 
          (lexemes[next_pos].value = ')') then
            next_pos += 1
          else
            next_pos := cur_pos + 1;
        end;
        if (lexemes[cur_pos].value = 'writeln') then
        begin
          Result := true;
          if (lexemes[cur_pos + 1].value = '(') then
            if ExpList(lexemes, cur_pos + 2, next_pos, cur_command) and 
            (lexemes[next_pos].value = ')') then
              next_pos += 1
            else if (lexemes[cur_pos + 2].value = ')') then
              next_pos := cur_pos + 3
            else
              next_pos := cur_pos + 1;
        end;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// WhichWay = "to" | "downto" ;
    static function WhichWay(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result := (lexemes[cur_pos].value = 'to') or 
              (lexemes[cur_pos].value = 'downto');
      if Result then
      begin
        upper_command.AddNode('WhichWay').AddNode(lexemes[cur_pos].value);
        next_pos := cur_pos + 1;
      end;
    end;
    
    /// ForStatement = "for", [ "var" ], Identifier, ":=", Expression, WhichWay, Expression, "do", Statement ;
    static function ForStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ForStatement');
      Result := (lexemes[cur_pos].value = 'for');
      if Result then
      begin
        cur_pos += 1;
        if (lexemes[cur_pos].value = 'var') then
        begin
          cur_pos += 1;
          cur_command.AddNode('var').AddNode(lexemes[cur_pos].value);
        end;
        Result :=
          (lexemes[cur_pos] is LPascalIdentifier) and
                  (lexemes[cur_pos + 1].value = ':=') and
                  Expression(lexemes, cur_pos + 2, next_pos, cur_command) and
                  WhichWay(lexemes, next_pos, next_pos, cur_command) and
                  Expression(lexemes, next_pos, next_pos, cur_command) and
                  (lexemes[next_pos].value = 'do') and
                  Statement(lexemes, next_pos + 1, next_pos, cur_command);
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// RepeatStatement = "repeat", [ Statement, { ";", Statement }, [ ";" ] ], "until", Expression ;
    static function RepeatStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('RepeatStatement');
      Result := (lexemes[cur_pos].value = 'repeat');
      if Result then
      begin
        cur_pos += 1;
        if Statement(lexemes, cur_pos, next_pos, cur_command) then
        begin
          repeat
            cur_pos := next_pos;
          until not ((lexemes[cur_pos].value = ';') and Statement(lexemes, cur_pos + 1, next_pos, cur_command));
          if (lexemes[cur_pos].value = ';') then
            cur_pos += 1;
        end;
        Result := 
          (lexemes[cur_pos].value = 'until') and
                  Expression(lexemes, cur_pos + 1, next_pos, cur_command);
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// WhileStatement = "while", Expression, "do", Statement ;
    static function WhileStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('WhileStatement');
      Result := 
        (lexemes[cur_pos].value = 'while') and 
              Expression(lexemes, cur_pos + 1, next_pos, cur_command) and 
              (lexemes[next_pos].value = 'do') and 
              Statement(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// CaseLabelList = Expression, { ",", Expression } ;
    static function CaseLabelList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('CaseLabelList');
      Result := Expression(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ',') and Expression(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// Case = CaseLabelList, ":", Statement ;
    static function &Case(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Case');
      Result := 
        CaseLabelList(lexemes, cur_pos, next_pos, cur_command) and 
              (lexemes[next_pos].value = ':') and 
              Statement(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// CaseStatement = "case", Expression, "of", Case, { ";", Case }, [ ";" ], "end" ;
    static function CaseStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('CaseStatement');
      Result := 
        (lexemes[cur_pos].value = 'case') and 
              Expression(lexemes, cur_pos + 1, next_pos, cur_command) and 
              (lexemes[next_pos].value = 'of') and 
              &Case(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and &Case(lexemes, cur_pos + 1, next_pos, cur_command));
        if (lexemes[cur_pos].value = ';') then
          cur_pos += 1;
        Result := (lexemes[cur_pos].value = 'end');
        if Result then
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// IfStatement = "if", Expression, "then", Statement, [ "else", Statement ] ;
    static function IfStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('IfStatement');
      Result := 
        (lexemes[cur_pos].value = 'if') and 
              Expression(lexemes, cur_pos + 1, next_pos, cur_command) and 
              (lexemes[next_pos].value = 'then') and 
              Statement(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        if not ((lexemes[cur_pos].value = 'else') and Statement(lexemes, cur_pos + 1, next_pos, cur_command)) then
          next_pos := cur_pos;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ProcedureCall = Designator, [ ActualParameters ] ;
    static function ProcedureCall(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ProcedureCall');
      Result := Designator(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        cur_pos := next_pos;
        if not ActualParameters(lexemes, cur_pos, next_pos, cur_command) then
          next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Assignment = Designator, AssignmentOperator, Expression ;
    static function Assignment(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Assignment');
      Result := Designator(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        Result := AssignmentOperator(lexemes, next_pos, cur_command) and
                  Expression(lexemes, next_pos + 1, next_pos, cur_command);
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ExceptionProc = "on", [ Identifier, ":" ], Type, "do", Statement ;
    static function ExceptionProc(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result := false;
      var cur_command := new node('ExceptionProc');
      if (lexemes[cur_pos].value = 'on') then
      begin
        cur_pos += 1;
        if (lexemes[cur_pos] is LPascalIdentifier) and
            (lexemes[cur_pos + 1].value = ':') then
        begin
          cur_command.AddNode(lexemes[cur_pos].value);
          cur_pos += 2;
        end;
        if &Type(lexemes, cur_pos, next_pos, cur_command) and
                (lexemes[next_pos].value = 'do') and
        Statement(lexemes, next_pos + 1, next_pos, cur_command) then
        begin
          Result := true;
          cur_pos := next_pos + 1
        end;
      end;
    end;
    
    /// ExceptionProcBlock = ExceptionProc, { ";", ExceptionProc }, [ [ ";" ] "else", Statement ]
    ///                    | Statement, { ";", Statement } ;
    static function ExceptionProcBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ExceptionProcBlock');
      if ExceptionProc(lexemes, cur_pos, next_pos, cur_command) then
      begin
        Result := true;
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and ExceptionProc(lexemes, cur_pos + 1, next_pos, cur_command));
        if (lexemes[cur_pos].value = ';') then
          cur_pos += 1;
        if (lexemes[cur_pos].value = 'else') then
          if Statement(lexemes, cur_pos + 1, next_pos, cur_command.AddNode('else')) then
          begin
            upper_command.AddNode(cur_command);
            exit;
          end
          else begin
            Result := false;
            exit;
          end
      end else
      if Statement(lexemes, cur_pos, next_pos, cur_command) then
      begin
        Result := true;
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and Statement(lexemes, cur_pos + 1, next_pos, cur_command));
      end;
      next_pos := cur_pos;
      upper_command.AddNode(cur_command);
    end;
    
    /// TryStatement --> "try", [ Statement, { ";", Statement} ], [ ";" ], ( "except", ExceptionProcBlock | "finally", [ Statement, { ";", Statement } ] ), [ ";" ], "end" ;
    static function TryStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('TryStatement');
      Result := false;
      if (lexemes[cur_pos].value = 'try') then
      begin
        cur_pos += 1;
        if Statement(lexemes, cur_pos, next_pos, cur_command) then
        begin
          repeat
            cur_pos := next_pos;
          until not ((lexemes[cur_pos].value = ';') and Statement(lexemes, cur_pos + 1, next_pos, cur_command));
        end;
        if (lexemes[cur_pos].value = ';') then
          cur_pos += 1;
        if (lexemes[cur_pos].value = 'except') then
        begin
          cur_pos += 1;
          if ExceptionProcBlock(lexemes, cur_pos, next_pos, cur_command.AddNode('except')) then
            cur_pos := next_pos;
          Result := true;
        end
        else if (lexemes[cur_pos].value = 'finally') then
        begin
          cur_pos += 1;
          var sub_command := cur_command.AddNode('finally');
          if Statement(lexemes, cur_pos, next_pos, sub_command) then
          begin
            repeat
              cur_pos := next_pos;
            until not ((lexemes[cur_pos].value = ';') and Statement(lexemes, cur_pos + 1, next_pos, sub_command));
          end;
          Result := true;
          cur_command.AddNode(sub_command);
        end;
        if (lexemes[cur_pos].value = ';') then
          cur_pos += 1;
        Result := Result and (lexemes[cur_pos].value = 'end');
      end;
      if Result then
      begin
        next_pos := cur_pos + 1;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Statement = Assignment
    ///           | IOStatement
    ///           | ProcedureCall
    ///           | IfStatement
    ///           | CaseStatement
    ///           | WhileStatement
    ///           | RepeatStatement
    ///           | ForStatement
    ///           | MemoryStatement
    ///           | var VariableDecl
    ///           | TryStatement
    ///           | StatementSequence ;
    static function Statement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result := true;
      if Assignment(lexemes, cur_pos, next_pos, upper_command) then exit;
      if IOStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if ProcedureCall(lexemes, cur_pos, next_pos, upper_command) then exit;
      if IfStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if CaseStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if WhileStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if RepeatStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if ForStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if MemoryStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if (lexemes[cur_pos].value = 'var') and
      VariableDecl(lexemes, cur_pos + 1, next_pos, upper_command) then
        exit;
      if TryStatement(lexemes, cur_pos, next_pos, upper_command) then exit;
      if StatementSequence(lexemes, cur_pos, next_pos, upper_command) then exit;
      Result := false;
    end;
    
    /// StatementSequence = "begin", [ Statement, { ";", Statement }, [ ";" ] ], "end" ;
    static function StatementSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('StatementSequence');
      Result := 
        (lexemes[cur_pos].value = 'begin');
      if Result then
      begin
        if Statement(lexemes, cur_pos + 1, next_pos, cur_command) then
        begin
          repeat
            cur_pos := next_pos;
          until not ((lexemes[cur_pos].value = ';') and Statement(lexemes, cur_pos + 1, next_pos, cur_command));
          if (lexemes[cur_pos].value = ';') then
            cur_pos += 1;
        end;
        Result := (lexemes[cur_pos].value = 'end');
        if Result then
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// FieldList = IdentList, ":", Type ;
    static function FieldList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FieldList');
      Result := 
        IdentList(lexemes, cur_pos, next_pos, cur_command) and 
              (lexemes[next_pos].value = ':') and 
              &Type(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// FieldListSequence = FieldList, { ";", FieldList } ;
    static function FieldListSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('FieldListSequence');
      Result := FieldList(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and FieldList(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// PointerType = "^", Identifier ;
    static function PointerType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result :=
        (lexemes[cur_pos].value = '^') and
              (lexemes[cur_pos + 1] is LPascalIdentifier);
      if Result then
      begin
        next_pos := cur_pos + 2;
        upper_command.AddNode('PointerType').AddNode(lexemes[cur_pos + 1].value);
      end;
    end;
    
    /// EnumType = "(", IdentList, ")" ;
    static function EnumType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('EnumType');
      Result :=
        (lexemes[cur_pos].value = '(') and
              IdentList(lexemes, cur_pos + 1, next_pos, cur_command) and
              (lexemes[next_pos].value = ')');
      if Result then
      begin
        next_pos += 1;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// SetType = "set", "of", Type ;
    static function SetType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('SetType');
      Result :=
        (lexemes[cur_pos].value = 'set') and
              (lexemes[cur_pos + 1].value = 'of') and
              &Type(lexemes, cur_pos + 2, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ClassPrefix = "auto", [ "sealed" ]
    ///             | "sealed", [ "auto" ] ;
    static function ClassPrefix(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassPrefix');
      Result := false;
      var &set: set of string := class_prefix;
      while (lexemes[cur_pos].value in &set) do
      begin
        Result := true;
        cur_command.AddNode(lexemes[cur_pos].value);
        Exclude(&set, lexemes[cur_pos].value);
        cur_pos += 1;
      end;
      if Result then
      begin
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ClassConstructor = "constructor", [ Identifier ], [ FormalParameters ], ";", StatementSequence ;
    static function ClassConstructor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassConstructor');
      Result := 
        (lexemes[cur_pos].value = 'constructor');
      if Result then
      begin
        cur_pos += 1;
        if (lexemes[cur_pos] is LPascalIdentifier) then
        begin
          cur_command.AddNode(lexemes[cur_pos].value);
          cur_pos += 1;
        end;
        if FormalParameters(lexemes, cur_pos, next_pos, cur_command) then
          cur_pos := next_pos;
        Result := (lexemes[cur_pos].value = ';') and StatementSequence(lexemes, cur_pos + 1, next_pos, cur_command);
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ClassDestructor = "destructor", [ Identifier ], ";", StatementSequence ;
    static function ClassDestructor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassDestructor');
      Result := 
        (lexemes[cur_pos].value = 'destructor');
      if Result then
      begin
        cur_pos += 1;
        if (lexemes[cur_pos] is LPascalIdentifier) then
        begin
          cur_command.AddNode(lexemes[cur_pos].value);
          cur_pos += 1;
        end;
        Result := (lexemes[cur_pos].value = ';') and StatementSequence(lexemes, cur_pos + 1, next_pos, cur_command);
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// AccessModifiers =  "public" | "protected", "internal" | "protected" | "internal" | "private", "protected" | "private" ;
    static function AccessModifiers(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('AccessModifiers');
      Result := 
        (lexemes[cur_pos].value = 'public') or
              (lexemes[cur_pos].value = 'private') or
              (lexemes[cur_pos].value = 'protected') or
              (lexemes[cur_pos].value = 'internal');
      if Result then
      begin
        if (lexemes[cur_pos].value = 'protected') and 
                (lexemes[cur_pos + 1].value = 'internal')
        or
        (lexemes[cur_pos].value = 'private') and 
                (lexemes[cur_pos + 1].value = 'protected') then
        begin
          cur_command.AddNode($'{lexemes[cur_pos].value} {lexemes[cur_pos + 1].value}');
          next_pos := cur_pos + 2;
        end
        else
        begin
          cur_command.AddNode(lexemes[cur_pos].value);
          next_pos := cur_pos + 1;
        end;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// PropertyReader = Expression | Designator ;
    static function PropertyReader(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('PropertyReader');
      Result := 
        Expression(lexemes, cur_pos, next_pos, cur_command) or
              Designator(lexemes, cur_pos, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// PropertyWriter =  Assignment | Designator ;
    static function PropertyWriter(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('PropertyWriter');
      Result := 
        Assignment(lexemes, cur_pos, next_pos, cur_command) or
              Designator(lexemes, cur_pos, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    
    /// Property =  "property", Identifier, [ "[", FieldList, "]" ], ":", Type, ( [ "read", PropertyReader ], "write", PropertyWriter | "read", PropertyReader, [ "write", PropertyWriter ] ), [ ";", "default" ] ;
    static function &Property(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Property');
      Result := 
        (lexemes[cur_pos].value = 'property') and
              (lexemes[cur_pos + 1] is LPascalIdentifier);
      if Result then
      begin
        cur_command.AddNode(lexemes[cur_pos + 1].value);
        cur_pos += 2;
        if (lexemes[cur_pos].value = '[') and
                FieldList(lexemes, cur_pos + 1, next_pos, cur_command) and
        (lexemes[next_pos].value = ']') then
          cur_pos := next_pos + 1;
        Result := (lexemes[cur_pos].value = ':') and
                  &Type(lexemes, cur_pos + 1, next_pos, cur_command);
        if Result then
        begin
          cur_pos := next_pos;
          var reader := (lexemes[cur_pos].value = 'read') and
          PropertyReader(lexemes, cur_pos + 1, next_pos, cur_command);
          if reader then
            cur_pos := next_pos;
          var writer := (lexemes[cur_pos].value = 'write') and
          PropertyWriter(lexemes, cur_pos + 1, next_pos, cur_command);
          if writer then
            cur_pos := next_pos;
          if (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value = 'default') then
            next_pos := cur_pos + 2;
          Result := reader or writer;
          if Result then
            upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    (*/// PropertyList =  Property, { ";", Property } ;
    static function PropertyList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('PropertyList');
      Result := &Property(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and &Property(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;*)
    
    /// ClassMember =  [ "static" ], ( FieldList | ProcedureDecl | FunctionDecl | Property | ClassConstructor | ClassDestructor ) ;
    static function ClassMember(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassMember');
      if (lexemes[cur_pos].value = 'static') then
      begin
        cur_pos += 1;
        cur_command.AddNode('static');
      end;
      Result := Assignment(lexemes, cur_pos, next_pos, cur_command) or
            FieldList(lexemes, cur_pos, next_pos, cur_command) or
            ProcedureDecl(lexemes, cur_pos, next_pos, cur_command) or 
            FunctionDecl(lexemes, cur_pos, next_pos, cur_command) or
            &Property(lexemes, cur_pos, next_pos, cur_command) or
            ClassConstructor(lexemes, cur_pos, next_pos, cur_command) or
            ClassDestructor(lexemes, cur_pos, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ClassSection =  [ AccessModifiers ], ClassMember, { ";", ClassMember } ;
    static function ClassSection(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassSection');
      if AccessModifiers(lexemes, cur_pos, next_pos, cur_command) then
        cur_pos := next_pos;
      Result := false;
      if ClassMember(lexemes, cur_pos, next_pos, cur_command) then
      begin
        Result := true;
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and ClassMember(lexemes, cur_pos + 1, next_pos, cur_command));
      end;
      next_pos := cur_pos;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ClassSectionSequence =  ClassSection, { ";", ClassSection } ;
    static function ClassSectionSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassSectionSequence');
      Result := ClassSection(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ';') and ClassSection(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// RecordType = "record", [ ClassSectionSequence ], [ ";" ], "end" ;
    static function RecordType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('RecordType');
      Result := false;
      if (lexemes[cur_pos].value = 'record') then
      begin
        cur_pos += 1;
        if ClassSectionSequence(lexemes, cur_pos, next_pos, cur_command) then
          if (lexemes[next_pos].value = ';') then
            cur_pos := next_pos + 1;
        Result := (lexemes[cur_pos].value = 'end');
        if Result then
        begin
          next_pos := cur_pos + 1;
          upper_command.AddNode(cur_command);
        end;
      end;
    end;
    
    /// ClassType = [ ClassPrefix ], "class", ( [ Parameters ], [ ClassSectionSequence ], [ ";" ], "end" | Parameters, [ [ ClassSectionSequence ], [ ";" ], "end" ] ) ;
    static function ClassType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ClassType');
      if ClassPrefix(lexemes, cur_pos, next_pos, cur_command) then
        cur_pos := next_pos;
      Result := (lexemes[cur_pos].value = 'class');
      if Result then
      begin
        cur_pos += 1;
        var par := Parameters(lexemes, cur_pos, next_pos, cur_command);
        if par then
          cur_pos := next_pos;
        var seq := ClassSectionSequence(lexemes, cur_pos, next_pos, cur_command);
        if seq then
          cur_pos := next_pos;
        if (lexemes[cur_pos].value = ';') then cur_pos += 1;
        var &end := (lexemes[cur_pos].value = 'end');
        if &end then
          next_pos := cur_pos + 1;
        Result := &end or par and (not seq);
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Subrange = Factor, "..", Factor ;
    static function Subrange(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Subrange');
      Result := 
        Factor(lexemes, cur_pos, next_pos, cur_command) and
              (lexemes[next_pos].value = '..') and
              Factor(lexemes, next_pos + 1, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ArrayType = "array", [ "[", ( Subrange, { ",", Subrange } | { "," } ), "]" ] "of", Type ;
    static function ArrayType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ArrayType');
      Result := (lexemes[cur_pos].value = 'array');
      if Result then
        if (lexemes[cur_pos + 1].value = '[') then
        begin
          cur_command.AddNode('Dimension');
          var dim := 0;
          if Subrange(lexemes, cur_pos + 2, next_pos, cur_command) then
          begin
            repeat
              cur_pos := next_pos;
              dim += 1;
            until not ((lexemes[cur_pos].value = ',') and Subrange(lexemes, cur_pos + 1, next_pos, cur_command));
            Result :=
              (lexemes[cur_pos].value = ']') and
                          (lexemes[cur_pos + 1].value = 'of') and
                          &Type(lexemes, cur_pos + 2, next_pos, cur_command);
            if Result then
              upper_command.AddNode(cur_command);
            exit;
          end;
          cur_pos += 2;
          dim += 1;
          while (lexemes[cur_pos].value = ',') do
          begin
            cur_pos += 1;
            dim += 1;
          end;
          cur_command[0].AddNode($'{dim}');
          Result :=
            (lexemes[cur_pos].value = ']') and
                      (lexemes[cur_pos + 1].value = 'of') and
                      &Type(lexemes, cur_pos + 2, next_pos, cur_command);
          if Result then
            upper_command.AddNode(cur_command);
          exit;
        end else
        begin
          Result :=
            (lexemes[cur_pos + 1].value = 'of') and
                      &Type(lexemes, cur_pos + 2, next_pos, cur_command);
        end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// TypeList = Type, { ",", Type } ;
    static function TypeList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('TypeList');
      Result := &Type(lexemes, cur_pos, next_pos, cur_command);
      if Result then
      begin
        repeat
          cur_pos := next_pos;
        until not ((lexemes[cur_pos].value = ',') and &Type(lexemes, cur_pos + 1, next_pos, cur_command));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Type = ArrayType | PointerType | RecordType | ClassType | SetType | EnumType | Designator, "<", TypeList, ">" | Designator ;
    static function &Type(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Type');
      Result := ArrayType(lexemes, cur_pos, next_pos, cur_command) or
            PointerType(lexemes, cur_pos, next_pos, cur_command) or
            RecordType(lexemes, cur_pos, next_pos, cur_command) or
            ClassType(lexemes, cur_pos, next_pos, cur_command) or
            SetType(lexemes, cur_pos, next_pos, cur_command) or
            EnumType(lexemes, cur_pos, next_pos, cur_command);
      if (not Result) and Designator(lexemes, cur_pos, next_pos, cur_command) then
      begin
        Result := true;
        cur_pos := next_pos;
        if not ((lexemes[cur_pos].value = '<') and
                TypeList(lexemes, cur_pos + 1, next_pos, cur_command) and
        (lexemes[next_pos].value = '>')) then
          next_pos := cur_pos
        else
          next_pos += 1;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    (*/// ConstFactor = Identifier | Number | "true" | "false" | StringValue | ynil ;
    static function ConstFactor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      Result :=
        (lexemes[cur_pos] is LPascalIdentifier) or
        (lexemes[cur_pos] is LPascalNumber) or
        (lexemes[cur_pos].value = 'true') or
        (lexemes[cur_pos].value = 'false') or
        (lexemes[cur_pos] is LPascalString) or
        (lexemes[cur_pos].value = 'nil');
      if Result then
      begin
        upper_command.AddNode('ConstFactor').AddNode(lexemes[cur_pos].value);
        next_pos := cur_pos + 1;
      end;
    end;*)
    
    /// VariableDecl = Identifier, ":", Type, ":=", Expression
    ///              | Identifier, ":=", Expression
    ///              | IdentList, ":", Type ;
    static function VariableDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('VariableDecl');
      cur_command.AddNode(lexemes[cur_pos].value);
      Result := 
        (lexemes[cur_pos] is LPascalIdentifier) and
              (lexemes[cur_pos + 1].value = ':') and
              &Type(lexemes, cur_pos + 2, next_pos, cur_command) and
              (lexemes[next_pos].value = ':=') and
              Expression(lexemes, next_pos + 1, next_pos, cur_command);
      if not Result then
      begin
        if cur_command.Count > 1 then
          cur_command._nodes.RemoveAt(1);
        Result := 
          (lexemes[cur_pos] is LPascalIdentifier) and
                  (lexemes[cur_pos + 1].value = ':=') and
                  Expression(lexemes, cur_pos + 2, next_pos, cur_command);
        if not Result then
        begin
          cur_command._nodes.RemoveAt(0);
          Result := 
            IdentList(lexemes, cur_pos, next_pos, cur_command) and 
                      (lexemes[next_pos].value = ':') and 
                      &Type(lexemes, next_pos + 1, next_pos, cur_command);
        end;
      end;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// TypeDef = Identifier, "=", Type ;
    static function TypeDef(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('TypeDef');
      cur_command.AddNode(lexemes[cur_pos].value);
      Result := 
        (lexemes[cur_pos] is LPascalIdentifier) and 
              (lexemes[cur_pos + 1].value = '=') and 
              &Type(lexemes, cur_pos + 2, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// ConstantDef = Identifier, [ ":", Type ], "=", Expression ;
    static function ConstantDef(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ConstantDef');
      Result := (lexemes[cur_pos] is LPascalIdentifier);
      if Result then
      begin
        cur_command.AddNode(lexemes[cur_pos].value);
        cur_pos += 1;
        if (lexemes[cur_pos].value = ':') and
        &Type(lexemes, cur_pos + 1, next_pos, cur_command) then
          cur_pos := next_pos;
        Result := (lexemes[cur_pos].value = '=') and 
                Expression(lexemes, cur_pos + 1, next_pos, cur_command);
        //    ConstExpression(lexemes, cur_pos + 1, next_pos);
        if Result then
          upper_command.AddNode(cur_command);
      end;
    end;
    
    /// VariableDeclBlock = "var", VariableDecl, ";", { VariableDecl, ";" } ;
    static function VariableDeclBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('VariableDeclBlock');
      Result :=
        (lexemes[cur_pos].value = 'var') and
              VariableDecl(lexemes, cur_pos + 1, next_pos, cur_command) and
              (lexemes[next_pos].value = ';');
      if Result then
      begin
        repeat
          cur_pos := next_pos + 1;
        until not (VariableDecl(lexemes, cur_pos, next_pos, cur_command) and (lexemes[next_pos].value = ';'));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// TypeDefBlock = "type", TypeDef, ";", { TypeDef, ";" } ;
    static function TypeDefBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('TypeDefBlock');
      Result :=
        (lexemes[cur_pos].value = 'type') and
              TypeDef(lexemes, cur_pos + 1, next_pos, cur_command) and
              (lexemes[next_pos].value = ';');
      if Result then
      begin
        repeat
          cur_pos := next_pos + 1;
        until not (TypeDef(lexemes, cur_pos, next_pos, cur_command) and (lexemes[next_pos].value = ';'));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ConstantDefBlock = "const", ConstantDef, ";", { ConstantDef, ";" } ;
    static function ConstantDefBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ConstantDefBlock');
      Result := 
        (lexemes[cur_pos].value = 'const') and
              ConstantDef(lexemes, cur_pos + 1, next_pos, cur_command) and
              (lexemes[next_pos].value = ';');
      if Result then
      begin
        repeat
          cur_pos := next_pos + 1;
        until not (ConstantDef(lexemes, cur_pos, next_pos, cur_command) and (lexemes[next_pos].value = ';'));
        next_pos := cur_pos;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Declarations = ( ConstantDefBlock | TypeDefBlock | VariableDeclBlock | SubprogDeclList ), { ConstantDefBlock | TypeDefBlock | VariableDeclBlock | SubprogDeclList } ;
    static function Declarations(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Declarations');
      Result := false;
      while ConstantDefBlock(lexemes, cur_pos, next_pos, cur_command) or 
            TypeDefBlock(lexemes, cur_pos, next_pos, cur_command) or 
            VariableDeclBlock(lexemes, cur_pos, next_pos, cur_command) or 
            SubprogDeclList(lexemes, cur_pos, next_pos, cur_command) do
      begin
        cur_pos := next_pos;
        Result := true;
      end;
      next_pos := cur_pos;
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// Block = [ Declarations ], StatementSequence ;
    static function Block(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Block');
      if Declarations(lexemes, cur_pos, next_pos, cur_command) then
        cur_pos := next_pos;
      Result := StatementSequence(lexemes, cur_pos, next_pos, cur_command);
      if Result then
        upper_command.AddNode(cur_command);
    end;
    
    /// IdentList = Identifier, { ",", Identifier } ;
    static function IdentList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('IdentList');
      Result := lexemes[cur_pos] is LPascalIdentifier;
      if Result then
      begin
        cur_command.AddNode(lexemes[cur_pos].value);
        next_pos := cur_pos + 1;
        while (lexemes[next_pos].value = ',') and (lexemes[next_pos + 1] is LPascalIdentifier) do
        begin
          cur_command.AddNode(lexemes[next_pos + 1].value);
          next_pos += 2;
        end;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// Parameters = "(", IdentList, ")" ;
    static function Parameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('Parameters');
      Result :=
        (lexemes[cur_pos].value = '(') and
              IdentList(lexemes, cur_pos + 1, next_pos, cur_command) and
              (lexemes[next_pos].value = ')');
      if Result then
      begin
        next_pos += 1;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// ProgramModule = [ "program", Identifier, [ Parameters ], ";" ], Block, "." ;
    static function ProgramModule(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('ProgramModule');
      if (lexemes[cur_pos].value = 'program') and
      (lexemes[cur_pos + 1] is LPascalIdentifier) then
      begin
        cur_command.AddNode('program').AddNode(lexemes[cur_pos + 1].value);
        if Parameters(lexemes, cur_pos + 2, next_pos, cur_command) then
          cur_pos := next_pos
        else
          cur_pos += 2;
        Result := (lexemes[cur_pos].value = ';');
        if Result then
          cur_pos += 1
        else
          exit;
      end;
      Result :=
        Block(lexemes, cur_pos, next_pos, cur_command) and
              (lexemes[next_pos].value = '.');
      if Result then
      begin
        next_pos += 1;
        upper_command.AddNode(cur_command);
      end;
    end;
    
    /// CompilationUnit = ProgramModule ;
    static function CompilationUnit(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
    begin
      var cur_command := new node('CompilationUnit');
      Result := ProgramModule(lexemes, cur_pos, next_pos, cur_command);
      if Result then upper_command.AddNode(cur_command);
    end;
    
    static function Analyze(some_source_code: string; var lexemes: List<Lexem>; var tree_command: node): boolean;
    begin
      if PascalLexicalAnalyzer.Analyze(some_source_code, lexemes) then
      begin
        var next_pos := 0;
        Result := CompilationUnit(lexemes, 0, next_pos, tree_command) and (next_pos = lexemes.Count);
      end;
    end;
    
    static function Analyze(some_source_code: string; var tree_command: node): boolean;
    begin
      var lexemes := new List<Lexem>();
      Result := Analyze(some_source_code, lexemes, tree_command);
    end;
  end;

begin
  var input_text: string;
  if ParamCount > 0 then
    input_text := ReadAllText(PABCSystem.ParamStr(1))
  else
    input_text := ReadAllText('pascal.pas');
  var lexemes := new List<Lexem>();
  var tree_command := new node('');
  write('Grammar analysis...');
  var grammar_result := PascalGrammarAnalyzer.Analyze(input_text, lexemes, tree_command).Println();
  write('Lexemes: ');
  PrintLexemes(lexemes);
  if grammar_result then PrettyPrint(tree_command[0], 0, '');
end.