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
    
    (*constructor;
    begin
      _value := '';
      _nodes := new List<node>();
    end;*)
    
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
  LIdentifier = class(Lexem);
  LNumber = class(Lexem);
  LNInteger = class(LNumber);
  LNReal = class(LNumber);
  LString = class(Lexem);
  LReserved = class(Lexem);
  LSymbol = class(Lexem);
  
  Relation_ = (Equal, NotEqual, InSet);

const
  letters = ['a'..'z', 'A'..'Z', '_'];
  digits = ['0'..'9'];
  simblos = [',', '.', ';', ':', '^', '(', ')', '[', ']', '<', '>', '=', '+', '-', '*', '/', '$', '@', '&', '#'];
  reserved = ['and', 'downto', 'if', 'or', 'then', 'array', 'else', 'in', 'packed', 'to', 'begin', 'end', 'label', 'procedure', 'type', 'case', 'file', 'mod', 'program', 'until', 'const', 'for', 'nil', 'record', 'var', 'div', 'function', 'not', 'repeat', 'while', 'do', 'goto', 'of', 'set', 'with', 'class', 'private', 'public', 'protected', 'internal', 'constructor', 'destructor', 'property', 'auto', 'sealed', 'abstract', 'forward', 'extensionmethod', 'default', 'try', 'except', 'finally', 'on', 'operator'];
  
  Unary_Operator = ['+', '-', 'not', '@', '^'];
  Mult_Operator = ['*', '/', 'div', 'mod', 'and', 'shl', 'shr'];
  Add_Operator = ['+', '-', 'or', 'xor'];
  _Relation_ = ['=', '<>', '<', '>', '<=', '>=', 'in', 'is'];
  Assign_Operator = [':=', '+=', '-=', '*=', '/='];
  Operator_4_Overloading = Unary_Operator + Mult_Operator + Add_Operator + _Relation_ - ['is'] + ['implicit', 'explicit'];
  class_prefix = ['auto', 'sealed', 'abstract'];
  //pre_declarations = ['forward', 'extensionmethod'];


function AssociativeToLeft(n: node): node;
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

function isLetter(str: string; cur_pos: integer): boolean;
begin
  Result := SafeCharIn(str, cur_pos, letters);
end;

function isDigit(str: string; cur_pos: integer): boolean;
begin
  Result := SafeCharIn(str, cur_pos, digits);
end;

function isIdentifier(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  if isLetter(str, cur_pos) then 
  begin
    Result := true;
    next_pos := cur_pos + 1;
    while(isLetter(str, next_pos) or isDigit(str, next_pos)) do
      next_pos += 1;
  end
  else Result := false;
end;

function isInteger(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  if isDigit(str, cur_pos) then 
  begin
    Result := true;
    next_pos := cur_pos + 1;
    while(isDigit(str, next_pos)) do
      next_pos += 1;
  end
  else Result := false;
end;

function isExpDesignator(str: string; cur_pos: integer): boolean;
begin
  var str_cur_pos := str[cur_pos];
  Result := (str_cur_pos = 'e') or (str_cur_pos = 'E');
end;

function isExponent(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  if isExpDesignator(str, cur_pos) then 
  begin
    var t := cur_pos + 1;
    var _chr := str[next_pos];
    if (_chr = '+') or (_chr = '-') then
      t += 1;
    Result := isInteger(str, t, next_pos);
    exit;
  end;
  Result := false;
end;

function isReal(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  if isInteger(str, cur_pos, next_pos) then 
    if SafeCharComp(str, next_pos, Relation_.Equal, '.') then
      if isInteger(str, next_pos + 1, next_pos) then
      begin
        Result := true;
        isExponent(str, next_pos, next_pos);
        exit;
      end;
  Result := false;
end;

function isNumber(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  Result := isInteger(str, cur_pos, next_pos) or isReal(str, cur_pos, next_pos);
end;

function isString(str: string; cur_pos: integer; var next_pos: integer): boolean;
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

function isReserved(str: string; cur_pos: integer; var next_pos: integer): boolean;
begin
  Result := false;
  if isIdentifier(str, cur_pos, next_pos) then
  begin
    Result := str.Substring(cur_pos - 1, next_pos - cur_pos).ToLower() in reserved;
  end;
end;

function isSymbol(str: string; cur_pos: integer; var next_pos: integer): boolean;
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

procedure AddNewLexem(lexemes: List<lexem>; str: string; l: lexem; cur_pos: integer; next_pos: integer);
begin
  l.start_index := cur_pos;
  l.value := str.Substring(cur_pos - 1, next_pos - cur_pos);
  if not (l is LString) then
    l.value := l.value.ToLower();
  lexemes.Add(l);
end;

function UnaryOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
begin
  Result := lexemes[cur_pos].value in Unary_Operator;
  if Result then
    upper_command.AddNode('UnaryOperator').AddNode($' {lexemes[cur_pos].value}');
end;

function MultOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
begin
  
  Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Mult_Operator);
  if Result then
    upper_command.AddNode('MultOperator').AddNode($' {lexemes[cur_pos].value}');
end;

function AddOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
begin
  Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, Add_Operator);
  if Result then
    upper_command.AddNode('AddOperator').AddNode($' {lexemes[cur_pos].value}');
end;

function Relation(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
begin
  Result := SafeLexemComp(lexemes, cur_pos, Relation_.InSet, _Relation_);
  if Result then
    upper_command.AddNode('Relation').AddNode($' {lexemes[cur_pos].value}');
end;

function AssignmentOperator(lexemes: List<lexem>; cur_pos: integer; upper_command: node): boolean;
begin
  Result := lexemes[cur_pos].value in Assign_Operator;
  if Result then
    upper_command.AddNode('AssignmentOperator').AddNode($' {lexemes[cur_pos].value}');
end;

// <Gramar>
/// IdentList --> yident {',' yident}
function IdentList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// Parameters --> '(' IdentList ')' 
function Parameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

function &Type(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

function ConstFactor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

function Subrange(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// VariableDecl --> yident ':' Type ':=' Expression
///                | yident ':=' Expression
///                | IdentList ':' Type
function VariableDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// StatementSequence --> ybegin Statement {';' Statement} [ ';' ] yend 
function StatementSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

function Block(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

function ConstExpression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// ActualParameters --> '(' ExpList ')'
function ActualParameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// Expression --> SimpleExpression [ Relation SimpleExpression ]
function Expression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// Designator --> yident [ DesignatorStuff ] 
function Designator(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// DesignatorStuff --> ('.' yident | '[' ExpList ']' | '^')
function DesignatorStuff(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

/// Statement --> Assignment
///             | ProcedureCall
///             | IfStatement
///             | CaseStatement
///             | WhileStatement
///             | RepeatStatement
///             | ForStatement
///             | IOStatement
///             | MemoryStatement
///             | StatementSequence
function Statement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean; forward;

(*/// PreDeclaration --> # forward | extensionmethod # {';' # forward | extensionmethod #}
function PreDeclaration(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer): boolean;
begin
  Result := false;
  var &set: set of string := pre_declarations;
  Result := (lexemes[cur_pos].value in &set);
  if Result then
  begin
    Exclude(&set, lexemes[cur_pos].value);
    cur_pos += 1;
    while (lexemes[cur_pos].value = ';') and (lexemes[cur_pos + 1].value in &set) do
    begin
      Exclude(&set, lexemes[cur_pos + 1].value);
      cur_pos += 2;
    end;
    next_pos := cur_pos;
  end;
end;*)

/// OneFormalParam --> [yvar] IdentList ':' Type
function OneFormalParam(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// FormalParameters --> '(' OneFormalParam {';' OneFormalParam} ')'
function FormalParameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// FunctionHeading --> ['static'] 'function' Designator [FormalParameters]
///                   | ['static'] 'function' operator OperatorForOverloading [FormalParameters]
function FunctionHeading(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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
      (lexemes[cur_pos + 2].value in Operator_4_Overloading) then
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

/// ProcedureHeading --> ['static'] 'procedure' Designator [FormalParameters] 
///                    | ['static'] 'procedure' operator AssignmentOperator [FormalParameters] 
function ProcedureHeading(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// FunctionDecl --> FunctionHeading ':' Type ';' forward
///                | FunctionHeading ':' Type ';' ['extensionmethod' ';'] Block
function FunctionDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ProcedureDecl --> ProcedureHeading ';' ['extensionmethod' ';'] Block
///                 | ProcedureHeading ';' forward
function ProcedureDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// SubprogDeclList --> (ProcedureDecl ';' | FunctionDecl ';')
function SubprogDeclList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Element --> ConstExpression ['..' ConstExpression ]
function Element(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// FunctionCall --> Designator ActualParameters
///                | new Type [ActualParameters]
function FunctionCall(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// SetValue --> '[' [Element {',' Element} ] ']' 
function SetValue(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Factor --> # ynumber | ystring | $ ystring | ytrue | yfalse | ynil | '(' Expression ')' | FunctionCall | ynot Factor | Setvalue | Designator # [DesignatorStuff]
function Factor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Factor');
  Result := (lexemes[cur_pos] is LNumber) or
    (lexemes[cur_pos] is LString) or 
    (lexemes[cur_pos].value = 'true') or 
    (lexemes[cur_pos].value = 'false') or 
    (lexemes[cur_pos].value = 'nil');
  if Result then
  begin
    next_pos := cur_pos + 1;
    cur_command.AddNode(lexemes[cur_pos].value);
  end
  else
  begin
    Result := 
      (lexemes[cur_pos].value = '$') and
      (lexemes[cur_pos + 1] is LString);
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
        next_pos := next_pos + 1
      else
        Result := FunctionCall(lexemes, cur_pos, next_pos, cur_command) or
          SetValue(lexemes, cur_pos, next_pos, cur_command) or
          Designator(lexemes, cur_pos, next_pos, cur_command);
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

/// Term --> Factor {MultOperator Factor} 
function Term(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// SimpleExpression --> [UnaryOperator] AssociativeToLeft(Term) {AddOperator AssociativeToLeft(Term)} 
function SimpleExpression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Expression --> AssociativeToLeft(SimpleExpression) [Relation AssociativeToLeft(SimpleExpression)]
function Expression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// MemoryStatement --> ynew '(' yident ')' | ydispose '(' yident ')'
function MemoryStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('MemoryStatement');
  Result := ((lexemes[cur_pos].value = 'new') or 
    (lexemes[cur_pos].value = 'dispose'))
    and
    (lexemes[cur_pos + 1].value = '(') and 
    (lexemes[cur_pos + 2] is LIdentifier) and 
    (lexemes[cur_pos + 3].value = ')');
  if Result then
  begin
    cur_command.AddNode(lexemes[cur_pos].value);
    cur_command.AddNode(lexemes[cur_pos + 2].value);
    upper_command.AddNode(cur_command);
    next_pos := cur_pos + 4;
  end;
end;

/// ExpList --> Expression { ',' Expression } 
function ExpList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ActualParameters --> '(' [ExpList] ')'
function ActualParameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// DesignatorStuff --> #'.' yident [ActualParameters] | '[' ExpList ']' | '^'# [DesignatorStuff]
function DesignatorStuff(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('DesignatorStuff');
  Result := false;
  if SafeLexemComp(lexemes, cur_pos, Relation_.Equal, '.') and 
    (lexemes[cur_pos + 1] is LIdentifier) then
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

/// Designator --> yident [ DesignatorStuff ] 
function Designator(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Designator');
  Result := (lexemes[cur_pos] is LIdentifier);
  if Result then
  begin
    cur_command.AddNode(lexemes[cur_pos].value);
    if not DesignatorStuff(lexemes, cur_pos + 1, next_pos, cur_command) then
      next_pos := cur_pos + 1;
    upper_command.AddNode(cur_command);
  end;
end;

/// DesignatorList --> Designator {',' Designator } 
function DesignatorList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// IOStatement --> yread '(' DesignatorList ')'
///               | ywrite '(' ExpList ')'
///               | yreadln [ '(' DesignatorList ')' ]
///               | ywriteln [ '(' ExpList ')' ] 
function IOStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// WhichWay --> yto | ydownto
function WhichWay(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  Result := (lexemes[cur_pos].value = 'to') or 
    (lexemes[cur_pos].value = 'downto');
  if Result then
  begin
    upper_command.AddNode('WhichWay').AddNode(lexemes[cur_pos].value);
    next_pos := cur_pos + 1;
  end;
end;

/// ForStatement --> yfor [var] yident ':=' Expression WhichWay Expression ydo Statement
function ForStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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
      (lexemes[cur_pos] is LIdentifier) and
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

/// RepeatStatement --> yrepeat [ Statement {';' Statement} [';'] ] yuntil Expression
function RepeatStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// WhileStatement --> ywhile Expression ydo Statement
function WhileStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// CaseLabelList --> ConstExpression {',' ConstExpression }
function CaseLabelList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('CaseLabelList');
  Result := Expression(lexemes, cur_pos, next_pos, cur_command);
  if Result then
  begin
    repeat
      cur_pos := next_pos;
    until not ((lexemes[cur_pos].value = ',') and ConstExpression(lexemes, cur_pos + 1, next_pos, cur_command));
    next_pos := cur_pos;
  end;
  if Result then
    upper_command.AddNode(cur_command);
end;

/// Case --> CaseLabelList ':' Statement 
function &Case(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Case');
  Result := 
    CaseLabelList(lexemes, cur_pos, next_pos, cur_command) and 
    (lexemes[next_pos].value = ':') and 
    Statement(lexemes, next_pos + 1, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// CaseStatement --> ycase Expression yof Case {';' Case} [';'] yend
function CaseStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// IfStatement --> yif Expression ythen Statement [yelse Statement] 
function IfStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ProcedureCall --> Designator [ActualParameters] 
function ProcedureCall(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Assignment --> Designator #':='|'+='|'-='|'*='|'/='# Expression 
function Assignment(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ExceptionProcBlock --> (on [Ident ':'] Type 'do' Statement ';')+ ['else' Statement ';']
///                      | {Statement ';'}
function ExceptionProcBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ExceptionProcBlock');
  Result := true;
  if (lexemes[cur_pos].value = 'on') then
  begin
    while true do
    begin
      if (lexemes[cur_pos].value = 'on') then
      begin
        cur_pos += 1;
        var sub_command := cur_command.AddNode('on');
        if (lexemes[cur_pos] is LIdentifier) and
        (lexemes[cur_pos + 1].value = ':') then
        begin
          sub_command.AddNode(lexemes[cur_pos].value);
          cur_pos += 2;
        end;
        if &Type(lexemes, cur_pos, next_pos, sub_command) and
        (lexemes[next_pos].value = 'do') and
        Statement(lexemes, next_pos + 1, next_pos, sub_command) and
        (lexemes[next_pos].value = ';') then
        begin
          cur_pos := next_pos + 1;
          continue;
        end
        else begin
          Result := false;
          exit;
        end;
      end;
      break;
    end;
    if (lexemes[cur_pos].value = 'else') then
      if Statement(lexemes, cur_pos + 1, next_pos, cur_command.AddNode('else')) and
      (lexemes[next_pos].value = ';') then
      begin
        next_pos += 1;
        upper_command.AddNode(cur_command);
        exit;
      end
      else begin
        Result := false;
        exit;
      end
  end
  else begin
    while (Statement(lexemes, cur_pos, next_pos, cur_command) and (lexemes[next_pos].value = ';')) do
      cur_pos := next_pos + 1;
  end;
  next_pos := cur_pos;
  upper_command.AddNode(cur_command);
end;

/// TryStatement --> 'try' {Statement ';'} 'except' ExceptionProcBlock 'end'
///                | 'try' {Statement ';'} 'finally'  {Statement ';'} 'end'
function TryStatement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('TryStatement');
  Result := false;
  if (lexemes[cur_pos].value = 'try') then
  begin
    cur_pos += 1;
    while (Statement(lexemes, cur_pos, next_pos, cur_command) and (lexemes[next_pos].value = ';')) do
      cur_pos := next_pos + 1;
    if (lexemes[cur_pos].value = 'except') then
    begin
      cur_pos += 1;
      Result := ExceptionProcBlock(lexemes, cur_pos, next_pos, cur_command.AddNode('except'));
      cur_pos := next_pos;
    end
    else if (lexemes[cur_pos].value = 'finally') then
    begin
      cur_pos += 1;
      var sub_command := cur_command.AddNode('finally');
      while (Statement(lexemes, cur_pos, next_pos, sub_command) and (lexemes[next_pos].value = ';')) do
        cur_pos := next_pos + 1;
      Result := true;
    end;
    Result := Result and (lexemes[cur_pos].value = 'end');
  end;
  if Result then
  begin
    next_pos := cur_pos + 1;
    upper_command.AddNode(cur_command);
  end;
end;

/// Statement --> Assignment
///             | IOStatement
///             | ProcedureCall
///             | IfStatement
///             | CaseStatement
///             | WhileStatement
///             | RepeatStatement
///             | ForStatement
///             | MemoryStatement
///             | var VariableDecl
///             | TryStatement
///             | StatementSequence
function Statement(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// StatementSequence --> ybegin [Statement {';' Statement} [ ';' ]] yend 
function StatementSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// FieldList --> IdentList ':' Type 
function FieldList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('FieldList');
  Result := 
    IdentList(lexemes, cur_pos, next_pos, cur_command) and 
    (lexemes[next_pos].value = ':') and 
    &Type(lexemes, next_pos + 1, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// FieldListSequence --> FieldList {';' FieldList} 
function FieldListSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// PointerType --> '^' yident
function PointerType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  Result :=
    (lexemes[cur_pos].value = '^') and
    (lexemes[cur_pos + 1] is LIdentifier);
  if Result then
  begin
    next_pos := cur_pos + 2;
    upper_command.AddNode('PointerType').AddNode(lexemes[cur_pos + 1].value);
  end;
end;

/// EnumType --> '(' IdentList ')'
function EnumType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// SetType --> set of Type
function SetType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('SetType');
  Result :=
    (lexemes[cur_pos].value = 'set') and
    (lexemes[cur_pos + 1].value = 'of') and
    &Type(lexemes, cur_pos + 2, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// ClassPrefix --> yauto [ ysealed ]
///               | ysealed [ yauto ]
function ClassPrefix(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ClassConstructor --> constructor [ident] [FormalParameters] ';' StatementSequence
function ClassConstructor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ClassConstructor');
  Result := 
    (lexemes[cur_pos].value = 'constructor');
  if Result then
  begin
    cur_pos += 1;
    if (lexemes[cur_pos] is LIdentifier) then
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

/// ClassDestructor --> destructor [ident] ';' StatementSequence
function ClassDestructor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ClassDestructor');
  Result := 
    (lexemes[cur_pos].value = 'destructor');
  if Result then
  begin
    cur_pos += 1;
    if (lexemes[cur_pos] is LIdentifier) then
    begin
      cur_command.AddNode(lexemes[cur_pos].value);
      cur_pos += 1;
    end;
    Result := (lexemes[cur_pos].value = ';') and StatementSequence(lexemes, cur_pos + 1, next_pos, cur_command);
  end;
  if Result then
    upper_command.AddNode(cur_command);
end;

/// AccessModifiers -->  public | protected internal | protected | internal | private protected | private
function AccessModifiers(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// PropertyReader --> Expression | Designator
function PropertyReader(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('PropertyReader');
  Result := 
    Expression(lexemes, cur_pos, next_pos, cur_command) or
    Designator(lexemes, cur_pos, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// PropertyWriter -->  Assignment | Designator
function PropertyWriter(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('PropertyWriter');
  Result := 
    Assignment(lexemes, cur_pos, next_pos, cur_command) or
    Designator(lexemes, cur_pos, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;


/// Property -->  property ident [ '[' FieldList ']' ] ":" Type [read PropertyReader] write PropertyWriter [';' default]
///            |  property ident [ '[' FieldList ']' ] ":" Type read PropertyReader [write PropertyWriter] [';' default]
function &Property(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Property');
  Result := 
    (lexemes[cur_pos].value = 'property') and
    (lexemes[cur_pos + 1] is LIdentifier);
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

/// PropertyList -->  Property {";" Property}
function PropertyList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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
end;

/// ClassSection -->  [ AccessModifiers ] (FieldListSequence | SubprogDeclList | PropertyList | ClassConstructor | ClassDestructor)
function ClassSection(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ClassSection');
  if AccessModifiers(lexemes, cur_pos, next_pos, cur_command) then
    cur_pos := next_pos;
  Result := false;
  while FieldListSequence(lexemes, cur_pos, next_pos, cur_command) or
  SubprogDeclList(lexemes, cur_pos, next_pos, cur_command) or
  PropertyList(lexemes, cur_pos, next_pos, cur_command) or
  ClassConstructor(lexemes, cur_pos, next_pos, cur_command) or
  ClassDestructor(lexemes, cur_pos, next_pos, cur_command) do
  begin
    cur_pos := next_pos;
    Result := true;
  end;
  next_pos := cur_pos;
  if Result then
    upper_command.AddNode(cur_command);
end;

/// ClassSectionSequence -->  ClassSection {';' ClassSection} 
function ClassSectionSequence(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// RecordType --> yrecord [ClassSectionSequence] [';'] yend 
function RecordType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ClassType --> [ClassPrefix] class [Parameters] [ClassSectionSequence] [';'] end 
///             | [ClassPrefix] class Parameters [ [ClassSectionSequence] [';'] end]
function ClassType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Subrange --> ConstFactor '..' ConstFactor
function Subrange(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Subrange');
  Result := 
    ConstFactor(lexemes, cur_pos, next_pos, cur_command) and
    (lexemes[next_pos].value = '..') and
    ConstFactor(lexemes, next_pos + 1, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// ArrayType --> yarray [ '[' Subrange {',' Subrange} ']' ] yof Type
///             | yarray [ '[' {','} ']' ] yof Type
function ArrayType(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// TypeList --> Type {',' Type}
function TypeList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Type --> ArrayType | PointerType | RecordType | ClassType | SetType | EnumType | Designator '<' TypeList '>' | Designator
function &Type(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ConstFactor --> yident | ynumber | ytrue | yfalse | ystring | ynil
function ConstFactor(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  Result :=
    (lexemes[cur_pos] is LIdentifier) or
    (lexemes[cur_pos] is LNumber) or
    (lexemes[cur_pos].value = 'true') or
    (lexemes[cur_pos].value = 'false') or
    (lexemes[cur_pos] is LString) or
    (lexemes[cur_pos].value = 'nil');
  if Result then
  begin
    upper_command.AddNode('ConstFactor').AddNode(lexemes[cur_pos].value);
    next_pos := cur_pos + 1;
  end;
end;

/// ConstExpression --> [UnaryOperator] ConstFactor
function ConstExpression(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ConstExpression');
  if UnaryOperator(lexemes, cur_pos, cur_command) then
    cur_pos += 1;
  Result := ConstFactor(lexemes, cur_pos, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// VariableDecl --> yident ':' Type ':=' Expression
///                | yident ':=' Expression
///                | IdentList ':' Type
function VariableDecl(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('VariableDecl');
  cur_command.AddNode(lexemes[cur_pos].value);
  Result := 
    (lexemes[cur_pos] is LIdentifier) and
    (lexemes[cur_pos + 1].value = ':') and
    &Type(lexemes, cur_pos + 2, next_pos, cur_command) and
    (lexemes[next_pos].value = ':=') and
    Expression(lexemes, next_pos + 1, next_pos, cur_command);
  if not Result then
  begin
    if cur_command.Count > 1 then
      cur_command._nodes.RemoveAt(1);
    Result := 
      (lexemes[cur_pos] is LIdentifier) and
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

/// TypeDef --> yident '=' Type
function TypeDef(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('TypeDef');
  cur_command.AddNode(lexemes[cur_pos].value);
  Result := 
    (lexemes[cur_pos] is LIdentifier) and 
    (lexemes[cur_pos + 1].value = '=') and 
    &Type(lexemes, cur_pos + 2, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// ConstantDef --> yident [':' Type] '=' ConstExpression
function ConstantDef(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ConstantDef');
  Result := (lexemes[cur_pos] is LIdentifier);
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

/// VariableDeclBlock --> yvar VariableDecl ';' {VariableDecl ';'} 
function VariableDeclBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// TypeDefBlock --> ytype TypeDef ';' {TypeDef ';'} 
function TypeDefBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ConstantDefBlock --> yconst ConstantDef ';' {ConstantDef ';'} 
function ConstantDefBlock(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Declarations --> (ConstantDefBlock | TypeDefBlock | VariableDeclBlock | SubprogDeclList)
function Declarations(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// Block --> [Declarations] StatementSequence
function Block(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('Block');
  if Declarations(lexemes, cur_pos, next_pos, cur_command) then
    cur_pos := next_pos;
  Result := StatementSequence(lexemes, cur_pos, next_pos, cur_command);
  if Result then
    upper_command.AddNode(cur_command);
end;

/// IdentList --> yident {',' yident}
function IdentList(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('IdentList');
  Result := lexemes[cur_pos] is LIdentifier;
  if Result then
  begin
    cur_command.AddNode(lexemes[cur_pos].value);
    next_pos := cur_pos + 1;
    while (lexemes[next_pos].value = ',') and (lexemes[next_pos + 1] is LIdentifier) do
    begin
      cur_command.AddNode(lexemes[next_pos + 1].value);
      next_pos += 2;
    end;
    upper_command.AddNode(cur_command);
  end;
end;

/// Parameters --> '(' IdentList ')' 
function Parameters(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
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

/// ProgramModule --> [yprogram yident [Parameters] ';'] Block '.'
function ProgramModule(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('ProgramModule');
  if (lexemes[cur_pos].value = 'program') and
  (lexemes[cur_pos + 1] is LIdentifier) then
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

/// CompilationUnit --> ProgramModule
function CompilationUnit(lexemes: List<lexem>; cur_pos: integer; var next_pos: integer; upper_command: node): boolean;
begin
  var cur_command := new node('CompilationUnit');
  Result := ProgramModule(lexemes, cur_pos, next_pos, cur_command);
  if Result then upper_command.AddNode(cur_command);
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

begin
  // 1. Lexical
  var input_text: string;
  var lexemes := new List<Lexem>();
  if ParamCount > 0 then
    input_text := ReadAllText(PABCSystem.ParamStr(1))
  else
    input_text := ReadAllText('pascal.pas');      /////////////////////////////////////////
  input_text := PABCSystem.Trim(input_text);
  write('Lexical analysis...');
  var cur_index := 1;
  while cur_index <= input_text.Length do
  begin
    var l: lexem;
    var next_index: integer;
    var isNotFind := true;
    var cur_char := input_text[cur_index];
    //var cur_ord := ord(cur_char);
    
    while (cur_char = #9) or (cur_char = #10) or (cur_char = #13) or (cur_char = ' ') do
    begin
      cur_index += 1;
      cur_char := input_text[cur_index];
    end;
    
    if (cur_char = '(') and (input_text[cur_index + 1] = '*') then
    begin
      cur_index := Pos('*)', input_text, cur_index + 2) + 2;
      continue;
    end;
    if (cur_char = '{') then
    begin
      cur_index := Pos('}', input_text, cur_index) + 1;
      continue;
    end;
    if (cur_char = '/') and (input_text[cur_index + 1] = '/') then
    begin
      cur_index := Pos(#10, input_text, cur_index) + 1;
      continue;
    end;
    
    if isSymbol(input_text, cur_index, next_index) then
    begin
      l := new LSymbol();
      isNotFind := false;
    end;
    
    if isNotFind and isReserved(input_text, cur_index, next_index) then
    begin
      if SafeLastLexemValueComp(lexemes, Relation_.Equal, '&') then
      begin
        lexemes.RemoveAt(lexemes.Count - 1);
        l := new LIdentifier();
        AddNewLexem(lexemes, input_text, l, cur_index - 1, next_index);
        cur_index := next_index;
        continue;
      end else
        l := new LReserved();
      isNotFind := false;
    end;
    
    if isNotFind and isIdentifier(input_text, cur_index, next_index) then
    begin
      l := new LIdentifier();
      isNotFind := false;
    end;
    
    if isNotFind and isReal(input_text, cur_index, next_index) then
    begin
      l := new LNReal();
      isNotFind := false;
    end;
    
    if isNotFind and isInteger(input_text, cur_index, next_index) then
    begin
      if SafeLastLexemValueComp(lexemes, Relation_.Equal, '#') then
      begin
        lexemes.RemoveAt(lexemes.Count - 1);
        l := new LString();
        AddNewLexem(lexemes, input_text, l, cur_index - 1, next_index);
        cur_index := next_index;
        continue;
      end else
        l := new LNInteger();
      isNotFind := false;
    end;
    
    if isNotFind and isString(input_text, cur_index, next_index) then
    begin
      l := new LString();
      isNotFind := false;
    end;
    
    if isNotFind then
    begin
      writeln();
      writeln($'Error: [{cur_index}] {input_text.Substring(cur_index-1, min(20, input_text.Length))} ...');
      exit;
    end
    else
    begin
      AddNewLexem(lexemes, input_text, l, cur_index, next_index);
      cur_index := next_index;
    end;
  end;
  writeln(True);
  //for var i := 0 to lexemes.Count - 1 do
  //  println($'[{i}] {lexemes[i].start_index}: "{lexemes[i].value}" ({lexemes[i].GetType.Name})');
  
  // 2. Grammar
  write('Grammar analysis...');
  var next_pos := 0;
  var tree_command := new node('');
  var grammar_result := CompilationUnit(lexemes, 0, next_pos, tree_command) and (next_pos = lexemes.Count).Println;
  if grammar_result then PrettyPrint(tree_command[0], 0, '');
end.