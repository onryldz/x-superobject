(*
  *                       Cross Super Object Toolkit
  *
  * Usage allowed under the restrictions of the Lesser GNU General Public License
  * or alternatively the restrictions of the Mozilla Public License 1.1
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  * the specific language governing rights and limitations under the License.
  *
  * Embarcadero Technologies Inc is not permitted to use or redistribute
  * this source code without explicit permission.
  *
  * Unit owner : Onur YILDIZ <onryldz10@gmail.com>
  * Web site   : http://www.caracaldev.org
  *
*)
unit XSuperJSON;

interface

uses
  SysUtils, Classes, Generics.Collections;


const
  CNull = 'null';
  MaxCHR = #127;

  Err_UnexpectedEndOfInput = 'Unexpected end of input';
  Err_Expected = 'Expected %s';
  Err_ExpectedButFound = '"%s" expected but "%s" found';
  Err_UnexpectedTokenILLEGAL = 'Unexpected token ILLEGAL';

type

  // ## Forward Declarations
  // -----------------------

  TJSONNull = class;
  TLexGenerator = class;
  TRoute = class;
  TPosition = record
    Col: Integer;
    Line: Integer;
  end;

  TDataType = (dtNil, dtNull, dtObject, dtArray, dtString, dtInteger, dtFloat, dtBoolean);

  // ## Exception

  TJSONSyntaxError = class(Exception)
  public
    constructor Create(const Msg: String; Pos: TPosition);
    constructor CreateFmt(const Msg: String; const Args: array of TVarRec; Pos: TPosition);
  end;


  // ## JSON Symbols
  // ---------------

  IJSONAncestor = interface
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
    function GetDataType: TDataType;
    function GetIsNull: Boolean;
    procedure AsJSONString(Str: TStringBuilder);
    property IsNull: Boolean read GetIsNull;
    property DataType: TDataType read GetDataType;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  TJSONAncestor = class abstract(TInterfacedObject, IJSONAncestor)
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  protected
    function GetDataType: TDataType; virtual;
    function GetIsNull: Boolean; virtual;
  public
    procedure AsJSONString(Str: TStringBuilder); virtual;
    property IsNull: Boolean read GetIsNull;
    property DataType: TDataType read GetDataType;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  IJSONValue<T> = interface(IJSONAncestor)
    function GetData: T;
    procedure SetData(const Value: T);
    procedure SetNull;
    property Value: T read GetData write SetData;
  end;

  TJSONValue<T> = class abstract(TJSONAncestor, IJSONValue<T>)
  public
    FNull: Boolean;
    FData: T;
  protected
    function GetData: T; virtual;
    procedure SetData(const Value: T); virtual;
    function GetIsNull: Boolean; override;
    property Value: T read GetData write SetData;
  public
    constructor Create(const Value: T);
    constructor CreateNull;
    procedure SetNull;
  end;

  IJSONNull = interface(IJSONValue<Boolean>) end;
  TJSONNull = class(TJSONValue<Boolean>, IJSONNull)
  protected
    procedure AsJSONString(Str: TStringBuilder); override;
    function GetIsNull: Boolean; override;
  end;

  IJSONBoolean = interface(IJSONValue<Boolean>)end;
  TJSONBoolean = class(TJSONValue<Boolean>, IJSONBoolean)
  protected
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    property Value;
  end;

  IJSONString = interface(IJSONValue<String>)end;
  TJSONString = class(TJSONValue<String>, IJSONString)
  protected
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    property Value;
  end;

  IJSONInteger = interface(IJSONValue<Int64>)end;
  TJSONInteger = class(TJSONValue<Int64>, IJSONInteger)
  protected
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    property Value;
  end;

  IJSONFloat = interface(IJSONValue<Double>)end;
  TJSONFloat = class(TJSONValue<Double>, IJSONFloat)
  protected
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    property Value;
  end;

  IJSONPair = interface
    function GetName: String;
    function GetValue: IJSONAncestor;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: IJSONAncestor);
    property Name: String read GetName write SetName;
    property JSONValue: IJSONAncestor read GetValue write SetValue;
  end;

  TJSONPair = class(TInterfacedObject, IJSONPair)
  private
    FName: String;
    FValue: IJSONAncestor;
    function GetName: String;
    function GetValue: IJSONAncestor;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: IJSONAncestor);
  public
    constructor Create(const aName: String; aValue: IJSONAncestor);
    destructor Destroy; override;
    property Name: String read GetName write SetName;
    property JSONValue: IJSONAncestor read GetValue write SetValue;
  end;

  TJSONEnumerator<T> = record
    Index : Integer;
    List : TList<T>;
    function MoveNext : Boolean;
    function GetCurrent : T;
    property Current : T read GetCurrent;
  end;

  IJSONObject = Interface(IJSONValue<IJSONPair>)
    function Count: Integer;
    function Get(const Name: String): IJSONPair; overload;
    function Get(const Index: Integer): IJSONPair; overload;
    procedure AddPair(P: IJSONPair); overload;
    procedure AddPair(Name: String; Value: IJSONAncestor); overload;
    procedure Remove(P: IJSONPair); overload;
    procedure Remove(const Name: String); overload;
    procedure Remove(const Index: Integer); overload;
    function GetEnumerator: TJSONEnumerator<IJSONPair>;
  end;

  TJSONObject = class(TJSONValue<IJSONPair>, IJSONObject)
  private
    FPairList: TList<IJSONPair>;
    FNull: Boolean;
  protected
    function GetIsNull: Boolean; override;
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function Get(const Name: String): IJSONPair; overload;
    function Get(const Index: Integer): IJSONPair; overload;
    procedure AddPair(P: IJSONPair); overload;
    procedure AddPair(Name: String; Value: IJSONAncestor); overload; inline;
    procedure Remove(P: IJSONPair); overload; inline;
    procedure Remove(const Name: String); overload;
    procedure Remove(const Index: Integer); overload;
    function GetEnumerator: TJSONEnumerator<IJSONPair>;
    class function ParseJSONValue(const Str: String): IJSONAncestor;
  end;

  IJSONArray = interface(IJSONValue<IJSONAncestor>)
    procedure Add(Val: IJSONAncestor);
    procedure Remove(Val: IJSONAncestor); overload;
    procedure Remove(Index: Integer); overload;
    procedure Clear;
    function Count: Integer;
    function Get(const I: Integer): IJSONAncestor;
    function GetEnumerator: TJSONEnumerator<IJSONAncestor>;
    property Index[const Int: Integer]: IJSONAncestor read Get; default;
  end;

  TJSONArray = class(TJSONValue<IJSONAncestor>, IJSONArray)
  private
    FList: TList<IJSONAncestor>;
    FNull: Boolean;
  protected
    function GetIsNull: Boolean; override;
    procedure AsJSONString(Str: TStringBuilder); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Val: IJSONAncestor);
    procedure Remove(Val: IJSONAncestor); overload;
    procedure Remove(Index: Integer); overload;
    procedure Clear;
    function Count: Integer;
    function Get(const I: Integer): IJSONAncestor;
    function GetEnumerator: TJSONEnumerator<IJSONAncestor>;
    property Index[const Int: Integer]: IJSONAncestor read Get; default;
  end;


  TJSONBuilder = class
  private
    LGen: TLexGenerator;
  public
    constructor Create(const JSON: String);
    destructor Destroy; override;
    function  ReadValue: IJSONAncestor;
    procedure ReadString(var Val: IJSONAncestor);
    procedure ReadInteger(var Val: IJSONAncestor);
    procedure ReadFloat(var Val: IJSONAncestor);
    procedure ReadObject(var Val: IJSONAncestor);
    procedure ReadTrue(var Val: IJSONAncestor);
    procedure ReadFalse(var Val: IJSONAncestor);
    procedure ReadNull(var Val: IJSONAncestor);
    procedure ReadArray(var Val: IJSONAncestor);
  end;

  TJSONInterpreter = class
  private
    LGen: TLexGenerator;
    FJSON: IJSONAncestor;
    FExceptionBlock: Boolean;
    function ReadName(Base: IJSONAncestor): IJSONAncestor;
    function ReadArrayIndex(Base: IJSONArray): IJSONAncestor;
    function ReadObject(Base: IJSONAncestor): IJSONObject;
    function ReadArray(Base: IJSONAncestor): IJSONArray;
    function ReadValue(Base: IJSONAncestor): IJSONAncestor;
    procedure CreateExcept(const S: String; Args: array of TVarRec); overload;
    procedure CreateExcept(const S: String); overload; inline;
  public
    constructor Create(const Expression: String; JSON: IJSONAncestor; ExceptionBlock: Boolean = False);
    destructor Destroy; override;
    function ReadExpression: IJSONAncestor;
  end;


  // ## Parse
  // --------

  TLexemType = ( ltNil,
                 ltSValue, ltIValue, ltDValue, ltNull, ltCLeft, ltCRight,
                 ltBLeft, ltBRight, ltBSlash, ltColon, ltDot, ltVirgule,
                 ltName,
                 ltTrue,
                 ltFalse );

  TLexemTypes = set of TLexemType;

  TLexBuff = class
    Capacity: Integer;
    Length : Integer;
    Buff: array of WideChar;
    function AsString: String;
    function AsInt64: Int64;
    function AsDouble: Double;
    function AsType: TLexemType;
    function AsHInt: Int64;
    procedure Add(Ch: WideChar);
    procedure Grow(ACount: Integer);
    procedure Clear; inline;
  end;

  ILexeme = ^TLexeme;
  TLexeme = record
    Pos: TPosition;
    Int: Int64;
    Str: String;
    Dbl: Double;
    LType: TLexemType;
  end;

  TParseProc = (ppNil, ppInteger, ppDouble, ppString, ppName, ppEscape, ppEscapeUChar);

  TTriggerProcs = set of (ttBuffer, ttEnd, ttBack);

  ITrigger = interface
    function GetNextRoute: TRoute;
    procedure SetNextRoute(const Value: TRoute);
    function GetParseProcs: TParseProc;
    function GetTriggerProcs: TTriggerProcs;
    procedure SetParseProcs(const Value: TParseProc);
    procedure SetTriggerProcs(const Value: TTriggerProcs);

    property NextRoute: TRoute read GetNextRoute write SetNextRoute;
    property TriggerProcs: TTriggerProcs read GetTriggerProcs write SetTriggerProcs;
    property ParseProcs: TParseProc read GetParseProcs write SetParseProcs;
  end;

  TTrigger = class(TInterfacedObject, ITrigger)
  private
    FTriggerProcs: TTriggerProcs;
    FParseProcs: TParseProc;
    FNextRoute: TRoute;
    function GetNextRoute: TRoute;
    procedure SetNextRoute(const Value: TRoute);
    function GetParseProcs: TParseProc; inline;
    function GetTriggerProcs: TTriggerProcs; inline;
    procedure SetParseProcs(const Value: TParseProc);
    procedure SetTriggerProcs(const Value: TTriggerProcs);
  public
    constructor Create(NextRoute: TRoute; TriggerProcs: TTriggerProcs; ParseProcs: TParseProc);
    property NextRoute: TRoute read GetNextRoute write SetNextRoute;
    property TriggerProcs: TTriggerProcs read GetTriggerProcs write SetTriggerProcs;
    property ParseProcs: TParseProc read GetParseProcs write SetParseProcs;
  end;

  IErrorTrigger = interface
    function GetMeessage: String;
    procedure SetMessage(const Value: String);

    property Message: String read GetMeessage write SetMessage;
  end;

  TErrorTrigger = class(TTrigger, IErrorTrigger)
  private
    FMessage: String;
    function GetMeessage: String; inline;
    procedure SetMessage(const Value: String);
  public
    constructor Create(const Message: String);
    property Message: String read GetMeessage write SetMessage;
  end;


  INoRouteTrigger = interface(ITrigger) end;
  TNoRouteTrigger = class(TTrigger, INoRouteTrigger)
  end;

  IUseRouteTrigger = interface(ITrigger) end;
  TUseRouteTrigger = class(TTrigger, IUseRouteTrigger)
  end;

  IJumpTrigger = interface(ITrigger) end;
  TJumpTrigger = class(TTrigger, IJumpTrigger)
  end;

  TRouteChars = set of Char;

  TRoute = class
  private
    FName: String;
    FTriggers: array[#0..MaxCHR] of ITrigger;
    function GetIndex(Ch: WideChar): ITrigger; inline;
    function GetName: String;
  public
    constructor Create(const Name: String);
    property Index[Ch: WideChar]: ITrigger read GetIndex;
    property Name: String read GetName;
    procedure Add(const Chars: TRouteChars; Trigger: ITrigger);
    procedure NoRoute(Trigger: ITrigger);
    property Index[Ch: WideChar]: ITrigger read GetIndex; default;
  end;

  TLexGrammar = class
  private
     FRoutes: TList<TRoute>;
  protected
     function FirstRoute: TRoute; virtual; abstract;
     function CreateRoute(const Name: String): TRoute;
     function EscapeSupport: Boolean; virtual;
     function EscapeRoute: TRoute; virtual;
  public
     constructor Create; virtual;
     destructor Destroy; override;
  end;

  TJSONGrammar = class(TLexGrammar)
  private
     rFirst,
     rName,
     rString,
     rString2,
     rInt,
     rDouble,

     rEscape,
     rEscapeRoute,
     rEscapeUChar: TRoute;

  protected
     function FirstRoute: TRoute; override;
     function EscapeSupport: Boolean; override;
     function EscapeRoute: TRoute; override;
  public
     constructor Create; override;
     destructor Destroy; override;
  end;

  TLexGenerator = class
  private
    FNextRoute: TRoute;
    FFirstRoute: TRoute;
    FBuffer: TLexBuff;
    FEscapeBuff: TLexBuff;
    FCurr: PWideChar;
    FCurrPos: TPosition;
    FLexem: ILexeme;
    FLexG: TLexGrammar;
    FEscapeSupport: Boolean;
    FEscapeRoute: TRoute;
    FExceptBlock: Boolean;
    procedure CreateLexeme;
    procedure NextLex;
    procedure KillLex; inline;
  public
    constructor Create(LexG: TLexGrammar = nil; ExceptBlock: Boolean = False);
    destructor Destroy; override;
    procedure Load(const Source: String);
    function  Check(LTyp: TLexemType): Boolean; overload;
    function  Check(LTyp: TLexemTypes): TLexemType; overload;
    function  CheckName(var S: String): Boolean;
    function  CheckKill(LTyp: TLexemType): Boolean; overload;
    function  CheckKill(LTyp: TLexemTypes): TLexemType; overload;
    function  Current: ILexeme; inline;
  public

  end;

  TSuperParser = class
  public
    class function ParseJSON(const S: String): IJSONAncestor;
  end;



implementation

uses
  XSuperObject;

const
  FloatFormat : TFormatSettings = ( DecimalSeparator : '.' );
  STokenTypes : array [TLexemType] of string = ('Nil',
                'String', 'Integer', 'Float', 'Null', '[', ']',
                '(', ')', '\', ':', '.', ',',
                '',
                'TRUE',
                'FALSE' );


  optAll = [#0..#255];
  optWhiteSpace = [' ', #0, #9, #10, #13];

  optAlpha = ['a'..'z', 'A'..'Z', '$', '_', #127];
  optSym = ['[', ']', '{', '}', ':', ',', '"', '''', '.'];
  optNumeric = ['0'..'9'];
  optEscape = ['b', 'f', 'n', 'r', 't', 'v', '''', '"', '\'];
  optEscapeUnicode = ['u'];
  optHex = ['A'..'F', 'a'..'f'] + optNumeric;
  optStop = optWhiteSpace + optSym;

  HexMap : array [0..15] of WideChar = ('0', '1', '2', '3', '4', '5', '6',
           '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

var
  JSONLexGrammar: TJSONGrammar;
  function iff(const Bool: Boolean; _true, _false: Variant): Variant; inline;
  begin
    if Bool then
       Result := _true
    else
       Result := _false;
  end;

  function ChrToUTF16(const ChrCode: Integer): String; inline;
  begin
     Result := '\u' +
               HexMap[ChrCode shr 12] +
               HexMap[(ChrCode shr 8) and 15] +
               HexMap[(ChrCode shr 4) and 15] +
               HexMap[ChrCode and 15];
  end;

  function StrToUTF16(const Str: String): String;
  var
    Tmp: PWideChar;
  begin
    Result := '';
    if Str = '' then
       Exit
    else
       Tmp := PWideChar(Pointer(Str));
    while Tmp^ <> #0 do
    begin
      case Tmp^ of
        #1..#31: case Tmp^ of
                  #8 : Result := Result + '\b';
                  #9 : Result := Result + '\t';
                  #10: Result := Result + '\n';
                  #11: Result := Result + '\v';
                  #12: Result := Result + '\f';
                  #13: Result := Result + '\r';
               else
                  Result := Result + ChrtoUTF16(Ord(Tmp^))
               end;
        #34{"}: Result := Result + '\"';
        #92{\}: Result := Result + '\\';
        #256..#65535: Result := Result + ChrtoUTF16(Ord(Tmp^));
      else
        Result := Result + Tmp^;
      end;
      Inc(Tmp);
    end;
  end;

{ TJSONAncestor }

procedure TJSONAncestor.AsJSONString(Str: TStringBuilder);
begin
  Str.Append('');
end;

function TJSONAncestor.GetDataType: TDataType;
begin
  with TCast.Create(Self) do
  begin
     Result := DataType;
     Free;
  end;
end;

function TJSONAncestor.GetIsNull: Boolean;
begin
  Result := Self is TJSONNull;
end;

function TJSONAncestor.GetAsVariant: Variant;
begin
  with TCast.Create(Self) do
  begin
     Result := AsVariant;
     Free;
  end;
end;

procedure TJSONAncestor.SetAsVariant(const Value: Variant);
begin
  with TCast.Create(Self) do
  begin
     AsVariant := Value;
     Free;
  end;
end;

{ TTokenBuff }

procedure TLexBuff.Add(Ch: WideChar);
begin
  if Length >= Capacity then
     Grow(256);
  Buff[Length] := Ch;
  Inc(Length);
end;

function TLexBuff.AsDouble: Double;
var
  Res: Extended;
begin
  Add(#0);
  if not TextToFloat(PWideChar(@Buff[0]), Res, fvExtended, FloatFormat)  then
     raise EConvertError.Create('')
  else
     Result := Res;
end;

function TLexBuff.AsHInt: Int64;
var
  I, J: Integer;
begin
  I := 0;
  Result := 0;
  while I < Length do
  begin
    J := Ord(Buff[I]);
    Inc(I);
    case J of
       Ord('a')..Ord('f') :
          J := J - (Ord('a') - 10);
       Ord('A')..Ord('F') :
          J := J - (Ord('A') - 10);
       Ord('0')..Ord('9') :
          J := J - Ord('0');
    else
       Continue;
    end;
    Result := (Result shl 4) or J;
  end;
end;

function TLexBuff.AsInt64: Int64;
begin
  Result := StrToInt64(AsString);
end;

function TLexBuff.AsString: String;
begin
  if Length = 0 then
     Result := ''
  else
  begin
     SetLength(Result, Length);
     Move(Buff[0], Pointer(Result)^, Length*SizeOf(WideChar));
  end;
end;

function TLexBuff.AsType: TLexemType;
begin
  Result := ltName;
   if Length = 0 then
      Exit;

   case Buff[0] of
      '[': Result := ltCLeft;
      ']': Result := ltCRight;
      ':': Result := ltColon;
      ',': Result := ltVirgule;
      '{': Result := ltBLeft;
      '}': Result := ltBRight;
      '.': Result := ltDot;
   else
      if CompareText(STokenTypes[ltTrue], AsString) = 0 then
         Result := ltTrue
      else
      if CompareText(STokenTypes[ltFalse], AsString) = 0 then
         Result := ltFalse
      else
      if CompareText(STokenTypes[ltNull], AsString) = 0 then
         Result := ltNull
   end;
end;

procedure TLexBuff.Clear;
begin
  Length := 0;
  Capacity := 0;
end;

procedure TLexBuff.Grow(ACount: Integer);
begin
  if Capacity = 0 then
    Capacity := ACount
  else
    repeat
      Capacity := Capacity * 2;
      if Capacity < 0 then
        OutOfMemoryError;
    until Capacity >= ACount;
  SetLength(Buff, Capacity);
end;

{ TSuperParser }

class function TSuperParser.ParseJSON(const S: String): IJSONAncestor;
var
  JSON: TJSONBuilder;
begin
  try
    JSON := TJSONBuilder.Create(S);
    Result := JSON.ReadValue;
  finally
    if Assigned(JSON) then
       JSON.Free;
  end;
end;

{ TTrigger }

constructor TTrigger.Create(NextRoute: TRoute; TriggerProcs: TTriggerProcs;
  ParseProcs: TParseProc);
begin
  FNextRoute := NextRoute;
  FTriggerProcs := TriggerProcs;
  FParseProcs := ParseProcs;
end;

function TTrigger.GetNextRoute: TRoute;
begin
  Result := FNextRoute;
end;

function TTrigger.GetParseProcs: TParseProc;
begin
  Result := FParseProcs;
end;

function TTrigger.GetTriggerProcs: TTriggerProcs;
begin
  Result := FTriggerProcs;
end;

procedure TTrigger.SetNextRoute(const Value: TRoute);
begin
  FNextRoute := Value;
end;

procedure TTrigger.SetParseProcs(const Value: TParseProc);
begin
  FParseProcs := Value;
end;

procedure TTrigger.SetTriggerProcs(const Value: TTriggerProcs);
begin
  FTriggerProcs := Value;
end;

{ TRoute }

procedure TRoute.Add(const Chars: TRouteChars; Trigger: ITrigger);
var
  Ch: WideChar;
begin
  Ch := #0;
  while Ch <= MaxCHR do
  begin
     if Ch in Chars then
        if not Assigned(FTriggers[Ch]) then
           FTriggers[Ch] := Trigger;
     Inc(Ch);
  end;
end;

constructor TRoute.Create(const Name: String);
begin
  FName := Name;
end;

function TRoute.GetIndex(Ch: WideChar): ITrigger;
begin
  if Ch > MaxCHR then Ch := MaxCHR;
  Result := FTriggers[Ch];
end;

function TRoute.GetName: String;
begin
  Result := FName;
end;

procedure TRoute.NoRoute(Trigger: ITrigger);
var
  Ch: WideChar;
begin
  Ch := #0;
  while Ch <= MaxCHR do
  begin
     if not Assigned(FTriggers[Ch]) then
        FTriggers[Ch] := Trigger;
     Inc(Ch);
  end;
end;

{ TLexGrammar }

constructor TLexGrammar.Create;
begin
  FRoutes := TList<TRoute>.Create;
end;

destructor TLexGrammar.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

function TLexGrammar.EscapeRoute: TRoute;
begin
  Result := Nil;
end;

function TLexGrammar.EscapeSupport: Boolean;
begin
  Result := False;
end;

function TLexGrammar.CreateRoute(const Name: String): TRoute;
begin
  Result := TRoute.Create(Name);
  FRoutes.Add(Result);
end;

{ TJSONGrammar }

constructor TJSONGrammar.Create;
begin
  inherited;

  rFirst := CreateRoute('First');
  rName := CreateRoute('Name');
  rString := CreateRoute('String');
  rString2 := CreateRoute('String2');
  rInt := CreateRoute('Int');
  rDouble := CreateRoute('Double');

  rEscape := CreateRoute('Escape');
  rEscapeRoute := CreateRoute('EscapeRoute');
  rEscapeUChar := CreateRoute('EscapeUChar');

  rEscape.Add( ['\'], TJumpTrigger.Create(rEscapeRoute, [], ppNil ));

  rEscapeRoute.Add(['u'], TJumpTrigger.Create(rEscapeUChar, [ttBuffer], ppNil));
  rEscapeRoute.NoRoute(TUseRouteTrigger.Create(rEscape, [ttBuffer, ttEnd], ppEscape));

  rEscapeUChar.Add(optHex, TUseRouteTrigger.Create(rEscapeUChar, [], ppEscapeUChar));
  rEscapeUChar.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rFirst.Add(optSym - ['"', ''''], TUseRouteTrigger.Create(rFirst, [ttBuffer, ttEnd], ppName));
  rFirst.Add(optAlpha, TUseRouteTrigger.Create(rName, [ttBuffer], ppNil));
  rFirst.Add(['"'], TJumpTrigger.Create(rString, [ttBuffer], ppNil));
  rFirst.Add([''''], TJumpTrigger.Create(rString2, [ttBuffer], ppNil));
  rFirst.Add(optNumeric, TUseRouteTrigger.Create(rInt, [ttBuffer], ppNil));
  rFirst.Add(optWhiteSpace - [#0], TJumpTrigger.Create(rFirst, [], ppNil));
  rFirst.Add(['-'], TUseRouteTrigger.Create(rInt, [ttBuffer], ppNil));
  rFirst.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rName.Add(optAll - optWhiteSpace - optSym, TUseRouteTrigger.Create(rName, [], ppNil));
  rName.Add(optWhiteSpace, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppName));
  rName.NoRoute(TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppName));

  rString.Add(optAll - ['"', #0, #10, #13], TUseRouteTrigger.Create(rString, [], ppNil));
  rString.Add(['"'], TJumpTrigger.Create(rFirst, [ttEnd], ppString));
  rString.Add([#0, #10, #13], TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rString2.Add(optAll - ['''', #0, #10, #13], TUseRouteTrigger.Create(rString2, [], ppNil));
  rString2.Add([''''], TJumpTrigger.Create(rFirst, [ttEnd], ppString));
  rString2.Add([#0, #10, #13], TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rInt.Add(optNumeric, TUseRouteTrigger.Create(rInt, [], ppNil));
  rInt.Add(['.'], TUseRouteTrigger.Create(rDouble, [], ppNil));
  rInt.Add(optStop, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppInteger));
  rInt.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

  rDouble.Add(optNumeric, TUseRouteTrigger.Create(rDouble, [], ppNil));
  rDouble.Add(optStop, TJumpTrigger.Create(rFirst, [ttEnd, ttBack], ppDouble));
  rDouble.NoRoute(TErrorTrigger.Create(ERR_UnexpectedTokenILLEGAL));

end;

destructor TJSONGrammar.Destroy;
begin
  rFirst.Free;
  rName.Free;
  rString.Free;
  rString2.Free;
  rInt.Free;
  rDouble.Free;
  rEscape.Free;
  rEscapeRoute.Free;
  rEscapeUChar.Free;
  inherited;
end;

function TJSONGrammar.EscapeRoute: TRoute;
begin
  Result := rEscape;
end;

function TJSONGrammar.EscapeSupport: Boolean;
begin
  Result := True;
end;

function TJSONGrammar.FirstRoute: TRoute;
begin
  Result := rFirst;
end;

{ TErrorTrigger }

constructor TErrorTrigger.Create(const Message: String);
begin
  inherited Create(Nil, [], ppNil);
  FMessage := Message;
end;

function TErrorTrigger.GetMeessage: String;
begin
  Result := FMessage;
end;

procedure TErrorTrigger.SetMessage(const Value: String);
begin
  FMessage := Value;
end;

{ TParser }

function TLexGenerator.Check(LTyp: TLexemTypes): TLexemType;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(ltNil);
  end;
  Result := iff(FLexem.LType in LTyp, FLexem.LType, ltNil);
end;

function TLexGenerator.Check(LTyp: TLexemType): Boolean;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(False);
  end;
  Result := FLexem.LType = LTyp;
end;

function TLexGenerator.CheckKill(LTyp: TLexemType): Boolean;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(False);
  end;
  if FLexem.LType = LTyp then
  begin
     KillLex;
     Result := True;
  end
  else
     Result := False;
end;

function TLexGenerator.CheckKill(LTyp: TLexemTypes): TLexemType;
begin
  if not Assigned(FLexem) then
  begin
     NextLex;
     if not Assigned(FLexem) then
        Exit(ltNil);
  end;
  if FLexem.LType in LTyp then
  begin
     Result := FLexem.LType;
     KillLex;
  end
  else
     Result := ltNil;
end;

function TLexGenerator.CheckName(var S: String): Boolean;
var
  lt: TLexemType;
begin
  lt := Check([ltSValue, ltName, ltDValue, ltIValue, ltTrue, ltFalse]);
  if lt in [ltSValue, ltName, ltTrue, ltFalse] then
  begin
     Result := True;
     S := FLexem.Str;
  end
  else
     Result := False;
end;

constructor TLexGenerator.Create(LexG: TLexGrammar; ExceptBlock: Boolean);
begin
  FExceptBlock := ExceptBlock;
  if not Assigned(LexG) then
     FLexG := JSONLexGrammar
  else
     FLexG := LexG;
  FFirstRoute := LexG.FirstRoute;
  FBuffer := TLexBuff.Create;
  FEscapeSupport := LexG.EscapeSupport;
  if FEscapeSupport then
  begin
     FEscapeRoute := LexG.EscapeRoute;
     FEscapeBuff := TLexBuff.Create;
  end;


end;

procedure TLexGenerator.CreateLexeme;
begin
  KillLex;
  New(FLexem);
  FillChar(FLexem.Pos, SizeOf(TPosition), 0);
  FLexem.LType := ltNull;
end;

function TLexGenerator.Current: ILexeme;
begin
  Result := FLexem;
end;

destructor TLexGenerator.Destroy;
begin
  FBuffer.Free;
  if FEscapeSupport then
     FEscapeBuff.Free;
  inherited;
end;

procedure TLexGenerator.KillLex;
begin
  if Assigned(FLexem) then
  begin
     Dispose(FLexem);
     FLexem := Nil;
  end;
end;

procedure TLexGenerator.Load(const Source: String);
begin
  FCurr := PWideChar(Source);
end;

procedure TLexGenerator.NextLex;
var
  Route: TRoute;
  Trigger: ITrigger;
  PBuffer: TLexBuff;
  UseEscape, UseEscapeEnd: Boolean;

  procedure ParseProc;
  begin
    case Trigger.ParseProcs of
      ppInteger: begin
         FLexem.Int := FBuffer.AsInt64;
         FLexem.LType := ltIValue;
      end;
      ppDouble:begin
         FLexem.Dbl := FBuffer.AsDouble;
         FLexem.LType := ltDValue;
      end;
      ppString:begin
         FLexem.LType := ltSValue;
      end;

      ppName: begin
         FLexem.Str := FBuffer.AsString;
         FLexem.LType := FBuffer.AsType;
      end;

      ppEscapeUChar: begin
         if FEscapeBuff.Length = 4 then
         begin
            FBuffer.Add( Chr(FEscapeBuff.AsHInt) );
            FEscapeBuff.Clear;
            UseEscapeEnd := True;
         end;
      end;

      ppEscape: begin
         case FEscapeBuff.Buff[0] of
           'b' : FBuffer.Add(#8);
           't' : FBuffer.Add(#9);
           'n' : FBuffer.Add(#10);
           'v' : FBuffer.Add(#11);
           'f' : FBuffer.Add(#12);
           'r' : FBuffer.Add(#13);
           '\' : FBuffer.Add('\');
           '"' : FBuffer.Add('"');
           '''': FBuffer.Add('''');
         else
           FBuffer.Add(FEscapeBuff.Buff[0]);
         end;
         FEscapeBuff.Clear;
      end;
    end;
    FLexem.Str := FBuffer.AsString;
  end;

  procedure GetRoute;
  begin
    if FEscapeSupport then
     begin
        Trigger := FEscapeRoute[FCurr^];
        if not Assigned(Trigger) then
        begin
           Trigger := Route[FCurr^];
           PBuffer := FBuffer;
           UseEscape := False;
           Exit;
        end;
         UseEscape := True;
         PBuffer := FEscapeBuff;
     end
     else
     begin
       Trigger := Route[FCurr^];
       UseEscape := False;
     end;
  end;
begin
  CreateLexeme;
  UseEscape := False;
  UseEscapeEnd := False;
  PBuffer := FBuffer;
  PBuffer.Clear;

  if FEscapeSupport then
     FEscapeBuff.Clear;

  Route := FFirstRoute;
  while Assigned(Route) do
  begin

     GetRoute;

     if not Assigned(Trigger) then
        Exit;

     if Trigger is TErrorTrigger then
        if FCurr^ = #0 then
           Break
        else
           if FExceptBlock then
              Abort
           else
              raise TJSONSyntaxError.Create( TErrorTrigger(Trigger).Message, FCurrPos);

     if Trigger is TUseRouteTrigger then
        PBuffer.Add(FCurr^);

     if (ttBuffer in Trigger.TriggerProcs) and (FLexem.Pos.Col = 0) then
        FLexem.Pos := FCurrPos;

     if not (ttBack in Trigger.TriggerProcs) then
     begin
        Inc(FCurr);
        if FCurr^ = #10 then
        begin
           Inc(FCurrPos.Line);
           FCurrPos.Col := 1;
        end
        else
           Inc(FCurrPos.Col);
     end;

     if Trigger.ParseProcs <> ppNil then
        ParseProc;

     if (ttEnd in Trigger.TriggerProcs) or UseEscapeEnd then
     begin
       if not UseEscape then
       begin
         FFirstRoute := Trigger.NextRoute;
         Exit;
       end;
       UseEscape := False;
       UseEscapeEnd := False;
       FEscapeRoute := FLexG.EscapeRoute;
     end
     else
     if UseEscape then
        FEscapeRoute := Trigger.NextRoute
     else
        Route := Trigger.NextRoute;
  end;
  KillLex;
end;

{ TJSONBuilder }

constructor TJSONBuilder.Create(const JSON: String);
begin
  LGen := TLexGenerator.Create(JSONLexGrammar);
  LGen.Load(JSON);
end;

destructor TJSONBuilder.Destroy;
begin
  LGen.Free;
  inherited;
end;

procedure TJSONBuilder.ReadArray(var Val: IJSONAncestor);
var
  Item: IJSONAncestor;
begin
  LGen.KillLex;
  Val := TJSONArray.Create;

  repeat
    Item := ReadValue;
    if Assigned(Item) then
       TJSONArray(Val).Add(Item);
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltCRight) then
     raise TJSONSyntaxError.Create(Err_UnexpectedEndOfInput, LGen.Current.Pos);
end;

procedure TJSONBuilder.ReadFalse(var Val: IJSONAncestor);
begin
  Val := TJSONBoolean.Create(False);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadFloat(var Val: IJSONAncestor);
begin
  Val := TJSONFloat.Create(LGen.Current.Dbl);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadInteger(var Val: IJSONAncestor);
begin
  Val := TJSONInteger.Create(LGen.Current.Int);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadNull(var Val: IJSONAncestor);
begin
  Val := TJSONNull.Create(True);
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadObject(var Val: IJSONAncestor);
var
  Name: String;
begin
  LGen.KillLex;
  Val := TJSONObject.Create;
  repeat
    if LGen.CheckName(Name) then
    begin
       LGen.KillLex;
       if not LGen.CheckKill(ltColon) then
          raise TJSONSyntaxError.CreateFmt(Err_Expected, [':'], LGen.Current.Pos);
       TJSONObject(Val).AddPair(TJSONPair.Create(Name, ReadValue));
    end
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltBRight) then
     raise TJSONSyntaxError.Create(Err_UnexpectedEndOfInput, LGen.Current.Pos);
end;

procedure TJSONBuilder.ReadString(var Val: IJSONAncestor);
begin
  Val := TJSONString.Create( LGen.Current.Str );
  LGen.KillLex;
end;

procedure TJSONBuilder.ReadTrue(var Val: IJSONAncestor);
begin
  Val := TJSONBoolean.Create(True);
  LGen.KillLex;
end;

function TJSONBuilder.ReadValue: IJSONAncestor;
begin
  case LGen.Check([ ltSValue, ltIValue, ltDValue, ltBLeft, ltCLeft,
                    ltTrue, ltFalse, ltNull ]) of
    ltSValue: ReadString(Result);
    ltIValue: ReadInteger(Result);
    ltDValue: ReadFloat(Result);
    ltBLeft : ReadObject(Result);
    ltTrue  : ReadTrue(Result);
    ltFalse : ReadFalse(Result);
    ltCLeft : ReadArray(Result);
    ltNull  : ReadNull(Result);
  else
    Result := Nil;
  end;
end;

{ TJSONString }

procedure TJSONString.AsJSONString(Str: TStringBuilder);
begin
  if IsNull then
     Str.Append( cNull )
  else
     Str.Append( '"' +  StrToUTF16(Value) + '"' );
end;

{ TJSONInteger }

procedure TJSONInteger.AsJSONString(Str: TStringBuilder);
begin
  if FNull then
     Str.Append( cNull )
  else
     Str.Append( Value );
end;


{ TJSONFloat }

procedure TJSONFloat.AsJSONString(Str: TStringBuilder);
begin
  if FNull then
     Str.Append( cNull )
  else
     Str.Append( FloatToStr(Value, FloatFormat) );
end;

{ TJSONBoolean }

procedure TJSONBoolean.AsJSONString(Str: TStringBuilder);
begin
   Str.Append( String(iff( IsNull, cNull, iff( Value, 'true', 'false') )) );
end;

{ TJSONNull }

procedure TJSONNull.AsJSONString(Str: TStringBuilder);
begin
  Str.Append( cNull );
end;

function TJSONNull.GetIsNull: Boolean;
begin
  Result := True;
end;

{ TJSONObject }

procedure TJSONObject.AddPair(P: IJSONPair);
var
  N: IJSONPair;
begin
  N := Get(P.Name);
  if Assigned(N) then
  begin
     FPairList.Remove(N);
     N := Nil;
  end;
  FPairList.Add(P);
end;

procedure TJSONObject.AddPair(Name: String; Value: IJSONAncestor);
begin
  AddPair( TJSONPair.Create(Name, Value) );
end;

procedure TJSONObject.AsJSONString(Str: TStringBuilder);
var
  P: IJSONPair;
  I,L: Integer;
begin
  if FNull then
     Str.Append( cNull )
  else
  begin
    Str.Append('{');
    L := Count-1;
    for I := 0 to L do
    begin
       P := FPairList[I];
       Str.Append('"' + StrToUTF16(P.Name) + '":');
       P.JSONValue.AsJSONString(Str);
       if I < L then
          Str.Append(',');
    end;
    Str.Append('}');
  end;
end;

function TJSONObject.Count: Integer;
begin
  Result := FPairList.Count;
end;

constructor TJSONObject.Create;
begin
  FPairList := TList<IJSONPair>.Create;
end;

destructor TJSONObject.Destroy;
begin
  FPairList.Free;
  inherited;
end;

function TJSONObject.Get(const Name: String): IJSONPair;
var
  P: IJSONPair;
begin
  for P in FPairList do
      if CompareText(Name, P.Name) = 0 then
         Exit(P);
  Result := Nil;
end;

function TJSONObject.GetIsNull: Boolean;
begin
  Result := FNull;
end;

function TJSONObject.Get(const Index: Integer): IJSONPair;
begin
  if (FPairList.Count = 0) or (FPairList.Count <= Index) then
     Result := Nil
  else
     Result := FPairList[Index];
end;

function TJSONObject.GetEnumerator: TJSONEnumerator<IJSONPair>;
begin
  Result.Index := -1;
  Result.List := FPairList;
end;

class function TJSONObject.ParseJSONValue(const Str: String): IJSONAncestor;
begin
  Result := TSuperParser.ParseJSON(Str);
end;

procedure TJSONObject.Remove(P: IJSONPair);
begin
  Remove(P.Name);
end;

procedure TJSONObject.Remove(const Index: Integer);
begin
  if Count > Index then
     FPairList.Delete(Index);
end;

procedure TJSONObject.Remove(const Name: String);
var
  R: IJSONPair;
begin
  R := Get(Name);
  if Assigned(R) then
  begin
     FPairList.Remove(R);
     R := Nil;
  end;
end;


{ TJSONPair }

constructor TJSONPair.Create(const aName: String; aValue: IJSONAncestor);
begin
  FName := aName;
  FValue := aValue;
end;

destructor TJSONPair.Destroy;
begin
  FValue := Nil;
  inherited;
end;


function TJSONPair.GetName: String;
begin
  Result := FName;
end;

function TJSONPair.GetValue: IJSONAncestor;
begin
  Result := FValue;
end;

procedure TJSONPair.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TJSONPair.SetValue(const Value: IJSONAncestor);
begin
  FValue := Value;
end;

{ TJSONSyntaxError }

constructor TJSONSyntaxError.Create(const Msg: String; Pos: TPosition);
begin
  inherited CreateFmt(Msg + '. (Line: %d Col: %d)', [Pos.Line, Pos.Col]);
end;

constructor TJSONSyntaxError.CreateFmt(const Msg: String; const Args: array of TVarRec;
  Pos: TPosition);
begin
  Create( Format(Msg, Args), Pos );
end;


{ TJSONArray }

procedure TJSONArray.Add(Val: IJSONAncestor);
begin
  if not FList.Contains(Val) then
     FList.Add(Val);
end;

procedure TJSONArray.AsJSONString(Str: TStringBuilder);
var
  I,L: Integer;
begin
  if FNull then
     Str.Append( cNull )
  else
  begin
    Str.Append('[');
    L := Count - 1;
    for I := 0 to L do
    begin
       FList[I].AsJSONString(Str);
       if I < L then
          Str.Append(',');
    end;
    Str.Append(']');
  end;
end;

procedure TJSONArray.Clear;
begin
  FList.Clear;
end;

function TJSONArray.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TJSONArray.Create;
begin
  FList := TList<IJSONAncestor>.Create;
end;

destructor TJSONArray.Destroy;
begin
  FList.Free;
  inherited;
end;

function TJSONArray.Get(const I: Integer): IJSONAncestor;
begin
  if (FList.Count = 0) or (Flist.Count <= I) then
     Result := Nil
  else
     Result := FList.Items[I]
end;

function TJSONArray.GetEnumerator: TJSONEnumerator<IJSONAncestor>;
begin
   Result.Index := -1;
   Result.List := FList;
end;

function TJSONArray.GetIsNull: Boolean;
begin
  Result := FNull;
end;

procedure TJSONArray.Remove(Val: IJSONAncestor);
begin
  FList.Remove(Val);
end;

procedure TJSONArray.Remove(Index: Integer);
begin
  FList.Delete(Index);
end;

{ TJSONValue<T> }

constructor TJSONValue<T>.Create(const Value: T);
begin
  FData := Value;
  FNull := False;
end;

constructor TJSONValue<T>.CreateNull;
begin
  FNull := True;
end;

function TJSONValue<T>.GetData: T;
begin
  Result := FData;
end;

function TJSONValue<T>.GetIsNull: Boolean;
begin
  Result := FNull;
end;

procedure TJSONValue<T>.SetData(const Value: T);
begin
  FData := Value;
end;

procedure TJSONValue<T>.SetNull;
begin
  FNull := True;
end;

{ TJSONInterpreter }

constructor TJSONInterpreter.Create(const Expression: String;
  JSON: IJSONAncestor; ExceptionBlock: Boolean = False);
begin
  LGen := TLexGenerator.Create(JSONLexGrammar, ExceptionBlock);
  LGen.Load(Expression);
  FJSON := JSON;
  FExceptionBlock := ExceptionBlock;
end;

procedure TJSONInterpreter.CreateExcept(const S: String;
  Args: array of TVarRec);
begin
  if FExceptionBlock then
     Abort
  else
     raise TJSONSyntaxError.CreateFmt(S, Args, LGen.Current.Pos);
end;

procedure TJSONInterpreter.CreateExcept(const S: String);
begin
  if FExceptionBlock then
     Abort
  else
     raise TJSONSyntaxError.Create(S, LGen.Current.Pos);
end;

destructor TJSONInterpreter.Destroy;
begin
  LGen.Free;
  inherited;
end;

function TJSONInterpreter.ReadArray(Base: IJSONAncestor): IJSONArray;
var
  Item: IJSONAncestor;
begin
  LGen.KillLex;
  Result := TJSONArray.Create;
  repeat
    Item := ReadValue(Base);
    if Assigned(Item) then
       TJSONArray(Result).Add(Item);
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltCRight) then
     CreateExcept(Err_UnexpectedEndOfInput);
end;

function TJSONInterpreter.ReadArrayIndex(Base: IJSONArray): IJSONAncestor;
var
  RName: IJSONAncestor;
  Index: Integer;
begin
  case LGen.Check([ltIValue, ltName]) of
    ltIValue:
      begin
         Index := StrToInt(LGen.Current.Str);
         LGen.KillLex;
      end;
    ltName:
      begin
         RName := ReadName(FJSON);
         if not (RName is TJSONInteger) then
            CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
         else
            Index := TJSONInteger(RName).Value;
      end
    else
       CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
  end;
  Result := Base.Index[Index];
  if not LGen.CheckKill(ltCRight) then
     CreateExcept(Err_Expected, [STokenTypes[ltCRight]]);
  if LGen.CheckKill(ltDot) then
  begin
     RName := ReadName(Result);
     if Assigned(RName) then
        Result := RName;
  end;
end;

function TJSONInterpreter.ReadExpression: IJSONAncestor;
begin
  try
    case LGen.Check([ltBLeft, ltCLeft]) of
       ltBLeft : Result := ReadObject(FJSON);
       ltCLeft : Result := ReadArray(FJSON);
    else
       Result := ReadName(FJSON);
    end;
  except
    on E: Exception do
    begin
      if FExceptionBlock then
         Result := Nil
      else
         raise;
    end;
  end;
end;

function TJSONInterpreter.ReadName(Base: IJSONAncestor): IJSONAncestor;
var
  Name: String;
  Pair: IJSONPair;
begin
  if not LGen.CheckName(Name) then
     Exit(Nil);

  if Base is TJSONArray then
  begin
    if LGen.Current.LType <> ltIValue then
       CreateExcept(Err_ExpectedButFound, [STokenTypes[ltIValue], STokenTypes[LGen.Current.LType]])
    else
       Result := TJSONArray(Base).Index[StrToInt(Name)];
  end
  else
  if Base is TJSONObject then
  begin
     Pair := TJSONObject(Base).Get(Name);
     if Pair = Nil then
        Exit(Nil)
     else
        Result := Pair.JSONValue;
     LGen.KillLex;
     if Assigned(Result) then
         case LGen.CheckKill([ltDot, ltCLeft]) of
           ltDot:
              Result := ReadName(Result);
           ltCLeft:
              begin
                 if Result is TJSONArray then
                    Result := ReadArrayIndex(TJSONArray(Result))
                 else
                    CreateExcept(Err_Expected, ['Array']);
              end;
         end;
  end
  else
    Result := Nil;
end;

function TJSONInterpreter.ReadObject(Base: IJSONAncestor): IJSONObject;
var
  Name: String;
begin
  LGen.KillLex;
  Result := TJSONObject.Create;
  repeat
    if LGen.CheckName(Name) then
    begin
       LGen.KillLex;
       if not LGen.CheckKill(ltColon) then
          CreateExcept(Err_Expected, [':']);
       TJSONObject(Result).AddPair(TJSONPair.Create(Name, ReadValue(Base)));
    end
  until not LGen.CheckKill(ltVirgule);

  if not LGen.CheckKill(ltBRight) then
     CreateExcept(Err_UnexpectedEndOfInput);
end;

function TJSONInterpreter.ReadValue(Base: IJSONAncestor): IJSONAncestor;
begin
  case LGen.Check([ ltSValue, ltIValue, ltDValue, ltBLeft, ltCLeft,
                    ltTrue, ltFalse, ltName, ltNull ]) of
    ltSValue: Result := TJSONString.Create(LGen.Current.Str);
    ltIValue: Result := TJSONInteger.Create(LGen.Current.Int);
    ltDValue: Result := TJSONFloat.Create(LGen.Current.Dbl);
    ltBLeft : Result := ReadObject(Base);
    ltTrue  : Result := TJSONBoolean.Create(True);
    ltFalse : Result := TJSONBoolean.Create(False);
    ltCLeft : Result := ReadArray(Base);
    ltNull  : Result := TJSONNull.Create(True);
    ltName  : begin
       Result := ReadName(Base);
       Exit;
    end
  else
    Result := Nil;
    Exit;
  end;
  LGen.KillLex;
end;

{ TSuperEnumerator<T> }

function TJSONEnumerator<T>.GetCurrent: T;
begin
  Result := List[Index]
end;

function TJSONEnumerator<T>.MoveNext: Boolean;
begin
  Result := Index < List.Count - 1;
  if Result then
    Inc(Index);
end;

initialization

  JSONLexGrammar := TJSONGrammar.Create;

finalization

  JSONLexGrammar.Free;

end.
