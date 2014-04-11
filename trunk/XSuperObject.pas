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
unit XSuperObject;

interface

uses
  Classes,
  Variants,
  SysUtils,
  Character,
  XSuperJSON,
  RTTI,
  TypInfo,
  Generics.Collections;

type

  SOException = class(Exception);

  ISuperObject = interface;
  ISuperArray = interface;
  TSuperObject = class;

  TMemberStatus = (jUnAssigned, jNull, jAssigned);

  IBase = interface
    function AsObject: ISuperObject;
    function AsArray: ISuperArray;
  end;

  TBase = class(TInterfacedObject, IBase)
    function AsObject: ISuperObject; virtual; 
    function AsArray: ISuperArray; virtual;
  end;

  IBaseJSON<T, Typ> = interface(IBase)
  ['{EBD49266-BEF2-4B79-9BAF-329F725E0568}']
    function GetBoolean(V: Typ): Boolean;
    function GetInteger(V: Typ): Int64;
    function GetString(V: Typ): String;
    procedure SetBoolean(V: Typ; const Value: Boolean);
    procedure SetInteger(V: Typ; const Value: Int64);
    procedure SetString(V: Typ; const Value: String);
    function GetObject(V: Typ): ISuperObject;
    procedure SetObject(V: Typ; const Value: ISuperObject);
    function GetArray(V: Typ): ISuperArray;
    procedure SetArray(V: Typ; const Value: ISuperArray);
    function GetDouble(V: Typ): Double;
    procedure SetDouble(V: Typ; const Value: Double);
    function GetVariant(V: Typ): Variant;
    procedure SetVariant(V: Typ; const Value: Variant);
    function GetSelf: T;
    function GetAncestor(V: Typ): IJSONAncestor;
    function GetNull(V: Typ): TMemberStatus;
    procedure SetNull(V: Typ; const Value: TMemberStatus);
    function GetDataType: TDataType;

    property Null[V: Typ]: TMemberStatus read GetNull write SetNull;
    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    property Ancestor[V: Typ]: IJSONAncestor read GetAncestor;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false); overload;
    procedure SaveTo(AFile: String; const Ident: Boolean = false); overload;
    function AsJSON(const Ident: Boolean = False): String;
    property Self: T read GetSelf;
    property DataType: TDataType read GetDataType;
  end;

  TJSONValueHelper = class helper for TJSONAncestor
  public
    function ValueEx<T>: Variant;
  end;

  TBaseJSON<T, Typ> = class(TBase, IBaseJSON<T, Typ>)
  protected
    FJSONObj: T;
    FCasted: IJSONAncestor;
    FInterface: IInterface;
    function ContainsEx(Key: Typ; Value: IJSONAncestor): Boolean;
    function  DefaultValueClass<TT: Class>(const Value: TValue): TT;
    procedure Member<T: Class>(const Name: String; const Value: TValue); overload;
    function  Member(const Name: String): Boolean; overload;
    function  GetValue<C: Class>(const Name: Typ): C;
    function  GetSelf: T;
    function  GetData(Key: Typ): IJSONAncestor;
    function  GetVariant(V: Typ): Variant;
    procedure SetVariant(V: Typ; const Value: Variant);
    function  GetDataType: TDataType;
  protected
    function GetObject(V: Typ): ISuperObject; virtual;
    function GetArray(V: Typ): ISuperArray; virtual;
    function GetBoolean(V: Typ): Boolean; virtual;
    function GetInteger(V: Typ): Int64; virtual;
    function GetString(V: Typ): String; virtual;
    function GetDouble(V: Typ): Double; virtual;
    function GetAncestor(V: Typ): IJSONAncestor; inline;
    function GetNull(V: Typ): TMemberStatus; virtual;
    procedure SetObject(V: Typ; const Value: ISuperObject); virtual;
    procedure SetArray(V: Typ; const Value: ISuperArray); virtual;
    procedure SetBoolean(V: Typ; const Value: Boolean); virtual;
    procedure SetInteger(V: Typ; const Value: Int64); virtual;
    procedure SetString(V: Typ; const Value: String); virtual;
    procedure SetDouble(V: Typ; const Value: Double); virtual;
    procedure SetNull(V: Typ; const Value: TMemberStatus); virtual;
  public
    constructor Create(JSON: String = '{}'); overload;
    constructor Create(JSON: T); overload;
    destructor Destroy; override;
    property Null[V: Typ]: TMemberStatus read GetNull write SetNull;
    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    property Ancestor[V: Typ]: IJSONAncestor read GetAncestor;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false); overload; virtual; abstract;
    procedure SaveTo(AFile: String; const Ident: Boolean = false); overload; virtual; abstract;
    function AsJSON(const Ident: Boolean = False): String; inline;
    property Self: T read GetSelf;
    property DataType: TDataType read GetDataType;
  end;


  ICast = interface
    function GetArray: ISuperArray;
    function GetBoolean: Boolean;
    function GetDataType: TDataType;
    function GetFloat: Double;
    function GetInteger: Int64;
    function GetObject: ISuperObject;
    function GetString: String;
    function GetName: String;
    function GetVariant: Variant;
    procedure SetBoolean(const Value: Boolean);
    procedure SetFloat(const Value: Double);
    procedure SetInteger(const Value: Int64);
    procedure SetString(const Value: String);
    procedure SetVariant(const Value: Variant);

    property AsObject: ISuperObject read GetObject;
    property AsArray: ISuperArray read GetArray;
    property AsString: String read GetString write SetString;
    property AsInteger: Int64 read GetInteger write SetInteger;
    property AsFloat: Double read GetFloat write SetFloat;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    property AsVariant: Variant read GetVariant write SetVariant;
    property DataType: TDataType read GetDataType;
    property Name: String read GetName;
    function ToString(const Ident: Boolean = False): String;
  end;

  TCast = class(TInterfacedObject, ICast)
  private
    FJSON: IJSONAncestor;
    FName: String;
    function GetArray: ISuperArray;
    function GetBoolean: Boolean;
    function GetDataType: TDataType;
    function GetFloat: Double;
    function GetInteger: Int64;
    function GetObject: ISuperObject;
    function GetString: String;
    procedure SetBoolean(const Value: Boolean);
    procedure SetFloat(const Value: Double);
    procedure SetInteger(const Value: Int64);
    procedure SetString(const Value: String);
    function GetName: String;
    function GetVariant: Variant;
    procedure SetVariant(const Value: Variant);
  public
    constructor Create(Base: IJSONAncestor); overload;
    constructor Create(Base: IJSONPair); overload;
    class function CreateFrom<T>(Base: T): ICast;
    destructor Destroy; override;
    property AsObject: ISuperObject read GetObject;
    property AsArray: ISuperArray read GetArray;
    property AsString: String read GetString write SetString;
    property AsInteger: Int64 read GetInteger write SetInteger;
    property AsFloat: Double read GetFloat write SetFloat;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;
    property AsVariant: Variant read GetVariant write SetVariant;
    property DataType: TDataType read GetDataType;
    property Name: String read GetName;
    function ToString(const Ident: Boolean = False): String;
  end;

  IMember = ICast;

  ISuperExpression = interface(ICast)
  end;

  TSuperExpression = class(TCast, ISuperExpression)
  private
    FInterpreter: TJSONInterpreter;
  public
    constructor Create(Base: IJSONAncestor; const Expr: String);
    destructor Destroy; override;
  end;

  TSuperEnumerator<T> = record
    Index : Integer;
    List : TJSONEnumerator<T>;
    function MoveNext : Boolean;
    function GetCurrent : ICast;
    property Current : ICast read GetCurrent;
  end;

  ISuperObject = interface(IBaseJSON<IJSONObject, String>)
  ['{B7E271F3-205B-4172-8532-BE03F2A6EDE7}']
    procedure First;
    procedure Next;
    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: IJSONAncestor;
    function GetOffset: Integer;
    function  GetExpr(const Code: String): ISuperExpression;

    procedure SetData(V: String; Data: Variant); overload;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;

    property Expression[const Code: String]: ISuperExpression read GetExpr; default;
    property Count: Integer read GetCount;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: IJSONAncestor read GetCurrentValue;
    property Offset: Integer read GetOffset;
    function Clone: ISuperObject;
    function GetEnumerator: TSuperEnumerator<IJSONPair>;
    function T: TSuperObject;
  end;

  TSuperObject = class(TBaseJSON<IJSONObject, String>, ISuperObject)
  private
    FOffset: Integer;
    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: IJSONAncestor;
    function GetOffset: Integer;
    function  GetExpr(const Code: String): ISuperExpression;
  protected
    function GetString(V: String): String; override;
    procedure SetNull(V: String; const Value: TMemberStatus); override;
  public
    procedure First;
    procedure Next;
    procedure SetData(V: String; Data: Variant); overload; inline;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;

    class function ParseStream(Stream: TStream): TSuperObject;
    class function ParseFile(FileName: String): TSuperObject;

    procedure SaveTo(Stream: TStream; const Ident: Boolean = false); overload; override;
    procedure SaveTo(AFile: String; const Ident: Boolean = false); overload; override;


    property Expression[const Code: String]: ISuperExpression read GetExpr; default;
    property Count: Integer read GetCount;
    property Offset: Integer read GetOffset;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: IJSONAncestor read GetCurrentValue;
    function GetEnumerator: TSuperEnumerator<IJSONPair>;
    function AsType<T>: T;
    function T: TSuperObject; inline;
    function Clone: ISuperObject;
    function AsObject: ISuperObject; override;
    function AsArray: ISuperArray; override;
  end;

  ISuperArray = interface(IBaseJSON<IJSONArray, Integer>)
  ['{41A2D578-CFAB-4924-8F15-0D0227F35412}']
    function GetLength: Integer;
    property Length: Integer read GetLength;
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    procedure Add(Value: IJSONAncestor); overload;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Clone: ISuperArray;
    function GetEnumerator: TSuperEnumerator<IJSONAncestor>;
  end;

  TSuperArray = class(TBaseJSON<IJSONArray, Integer>, ISuperArray)
  private
    function GetLength: Integer;
  protected
    procedure SetNull(V: Integer; const aValue: TMemberStatus); override;
  public
    procedure Add(Value: IJSONAncestor); overload;
    procedure Add(Value: ISuperObject); overload;
    procedure Add(Value: ISuperArray); overload;
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    procedure Delete(Index: Integer);
    procedure Clear;
    property Length: Integer read GetLength;
    function GetEnumerator: TSuperEnumerator<IJSONAncestor>;
    procedure SaveTo(Stream: TStream; const Ident: Boolean = false); overload; override;
    procedure SaveTo(AFile: String; const Ident: Boolean = false); overload; override;
    function Clone: ISuperArray;
    function AsArray: ISuperArray; override;
  end;

  TSuperProperty = class(TRttiProperty)
  public
    ArrayRawData: Pointer;
  end;

  TSuperField = class(TRttiField)
  public
    ArrayRawData: Pointer;
  end;

  TSerializeParse = class
  public
    // ** Read
    class procedure ReadObject(AObject: TObject; IResult: ISuperObject);
    class procedure ReadRecord(Info: PTypeInfo; ARecord: Pointer; IResult: ISuperObject);
    class function  ReadRecordEx<T>(Rec: T): ISuperObject;
    class procedure ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure ReadMember<T, Typ>(Member: Typ; RType: PTypeInfo; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);

    class procedure ReadSet(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfArray(Val: Variant; IJsonData: ISuperArray);
    class procedure ReadTValueOfArray(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfObject(Val: Variant; const Name: String; IJsonData: ISuperObject);

    // ** Write
    class procedure WriteObject(AObject: TObject; IData: ISuperObject);
    class procedure WriteRecord(Info: PTypeInfo; ARecord: Pointer; IData: ISuperObject);
    class procedure WriteRecordEx<T>(Rec: T; IData: ISuperObject);
    class procedure WriteMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure WriteMember<T, Typ>(Data: Pointer; Member: Typ; TypeKind: TTypeKind; MemberValue: TRttiMember; IJsonData: IBaseJSON<T, Typ>);
    class procedure WriteSet(Data: Pointer; Member: TRttiMember; IJSONData: ISuperArray);
    class procedure SetValue<Typ>(Data: Pointer; Member: TRttiMember; MIdx: Typ; Val: TValue);
    class function  GetValue<Typ>(Data: Pointer; Member: TRttiMember; MIdx: Typ): TValue;
    class function  GetMemberTypeInfo(Member: TRttiMember; const GetArray: Boolean = true): PTypeInfo; inline;
    class function  GetMemberType(Member: TRttiMember; const GetArray: Boolean = true): TRttiType; //inline;
    class function  GetArrayRawData(Member: TRttiMember): Pointer; inline;
    class procedure SetArrayRawData(Member: TRttiMember; RawData: Pointer);
    class procedure ClearArrayRawData(Member: TRttiMember);

    class function  ObjectConstructorParamCount(Instance: TClass): Integer;
    class function  ObjectConstructor(Instance: TClass): TObject;
    class function  CheckObject<Typ>(Data: Pointer; Member: TRttiMember; MIdx: Typ; var Obj: TObject): Boolean;
  end;

  TSuperObjectHelper = class helper for TObject
  public
    function AsJSON: String;
    function AsJSONObject: ISuperObject;
    constructor FromJSON(const JSON: String); overload;
    constructor FromJSON(JSON: ISuperObject); overload;
  end;

  TBaseSuperRecord<T> = class
  public
    class function AsJSON(Rec: T): String;
    class function AsJSONObject(Rec: T): ISuperObject;
    class function FromJSON(JSON: String): T; overload;
    class function FromJSON(JSON: ISuperObject): T; overload;
  end;

  TSuperRecord<T: Record> = class(TBaseSuperRecord<T>);

  function SO(JSON: String = '{}'): ISuperObject;
  function SA(JSON: String = '[]'): ISuperArray;

  // ** Zero Based Strings Definations...
 {$UNDEF XE2UP}
 {$IFDEF DCC}
   {$IF CompilerVersion >= 24}
     {$DEFINE XE2UP}
   {$IFEND}
 {$ENDIF}

 {$IFDEF XE2UP}
   const CharIndex = Low(String);
 {$ELSE}
   const CharIndex = 1;
 {$ENDIF}

var
  JSONDateFormat: String = 'yyyy-mm-dd"T"hh:mm:ss';

implementation


function SO(JSON: String): ISuperObject;
begin
  Result := TSuperObject.Create(JSON);
end;

function SA(JSON: String): ISuperArray;
begin
  Result := TSuperArray.Create(JSON);
end;

{ TSuperObject }

constructor TBaseJSON<T, Typ>.Create(JSON: String);
var
  JVal: IJSONAncestor;
begin
  if (Self.InheritsFrom(TSuperArray)) and (Trim(JSON) = '{}') then JSON := '[]';
  JVal := TJSONObject.ParseJSONValue(JSON);
  if JVal.QueryInterface(GetTypeData(TypeInfo(T)).Guid, FJSONObj) = S_OK then
     FInterface := TValue.From<T>(FJSONObj).AsInterface
  else
     FCasted := JVal
end;

function TBaseJSON<T, Typ>.GetValue<C>(const Name: Typ): C;
begin
  if Self.InheritsFrom(TSuperObject) then
    with TJSONObject(FInterface).Get(TValue.From<Typ>(Name).AsString) do
       if JsonValue is TJSONNull then
          Result := Nil
       else
          Result := JSonValue as C
  else
  if Self.InheritsFrom(TSuperArray) then
    Result := TJSONArray(FInterface).Get(TValue.From<Typ>(Name).AsInteger) as C
  else
    Result := Nil;
end;

function TBaseJSON<T, Typ>.GetVariant(V: Typ): Variant;
begin
  case GetType(V) of
    varString: Result := S[V];
    varInt64: Result := I[V];
    varDouble: Result := F[V];
    varBoolean: Result := B[V];
  else
    Result := Variants.Null;
  end;
end;

function TBaseJSON<T, Typ>.Member(const Name: String): Boolean;
begin
  if Self.InheritsFrom(TSuperObject) then
    Result := Assigned(TJSONObject(FInterface).Get(NAme))
  else
    Result := Assigned(TJSONArray(FInterface).Get(StrToInt(Name)))
end;

procedure TBaseJSON<T, Typ>.Member<T>(const Name: String; const Value: TValue);
var
  Pair: IJSONPair;
  Index: Integer;
begin
  if Self.InheritsFrom(TSuperObject) then
  begin
    Pair := TJSONObject(FInterface).Get(Name);
    if not Assigned(Pair) then
    begin
      TJSONObject(FInterface).AddPair(Name, DefaultValueClass<T>(Value) as TJSONAncestor );
      Exit;
    end;
    if Assigned(Pair.JsonValue) then
      Pair.JsonValue := Nil;
    Pair.JsonValue := DefaultValueClass<T>(Value) as TJSONAncestor;
  end
  else
  begin
    Index := StrToInt(Name);
    if TJSONArray(FInterface).Count - 1 < Index then
      while TJSONArray(FInterface).Count - 1 < Index do
        TJSONArray(FInterface).Add(DefaultValueClass<T>(Value) as TJSONAncestor);
  end;

end;

function TBaseJSON<T, Typ>.AsJSON(const Ident: Boolean): String;
var
  SBuild: TJSONWriter;
begin
  try
    SBuild := TJSONWriter.Create(Ident);
    if Assigned(FCasted) then
       FCasted.AsJSONString(SBuild)
    else
       TJSONAncestor(FInterface).AsJSONString(SBuild);
    Result := SBuild.ToString;
  finally
    SBuild.Free;
  end;

end;

function TBaseJSON<T, Typ>.Contains(Key: Typ): Boolean;
begin
  Result := GetData(Key) <> Nil;
end;

function TBaseJSON<T, Typ>.ContainsEx(Key: Typ; Value: IJSONAncestor): Boolean;
begin
  Value := GetData(Key);
  Result := Value <> Nil;
end;

constructor TBaseJSON<T, Typ>.Create(JSON: T);
begin
  FJSONObj := JSON;
  FCasted := nil;
  FInterface := TValue.From<T>(JSON).AsInterface;
end;

function TBaseJSON<T, Typ>.DefaultValueClass<TT>(const Value: TValue): TT;
var
  r: TRttiContext;
  ty: TRttiType;
  w: word;
begin
  if TJSONString.InheritsFrom(TT) then
    Result := TJSONString.Create(Value.AsString) as TT
  else if TJSONInteger.InheritsFrom(TT) then
    Result := TJSONInteger.Create(Value.AsVariant) as TT
  else if TJSONFloat.InheritsFrom(TT) then
    Result := TJSONFloat.Create(Value.AsVariant) as TT
  else if TJSONBoolean.InheritsFrom(TT) then
    Result := TJSONBoolean.Create(Value.AsBoolean) as TT
  else if TJSONNull.InheritsFrom(TT) then
    Result := TJSONNull.Create(Value.AsBoolean) as TT
  else if TJSONArray.InheritsFrom(TT) then
  begin
    if not Value.IsEmpty then
      with Value do
      begin
        Result := AsInterface as TT;
        Exit;
      end;
    Result := TJSONArray.Create as TT;
  end
  else if TJSONObject.InheritsFrom(TT) then
  begin
    if not Value.IsEmpty then
      with Value do
      begin
        Result := AsInterface as TT;
        Exit;
      end;
    Result := TJSONObject.Create as TT;
  end
  else
  begin
    r := TRttiContext.Create;
    ty := r.GetType(TClass(TT));
    if ty = nil then
      exit(Nil);
    try
      Result := TT(ty.GetMethod('Create').Invoke(ty.AsInstance.MetaclassType, []).AsObject);
    except
      if Assigned(ty) then
        ty.Free;
      raise;
    end;
    r.Free;
  end;
end;

destructor TBaseJSON<T, Typ>.Destroy;
begin
  inherited;
end;

function TBaseJSON<T, Typ>.GetBoolean(V: Typ): Boolean;
begin
  Result := False;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONAncestor>(V).ValueEx<Boolean>;
end;

function TBaseJSON<T, Typ>.GetData(Key: Typ): IJSONAncestor;
var
  P: IJsonPair;
begin
  if Self.InheritsFrom(TSuperObject) then
  begin
     P := TJSONObject(FInterface).Get(TValue.From<Typ>(Key).AsString);
     if Assigned(P) then
        Result := P.JsonValue
     else
        Result := Nil
  end
  else
  if Self.InheritsFrom(TSuperArray) then
     Result := TJSONArray(FInterface).Get(TValue.From<Typ>(Key).AsInteger);
end;

function TBaseJSON<T, Typ>.GetDataType: TDataType;
var
  Cast: ICast;
begin
  if TValue.From<T>(FJSONObj).AsInterface <> nil then
     Cast := TCast.CreateFrom<T>(FJSONObj)
  else
  if Assigned(FCasted)  then
     Cast := TCast.Create(FCasted)
  else
     Exit(dtNil);
  Result := Cast.DataType
end;

function TBaseJSON<T, Typ>.GetDouble(V: Typ): Double;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
     if GetType(V) = varInt64 then
        Result := GetValue<TJSONInteger>(V).ValueEx<Int64>
     else
        Result := GetValue<TJSONFloat>(V).ValueEx<Double>;
end;

function TBaseJSON<T, Typ>.GetInteger(V: Typ): Int64;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONInteger>(V).ValueEx<Int64>;
end;

function TBaseJSON<T, Typ>.GetNull(V: Typ): TMemberStatus;
var
  Val: IJSONAncestor;
begin
  if ContainsEx(V, Val) then begin
     if Val is TJSONNull then
        Result := jNull
     else
        Result := jAssigned
  end else
        Result := jUnAssigned;
end;

function TBaseJSON<T, Typ>.GetArray(V: Typ): ISuperArray;
var
  J: IJSONArray;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
     Member<TJSONArray>(TValue.From<Typ>(V).AsVariant, TValue.Empty);
  J := GetValue<TJSONArray>(V);
  Result := TSuperArray.Create(J);
end;

function TBaseJSON<T, Typ>.GetObject(V: Typ): ISuperObject;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
    Member<TJSONObject>(TValue.From<Typ>(V).AsVariant, TValue.Empty);

  Result := TSuperObject.Create(GetValue<TJSONObject>(V));
end;

function TBaseJSON<T, Typ>.GetAncestor(V: Typ): IJSONAncestor;
begin
  Result := GetData(V);
end;

function TBaseJSON<T, Typ>.GetString(V: Typ): String;
begin
  Result := '';
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONString>(V).ValueEx<String>;
end;

function TBaseJSON<T, Typ>.GetType(Key: Typ): TVarType;
var
  Temp: IJSONAncestor;
begin
  Temp := GetData(Key);
  if Temp = Nil then
     Result := varUnknown
  else if Temp is TJSONString then
     Result := varString
  else if Temp is TJSONFloat then
     Result := varDouble
  else if Temp is TJSONInteger then
      Result := varInt64
  else if Temp is TJSONNull then
     Result := varNull
  else if Temp is TJSONObject then
     Result := varObject
  else if Temp is TJSONArray then
     Result := varArray
  else if Temp is TJSONBoolean then
     Result := varBoolean
end;

procedure TBaseJSON<T, Typ>.SetArray(V: Typ; const Value: ISuperArray);
begin
  Member<TJSONArray>(TValue.From<Typ>(V).AsVariant, TValue.From<IJSONArray>(Value.Self) )
end;

procedure TBaseJSON<T, Typ>.SetBoolean(V: Typ; const Value: Boolean);
begin
  Member<TJSONBoolean>(TValue.From<Typ>(V).AsVariant, Value)
end;

procedure TBaseJSON<T, Typ>.SetDouble(V: Typ; const Value: Double);
begin
  Member<TJSONFloat>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseJSON<T, Typ>.SetInteger(V: Typ; const Value: Int64);
begin
  Member<TJSONInteger>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseJSON<T, Typ>.SetNull(V: Typ; const Value: TMemberStatus);
begin
end;

procedure TBaseJSON<T, Typ>.SetObject(V: Typ; const Value: ISuperObject);
begin
  Member<TJSONObject>(TValue.From<Typ>(V).AsVariant, TValue.From<IJSONObject>(Value.Self) )
end;

procedure TBaseJSON<T, Typ>.SetString(V: Typ; const Value: String);
begin
  Member<TJSONString>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseJSON<T, Typ>.SetVariant(V: Typ; const Value: Variant);
var
  VTyp: TVarType;
begin
  if VarIsNull(Value) then
     Null[V] := jNull
  else
  begin
     VTyp := GetType(V);
     if VTyp = varUnknown then
        VTyp := VarType(Value);
     case VTyp of
       varString, varUString:
          S[V] := Value;
       varInt64, varInteger, varByte:
          I[V] := Value;
       varDouble, varCurrency:
          F[V] := Value;
       varBoolean:
          B[V] := Value;
       varDate:
          S[V] := FormatDateTime( JSONDateFormat, TDateTime(Value) );
       varNull:
          Null[V] := jNull;
     end;
  end;
end;

function TBaseJSON<T, Typ>.GetSelf: T;
begin
  Result := FJSONObj;
end;


{ TSuperObject }

function TSuperObject.AsArray: ISuperArray;
begin
  if not Assigned(FCasted) or not (FCasted is TJSONArray) then 
     Exit(Nil)
  else
     Result := TSuperArray.Create(IJSONArray(FCasted));
end;

function TSuperObject.AsObject: ISuperObject;
begin
  Result := Self;
end;

function TSuperObject.AsType<T>: T;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TypeInfo(T));
    if not Assigned(Typ) then
       Exit;
    if Typ.IsRecord then
       Result := TBaseSuperRecord<T>.FromJSON(Self)
    else if Typ.IsInstance then
    begin
      Result := Typ.GetMethod('Create').Invoke(Typ.AsInstance.MetaclassType, []).AsType<T>;
      TSerializeParse.WriteObject(TValue.From<T>(Result).AsObject, Self);
    end
    else
      raise SOException.Create('Unsupported type.');
  except
    Ctx.Free;
    raise;
  end;
end;

function TSuperObject.Clone: ISuperObject;
begin
  Result := SO(AsJSON);
end;

procedure TSuperObject.First;
begin
  FOffset := 0;
end;

function TSuperObject.GetCount: Integer;
begin
  Result := FJSONObj.Count;
end;

function TSuperObject.GetCurrentKey: String;
begin
  Result := FJSONObj.Get(FOffset).Name;
end;

function TSuperObject.GetCurrentValue: IJSONAncestor;
begin
  Result := FJSONObj.Get(FOffset).JsonValue;
end;

function TSuperObject.GetEnumerator: TSuperEnumerator<IJSONPair>;
begin
  Result.Index := -1;
  Result.List := TJSONObject(FJSONObj).GetEnumerator
end;

function TSuperObject.GetEoF: Boolean;
begin
  Result := FOffset > Count - 1;
end;

function TSuperObject.GetExpr(const Code: String): ISuperExpression;
begin
  Result := TSuperExpression.Create(FJSONObj, Code);
end;


function TSuperObject.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TSuperObject.GetString(V: String): String;
begin
  Result := inherited GetString(V);
end;

procedure TSuperObject.Next;
begin
  Inc(FOffset);
end;

class function TSuperObject.ParseFile(FileName: String): TSuperObject;
var
  Strm: TFileStream;
begin
  Strm := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := ParseStream(Strm);
  finally
    Strm.Free;
  end;
end;

class function TSuperObject.ParseStream(Stream: TStream): TSuperObject;
var
  Strm: TStringStream;
begin
  Strm := TStringStream.Create;
  try
    Strm.LoadFromStream(Stream);
    Result := TSuperObject.Create( Strm.DataString );
  finally
    Strm.Free;
  end;
end;

procedure TSuperObject.SaveTo(Stream: TStream; const Ident: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident) );
  try
     S.SaveToStream(S);
  finally
     S.Free;
  end;
end;

procedure TSuperObject.SaveTo(AFile: String; const Ident: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident) );
  try
     S.SaveToFile(AFile);
  finally
     S.Free;
  end;
end;

procedure TSuperObject.SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings);
begin
  case VarType(Data) of
    varNull:
        FJSONObj.AddPair(V, TJSONNull.Create(True));

    varDate:
        FJSONObj.AddPair(V, TJSONString.Create(DateTimeToStr(TDateTime(Data), AFormatSettings)));

    varInteger:
        FJSONObj.AddPair(V, TJSONInteger.Create(Integer(Data)));

    varBoolean:
        FJSONObj.AddPair(V, TJSONBoolean.Create(Data));

    varString, varUString:
        FJSONObj.AddPair(V, TJSONString.Create(String(Data)));

    varDouble:
        FJSONObj.AddPair(V, TJSONFloat.Create(Double(Data)));

    vtCurrency:
        FJSONObj.AddPair(V, TJSONFloat.Create(Currency(Data)));

    varInt64: FJSONObj.AddPair(V, TJSONInteger.Create(Int64(Data)));
  end;
end;

procedure TSuperObject.SetNull(V: String; const Value: TMemberStatus);
var
  Val: IJSONAncestor;
begin
  if Value = jAssigned then
     Exit;
  with TJSONObject(FJSONObj) do begin
       if ContainsEx(V, Val) then
       begin
          case Value of
            jUnAssigned:
              Remove(V);
            jNull: begin
              Remove(V);
              AddPair(V, TJSONNull.Create(True));
            end;
          end;
       end
       else
          AddPair(V, TJSONNull.Create(True));
  end;
end;

function TSuperObject.T: TSuperObject;
begin
  Result := Self;
end;

procedure TSuperObject.SetData(V: String; Data: Variant);
begin
  SetData(V, Data, FormatSettings);
end;

{ TSuperArray }

procedure TSuperArray.Add(Value: Variant; DateFormat: TFormatSettings);
begin
  if VarIsNull(Value) then
  begin
    TJSONArray(FJSONObj).Add(TJSONNull.Create(True));
    Exit;
  end;

  case VarType(Value) of
    varDate :
       TJSONArray(FJSONObj).Add(TJSONString.Create(DateTimeToStr(TDateTime(Value), DateFormat)));
    varBoolean:
       TJSONArray(FJSONObj).Add(TJSONBoolean.Create(Value));

    else
      with TValue.FromVariant(Value) do
          case Kind of
             tkInteger, tkInt64:
                TJSONArray(FJSONObj).Add(TJSONInteger.Create(Int64(Value)));

             tkFloat:
                TJSONArray(FJSONObj).Add(TJSONFloat.Create(Double(Value)));

             tkString, tkWChar, tkLString, tkWString, tkUString, tkChar:
                TJSONArray(FJSONObj).Add(TJSONString.Create(Value));
          end;
  end;

end;

procedure TSuperArray.Add(Value: Variant);
var 
  P: Pointer;
begin
  if VarType(Value) = varUnknown  then with IUnknown(Value) do
  begin
     if QueryInterface(ISuperObject, P) = S_OK then
        Add(ISuperObject(P))
     else
     if QueryInterface(ISuperArray, P) = S_OK then
        Add(ISuperArray(P))
     else
     if QueryInterface(IJSONAncestor, P) = S_OK then
        Add(IJSONAncestor(P));
  end
  else
  Add(Value, FormatSettings);
end;

function TSuperArray.AsArray: ISuperArray;
begin
  Result := Self;
end;

procedure TSuperArray.Add(Value: IJSONAncestor);
begin
  TJSONArray(FJSONObj).Add(Value);
end;

procedure TSuperArray.Clear;
begin
  FJSONObj.Clear;
end;

function TSuperArray.Clone: ISuperArray;
begin
  Result := SA(AsJSON);
end;

procedure TSuperArray.Delete(Index: Integer);
begin
  FJSONObj.Remove(Index);
end;

function TSuperArray.GetEnumerator: TSuperEnumerator<IJSONAncestor>;
begin
  Result.Index := -1;
  Result.List := TJSONArray(FJSONObj).GetEnumerator
end;

function TSuperArray.GetLength: Integer;
begin
  Result := TJSONArray(FJSONObj).Count;
end;

procedure TSuperArray.SaveTo(Stream: TStream; const Ident: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident) );
  try
     S.SaveToStream(S);
  finally
     S.Free;
  end;
end;

procedure TSuperArray.SaveTo(AFile: String; const Ident: Boolean);
var
  S: TStringStream;
begin
  S := TStringStream.Create( AsJSON(Ident) );
  try
     S.SaveToFile(AFile);
  finally
     S.Free;
  end;
end;

procedure TSuperArray.SetNull(V: Integer; const aValue: TMemberStatus);
var
  Val: IJSONAncestor;
begin
  if aValue = jAssigned then
     Exit;
  with FJSONObj do begin
       if ContainsEx(V, Val) then
       begin
          case aValue of
            jUnAssigned:
              Remove(V);
            jNull: begin
              Index[V] := TJSONNull.Create(True);
            end;
          end;
       end
       else
          Member<TJSONNull>(IntToStr(V), TValue.From<Boolean>(True));
  end;
end;

procedure TSuperArray.Add(Value: ISuperObject);
begin
  Add(Value.Self);
end;

procedure TSuperArray.Add(Value: ISuperArray);
begin
  Add(Value.Self);
end;

{ TSuperObjectHelper }

function TSuperObjectHelper.AsJSON: String;
begin
  Result := AsJSONObject.AsJSON;
end;

constructor TSuperObjectHelper.FromJSON(const JSON: String);
var
  IData: ISuperObject;
begin
  inherited Create;
  IData := TSuperObject.Create(JSON);
  TSerializeParse.WriteObject(Self, IData);
end;

function TSuperObjectHelper.AsJSONObject: ISuperObject;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    TSerializeParse.ReadObject(Self, IResult);
  finally
    Result := IResult;
  end;
end;

constructor TSuperObjectHelper.FromJSON(JSON: ISuperObject);
begin
  inherited Create;
  TSerializeParse.WriteObject(Self, JSON);
end;

{ TSerializeParse }

class procedure TSerializeParse.ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
var
  Prop: TRttiProperty;
  Field: TRttiField;
begin
  for Prop in aType.GetProperties do
      ReadMember<IJSONObject, String>(Prop.Name, Prop.PropertyType.Handle, Prop.GetValue(Data), IJSonData);
  for Field in aType.GetFields do
      ReadMember<IJSONObject, String>(Field.Name, Field.FieldType.Handle, Field.GetValue(Data), IJSonData);
end;

class procedure TSerializeParse.ReadObject(AObject: TObject; IResult: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AObject.ClassType);
    if not Assigned(Typ) then Exit;
    ReadMembers(AObject, Typ, IResult) ;
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.ReadRecord(Info: PTypeInfo; ARecord: Pointer; IResult: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiRecordType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Info).AsRecord;
    if not Assigned(Typ) then Exit;
    ReadMembers(ARecord, Typ, IResult) ;
  finally
    Ctx.Free;
  end;
end;

class function TSerializeParse.ReadRecordEx<T>(Rec: T): ISuperObject;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    with TValue.From<T>(Rec) do
      ReadRecord(TypeInfo, GetReferenceToRawData, IResult);
  finally
    Result := IResult;
  end;
end;

class function TSerializeParse.CheckObject<Typ>(Data: Pointer;
  Member: TRttiMember; MIdx: Typ; var Obj: TObject): Boolean;
var
  rtype: TRttiType;
  rawData: Pointer;
  Val: TValue;
begin
  Obj := Nil;
  rawData := GetArrayRawData(Member);
  rtype := GetMemberType(Member);
   if rawData <> nil then
  begin
    Obj := GetValue<Typ>(rawData, Member, MIdx).AsObject;
    if (Obj = Nil) and (ObjectConstructorParamCount(rtype.AsInstance.MetaclassType) = 0 ) then
    begin
      Obj := ObjectConstructor(rtype.AsInstance.MetaclassType);
      TValue.Make(@Obj, rtype.Handle , Val);
      SetValue<Typ>(rawData, Member, MIdx, Val);
    end;
  end
  else
  begin
    Obj := GetValue<String>(Data, Member, '').AsObject;
    if (Obj = Nil) and (ObjectConstructorParamCount(rtype.AsInstance.MetaclassType) = 0 ) then
    begin
      Obj := ObjectConstructor(rtype.AsInstance.MetaclassType);
      TValue.Make(@Obj, rtype.Handle , Val);
      SetValue<String>(Data, Member, '', Val);
    end;
  end;
  Result := Obj <> nil;
end;

class procedure TSerializeParse.ClearArrayRawData(Member: TRttiMember);
begin
  if Member is TRttiProperty  then
     TSuperProperty(Member).ArrayRawData := Nil
  else
  if Member is TRttiField then
     TSuperField(Member).ArrayRawData:= Nil

end;

class function TSerializeParse.GetArrayRawData(Member: TRttiMember): Pointer;
begin
  if Member is TRttiProperty  then
     Result := TSuperProperty(Member).ArrayRawData
  else
  if Member is TRttiField then
     Result := TSuperField(Member).ArrayRawData
end;

class function TSerializeParse.GetMemberType(Member: TRttiMember; const GetArray: Boolean): TRttiType;
begin
  if Member is TRttiProperty  then
  begin
     Result := TRttiProperty(Member).PropertyType;
     if GetArray and (TSuperProperty(Member).ArrayRawData <> Nil) then
        Result := TRttiDynamicArrayType(Result).ElementType;
  end
  else
  if Member is TRttiField then
  begin
     Result := TRttiField(Member).FieldType;
     if GetArray and (TSuperField(Member).ArrayRawData <> Nil) then
        Result := TRttiDynamicArrayType(Result).ElementType;
  end;
end;

class function TSerializeParse.GetMemberTypeInfo(
  Member: TRttiMember; const GetArray: Boolean): PTypeInfo;
begin
  Result := GetMemberType(Member, GetArray).Handle
end;

class function TSerializeParse.GetValue<Typ>(Data: Pointer;
  Member: TRttiMember; MIdx: Typ): TValue;
begin
  if (TypeInfo(Typ) = TypeInfo(Integer) ) and ( GetMemberTypeInfo(Member, False).Kind = tkDynArray ) then
      Result := GetValue<String>(GetArrayRawData(Member), Member, '')
                        .GetArrayElement(TValue.From<Typ>(MIdx).AsInteger)
  else

  if Member is TRttiProperty  then
     Result := TRttiProperty(Member).GetValue(Data)
  else
  if Member is TRttiField then
     Result := TRttiField(Member).GetValue(Data);
end;

class function TSerializeParse.ObjectConstructor(
  Instance: TClass): TObject;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Instance);
    Result := Typ.GetMethod('Create').Invoke(Instance, []).AsObject;
  finally
    Ctx.Free;
  end;
end;

class function TSerializeParse.ObjectConstructorParamCount(
  Instance: TClass): Integer;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Mtd: TRttiMethod;
begin
  Result := -1;
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Instance);
    if not Assigned(Typ) then Exit;
    Mtd := Typ.GetMethod('Create');
    if not Assigned(Mtd) then Exit;
    Result := Length( Mtd.GetParameters );
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.ReadMember<T, Typ>(Member: Typ; RType: PTypeInfo; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);
var
  I: Integer;
  SubVal: TValue;
begin
  if RType = TypeInfo(TDateTime) then
     IJSonData.S[Member] := FormatDateTime(JSONDateFormat, MemberValue.AsType<TDateTime>)
  else
  case RType.Kind of
    tkInteger:
       IJSonData.I[Member] := MemberValue.AsInteger;

    tkInt64:
       IJSonData.I[Member] := MemberValue.AsInt64;

    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
       IJSonData.S[Member] := MemberValue.AsString ;

    tkEnumeration:
       if MemberValue.TypeInfo = TypeInfo(Boolean) then
          IJsonData.B[Member] := Boolean( MemberValue.AsOrdinal )
       else
          IJsonData.I[Member] := MemberValue.AsOrdinal;

    tkFloat:
       IJsonData.F[Member] := MemberValue.AsExtended;

    tkSet:
       ReadSet(MemberValue, IJsonData.A[Member]);

    tkClass, tkPointer:
       if MemberValue.IsObject and (MemberValue.AsObject <> Nil) then
          ReadObject(MemberValue.AsObject, IJSonData.O[Member]);

    tkVariant:
       if TypeInfo(Typ) = TypeInfo(String) then
          ReadVariantOfObject(MemberValue.AsVariant, TValue.From<Typ>(Member).AsString, ISuperObject(IJsonData))
       else
       if TypeInfo(Typ) = TypeInfo(Integer) then
          ReadVariantOfArray(MemberValue.AsVariant, ISuperArray(IJsonData) );

    tkArray, tkDynArray:
       with MemberValue do
           for I := 0 to GetArrayLength - 1 do
           begin
               SubVal := GetArrayElement(I);
               ReadMember<IJSONArray, Integer>( I, SubVal.TypeInfo, SubVal, IJsonData.A[Member]);
           end;

    tkRecord:
       ReadRecord(MemberValue.TypeInfo, MemberValue.GetReferenceToRawData, IJSonData.O[Member]);

    tkInterface:
      if (TypeInfo(ISuperObject) = MemberValue.TypeInfo) then
         IJsonData.O[Member] := MemberValue.AsType<ISuperObject>.Clone
      else
      if (TypeInfo(ISuperArray) = MemberValue.TypeInfo) then
         IJsonData.A[Member] := MemberValue.AsType<ISuperArray>.Clone;

  end;
end;

class procedure TSerializeParse.ReadSet(Val: TValue;  IJsonData: ISuperArray);
var
  S: TIntegerSet;
  I: Integer;
begin
  Integer(S) := TValueData(Val).FAsULong;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
       IJsonData.Add(I);
end;

class procedure TSerializeParse.ReadTValueOfArray(Val: TValue;
  IJsonData: ISuperArray);
begin

end;

class procedure TSerializeParse.ReadVariantOfArray(Val: Variant; IJsonData: ISuperArray);
begin
   IJSonData.Add(Val);
end;

class procedure TSerializeParse.ReadVariantOfObject(Val: Variant; const Name: String; IJsonData: ISuperObject);
begin
  IJsonData.SetData(Name, Val);
end;



class procedure TSerializeParse.SetArrayRawData(Member: TRttiMember;
  RawData: Pointer);
begin
  if Member is TRttiProperty  then
     TSuperProperty(Member).ArrayRawData := RawData
  else
  if Member is TRttiField then
     TSuperField(Member).ArrayRawData:= RawData

end;

class procedure TSerializeParse.SetValue<Typ>(Data: Pointer; Member: TRttiMember; MIdx: Typ; Val: TValue);
begin
  if (TypeInfo(Typ) = TypeInfo(Integer) ) and ( GetMemberTypeInfo(Member, False).Kind = tkDynArray ) then
      GetValue<String>(GetArrayRawData(Member), Member, '').SetArrayElement(TValue.From<Typ>(MIdx).AsInteger, Val)
  else
  if Member is TRttiProperty  then
     TRttiProperty(Member).SetValue(Data, Val)
  else
  if Member is TRttiField then
     TRttiField(Member).SetValue(Data, Val);
end;

class procedure TSerializeParse.WriteMember<T, Typ>(Data: Pointer; Member: Typ;
  TypeKind: TTypeKind; MemberValue: TRttiMember; IJsonData: IBaseJSON<T, Typ>);
var
  I,J: Integer;
  P: Pointer;
  V: Variant;
  SubVal: TValue;
  Obj: TObject;

begin
  if not IJsonData.Contains(Member) then
     Exit;

  case TypeKind of
    tkInteger:
       SetValue<Typ>(Data, MemberValue, Member, Integer(IJSonData.I[Member]));

    tkInt64:
       SetValue<Typ>(Data, MemberValue, Member, IJSonData.I[Member]);

    tkChar,  tkWChar:
       if IJsonData.S[Member] > '' then
          SetValue<Typ>(Data, MemberValue, Member, TValue.From<Char>(IJSonData.S[Member]{$IFDEF XE2UP}.Chars[CharIndex]{$ELSE}[CharIndex]{$ENDIF}));

    tkString,tkLString, tkWString, tkUString:
       SetValue<Typ>(Data, MemberValue, Member, IJSonData.S[Member]);

    tkEnumeration:
       if GetMemberTypeInfo(MemberValue) = TypeInfo(Boolean) then
       begin
          SetValue<Typ>(Data, MemberValue, Member, IJSONData.B[Member]);
       end
       else
       begin
          TValue.Make(IJSONData.I[Member], GetMemberTypeInfo(MemberValue), SubVal );
          SetValue<Typ>(Data, MemberValue, Member, SubVal);
       end;


    tkFloat:
       SetValue<Typ>(Data, MemberValue, Member, IJsonData.F[Member]);

    tkSet:
       WriteSet(Data, MemberValue, IJsonData.A[Member]);

    tkClass:
       begin
          if CheckObject<Typ>(Data, MemberValue, Member, Obj) then
             WriteObject(Obj, IJSonData.O[Member]);
       end;

    tkVariant:
       begin
         V := IJSONData.V[Member];
         if not VarIsNull(V) then
         begin
            TValue.Make(@V, GetMemberTypeInfo(MemberValue), SubVal);
            SetValue<Typ>(Data, MemberValue, Member, SubVal);
         end;
       end;

    tkArray: raise SOException.Create('There is no support for static array.');

    tkDynArray:
      begin
        SetArrayRawData(MemberValue, Data);
        J := IJSonData.A[Member].Length;
        SubVal := GetValue<Typ>(Data, MemberValue, Member);
        DynArraySetLength(PPointer(SubVal.GetReferenceToRawData)^, SubVal.TypeInfo, 1, @J);
        SetValue<String>(Data, MemberValue,'', SubVal );
        for I := 0 to J-1 do
            WriteMember<IJSONArray, Integer>
                       (SubVal.GetReferenceToRawArrayElement(I),
                        I,
                        GetMemberType(MemberValue).TypeKind,
                        MemberValue,
                        IJsonData.A[Member]);
       ClearArrayRawData(MemberValue);
      end;

    tkRecord:
      begin
         P := IValueData(TValueData( GetValue<Typ>(Data, MemberValue, Member) ).FValueData).GetReferenceToRawData;
         WriteRecord(GetMemberTypeInfo(MemberValue), P, IJSonData.O[Member]);
         TValue.Make(P, GetMemberTypeInfo(MemberValue), SubVal);
         SetValue<Typ>(Data, MemberValue, Member, SubVal );
      end;

    tkInterface:
      if (TypeInfo(ISuperObject) = GetMemberTypeInfo(MemberValue)) And (IJsonData.Ancestor[Member].DataType = dtObject) then
          SetValue<Typ>(Data, MemberValue, Member, TValue.From<ISuperObject>(IJsonData.O[Member].Clone))
      else
      if (TypeInfo(ISuperArray) = GetMemberTypeInfo(MemberValue)) And (IJsonData.Ancestor[Member].DataType = dtArray) then
          SetValue<Typ>(Data, MemberValue, Member, TValue.From<ISuperArray>(IJsonData.A[Member].Clone));
  end;
end;

class procedure TSerializeParse.WriteMembers(Data: Pointer; aType: TRttiType;
  IJsonData: ISuperObject);
var
  Prop: TRttiProperty;
  Field: TRttiField;
begin
  for Prop in aType.GetProperties do
      if Prop.PropertyType <> Nil then
         WriteMember<IJSONObject, String>(Data, Prop.Name, Prop.PropertyType.TypeKind, TSuperProperty(Prop), IJSonData);
  for Field in aType.GetFields do
      if Field.FieldType <> Nil then
         WriteMember<IJSONObject, String>(Data, Field.Name, Field.FieldType.TypeKind, TSuperField(Field), IJSonData);
end;

class procedure TSerializeParse.WriteObject(AObject: TObject;
  IData: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AObject.ClassType);
    if not Assigned(Typ) then Exit;
    WriteMembers(AObject, Typ, IData);
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.WriteRecord(Info: PTypeInfo; ARecord: Pointer;
  IData: ISuperObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(Info);
    if not Assigned(Typ) then Exit;
    WriteMembers(ARecord, Typ, IData);
  finally
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.WriteRecordEx<T>(Rec: T;
  IData: ISuperObject);
begin
   with TValue.From<T>(Rec) do
        WriteRecord(TypeInfo, GetReferenceToRawData, IData);
end;

class procedure TSerializeParse.WriteSet(Data: Pointer; Member: TRttiMember;
  IJSONData: ISuperArray);
var
  Sets: TIntegerSet;
  I: Integer;
  Val: TValue;
begin
  for I := 0 to IJSONData.Length -1 do
      Include(Sets, IJSONData.I[I]);
  TValue.Make(Integer(Sets), GetMemberTypeInfo(Member), Val);
  SetValue<String>(Data, Member, '', Val);
end;

{ TSuperRecord<T> }

class function TBaseSuperRecord<T>.AsJSON(Rec: T): String;
begin
  Result := AsJSONObject(Rec).AsJSON;
end;

class function TBaseSuperRecord<T>.FromJSON(JSON: String): T;
begin
   Result := FromJSON(SO(JSON));
end;

class function TBaseSuperRecord<T>.AsJSONObject(Rec: T): ISuperObject;
begin
  Result := XSuperObject.TSerializeParse.ReadRecordEx<T>(Rec);
end;

class function TBaseSuperRecord<T>.FromJSON(JSON: ISuperObject): T;
var
  Val: TValue;
  P: Pointer;
begin
  FillChar(Result, SizeOf(T), 0);
  Val := TValue.From<T>(Result);
  P := IValueData(TValueData(Val).FValueData).GetReferenceToRawData;
  TSerializeParse.WriteRecord(Val.TypeInfo, P, JSON);
  Result := T(P^);
end;

{ TJSONValueHelper }

function TJSONValueHelper.ValueEx<T>: Variant;
var
  Valuable: Boolean;
  pV: PTypeInfo;
const
  Int = 0;
  Str = '';
begin
  Valuable := (Self <> Nil) and not isNull;
  pV := TypeInfo(T);
  if pV = TypeInfo(Int64) then begin
     if Valuable then
        Result := (Self as TJSONInteger).Value
     else
        Result := Int;
  end
  else
  if pV = TypeInfo(Double) then begin
     if Valuable then
        Result := (Self as TJSONFloat).Value
     else
        Result := Int
  end
  else
  if pV = TypeInfo(Boolean) then begin
     if Valuable then
        Result := (Self as TJSONBoolean).Value
     else
        Result := False
  end
  else
  if pV = TypeInfo(String) then
     if Valuable then
        Result := (Self as TJSONString).Value
     else
        Result := Str
end;

{ TSuperExpression }

constructor TSuperExpression.Create(Base: IJSONAncestor; const Expr: String);
begin
  FInterpreter := TJSONInterpreter.Create(Expr, Base);
  inherited Create(FInterpreter.ReadExpression);
end;

destructor TSuperExpression.Destroy;
begin
  FInterpreter.Free;
  inherited;
end;

{ TCast }

constructor TCast.Create(Base: IJSONAncestor);
begin
  FJSON := Base;
  FName := '';
end;

constructor TCast.Create(Base: IJSONPair);
begin
  FJSON := Base.JSONValue;
  FName := Base.Name;
end;

class function TCast.CreateFrom<T>(Base: T): ICast;
var
  IFace: IInterface;
begin
  IFace := TValue.From<T>(Base).AsInterface;
  if IFace is TJSONAncestor then
     Result := TCast.Create(IFace as TJSONAncestor)
  else
  if IFace is TJSONPair then
     Result := TCast.Create(IFace as TJSONPair)
  else
     Result := TCast.Create(TJSONAncestor(Nil));
end;

destructor TCast.Destroy;
begin
  FJSON := Nil;
  inherited;
end;

function TCast.GetArray: ISuperArray;
begin
  if not Assigned(FJSON) then
     Result := Nil
  else
     Result := TSuperArray.Create(FJSON as TJSONArray);
end;

function TCast.GetBoolean: Boolean;
begin
  if not Assigned(FJSON) then
     Result := False
  else
     Result := TJSONBoolean(FJSON).Value;
end;

function TCast.GetDataType: TDataType;
begin
  if FJSON = Nil then
     Result := dtNil
  else if FJSON is TJSONNull then
     Result := dtNull
  else if FJSON is TJSONString then
     Result := dtString
  else if FJSON is TJSONInteger then
     Result := dtInteger
  else if FJSON is TJSONFloat then
     Result := dtFloat
  else if FJSON is TJSONBoolean then
     Result := dtBoolean
  else if FJSON is TJSONObject then
     Result := dtObject
  else if FJSON is TJSONArray then
     Result := dtArray
  else
     raise SOException.Create('Unknown JSON Type');
end;

function TCast.GetFloat: Double;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     Result := TJSONFloat(FJSON).Value;
end;

function TCast.GetInteger: Int64;
begin
  if not Assigned(FJSON) then
     Result := 0
  else
     Result := TJSONInteger(FJSON).Value;
end;

function TCast.GetName: String;
begin
  Result := FName;
end;

function TCast.GetObject: ISuperObject;
begin
  if not Assigned(FJSON) then
     Result := Nil
  else
     Result := TSuperObject.Create(FJSON as TJSONObject);
end;

function TCast.GetString: String;
begin
  if not Assigned(FJSON) then
     Result := ''
  else
     Result := TJSONString(FJSON).Value;
end;

function TCast.GetVariant: Variant;
begin
   case DataType of
     dtNil, dtNull, dtObject, dtArray:
        Result := Null;
     dtString:
        Result := AsString;
     dtInteger:
        Result := AsInteger;
     dtFloat:
        Result := AsFloat;
     dtBoolean:
        Result := AsBoolean;
   end;
end;

procedure TCast.SetBoolean(const Value: Boolean);
begin
  if not Assigned(FJSON) then Exit;
  TJSONBoolean(FJSON).Value := Value;
end;

procedure TCast.SetFloat(const Value: Double);
begin
  if not Assigned(FJSON) then Exit;
  TJSONFloat(FJSON).Value := Value;
end;

procedure TCast.SetInteger(const Value: Int64);
begin
  if not Assigned(FJSON) then Exit;
  TJSONInteger(FJSON).Value := Value;
end;

procedure TCast.SetString(const Value: String);
begin
  if not Assigned(FJSON) then Exit;
  TJSONString(FJSON).Value := Value;
end;


procedure TCast.SetVariant(const Value: Variant);
begin
  case DataType of
     dtString:
        AsString := VarToStr(Value);
     dtInteger:
        AsInteger := Value;
     dtFloat:
        AsFloat   := Value;
     dtBoolean:
        AsBoolean := Value;
   end;
end;

function TCast.ToString(const Ident: Boolean = False): String;
var
  SBuilder: TJSONWriter;
begin
  try
    SBuilder := TJSONWriter.Create(Ident);
    FJSON.AsJSONString(SBuilder);
    Result := SBuilder.ToString;
  finally
    SBuilder.Free;
  end;
end;

{ TJSONEnumerator<T> }

function TSuperEnumerator<T>.GetCurrent: ICast;
begin
  Result := TCast.CreateFrom<T>(List.List[Index]);
end;

function TSuperEnumerator<T>.MoveNext: Boolean;
begin
  Result := Index < List.List.Count - 1;
  if Result then
    Inc(Index);
end;

{ TBase }

function TBase.AsArray: ISuperArray;
begin
  Result := Nil;
end;

function TBase.AsObject: ISuperObject;
begin
  Result := Nil;
end;

end.
