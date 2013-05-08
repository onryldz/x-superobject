unit XSuperObject;

interface

uses
  Classes,
  Variants,
  SysUtils,
  DBXJSON,
  RTTI,
  TypInfo;

type

  ISuperObject = interface;
  ISuperArray = interface;

  IBaseSuperObject<T: Class; Typ> = interface
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

    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;

    function asJSon: String;
  end;

  TBaseSuperObject<T: Class; Typ> = class(TInterfacedObject,
    IBaseSuperObject<T, Typ>)
  private
    FJSONObj: T;
    FJSONObjIsRef: Boolean;
    function DefaultValueClass<TT: Class>(const Value: Variant): TT;
    procedure Member<T: Class>(const Name: String;
      const Value: Variant); overload;
    function Member(const Name: String): Boolean; overload;
    function GetValue<T: Class>(const Name: Typ): T;
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
  public
    constructor Create(JSON: String = '{}'); overload;
    constructor Create(JSON: TJSONValue); overload;
    destructor Destroy; override;
    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    function asJSon: String;
  end;

  ISuperObject = interface(IBaseSuperObject<TJSONObject, String>)
    procedure First;
    procedure Next;

    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: TJSONValue;
    function GetAsString: String;
    function GetOffset: Integer;

    property Count: Integer read GetCount;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: TJSONValue read GetCurrentValue;
    property AsString: String read GetAsString;
    property Offset: Integer read GetOffset;
  end;

  ISuperArray = interface(IBaseSuperObject<TJSONArray, Integer>)
    function GetLenght: Integer;
    property Lenght: Integer read GetLenght;
    procedure Add(Value: Variant; DateFormat: TFormatSettings);
  end;

  TSuperObject = class(TBaseSuperObject<TJSONObject, String>, ISuperObject)
  private
    FOffset: Integer;
    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: TJSONValue;
    function GetAsString: String;
    function GetOffset: Integer;
  public
    procedure First;
    procedure Next;
    property Count: Integer read GetCount;
    property Offset: Integer read GetOffset;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: TJSONValue read GetCurrentValue;
    property AsString: String read GetAsString;
  end;

  TSuperArray = class(TBaseSuperObject<TJSONArray, Integer>, ISuperArray)
  private
    function GetLenght: Integer;
  public
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    property Lenght: Integer read GetLenght;
  end;

implementation

{ TSuperObject }

constructor TBaseSuperObject<T, Typ>.Create(JSON: String);
begin
  FJSONObj := TJSONObject.ParseJSONValue(JSON) as T;
  FJSONObjIsRef := False;
end;

function TBaseSuperObject<T, Typ>.GetValue<T>(const Name: Typ): T;
begin
  if FJSONObj is TJSONObject then
    Result := TJSONObject(FJSONObj).Get(TValue.From<Typ>(Name).AsString)
      .JsonValue as T
  else
    Result := TJSONArray(FJSONObj).Get(TValue.From<Typ>(Name).AsInteger) as T
end;

function TBaseSuperObject<T, Typ>.Member(const Name: String): Boolean;
begin
  if FJSONObj is TJSONObject then
    Result := Assigned(TJSONObject(FJSONObj).Get(NAme));
end;

procedure TBaseSuperObject<T, Typ>.Member<T>(const Name: String;
  const Value: Variant);
var
  Pair: TJSONPair;
begin
  if FJSONObj is TJSONObject then
  begin
    Pair := TJSONObject(FJSONObj).Get(Name);

    if not Assigned(Pair) then
    begin
      TJSONObject(FJSONObj).AddPair(Name,
        (DefaultValueClass<T>(Value) as TJSONValue));
      exit;
    end;

    if Assigned(Pair.JsonValue) then
      Pair.JsonValue.Free;

    Pair.JsonValue := DefaultValueClass<T>(Value) as TJSONValue;
  end;
end;

function TBaseSuperObject<T, Typ>.asJSon: String;
begin
  Result := TJSONValue(FJSONObj).ToString;
end;

constructor TBaseSuperObject<T, Typ>.Create(JSON: TJSONValue);
begin
  FJSONObj := JSON as T;
  FJSONObjIsRef := True;
end;

function TBaseSuperObject<T, Typ>.DefaultValueClass<TT>
  (const Value: Variant): TT;
var
  r: TRttiContext;
  ty: TRttiType;
  w: word;
begin
  if TJSONString.InheritsFrom(TT) then
    Result := TJSONString.Create(Value) as TT
  else if TJSONNumber.InheritsFrom(TT) then
    Result := TJSONNumber.Create(Value) as TT
  else if TJSONObject.InheritsFrom(TT) then
  begin
    if not VarIsNull(Value) then
      with TValue.FromVariant(Value) do
        if IsObject then
        begin
          Result := AsObject as TT;
          exit;
        end;
    Result := TJSONObject.Create as TT;
  end
  else
  begin
    r := TRttiContext.Create;
    ty := r.GetType(TT);
    if ty = nil then
      exit(Nil);
    try
      Result := TT(ty.GetMethod('Create').Invoke(ty.AsInstance.MetaclassType,
        []).AsObject);
    except
      if Assigned(ty) then
        ty.Free;
      raise;
    end;
    r.Free;
  end;
end;

destructor TBaseSuperObject<T, Typ>.Destroy;
begin
  if not FJSONObjIsRef then
  begin
    FJSONObj.Free;
    FJSONObj := Nil;
  end;
  inherited;
end;

function TBaseSuperObject<T, Typ>.GetBoolean(V: Typ): Boolean;
begin
  Result := False;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONValue>(V) is TJSONTrue;
end;

function TBaseSuperObject<T, Typ>.GetDouble(V: Typ): Double;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONNumber>(V).AsDouble;
end;

function TBaseSuperObject<T, Typ>.GetInteger(V: Typ): Int64;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONNumber>(V).AsInt64;
end;

function TBaseSuperObject<T, Typ>.GetArray(V: Typ): ISuperArray;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
    Member<TJSONArray>(TValue.From<Typ>(V).AsVariant, Null);

  Result := TSuperArray.Create(GetValue<TJSONArray>(V));
end;

function TBaseSuperObject<T, Typ>.GetObject(V: Typ): ISuperObject;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
    Member<TJSONObject>(TValue.From<Typ>(V).AsVariant, Null);

  Result := TSuperObject.Create(GetValue<TJSONObject>(V));
end;

function TBaseSuperObject<T, Typ>.GetString(V: Typ): String;
begin
  Result := '';
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONString>(V).Value;
end;

procedure TBaseSuperObject<T, Typ>.SetArray(V: Typ; const Value: ISuperArray);
begin

end;

procedure TBaseSuperObject<T, Typ>.SetBoolean(V: Typ; const Value: Boolean);
begin
  if Value then
    Member<TJSONTrue>(TValue.From<Typ>(V).AsVariant, Null)
  else
    Member<TJSONFalse>(TValue.From<Typ>(V).AsVariant, Null)
end;

procedure TBaseSuperObject<T, Typ>.SetDouble(V: Typ; const Value: Double);
begin

end;

procedure TBaseSuperObject<T, Typ>.SetInteger(V: Typ; const Value: Int64);
begin
  Member<TJSONNumber>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseSuperObject<T, Typ>.SetObject(V: Typ; const Value: ISuperObject);
begin
  Member<TJSONObject>(TValue.From<Typ>(V).AsVariant,
    TValue(TSuperObject(Value).FJSONObj).AsVariant);
end;

procedure TBaseSuperObject<T, Typ>.SetString(V: Typ; const Value: String);
begin
  Member<TJSONString>(TValue.From<Typ>(V).AsVariant, Value);
end;

{ TSuperObject }

procedure TSuperObject.First;
begin
  FOffset := 0;
end;

function TSuperObject.GetAsString: String;
begin
  Result := FJSONObj.Value;
end;

function TSuperObject.GetCount: Integer;
begin
  Result := FJSONObj.Size;
end;

function TSuperObject.GetCurrentKey: String;
begin
  Result := FJSONObj.Get(FOffset).JsonString.Value;
end;

function TSuperObject.GetCurrentValue: TJSONValue;
begin
  Result := FJSONObj.Get(FOffset).JsonValue;
end;

function TSuperObject.GetEoF: Boolean;
begin
  Result := FOffset > Count - 1;
end;

function TSuperObject.GetOffset: Integer;
begin
  Result := FOffset;
end;

procedure TSuperObject.Next;
begin
  Inc(FOffset);
end;

{ TSuperArray }

procedure TSuperArray.Add(Value: Variant; DateFormat: TFormatSettings);
begin

  if VarIsNull(Value) then
  begin
    FJSONObj.AddElement(TJSONNull.Create);
    exit;
  end;

  if VarType(Value) = varDate then
    FJSONObj.Add(DateTimeToStr(TDateTime(Value), DateFormat))
  else
    with TValue.FromVariant(Value) do
    begin
      case Kind of
        tkInteger, tkInt64:
          FJSONObj.Add(Int64(Value));

        tkFloat:
          FJSONObj.Add(Double(Value));

        tkString, tkWChar, tkLString, tkWString, tkUString, tkChar:
          FJSONObj.Add(String(Value));

      end;
    end;
end;

procedure TSuperArray.Add(Value: Variant);
begin
   Add(Value, FormatSettings);
end;

function TSuperArray.GetLenght: Integer;
begin
  Result := TJSONArray(FJSONObj).Size;
end;

end.