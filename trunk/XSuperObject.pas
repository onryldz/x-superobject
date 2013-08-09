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
  DBXJSON,
  Data.DBXJSONReflect,
  RTTI,
  TypInfo,
  Generics.Collections;

type

  ISuperObject = interface;
  ISuperArray = interface;

  IBaseJSON<T: Class; Typ> = interface
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

    property S[V: Typ]: String read GetString write SetString;
    property I[V: Typ]: Int64 read GetInteger write SetInteger;
    property B[V: Typ]: Boolean read GetBoolean write SetBoolean;
    property F[V: Typ]: Double read GetDouble write SetDouble;
    property O[V: Typ]: ISuperObject read GetObject write SetObject;
    property A[V: Typ]: ISuperArray read GetArray write SetArray;
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;

    function AsJSON: String;
    property Self: T read GetSelf;
  end;

  TBaseJSON<T: Class; Typ> = class(TInterfacedObject, IBaseJSON<T, Typ>)
  private
    FJSONObj: T;
    FJSONObjIsRef: Boolean;
    function DefaultValueClass<TT: Class>(const Value: TValue): TT;
    procedure Member<T: Class>(const Name: String; const Value: TValue); overload;
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
    function GetSelf: T;
    function GetData(Key: Typ): TJSONValue;
    function GetVariant(V: Typ): Variant;
    procedure SetVariant(V: Typ; const Value: Variant);
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
    property V[V: Typ]: Variant read GetVariant write SetVariant;
    function Contains(Key: Typ): Boolean;
    function GetType(Key: Typ): TVarType;
    function AsJSON: String;
    property Self: T read GetSelf;
  end;

  ISuperObject = interface(IBaseJSON<TJSONObject, String>)
    procedure First;
    procedure Next;

    function GetEoF: Boolean;
    function GetCount: Integer;
    function GetCurrentKey: String;
    function GetCurrentValue: TJSONValue;
    function GetAsString: String;
    function GetOffset: Integer;

    procedure SetData(V: String; Data: Variant); overload;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;

    property Count: Integer read GetCount;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: TJSONValue read GetCurrentValue;
    property AsString: String read GetAsString;
    property Offset: Integer read GetOffset;
  end;

  TSuperObject = class(TBaseJSON<TJSONObject, String>, ISuperObject)
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
    procedure SetData(V: String; Data: Variant); overload; inline;
    procedure SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings); overload;

    property Count: Integer read GetCount;
    property Offset: Integer read GetOffset;
    property EoF: Boolean read GetEoF;
    property CurrentKey: String read GetCurrentKey;
    property CurrentValue: TJSONValue read GetCurrentValue;
    property AsString: String read GetAsString;
  end;

  ISuperArray = interface(IBaseJSON<TJSONArray, Integer>)
    function GetLenght: Integer;
    property Lenght: Integer read GetLenght;
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
  end;

  TSuperArray = class(TBaseJSON<TJSONArray, Integer>, ISuperArray)
  private
    function GetLenght: Integer;
  public
    procedure Add(Value: Variant; DateFormat: TFormatSettings); overload;
    procedure Add(Value: Variant); overload;
    property Lenght: Integer read GetLenght;
  end;

  TSerializeParse = class
  public
    // ** Read
    class procedure ReadObject(AObject: TObject; IResult: ISuperObject);
    class procedure ReadRecord(Info: PTypeInfo; ARecord: Pointer; IResult: ISuperObject);
    class function  ReadRecordSpesific<T: Record>(Rec: T): String;
    class procedure ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure ReadMember<T: Class; Typ>(Member: Typ; TypeKind: TTypeKind; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);

    class procedure ReadSet(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfArray(Val: Variant; IJsonData: ISuperArray);
    class procedure ReadTValueOfArray(Val: TValue; IJsonData: ISuperArray);
    class procedure ReadVariantOfObject(Val: Variant; const Name: String; IJsonData: ISuperObject);

    // ** Write
    class procedure WriteObject(AObject: TObject; IData: ISuperObject);
    class procedure WriteRecord(Info: PTypeInfo; ARecord: Pointer; IData: ISuperObject);
    class procedure WriteRecordSpesific<T: Record>(Rec: T; IData: ISuperObject);
    class procedure WriteMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
    class procedure WriteMember<T: Class; Typ>(Data: Pointer; Member: Typ; TypeKind: TTypeKind; MemberValue: TRttiMember; IJsonData: IBaseJSON<T, Typ>);
    class procedure WriteSet(Data: Pointer; Member: TRttiMember; IJSONData: ISuperArray);
    class procedure SetValue(Data: Pointer; Member: TRttiMember; Val: TValue);
    class function  GetValue(Data: Pointer; Member: TRttiMember): TValue;
    class function  GetMemberTypeInfo(Member: TRttiMember): PTypeInfo; inline;

    class function  ObjectConstructorParamCount(Instance: TClass): Integer;
    class function  ObjectConstructor(Instance: TClass): TObject;
    class function  CheckObject(Data: Pointer; Member: TRttiMember; var Obj: TObject): Boolean;
  end;

  TSuperObjectHelper = class helper for TObject
  public
    function AsJSON: String;
    constructor ToJSON(const JSON: String);
  end;

  TSuperRecord<T: Record> = class
  public
    class function AsJSON(Rec: T): String;
    class function ToJSON(JSON: String): T;
  end;

implementation



{ TSuperObject }

constructor TBaseJSON<T, Typ>.Create(JSON: String);
begin
  FJSONObj := TJSONObject.ParseJSONValue(JSON) as T;
  FJSONObjIsRef := False;
end;

function TBaseJSON<T, Typ>.GetValue<T>(const Name: Typ): T;
begin
  if FJSONObj is TJSONObject then
    Result := TJSONObject(FJSONObj).Get(TValue.From<Typ>(Name).AsString).JsonValue as T
  else
    Result := TJSONArray(FJSONObj).Get(TValue.From<Typ>(Name).AsInteger) as T
end;

function TBaseJSON<T, Typ>.GetVariant(V: Typ): Variant;
begin
  case GetType(V) of
    varString: Result := S[V];
    varInt64: Result := I[V];
    varDouble: Result := F[V];
    varBoolean: Result := B[V];
  else
    Result := Null;
  end;
end;

function TBaseJSON<T, Typ>.Member(const Name: String): Boolean;
begin
  if FJSONObj is TJSONObject then
    Result := Assigned(TJSONObject(FJSONObj).Get(NAme))
  else
    Result := Assigned(TJSONArray(FJSONObj).Get(StrToInt(Name)))
end;

procedure TBaseJSON<T, Typ>.Member<T>(const Name: String; const Value: TValue);
var
  Pair: TJSONPair;
  Index: Integer;
begin
  if FJSONObj is TJSONObject then
  begin
    Pair := TJSONObject(FJSONObj).Get(Name);

    if not Assigned(Pair) then
    begin
      TJSONObject(FJSONObj).AddPair(Name, (DefaultValueClass<T>(Value) as TJSONValue));
      exit;
    end;

    if Assigned(Pair.JsonValue) then
      Pair.JsonValue.Free;

    Pair.JsonValue := DefaultValueClass<T>(Value) as TJSONValue;
  end
  else
  begin
    Index := StrToInt(Name);
    if TJSONArray(FJSONObj).Size - 1 < Index then
      while TJSONArray(FJSONObj).Size - 1 < Index do
        TJSONArray(FJSONObj).AddElement(DefaultValueClass<T>(Value) as TJSONValue);
  end;

end;

function TBaseJSON<T, Typ>.asJSon: String;
begin
  Result := TJSONValue(FJSONObj).ToString;
end;

function TBaseJSON<T, Typ>.Contains(Key: Typ): Boolean;
begin
  Result := GetData(Key) <> Nil;
end;

constructor TBaseJSON<T, Typ>.Create(JSON: TJSONValue);
begin
  FJSONObj := JSON as T;
  FJSONObjIsRef := True;
end;

function TBaseJSON<T, Typ>.DefaultValueClass<TT>(const Value: TValue): TT;
var
  r: TRttiContext;
  ty: TRttiType;
  w: word;
begin
  if TJSONString.InheritsFrom(TT) then
    Result := TJSONString.Create(Value.AsString) as TT
  else if TJSONNumber.InheritsFrom(TT) then
    Result := TJSONNumber.Create(Value.AsVariant) as TT
  else if TJSONObject.InheritsFrom(TT) then
  begin
    if not Value.IsEmpty then
      with Value do
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
  if not FJSONObjIsRef then
  begin
    FJSONObj.Free;
    FJSONObj := Nil;
  end;
  inherited;
end;

function TBaseJSON<T, Typ>.GetBoolean(V: Typ): Boolean;
begin
  Result := False;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONValue>(V) is TJSONTrue;
end;

function TBaseJSON<T, Typ>.GetData(Key: Typ): TJSONValue;
var
  P: TJsonPair;
begin
  if FJSONObj is TJSONObject then
  begin
     P := TJSonObject(FJSonObj).Get(TValue.From<Typ>(Key).AsString);
     if Assigned(P) then
        Result := P.JsonValue
     else
        Result := Nil
  end
  else
  if FJSONObj is TJSONArray then
     Result := TJSONArray(FJSonObj).Get(TValue.From<Typ>(Key).AsInteger);
end;

function TBaseJSON<T, Typ>.GetDouble(V: Typ): Double;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONNumber>(V).AsDouble;
end;

function TBaseJSON<T, Typ>.GetInteger(V: Typ): Int64;
begin
  Result := 0;
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONNumber>(V).AsInt64;
end;

function TBaseJSON<T, Typ>.GetArray(V: Typ): ISuperArray;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
    Member<TJSONArray>(TValue.From<Typ>(V).AsVariant, TValue.Empty);

  Result := TSuperArray.Create(GetValue<TJSONArray>(V));
end;

function TBaseJSON<T, Typ>.GetObject(V: Typ): ISuperObject;
begin
  Result := Nil;
  if not Member(TValue.From<Typ>(V).AsVariant) then
    Member<TJSONObject>(TValue.From<Typ>(V).AsVariant, TValue.Empty);

  Result := TSuperObject.Create(GetValue<TJSONObject>(V));
end;

function TBaseJSON<T, Typ>.GetString(V: Typ): String;
begin
  Result := '';
  if Member(TValue.From<Typ>(V).AsVariant) then
    Result := GetValue<TJSONString>(V).Value;
end;

function TBaseJSON<T, Typ>.GetType(Key: Typ): TVarType;
var
  Temp: TJSONValue;
begin
  Temp := GetData(Key);
  if Temp = Nil then
     Result := varUnknown
  else if Temp.ClassType = TJSONString then
     Result := varString
  else if Temp.ClassType = TJSONNumber then begin
     if Pos('.', TJSONNumber(Temp).Value) > 0 then
        Result := varDouble
     else
        Result := varInt64
  end
  else if Temp.ClassType = TJSONNull then
     Result := varNull
  else if Temp.ClassType = TJSONObject then
     Result := varObject
  else if Temp.ClassType = TJSONArray then
     Result := varArray
  else if (Temp.ClassType = TJSONTrue) or (Temp.ClassType = TJSONFalse) then
     Result := varBoolean
end;

procedure TBaseJSON<T, Typ>.SetArray(V: Typ; const Value: ISuperArray);
begin

end;

procedure TBaseJSON<T, Typ>.SetBoolean(V: Typ; const Value: Boolean);
begin
  if Value then
    Member<TJSONTrue>(TValue.From<Typ>(V).AsVariant, TValue.Empty)
  else
    Member<TJSONFalse>(TValue.From<Typ>(V).AsVariant, TValue.Empty)
end;

procedure TBaseJSON<T, Typ>.SetDouble(V: Typ; const Value: Double);
begin

end;

procedure TBaseJSON<T, Typ>.SetInteger(V: Typ; const Value: Int64);
begin
  Member<TJSONNumber>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseJSON<T, Typ>.SetObject(V: Typ; const Value: ISuperObject);
begin
  Member<TJSONObject>(TValue.From<Typ>(V).AsVariant, TValue.From<T>(Value.Self) );
end;

procedure TBaseJSON<T, Typ>.SetString(V: Typ; const Value: String);
begin
  Member<TJSONString>(TValue.From<Typ>(V).AsVariant, Value);
end;

procedure TBaseJSON<T, Typ>.SetVariant(V: Typ; const Value: Variant);
var
  VTyp: TVarType;
begin
  VTyp := GetType(V);
  if VTyp = varUnknown then
     VTyp := VarType(Value);
  case VTyp of
    varString, varUString:   S[V] := Value;
    varInt64, varInteger, varByte:    I[V] := Value;
    varDouble, varCurrency:   F[V] := Value;
    varBoolean:  B[V] := Value;
  end;
end;

function TBaseJSON<T, Typ>.GetSelf: T;
begin
  Result := FJSONObj;
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

procedure TSuperObject.SetData(V: String; Data: Variant; AFormatSettings: TFormatSettings);
begin
  case VarType(Data) of
    varNull:
        FJSONObj.AddPair(V, TJSONNull.Create);

    varDate:
        FJSONObj.AddPair(V, DateTimeToStr(TDateTime(Data), AFormatSettings));

    varInteger:
        FJSONObj.AddPair(V, TJSONNumber.Create(Integer(Data)));

    varBoolean:
        if Data then
           FJSONObj.AddPair(V, TJSONTrue.Create)
        else
           FJSONObj.AddPair(V, TJSONFalse.Create);

    varString, varUString:
        FJSONObj.AddPair(V, TJSONString.Create(String(Data)));

    varDouble:
        FJSONObj.AddPair(V, TJSONNumber.Create(Double(Data)));

    vtCurrency:
        FJSONObj.AddPair(V, TJSONNumber.Create(Currency(Data)));

    varInt64: FJSONObj.AddPair(V, TJSONNumber.Create(Int64(Data)));
  end;
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

{ TSuperObjectHelper }

function TSuperObjectHelper.asJSon: String;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    TSerializeParse.ReadObject(Self, IResult);
  finally
    Result := IResult.AsJSON;
  end;
end;

constructor TSuperObjectHelper.ToJSON(const JSON: String);
var
  IData: ISuperObject;
begin
  inherited Create;
  IData := TSuperObject.Create(JSON);
  TSerializeParse.WriteObject(Self, IData);
end;



{ TSerializeParse }

class procedure TSerializeParse.ReadMembers(Data: Pointer; aType: TRttiType; IJsonData: ISuperObject);
var
  Prop: TRttiProperty;
  Field: TRttiField;
begin
  for Prop in aType.GetProperties do
      ReadMember<TJSONObject, String>(Prop.Name, Prop.PropertyType.TypeKind, Prop.GetValue(Data), IJSonData);
  for Field in aType.GetFields do
      ReadMember<TJSONObject, String>(Field.Name, Field.FieldType.TypeKind, Field.GetValue(Data), IJSonData);
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
    if Assigned(Typ) then
       Typ.Free;
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
    if Assigned(Typ) then
       Typ.Free;
  end;
end;

class function TSerializeParse.ReadRecordSpesific<T>(Rec: T): String;
var
  IResult: ISuperObject;
begin
  try
    IResult := TSuperObject.Create;
    with TValue.From<T>(Rec) do
      ReadRecord(TypeInfo, GetReferenceToRawData, IResult);
  finally
    Result := IResult.AsJSON;
  end;
end;

class function TSerializeParse.CheckObject(Data: Pointer;
  Member: TRttiMember; var Obj: TObject): Boolean;
begin
  Obj := Nil;
  if (Member is TRttiProperty) then
  begin
    Obj := TRttiProperty(Member).GetValue(Data).AsObject;
    if (Obj = Nil) then
       if (ObjectConstructorParamCount( TRttiProperty(Member).PropertyType.AsInstance.MetaclassType ) <> 0 ) then
          Exit(False)
       else
       begin
          Obj := ObjectConstructor(TRttiProperty(Member).PropertyType.AsInstance.MetaclassType);
          TRttiProperty(Member).SetValue(Data, Obj);
       end;
  end
  else
  if (Member is TRttiField) then
  begin
    Obj := TRttiField(Member).GetValue(Data).AsObject;
    if (Obj = Nil) then
       if (ObjectConstructorParamCount( TRttiField(Member).FieldType.AsInstance.MetaclassType ) <> 0 ) then
          Exit(False)
       else
       begin
          Obj := ObjectConstructor(TRttiProperty(Member).PropertyType.AsInstance.MetaclassType);
          TRttiField(Member).SetValue(Data, Obj);
       end;
  end;
  Result := True;
end;

class function TSerializeParse.GetMemberTypeInfo(
  Member: TRttiMember): PTypeInfo;
begin
  if Member is TRttiProperty  then
     Result := TRttiProperty(Member).PropertyType.Handle
  else
  if Member is TRttiField then
     Result := TRttiField(Member).FieldType.Handle;
end;

class function TSerializeParse.GetValue(Data: Pointer;
  Member: TRttiMember): TValue;
begin
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
    Typ.Free;
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
    if Assigned(Typ) then
       Typ.Free;
    Ctx.Free;
  end;
end;

class procedure TSerializeParse.ReadMember<T, Typ>(Member: Typ; TypeKind: TTypeKind; MemberValue: TValue; IJsonData: IBaseJSON<T, Typ>);
var
  I: Integer;
  SubVal: TValue;
begin

  case TypeKind of
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
       if MemberValue.IsObject then
          ReadObject(MemberValue.AsObject, IJSonData.O[Member]);

    tkVariant:
       if TypeInfo(Typ) = TypeInfo(String) then
          ReadVariantOfObject(MemberValue.AsVariant, TValue.From<Typ>(Member).AsString, ISuperObject(IJsonData));

    tkArray, tkDynArray:
       with MemberValue do
           for I := 0 to GetArrayLength - 1 do
           begin
               SubVal := GetArrayElement(I);
               ReadMember<TJSONArray, Integer>( I, SubVal.Kind, SubVal, IJsonData.A[Member]);
           end;

    tkRecord:
       ReadRecord(MemberValue.TypeInfo, MemberValue.GetReferenceToRawData, IJSonData.O[Member]);

    tkClassRef: ;
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



class procedure TSerializeParse.SetValue(Data: Pointer; Member: TRttiMember; Val: TValue);
begin
  if Member is TRttiProperty  then
     TRttiProperty(Member).SetValue(Data, Val)
  else
  if Member is TRttiField then
     TRttiField(Member).SetValue(Data, Val);
end;

class procedure TSerializeParse.WriteMember<T, Typ>(Data: Pointer; Member: Typ;
  TypeKind: TTypeKind; MemberValue: TRttiMember; IJsonData: IBaseJSON<T, Typ>);
var
  I: Integer;
  P: Pointer;
  V: Variant;
  SubVal: TValue;
  Obj: TObject;

begin
  if not IJsonData.Contains(Member) then
     Exit;

  case TypeKind of
    tkInteger:
       SetValue(Data, MemberValue, Integer(IJSonData.I[Member]));

    tkInt64:
       SetValue(Data, MemberValue, IJSonData.I[Member]);

    tkChar,  tkWChar:
       if IJsonData.S[Member] > '' then
          SetValue(Data, MemberValue, TValue.From<Char>(IJSonData.S[Member]{$IFDEF VER250}{XE4}.Char(1){$ELSE}[1]{$ENDIF}));

    tkString,tkLString, tkWString, tkUString:
       SetValue(Data, MemberValue, IJSonData.S[Member]);

    tkEnumeration:
       if GetMemberTypeInfo(MemberValue) = TypeInfo(Boolean) then
       begin
          SetValue(Data, MemberValue, IJSONData.B[Member]);
       end
       else
       begin
          TValue.Make(IJSONData.I[Member], GetMemberTypeInfo(MemberValue), SubVal );
          SetValue(Data, MemberValue, SubVal);
       end;


    tkFloat:
       SetValue(Data, MemberValue, IJsonData.F[Member]);

    tkSet:
       WriteSet(Data, MemberValue, IJsonData.A[Member]);

    tkClass:
       begin
          if CheckObject(Data, MemberValue, Obj) then
             WriteObject(Obj, IJSonData.O[Member]);
       end;

    tkVariant:
       if TypeInfo(Typ) = TypeInfo(String) then
       begin
         V := IJSONData.V[Member];
         if not VarIsNull(V) then
         begin
            TValue.Make(V, GetMemberTypeInfo(MemberValue), SubVal);
            SetValue(Data, MemberValue, SubVal);
         end;
       end;

    tkArray:;

    tkDynArray:;

    tkRecord:
    begin
       P := IValueData(TValueData( GetValue(Data, MemberValue) ).FValueData).GetReferenceToRawData;
       WriteRecord(GetMemberTypeInfo(MemberValue), P, IJSonData.O[Member]);
       TValue.Make(P, GetMemberTypeInfo(MemberValue), SubVal);
       SetValue(Data, MemberValue, SubVal );
    end;

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
         WriteMember<TJSONObject, String>(Data, Prop.Name, Prop.PropertyType.TypeKind, Prop, IJSonData);
  for Field in aType.GetFields do
      if Field.FieldType <> Nil then
         WriteMember<TJSONObject, String>(Data, Field.Name, Field.FieldType.TypeKind, Field, IJSonData);
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
    if Assigned(Typ) then
       Typ.Free;
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
    if Assigned(Typ) then
       Typ.Free;
  end;
end;

class procedure TSerializeParse.WriteRecordSpesific<T>(Rec: T;
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
  for I := 0 to IJSONData.Lenght -1 do
      Include(Sets, IJSONData.I[I]);
  TValue.Make(Integer(Sets), GetMemberTypeInfo(Member), Val);
  SetValue(Data, Member, Val);
end;

{ TSuperRecord<T> }

class function TSuperRecord<T>.AsJSON(Rec: T): String;
begin
  Result := XSuperObject.TSerializeParse.ReadRecordSpesific<T>(Rec);
end;


class function TSuperRecord<T>.ToJSON(JSON: String): T;
var
  IData: ISuperObject;
  Val: TValue;
  P: Pointer;
begin
  Val := TValue.From<T>(Result);
  IData := TSuperObject.Create(JSON);
  P := IValueData(TValueData(Val).FValueData).GetReferenceToRawData;
  TSerializeParse.WriteRecord(Val.TypeInfo, P, IData);
  Move(P^, Result, SizeOf(T));
end;

end.
