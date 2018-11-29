unit json2pas;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fgl;

type

  (*
    enum representing possible json types
  *)
  TJ2PasType = (
    jtString,
    jtInt,
    jtFloat,
    jtBool,
    jtArray,
    jtObject
  );

  TJ2PasTypes = set of TJ2PasType;

  TJ2PasProp = class;
  TJ2PasPropClass = class of TJ2PasProp;

  { TJ2PasProp }
  (*
    an individual json property
  *)
  TJ2PasProp = class(TPersistent)
  strict private
    FName: String;
    FType: TJ2PasType;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetMeta: TJ2PasPropClass;virtual;
  public
    property Name : String read FName write FName;
    property JType : TJ2PasType read FType write FType;
    property MetaClass : TJ2PasPropClass read GetMeta;
    function Equals(Obj: TObject): boolean; override;
  end;



  { TJ2PasArrayProp }

  TJ2PasArrayProp = class(TJ2PasProp)
  strict private
    FArrayType: TJ2PasType;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetMeta: TJ2PasPropClass; override;
  public
    property ArrayType : TJ2PasType read FArrayType write FArrayType;
  end;

  { TJ2PasArrayObject }

  TJ2PasArrayObject = class(TJ2PasArrayProp)
  strict private
    FObjectName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetMeta: TJ2PasPropClass; override;
  public
    property ObjectName : String read FObjectName write FObjectName;
  end;

  (*
    list of json properties
  *)
  TJ2PasProps = TFPGObjectList<TJ2PasProp>;

  //forward
  TJ2PasObject = class;

  (*
    list of json objects
  *)
  TJ2PasObjects = TFPGObjectList<TJ2PasObject>;

  TNameFormat = procedure(Const AProperty:TJ2PasProp;Var ANewName:String);

  { TJ2PasParseOptions }
  (*
    options used in object parsing
  *)
  TJ2PasParseOptions = packed record
  strict private
    FIgnore: Boolean;
    FNameFormat: TNameFormat;
  public
    property IgnoreNulls : Boolean read FIgnore write FIgnore;
    property NameFormat : TNameFormat read FNameFormat write FNameFormat;
  end;

  { TJ2PasObject }
  (*
    representation of a json object
  *)
  TJ2PasObject = class
  strict private
    FName: String;
    FProps: TJ2PasProps;
    class function GetObjects: TJ2PasObjects; static;
  strict protected
  public
    //properties
    property Name : String read FName write FName;
    property Properties : TJ2PasProps read FProps;
    class property Objects : TJ2PasObjects read GetObjects;

    //methods
    function Equals(Obj: TObject): boolean; override;
    class function ObjectExists(Const AProperties:TJ2PasProps;Out Index:Integer;
      Const AAddIfFalse:Boolean=True;Const AName:String=''):Boolean;static;
    class function Parse(Const AJSON:String;Out JObject:TJ2PasObject;
      out Error:String):Boolean;static;
    constructor Create;virtual;overload;
    constructor Create(Const AOwnsProps:Boolean);virtual;overload;
    destructor Destroy; override;
  end;

  (*
    given a list of objects, will find one by name
  *)
  function ObjectByName(Const AObjects:TJ2PasObjects;Const AName:String;
    Out AObject:TJ2PasObject):Boolean;
var
  (*
    assign a default name format and this will be used when parsing
  *)
  DefaultNameFormat : TNameFormat;

implementation
var
  GlobalObjects : TJ2PasObjects;

function ObjectByName(const AObjects: TJ2PasObjects;Const AName:String;
  out AObject: TJ2PasObject): Boolean;
var
  I:Integer;
begin
  Result:=False;
  for I := 0 to Pred(AObjects.Count) do
  begin
    if AObjects[I].Name = AName then
    begin
      AObject:=AObjects[I];
      Result:=True;
      Exit;
    end;
  end;
end;

{ TJ2PasArrayObject }

procedure TJ2PasArrayObject.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if not (Dest is TJ2PasArrayObject) then
    Exit;
  TJ2PasArrayObject(Dest).ObjectName:=FObjectName;
end;

function TJ2PasArrayObject.GetMeta: TJ2PasPropClass;
begin
  Result:=TJ2PasArrayObject;
end;

{ TJ2PasArrayProp }

procedure TJ2PasArrayProp.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if not (Dest is TJ2PasArrayProp) then
    Exit;
  TJ2PasArrayProp(Dest).ArrayType:=FArrayType;
end;

function TJ2PasArrayProp.GetMeta: TJ2PasPropClass;
begin
  Result:=TJ2PasArrayProp;
end;

{ TJ2PasProp }

function TJ2PasProp.GetMeta: TJ2PasPropClass;
begin
  Result:=TJ2PasProp;
end;

procedure TJ2PasProp.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TJ2PasProp) then
    Exit;
  TJ2PasProp(Dest).Name:=FName;
  TJ2PasProp(Dest).JType:=FType;
end;

function TJ2PasProp.Equals(Obj: TObject): boolean;
begin
  Result:=False;
  if not (Obj is TJ2PasProp) then
    Exit;
  if (TJ2PasProp(Obj).Name = FName) and (TJ2PasProp(Obj).JType = FType) then
    Result:=True;
end;

{ TJ2PasObject }

class function TJ2PasObject.GetObjects: TJ2PasObjects;
begin
  Result:=GlobalObjects;
end;

function TJ2PasObject.Equals(Obj: TObject): boolean;
var
  LObj:TJ2PasObject;
  I:Integer;

  (*
    checks to see if a property exists in an object
  *)
  function PropExists(Const AObject:TJ2PasObject;
    Const AProp:TJ2PasProp):Boolean;
  var
    I:Integer;
  begin
    Result:=False;
    for I := 0 to Pred(AObject.Properties.Count) do
      if AObject.Properties[I].Equals(AProp) then
        Exit(True)
  end;

begin
  Result:=False;
  if not (Obj is TJ2PasObject) then
    Exit;

  LObj:=TJ2PasObject(Obj);

  //mismatched count invalidate
  if Properties.Count <> LObj.Properties.Count then
    Exit;

  for I := 0 to Pred(Properties.Count) do
    if not PropExists(LObj,Properties[I]) then
      Exit;

  //otherwise we've matched all properties
  Result:=True;
end;

class function TJ2PasObject.ObjectExists(const AProperties: TJ2PasProps; out
  Index: Integer; const AAddIfFalse: Boolean;Const AName:String): Boolean;
var
  I:Integer;
  LObj,
  LNewObj:TJ2PasObject;
  LProp:TJ2PasProp;
begin
  Result:=False;
  Index:=-1;
  LObj:=TJ2PasObject.Create(False);
  try
    //look through all objects
    for I := 0 to Pred(Objects.Count) do
    begin
      //assign the properties to local object for comparison
      LObj.Properties.Assign(AProperties);

      //return if we find a matching object
      if Objects[I].Equals(LObj) then
      begin
        Index:=I;
        Result:=True;
        Exit;
      end;
    end;

    //otherwise, we haven't found a match, see if we need to add
    if AAddIfFalse then
    begin
      LNewObj:=TJ2PasObject.Create(true);
      LNewObj.Name:=AName;

      //copy properties
      for I := 0 to Pred(AProperties.Count) do
      begin
        LProp:=AProperties[I].MetaClass.Create;
        LProp.Assign(AProperties[I]);
        LNewObj.Properties.Add(LProp);
      end;

      //add object to global
      Index:=Objects.Add(LNewObj);
    end;
  finally
    LObj.Free;
  end;
end;

class function TJ2PasObject.Parse(const AJSON: String;
  out JObject: TJ2PasObject;out Error:String): Boolean;
var
  I:Integer;
  LData:TJSONData;
  LJSON:TJSONObject;

  function JTypeToJ2PType(Const AType:TJSONtype;Const AValue:String=''):TJ2PasType;
  var
    LInt:Integer;
  begin
    case AType of
      TJSONtype.jtObject: Result:=TJ2PasType.jtObject;
      TJSONtype.jtString: Result:=TJ2PasType.jtString;
      TJSONtype.jtBoolean: Result:=TJ2PasType.jtBool;
      TJSONtype.jtArray: Result:=TJ2PasType.jtArray;
      TJSONtype.jtNumber:
        begin
          if TryStrToInt(AValue,LInt) then
            Result:=jtFloat
          else
            Result:=jtInt;
        end;
      else
        Result:=TJ2PasType.jtString;
    end;
  end;

  procedure AddBasicType(Const AData:TJSONData;Const AName:String);
  var
    LProp:TJ2PasProp;
  begin
    LProp:=TJ2PasProp.Create;
    LProp.Name:=AName;

    //map j2pas types
    LProp.JType:=JTypeToJ2PType(AData.JSONType,AData.Value);

    //add the property to the object if it doesn't exist
    if JObject.Properties.IndexOf(LProp) < 0 then
      JObject.Properties.Add(LProp)
    else
      LProp.Free;
  end;

  procedure AddArrayType(Const AJSONArray:TJSONArray;Const AName:String);
  var
    LProp:TJ2PasProp;
    LData:TJSONData;
    LObj:TJ2PasObject;
    LError:String;
    I:Integer;
  begin
    //if we cannot determine the type of the first value we cannot had the prop
    if not Assigned(AJSONArray) or (AJSONArray.Count < 1) then
      Exit;

    //get reference to data
    LData:=AJSONArray.Items[0];

    //based on type fill out the array properties
    case LData.JSONType of
      TJSONtype.jtNumber, TJSONtype.jtString, TJSONtype.jtBoolean:
        begin
          LProp:=TJ2PasArrayProp.Create;
          LProp.JType:=TJ2PasType.jtArray;
          LProp.Name:=AName;
          TJ2PasArrayProp(LProp).JType:=JTypeToJ2PType(LData.JSONType,LData.Value);
        end;
      TJSONtype.jtArray:
        begin
          //we only handle one level deep for arrays, so for arrays of arrays
          //just mark as string
          LProp:=TJ2PasArrayProp.Create;
          LProp.JType:=TJ2PasType.jtArray;
          LProp.Name:=AName;
          TJ2PasArrayProp(LProp).ArrayType:=TJ2PasType.jtString;
        end;
      TJSONtype.jtObject:
        begin
          LProp:=TJ2PasArrayObject.Create;
          LProp.JType:=TJ2PasType.jtArray;
          LProp.Name:=AName;
          TJ2PasArrayObject(LProp).ArrayType:=TJ2PasType.jtObject;

          //attempt to parse this object
          if not Parse(LData.AsJSON,LObj,LError) then
          begin
            LProp.Free;
            Exit;
          end;

          //check if the object exists, if so use it's name rather than this one
          if ObjectExists(LObj.Properties,I,True,AName + 'Item') then
            TJ2PasArrayObject(LProp).ObjectName:=Objects[I].Name
          else
            TJ2PasArrayObject(LProp).ObjectName:=AName + 'Item';
        end;
    end;

    //add property
    JObject.Properties.Add(LProp);
  end;

  procedure AddObjectType(Const AObject:TJSONObject;Const AName:String);
  var
    LProp:TJ2PasProp;
    LObj:TJ2PasObject;
    LError:String;
    I:Integer;
  begin
    LProp:=TJ2PasProp.Create;
    LProp.Name:=AName;
    LProp.JType:=TJ2PasType.jtObject;

    //first look to see if this object exists by name, if not we need to add it
    if not ObjectByName(Objects,AName,LObj) then
    begin
      //try to parse, if we can't free property
      if not Parse(AObject.AsJSON,LObj,LError) then
      begin
        LProp.Free;
        Exit;
      end
      //otherwise check for existance on properties, and add if exists
      //use the name that is set their rather than the one provided to us
      else
        if ObjectExists(LObj.Properties,I,True,AName) then
          LProp.Name:=Objects[I].Name;
    end;

    //add property
    JObject.Properties.Add(LProp);
  end;

begin
  Result:=False;
  try
    LData:=GetJSON(AJSON);

    //handle bad json
    if not Assigned(LData) then
    begin
      Error:='json is invalid';
      Exit;
    end;

    //make sure we have a valid json object
    if not (LData.JSONType = TJSONtype.jtObject) then
    begin
      Error:='json is not a valid object';
      Exit;
    end;

    LJSON:=TJSONObject(LData);
    try
      JObject:=TJ2PasObject.Create(True);

      //translate properties to j2pas properties
      for I := 0 to Pred(LJSON.Count) do
      begin
        LData:=LJSON.Items[I];
        case LData.JSONType of
          //handle basic types
          TJSONtype.jtNumber, TJSONtype.jtString, TJSONtype.jtBoolean:
            begin
              AddBasicType(LData,LJSON.Names[I]);
            end;
          //for arrays we need to determine if it's values are basic type
          //or a complex type (array/object) by looking at the first item
          TJSONtype.jtArray:
            begin
              AddArrayType(TJSONArray(LData),LJSON.Names[I]);
            end;
          //object types can be handled with recursion
          TJSONtype.jtObject:
            begin
              AddObjectType(TJSONObject(LData),LJSON.Names[I]);
            end;
        end;
      end;
      Result:=True;
    finally
      LJSON.Free;
    end;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TJ2PasObject.Create;
begin
  Create(True);
end;

constructor TJ2PasObject.Create(const AOwnsProps: Boolean);
begin
  FProps:=TJ2PasProps.Create(AOwnsProps);
end;

destructor TJ2PasObject.Destroy;
begin
  FProps.Free;;
  inherited Destroy;
end;

initialization
  GlobalObjects:=TJ2PasObjects.Create(True);
finalization
  GlobalObjects.Free;
end.

