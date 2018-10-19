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
      Const AAddIfFalse:Boolean=True):Boolean;static;
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
  Objects : TJ2PasObjects;

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
  inherited AssignTo(Dest);
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

class function TJ2PasObject.GetObjects: TJ2PasObjects; static;
begin
  Result:=Objects;
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
  Index: Integer; const AAddIfFalse: Boolean): Boolean;
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
begin
  Result:=False;
  try
    //todo - parse string and create object
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
  Objects:=TJ2PasObjects.Create(True);
finalization
  Objects.Free;
end.

