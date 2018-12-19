{ json2pas

  Copyright (c) 2018 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit json2pas.producer.std.pascal;

{$mode delphi}

interface

uses
  Classes, SysUtils, json2pas, json2pas.producer.std;

type

  TMetaType = (mtSimple, mtObject, mtCollection, mtObjectCollection);

  { TMetadata }
  (*
    simple structure for holding some meta data on a j2pas object
    that can be used to construct a pascal unit
  *)
  TMetadata = record
  strict private
    FCollectName: String;
    FConstName: String;
    FConstVal: String;
    FFieldName: String;
    FFieldType: String;
    FType: TMetaType;
  public
    property ConstantName : String read FConstName write FConstName;
    property ConstantValue : String read FConstVal write FConstVal;
    property FieldName : String read FFieldName write FFieldName;
    property FieldType : String read FFieldType write FFieldType;
    property CollectionName : String read FCollectName write FCollectName;
    property MetaType : TMetaType read FType write FType;
  end;

  TMetaDatas = array of TMetadata;

  { IHeaderSection }
  (*
    base interface for pascal header sections
  *)
  IHeaderSection = interface(ISectionWriter)
    ['{AE9B633A-2C9D-4931-87AA-61649B47DABD}']
    function GetCmt: String;
    function GetUnit: String;
    procedure SetCmt(Const AValue: String);
    procedure SetUnit(Const AValue: String);

    (*
      the name of this unit
    *)
    property UnitName : String read GetUnit write SetUnit;

    (*
      a comment that is placed above the unit name
    *)
    property HeaderComment : String read GetCmt write SetCmt;
  end;


  { IIntfUsesSection }
  (*
    base interface for pascal interface and uses section
  *)
  IIntfUsesSection = interface(ISectionWriter)
    ['{927EE508-D758-4752-A711-BBC0F1BB8F88}']
    //property methods
    function GetUnits: TStringList;

    //properties
    property Units : TStringList read GetUnits;
  end;

  { ITypeSection }
  (*
    writes the type section and object definition
  *)
  ITypeSection = interface(ISectionWriter)
    ['{7FDDEA54-A933-4724-B152-D85CBE0C177F}']
    //property methods
    function GetIncImpl: Boolean;
    function GetIncIntf: Boolean;
    procedure SetIncImpl(Const AValue: Boolean);
    procedure SetIncIntf(Const AValue: Boolean);

    //properties
    property IncludeInterface : Boolean read GetIncIntf write SetIncIntf;
    property IncludeImplementation : Boolean read GetIncImpl write SetIncImpl;
  end;

  { IImplUsesSection }
  (*
    base interface for pascal implementation uses section
  *)
  IImplUsesSection = interface(ISectionWriter)
    ['{927EE508-D758-4752-A711-BBC0F1BB8F88}']
    //property methods
    function GetUnits: TStringList;

    //properties
    property Units : TStringList read GetUnits;
  end;

  { IImplSection }
  (*
    base interface for writing the implementation associated with what was
    written by the type writer
  *)
  IImplSection = interface(ISectionWriter)
    ['{9BDBB332-BFE3-4F30-AF36-E1215A7E3EC4}']
  end;

  { THeaderSectionImpl }
  (*
    base implementation for head section
  *)
  THeaderSectionImpl = class(TSectionWriterImpl,IHeaderSection)
  strict private
    FUnitName,
    FComment: String;
    function GetCmt: String;
    function GetUnit: String;
    procedure SetCmt(Const AValue: String);
    procedure SetUnit(Const AValue: String);
  strict protected
    function DoWrite(const AObjects: TJ2PasObjects;
      out Content, Error: String): Boolean; override;
  public
    property UnitName : String read GetUnit write SetUnit;
    property HeaderComment : String read GetCmt write SetCmt;
  end;

  { TIntfUsesSectionImpl }

  TIntfUsesSectionImpl = class(TSectionWriterImpl,IIntfUsesSection)
  strict private
    FUnits: TStringList;
    function GetUnits: TStringList;
  strict protected
    function DoWrite(const AObjects: TJ2PasObjects;
      out Content, Error: String): Boolean; override;
  public
    property Units : TStringList read GetUnits;
    constructor Create(Const ASectionIdentifer:String); override;
    destructor Destroy; override;
  end;

  { TImplUsesSectionImpl }

  TImplUsesSectionImpl = class(TSectionWriterImpl,IImplUsesSection)
  strict private
    FUnits: TStringList;
    function GetUnits: TStringList;
  strict protected
    function DoWrite(const AObjects: TJ2PasObjects;
      out Content, Error: String): Boolean; override;
  public
    property Units : TStringList read GetUnits;
    constructor Create(Const ASectionIdentifer:String); override;
    destructor Destroy; override;
  end;

  { TTypeSectionImpl }

  TTypeSectionImpl = class(TSectionWriterImpl,ITypeSection)
  public
    const
      OBJECT_TEMPLATE =
        '%s = class(%s,%s)' + sLineBreak + //name , name, intf
        'public' + sLineBreak + //begin public prop consts
        '  const' + sLineBreak +
        '%s' + sLineBreak + //property consts
        'strict private' + sLineBreak +
        '%s' + sLineBreak + //private variable declarations
        '%s' + sLineBreak + //private property methods
        'strict protected' + sLineBreak +
        '%s' + sLineBreak + //protected methods
        'public' + sLineBreak +
        '%s' + sLineBreak + //public properties
        '%s' + sLineBreak + //public methods
        'end;'; //object is finished
      INTF_TEMPLATE =
        '%s = interface' + sLineBreak +
        '%s' + sLineBreak + //interface guid
        '  //property methods' + sLineBreak +
        '%s' + sLineBreak +
        '  //properties' + sLineBreak +
        '%s' + sLineBreak +
        '  //methods' + sLineBreak +
        '%s' + sLineBreak +
        'end;'; //interface is finished
  strict private
    FIncImpl,
    FIncIntf: Boolean;
    function GetIncImpl: Boolean;
    function GetIncIntf: Boolean;
    procedure SetIncImpl(Const AValue: Boolean);
    procedure SetIncIntf(Const AValue: Boolean);
  strict protected
    function DoWrite(const AObjects: TJ2PasObjects;
      out Content, Error: String): Boolean; override;
  public
    property IncludeInterface : Boolean read GetIncIntf write SetIncIntf;
    property IncludeImplementation : Boolean read GetIncImpl write SetIncImpl;
    constructor Create(Const ASectionIdentifer:String); override;
  end;

  { TImplSectionImpl }

  TImplSectionImpl = class(TSectionWriterImpl,IImplSection)
  strict protected
    const
      FUNC_TEMPLATE =
        'function %s(%s):%s;' + sLineBreak + //name | args | return
        '%s' + sLineBreak + //const | type | var | nested procs
        'begin' + sLineBreak +
        '%s' + sLineBreak + // method body
        'end;';
      PROC_TEMPLATE =
        'procedure %s(%s);' + sLineBreak + //name | args
        'begin' + sLineBreak +
        '%s' + sLineBreak + // method body
        'end;';
  strict private
  strict protected
    function DoWrite(const AObjects: TJ2PasObjects;
      out Content, Error: String): Boolean; override;
  public
  end;

  IPascalProducer = interface
    ['{2154A13D-477C-43D6-88A2-4C667A32FE04}']
  end;

  { TPascalProducerImpl }

  TPascalProducerImpl = class(TStandardProducerImpl,IPascalProducer)
  strict private
    FHeader: IHeaderSection;
    FUses: IIntfUsesSection;
    FType: ITypeSection;
    FImplUses: IImplUsesSection;
    FImpl: IImplSection;
  strict protected
    function PrepareSource(const AUnitName: String;
      const AObjects: TJ2PasObjects; out Source, Error: String): Boolean;
      override;
    procedure DoAddWriters(const AWriters: TSectionWriters); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  strutils;

function GetCollectionName(Const AType:String;Const AIsBasicType:Boolean=True;
  Const AIncludeDefinition:Boolean=False):String;
var
  LType:String;
begin
  Result:='';
  if AIsBasicType then
    Result:='T' + AType + 'List' + IfThen(
      AIncludeDefinition,
      ' = TFPGList<' + AType + '>;',
      ''
    )
  else
  begin
    //we deal with interface collections so strip i off of type
    LType:=IfThen(LowerCase(AType[1]) = 'i', Copy(AType,2,AType.Length - 1),AType);
    Result:='T' +
      //if we end in 'y' strip this and pluralize with ies
      IfThen(
        LowerCase(LType[LType.Length]) = 'y',
        Copy(LType,1,LType.Length - 1) + 'ie',
        LType
      ) + 's' +
      IfThen(
        AIncludeDefinition,
        ' = TFPGInterfacedObjectList<I' + LType + '>;',
        ''
      );
  end;
end;

function FormatIntfName(Const AName:String):String;
begin
  //zero length names exit
  if AName.Length < 1 then
    Exit('INoName');

  if AName.Length = 1 then
    Exit('I' + AName);

  //if T is the beginning (ie. TWidget) replace with I (IWidget)
  if AName[1] = 'T' then
    Result:='I' + Copy(AName,2,AName.Length - 1)
  //otherwise prefix with I
  else
    Result:='I' + AName;
end;

function FormatObjName(Const AName:String):String;
begin
  Result:=AName + 'Impl';
end;

function FormatObjNameToParam(Const AName:String):String;
begin
  //this method assumes at least two chars and will just
  //strip the first character out (TCar -> Car)
  Result:=Copy(AName,2,AName.Length - 1);
end;

//helper for translating jtypes
function BasicJTypeToType(Const AJType:TJ2PasType):String;
begin
  Result:='';
  case AJType of
    jtBool: Exit('Boolean');
    jtString: Exit('String');
    jtInt: Exit('Integer');
    jtFloat: Exit('Single');
  end;
end;

function PropertyMethods(Const AObject:TJ2PasObject) : String;
var
  I:Integer;
  LTmp:TStringList;
begin
  Result:='';

  LTmp:=TStringList.Create;
  try
    //loop through properties and concatenate
    for I := 0 to Pred(AObject.Properties.Count) do
    begin
      //todo - depending on jtype handle this a little differently
      case AObject.Properties[I].JType of
        jtBool, jtString, jtInt, jtFloat:
          begin
            //add getter and setter
            LTmp.Add('function Get' + AObject.Properties[I].Name +
              ':' + BasicJTypeToType(AObject.Properties[I].JType) + ';'
            );
            LTmp.Add('procedure Set' + AObject.Properties[I].Name +
              '(Const A' + AObject.Properties[I].Name + ':' +
              BasicJTypeToType(AObject.Properties[I].JType) + ');'
            );
          end;
        jtArray:
          begin
            //see if we have basic type or object type
            case TJ2PasArrayProp(AObject.Properties[I]).ArrayType of
              jtBool, jtString, jtInt, jtFloat:
                begin
                  //only getter in this case since we are using an object
                  LTmp.Add('function Get' + AObject.Properties[I].Name +
                    ':' + GetCollectionName(AObject.Properties[I].Name,True,False) + ';'
                  );
                end;
              jtObject:
                begin
                  //allow both getter and setter since we are now interfaced collections
                  LTmp.Add('function Get' + AObject.Properties[I].Name +
                    ':' + GetCollectionName(
                      FormatIntfName(TJ2PasArrayObject(AObject.Properties[I]).ObjectName),
                      False,
                      False
                    ) + ';'
                  );
                  (* right now, just don't allow set of these, but we could clear -> assign.. just might be confusing for owernship
                  LTmp.Add('procedure Set' + AObject.Properties[I].Name +
                    '(Const A' + AObject.Properties[I].Name + ':' +
                    GetCollectionName(
                      FormatIntfName(TJ2PasArrayObject(AObject.Properties[I]).ObjectName),
                      False,
                      False
                    ) + ');'
                  );
                  *)
                end;
            end;
          end;
        jtObject:
          begin
            //interfaced so allow both getter and setter on objects
            LTmp.Add('function Get' + FormatObjNameToParam(FormatIntfName(AObject.Properties[I].Name)) +
              ':' + FormatIntfName(AObject.Properties[I].Name) + ';'
            );
            LTmp.Add('procedure Set' + FormatObjNameToParam(FormatIntfName(AObject.Properties[I].Name)) +
              '(Const A' + FormatObjNameToParam(FormatIntfName(AObject.Properties[I].Name)) + ':' +
              FormatIntfName(AObject.Properties[I].Name) + ');'
            );
          end;
      end;
    end;
    Result:=LTmp.Text;
  finally
    LTmp.Free;
  end;
end;

function Properties(Const AObject:TJ2PasObject) : String;
var
  I:Integer;
  LName:String;

begin
  Result:='';
  for I := 0 to Pred(AObject.Properties.Count) do
  begin
    //new property
    if I <> 0 then
      Result:=Result + sLineBreak;

    //get the property name
    LName:=AObject.Properties[I].Name;

    //depending on the jtype format the property differently
    case AObject.Properties[I].JType of
      jtBool, jtString, jtInt, jtFloat:
        begin
          Result:=
            Result + 'property ' +
            LName + ' : ' +
            BasicJTypeToType(AObject.Properties[I].JType) + ' read Get' +
            LName + ' write Set' + LName +';';
        end;
      jtArray:
        begin
          if TJ2PasArrayProp(AObject.Properties[I]).ArrayType <> jtObject then
            Result:=
              Result + 'property ' +
              LName + ' : ' +
              GetCollectionName(LName,True,False) + ' read Get' +
              LName + ' write Set' + LName +';'
          else
            Result:=
              Result + 'property ' +
              LName + ' : ' +
              GetCollectionName(
                FormatIntfName(TJ2PasArrayObject(AObject.Properties[I]).ObjectName),
                False,
                False
              ) + ' read Get' +
              LName (*+ ' write Set' +
              LName *)+';';
        end;
      jtObject:
        begin
          //format as an interface (which guarantees at least 2 chars)
          //then remove the first one (ie. ICar -> Car)
          LName:=FormatObjNameToParam(FormatIntfName(LName));
          Result:=
            Result + 'property ' +
            LName + ' : ' +
            FormatIntfName(AObject.Properties[I].Name) + ' read Get' +
            LName + ' write Set' +
            LName +';'
        end;
    end;
  end;
end;

function PrivateVars(Const AObject:TJ2PasObject):String;
const
  START = Length('property ') + 1;
var
  I:Integer;
  LTmp:TStringList;
begin
  Result:='';
  LTmp:=TStringList.Create;
  try
    //already wrote the public property code so going to cheat and parse
    LTmp.Text:=Properties(AObject);
    for I := 0 to Pred(LTmp.Count) do
    begin
      //use result as a buffer to parse the type
      Result:=Copy(LTmp[I],START,LTmp[I].Length - START);
      Result:=Copy(Result,Succ(Result.IndexOf(': ')),Result.Length);
      Result:=Copy(Result,1,Result.IndexOf(' read'));

      //copy the name of the variable and prefix 'F' and append the type
      LTmp[I]:='F' + Copy(
        LTmp[I],
        START,
        LTmp[I].IndexOf(': ') - START
      ) + Result +  ';';
    end;
    Result:=LTmp.Text;
  finally
    LTmp.Free;
  end;
end;

function PropertyConst(Const AProperty:TJ2PasProp):String;
const
  LOW_CHAR = Ord('A');
  HIGH_CHAR = Ord('Z');
var
  I:Integer;
  LName:String;
begin
  Result:='';
  case AProperty.JType of
    jtBool, jtString, jtInt, jtFloat: LName:=AProperty.Name;
    jtArray:
      begin
        //objects need additional formatting
        if TJ2PasArrayProp(AProperty).ArrayType = jtObject then
        begin
          LName:=FormatObjNameToParam(FormatIntfName(TJ2PasArrayObject(AProperty).ObjectName));
        end
        else
          LName:=AProperty.Name;
      end;
    jtObject:
      begin
        LName:=FormatObjNameToParam(FormatIntfName(AProperty.Name));
      end;
  end;

  //format the name where capital letters mark a break in the word
  //so that TestProperty would get translated to TEST_PROPERTY
  //and also prefix this with PROP, so PROP_TEST_PROPERTY
  for I := 1 to LName.Length do
  begin
    if (I <> 1) and (Ord(LName[I]) in [LOW_CHAR..HIGH_CHAR]) then
      Result:=Result + '_' + LName[I]
    else
      Result:=Result + UpperCase(LName[I]);
  end;
  Result:='PROP_' + Result;
end;

function GetMetadata(Const AObject:TJ2PasObject):TMetadatas;
var
  I:Integer;
  LProps:TStringList;
  LProp:TJ2PasProp;
begin
  (*
    note to others:
    this method may move... or go away entirely just as all the other
    non member methods. these were mainly a way to "get started" with this
    library to try some different ideas out
  *)

  //init the result to the length of the properties
  SetLength(Result,AObject.Properties.Count);

  LProps:=TStringList.Create;
  try
    //fragile... but done after a lot of quick text parse hackery was
    //written, so this may get cleaned up later. anyway, this should
    //always guarantee proper ordering of properties
    LProps.NameValueSeparator:=':';
    LProps.Text:=PrivateVars(AObject).Replace(';','').Replace(' ','');

    //iterate properties and fill out the result metadata records
    for I := 0 to Pred(AObject.Properties.Count) do
    begin
      LProp:=AObject.Properties[I];

      //assign "common" meta data values
      Result[I].ConstantName:=PropertyConst(LProp);
      Result[I].ConstantValue:=LProp.OriginalName;
      Result[I].FieldName:=LProps.Names[I];
      Result[I].FieldType:=LProps.ValueFromIndex[I];

      //split out logic for simple/object/array types when dealing
      //with the collection name and metatype (corresponds to FieldType string)
      if LProp.JType in [jtBool,jtInt,jtFloat,jtString] then
      begin
        Result[I].CollectionName:=GetCollectionName(LProp.Name);
        Result[I].MetaType:=mtSimple;
      end
      else if LProp.JType = jtObject then
      begin
        Result[I].CollectionName:=GetCollectionName(FormatIntfName(LProp.Name),False);
        Result[I].MetaType:=mtObject;
      end
      else if LProp.JType = jtArray then
      begin
        //collection of simple types
        if TJ2PasArrayProp(LProp).ArrayType in [jtBool,jtInt,jtFloat,jtString] then
        begin
          Result[I].CollectionName:=GetCollectionName(LProp.Name);
          Result[I].FieldType:=BasicJTypeToType(LProp.JType);
          Result[I].MetaType:=mtCollection;
        end
        //collection of object
        else
        begin
          //in this case the fieldtype needs to be the type of object in the collection
          Result[I].CollectionName:=LProps.ValueFromIndex[I];
          Result[I].FieldType:=FormatIntfName(TJ2PasArrayObject(LProp).ObjectName);
          Result[I].MetaType:=mtObjectCollection;
        end;
      end;
    end;
  finally
    LProps.Free;
  end;
end;

{ TImplSectionImpl }

function TImplSectionImpl.DoWrite(const AObjects: TJ2PasObjects; out Content,
  Error: String): Boolean;
const
  CATCH_TEMPLATE =
    'try' + sLineBreak +
    '%s' + sLineBreak +
    'except on E:Exception do' + sLineBreak +
    '%s' + sLineBreak +
    'end;';
  TRY_TEMPLATE =
    'try' + sLineBreak +
    '%s' + sLineBreak +
    'finally' + sLineBreak +
    '%s' + sLineBreak +
    'end;';
var
  I:Integer;
  LTmp:TStringList;

  (*
    todo - could probably make these virtual protected methods so children
    would have an easier time extended or changing desired formatting
    or json libraries, but right now just trying things out
  *)

  function GetFromJSON(Const AObject:TJ2PasObject):String;
  var
    I:Integer;
    LData:TMetadatas;
    LTmp,
    LLocals:TStringList;
    LLocal:String;
  begin
    //fetch metadata for the object
    LData:=GetMetadata(AObject);
    LLocals:=TStringList.Create;
    try
      LTmp:=TStringList.Create;
      try
        //add local json handling vars
        LLocals.Add('LData : TJSONData;');
        LLocals.Add('LObj : TJSONObject;');

        //handle initial json parsing
        LTmp.Add('//attempt to parse JSON object');
        LTmp.Add('LData:=GetJSON(AJSON);');
        LTmp.Add('if not Assigned(LData) or (LData.JsonType <> jtObject) then');
        LTmp.Add(
          Indent(
            'raise Exception.Create(' +
            QuotedStr('DoFromJSON::invalid JSON object in ')
            + ' + Self.Classname);',
            1
          )
        );

        //cast to object
        LTmp.Add(sLineBreak + '//cast TJSONData -> TJSONObject');
        LTmp.Add('LObj:=TJSONObject(LData);');

        //iterate our metadata to fill out method content
        for I := 0 to High(LData) do
        begin
          //depending on the type of the property (basic/object/collection)
          //we will need to handle the assignment differently. additionally,
          //we may want to handle the concept of "required" or "not-required"
          //properties inside the JSON, or defaults when non-existant
          case LData[I].MetaType of
            mtSimple:
              begin
                LTmp.Add(LData[I].FieldName + ':=LObj.Get(' + LData[I].ConstantName + ');');
              end;
            mtObject:
              begin
                //for objects we need to load from json
                LTmp.Add(LData[I].FieldName + '.FromJSON(' + 'LObj.Get(' + LData[I].ConstantName + ',''{}''));');
              end;
            mtCollection:
              begin
                //simple type collection just call add for as manybjects in array
                if LLocals.Text.IndexOf('LArray') < 0 then
                  LLocals.Add('LArray : TJSONArray;');

                //will need a counter var
                if LLocals.Text.IndexOf('I : Integer') < 0 then
                  LLocals.Add('I : Integer;');

                LTmp.Add('');
                LTmp.Add('//assign our array to local variable');
                LTmp.Add('LArray:=LObj.Arrays[' +LData[I].ConstantName + '];');
                LTmp.Add('');
                LTmp.Add('//iterate and add basic type to collection');
                LTmp.Add('for I:=0 to Pred(LArray.Count) do');
                LTmp.Add(Indent(LData[I].FieldName + '.Add(LArray.Items[I].Value);',1));
              end;
            mtObjectCollection:
              begin
                //substring is 0 index based
                LLocal:='L' + LData[I].FieldType.Substring(1);

                //check existence of local var for intf, if not then add
                if LLocals.IndexOf(LLocal) < 0 then
                  LLocals.Add(LLocal + ' : ' + LData[I].FieldType);

                //build out code to assign to a local variable, and add to the
                //private object collection
                LTmp.Add('');
                LTmp.Add('//assign our array to local variable');
                LTmp.Add('LArray:=LObj.Arrays[' +LData[I].ConstantName + '];');
                LTmp.Add('');
                LTmp.Add('//iterate and add object type to collection');
                LTmp.Add('for I:=0 to Pred(LArray.Count) do');
                LTmp.Add('begin');
                LTmp.Add(Indent('//first get a local reference',1));
                LTmp.Add(Indent(LLocal + ':=' + FormatObjName('T' + LData[I].FieldType.SubString(1)) + '.Create;',1));
                LTmp.Add('');
                LTmp.Add(Indent('//attempt to deserialize with json at index',1));
                LTmp.Add(Indent('if not ' + LData[I].FieldName + '.FromJSON(LArray.Items[I].Value,Error)) then',1));
                LTmp.Add(Indent('Exit;',2));
                LTmp.Add('');
                LTmp.Add(Indent('//add to collection',1));
                LTmp.Add(Indent(LData[I].FieldName + '.Add(' + LLocal + ');',1));
                LTmp.Add('');
                LTmp.Add(Indent('//nil local reference before iterating again',1));
                LTmp.Add(Indent(LLocal + ':=nil;',1));
                LTmp.Add('end;');
              end;
          end;
        end;

        //use result as buffer for the method content
        Result:=LTmp.Text;
      finally
        LTmp.Free;
      end;

      //format the result
      Result:=Format(
        FUNC_TEMPLATE,
        [
          FormatObjName(AObject.Name) + '.DoFromJSON',
          'Const AJSON:String;Out Error:String',
          'Boolean;virtual',
          'var' + sLineBreak + Indent(LLocals.Text,1),
          Indent(
            '//init result' + sLineBreak + 'Result:=False;' + sLineBreak +
            Format(
              CATCH_TEMPLATE,
              [
                Indent(Result,1) +
                Indent(sLineBreak + '//success' + sLineBreak + 'Result:=True;',1),
                Indent('Error:=E.Message;',1)
              ]
            )
            ,
            1
          )
        ]
      );
    finally
      LLocals.Free;
    end;

  end;

  function GetToJSON(Const AObject:TJ2PasObject):String;
  var
    LData:TMetaDatas;
    I:Integer;
    LLocals,
    LTmp:TStringList;
  begin
    Result:='';
    LData:=GetMetadata(AObject);
    LLocals:=TStringList.Create;
    try
      //will use local json object to build result
      LLocals.Add('LObj : TJSONObject;');

      LTmp:=TStringList.Create;
      try
        //iterate metadata and build method body
        for I := 0 to High(LData) do
        begin
          case LData[I].MetaType of
            mtSimple:
              begin
                //simple types lowest level Add should be fine
                LTmp.Add('LObj.Add(' + LData[I].ConstantName + ','
                  + LData[I].FieldName + ');'
                );
              end;
            mtObject:
              begin
                //object types create an object and add
                LTmp.Add('LObj.Add(' + LData[I].ConstantName + ','
                  + 'GetJSON(' + LData[I].FieldName + '.ToJSON));'
                );
              end;
            mtCollection:
              begin
                //collection types we need to use local array
                if LLocals.Text.IndexOf('LArray : TJSONArray;') < 0 then
                  LLocals.Add('LArray : TJSONArray;');

                if LLocals.Text.IndexOf('I : Integer;') < 0 then
                  LLocals.Add('I : Integer;');

                //add to array in loop
                LTmp.Add('');
                LTmp.Add('//assign new array to local var');
                LTmp.Add('LArray:=TJSONArray.Create');
                LTmp.Add('for I:=0 to Pred(' + LData[I].FieldName + '.Count) do');
                LTmp.Add('begin');
                LTmp.Add(Indent('//add basic type for private var to local array',1));
                LTmp.Add(Indent('LArray.Add(' + LData[I].FieldName + '[I]);',1));
                LTmp.Add('end;');
                LTmp.Add('');
                LTmp.Add('//add the array to result object (will free)');
                LTmp.Add('LObj.Add(' + LData[I].ConstantName + ',LArray)');
              end;
            mtObjectCollection:
              begin
                //collection types we need to use local array
                if LLocals.Text.IndexOf('LArray : TJSONArray;') < 0 then
                  LLocals.Add('LArray : TJSONArray;');

                if LLocals.Text.IndexOf('I : Integer;') < 0 then
                  LLocals.Add('I : Integer;');

                //add to array in loop
                LTmp.Add('');
                LTmp.Add('//assign new array to local var');
                LTmp.Add('LArray:=TJSONArray.Create');
                LTmp.Add('for I:=0 to Pred(' + LData[I].FieldName + '.Count) do');
                LTmp.Add('begin');
                LTmp.Add(Indent('//add object type for private var to local array',1));
                LTmp.Add(Indent('LArray.Add(GetJSON(' + LData[I].FieldName + '[I].ToJSON));',1));
                LTmp.Add('end;');
                LTmp.Add('');
                LTmp.Add('//add the array to result object (will free)');
                LTmp.Add('LObj.Add(' + LData[I].ConstantName + ',LArray)');
              end;
          end;
        end;

        //result as buffer for method body
        Result:='LObj:=TJSONObject.Create;' + sLineBreak +
          Format(
            TRY_TEMPLATE,
            [
              Indent(
                LTmp.Text + sLineBreak + '//set output' + sLineBreak +
                  'JSON:=LObj.ToJSON;',
                1
              ),
              Indent('LObj.Free',1)
            ]
        );
      finally
        LTmp.Free;
      end;

      //format the result
      Result:=Format(
        FUNC_TEMPLATE,
        [
          FormatObjName(AObject.Name) + '.DoToJSON',
          'Out JSON,Error:String',
          'Boolean;virtual',
          'var' + sLineBreak + Indent(LLocals.Text,1),
          Indent(
            '//init result' + sLineBreak + 'Result:=False;' + sLineBreak +
            Format(
              CATCH_TEMPLATE,
              [
                Indent(Result,1) +
                Indent(sLineBreak + '//success' + sLineBreak + 'Result:=True;',1),
                Indent('Error:=E.Message;',1)
              ]
            )
            ,
            1
          )
        ]
      );
    finally
      LLocals.Free;
    end;
  end;

  function GetPropMethods(Const AObject:TJ2PasObject):String;
  begin
    //todo - hack together something
    Result:='';
  end;

  function GetConstructor(Const AObject:TJ2PasObject):String;
  begin
    //todo - defaults for private vars/objects
    Result:='';
  end;

  function GetDestructor(Const AObject:TJ2PasObject):String;
  begin
    //todo - nil out interfaces and free objects
    Result:='';
  end;

begin
  Result:=False;
  Content:='';
  try
    //for each object in the unit make calls to localized helper
    //methods to generate the implementation code
    LTmp:=TStringList.Create;
    try
      for I := 0 to Pred(AObjects.Count) do
      begin
        LTmp.Add(GetPropMethods(AObjects[I]));
        LTmp.Add(sLineBreak + GetFromJSON(AObjects[I]));
        LTmp.Add(sLineBreak + GetToJSON(AObjects[I]));
        LTmp.Add(sLineBreak + GetConstructor(AObjects[I]));
        LTmp.Add(sLineBreak + GetDestructor(AObjects[I]));

        //success
        Result:=True;
        //Error:='not implemented';//deleteme
      end;

      //update content with tmp
      Content:=LTmp.Text;
    finally
      LTmp.Free;
    end;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

{ TPascalProducerImpl }

function TPascalProducerImpl.PrepareSource(const AUnitName: String;
  const AObjects: TJ2PasObjects; out Source, Error: String): Boolean;
begin
  //configure writers
  FHeader.UnitName:=AUnitName;
  //FType.InclIntf:=FIncludeIntf;//todo - make this a property
  Result:=inherited PrepareSource(AUnitName, AObjects, Source, Error);
end;

procedure TPascalProducerImpl.DoAddWriters(const AWriters: TSectionWriters);
begin
  AWriters.Add(FHeader);
  AWriters.Add(FUses);
  AWriters.Add(FType);
  AWriters.Add(FImplUses);
  AWriters.Add(FImpl);
end;

constructor TPascalProducerImpl.Create;
begin
  inherited Create;
  FHeader:=THeaderSectionImpl.Create('header');

  //initialize units
  FUses:=TIntfUsesSectionImpl.Create('interface_uses');
  FUses.IndentLevel:=1;
  FUses.Units.Add('fgl');

  //initialize impl units
  FImplUses:=TImplUsesSectionImpl.Create('implementation_uses');
  FImplUses.IndentLevel:=1;
  FImplUses.Units.Add('fpjson');
  FImplUses.Units.Add('jsonparser');

  //initialize type
  FType:=TTypeSectionImpl.Create('type');
  FType.IndentLevel:=1;

  //initialize impl
  FImpl:=TImplSectionImpl.Create('implementation');
  FImpl.IndentLevel:=0;
end;

destructor TPascalProducerImpl.Destroy;
begin
  FHeader:=nil;
  FUses:=nil;
  FType:=nil;
  FImplUses:=nil;
  inherited Destroy;
end;

{ TTypeSectionImpl }

function TTypeSectionImpl.GetIncImpl: Boolean;
begin
  Result:=FIncImpl;
end;

function TTypeSectionImpl.GetIncIntf: Boolean;
begin
  Result:=FIncIntf;
end;

procedure TTypeSectionImpl.SetIncImpl(const AValue: Boolean);
begin
  FIncImpl:=AValue;
end;

procedure TTypeSectionImpl.SetIncIntf(const AValue: Boolean);
begin
  FIncIntf:=AValue;
end;

function TTypeSectionImpl.DoWrite(const AObjects: TJ2PasObjects; out Content,
  Error: String): Boolean;
var
  I:Integer;
  LTmpStr:String;
  LTmp,
  LTmpTypes:TStringList;
  LObj:TJ2PasObject;
  LProp:TJ2PasProp;
  LArray:TJ2PasArrayProp;

  //return the const declarations and the original name
  function PropConsts(Const AObject:TJ2PasObject):String;
  var
    I:Integer;
    LTmp:TStringList;
  begin
    Result:='';
    LTmp:=TStringList.Create;
    try
      for I := 0 to Pred(AObject.Properties.Count) do
        LTmp.Add(PropertyConst(AObject.Properties[I]) +
          ' = ' + QuotedStr(AObject.Properties[I].OriginalName) + ';'
        );
      Result:=LTmp.Text;
    finally
      LTmp.Free;
    end;
  end;

  //return the protected method signatures
  function ProtectedMethods : String;
  begin
    Result:=
      'function DoToJSON:String;virtual;' + sLineBreak +
      'function FromJSON:String;virtual;';
  end;

  //return the public method signatures
  function MethodSignatures(Const AIntf:Boolean=False) : String;
  begin
    Result:=
      'function ToJSON:String;' + sLineBreak +
      'function FromJSON:String;' + sLineBreak +
      //if requesting for interface then don't include these
      IfThen(
        AIntf,'',
        'constructor Create;virtual;' + sLineBreak +
        'destructor Destroy;override;'
      );
  end;

begin
  Result:=False;
  try
    //start with the type keyword followed by two line breaks
    Content:='type' + DupeString(sLineBreak,2);

    //used for easy appending
    LTmp:=TStringList.Create;
    try
      //depending on what include (intf/impl)  write the interface
      //and/or implementation definitions to content
      if FIncIntf then
      begin
        LTmpTypes:=TStringList.Create;
        try
          //add all of the collection types above the interface
          for LObj in AObjects do
          begin
            for LProp in LObj.Properties do
            begin
              if LProp.JType <> jtArray then
                Continue;

              //cast as array
              LArray:=TJ2PasArrayProp(LProp);

              //object collections handled different than standard
              if LArray.ArrayType = jtObject then
              begin
                LTmpStr:='//forward' + sLineBreak +
                  FormatIntfName(TJ2PasArrayObject(LArray).ObjectName) +
                  ' = interface;';

                //add forward declarations
                if LTmpTypes.IndexOf(LTmpStr) < 0 then
                  LTmpTypes.Add(LTmpStr);

                LTmpStr:=GetCollectionName(FormatIntfName(TJ2PasArrayObject(LArray).ObjectName),False,True);
                if LTmpTypes.IndexOf(LTmpStr) < 0 then
                  LTmpTypes.Add(LTmpStr);
              end
              else
              begin
                //this is ugly code... sorry will clean up later... maybe
                LTmpStr:=GetCollectionName(LArray.Name,True,True);
                LTmpStr:=LTmpStr.Replace('<' + LArray.Name + '>','<' + BasicJTypeToType(LArray.ArrayType) + '>');
                if LTmpTypes.IndexOf(LTmpStr) < 0 then
                  LTmpTypes.Add(LTmpStr);
              end;
            end;
          end;

          //lastly add to tmp
          LTmp.Add(LTmpTypes.Text);
        finally
          LTmpTypes.Free;
        end;
        //for each of the objects, fill out the interface template
        for I := 0 to Pred(AObjects.Count) do
        begin
          LTmp.Add(
            Format(
              INTF_TEMPLATE,
              [
                FormatIntfName(AObjects[I].Name),
                Indent('[' + TGuid.NewGuid.ToString(False) + ']',1),
                Indent(PropertyMethods(AObjects[I]),1),
                Indent(Properties(AObjects[I]),1),
                Indent(MethodSignatures(True),1)
              ]
            ) + sLineBreak
          );
        end;
      end;

      if FIncImpl then
      begin
        //for each object fill out the object template
        for I := 0 to Pred(AObjects.Count) do
        begin
          LTmp.Add(
            Format(
              OBJECT_TEMPLATE,
              [
                FormatObjName(AObjects[I].Name),
                'TInterfacedObject', //todo - perhaps make this more dynamic, property?
                FormatIntfName(AObjects[I].Name),
                Indent(PropConsts(AObjects[I]),2),
                Indent(PrivateVars(AObjects[I]),1),
                Indent(PropertyMethods(AObjects[I]),1),
                Indent(ProtectedMethods,1),
                Indent(Properties(AObjects[I]),1),
                Indent(MethodSignatures(False),1)
              ]
            ) + sLineBreak
          );
        end;
      end;

      //append to content
      Content:=Content + LTmp.Text;

      //success
      Result:=True;
    finally
      LTmp.Free;
    end;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TTypeSectionImpl.Create(const ASectionIdentifer: String);
begin
  inherited Create(ASectionIdentifer);
  FIncImpl:=True;
  FIncIntf:=True;
  IndentIgnoreLines.Add(0);
end;

{ TImplUsesSectionImpl }

function TImplUsesSectionImpl.GetUnits: TStringList;
begin
  Result:=FUnits;
end;

function TImplUsesSectionImpl.DoWrite(const AObjects: TJ2PasObjects; out
  Content, Error: String): Boolean;
begin
  Result:=False;
  try
    Content:='implementation' + sLineBreak;

    //if we have any units then append to the end
    if FUnits.Count > 0 then
      Content:=Content + 'uses' + sLineBreak + FUnits.CommaText + ';';
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TImplUsesSectionImpl.Create(Const ASectionIdentifer:String);
begin
  inherited Create(ASectionIdentifer);
  FUnits:=TStringList.Create;

  //we ignore the 'implementation' line for indents
  IndentIgnoreLines.Add(0);

  //ignore the 'uses'
  IndentIgnoreLines.Add(1);
end;

destructor TImplUsesSectionImpl.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

{ TIntfUsesSectionImpl }

function TIntfUsesSectionImpl.GetUnits: TStringList;
begin
  Result:=FUnits;
end;

function TIntfUsesSectionImpl.DoWrite(const AObjects: TJ2PasObjects; out
  Content, Error: String): Boolean;
begin
  Result:=False;
  try
    Content:='interface' + sLineBreak;

    //if we have any units then append to the end
    if FUnits.Count > 0 then
      Content:=Content + 'uses' + sLineBreak + FUnits.CommaText + ';';
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TIntfUsesSectionImpl.Create(Const ASectionIdentifer:String);
begin
  inherited Create(ASectionIdentifer);
  FUnits:=TStringList.Create;

  //we ignore the 'interface' line for indents
  IndentIgnoreLines.Add(0);

  //ignore 'uses'
  IndentIgnoreLines.Add(1);
end;

destructor TIntfUsesSectionImpl.Destroy;
begin
  FUnits.Free;
  inherited Destroy;
end;

{ THeaderSectionImpl }

function THeaderSectionImpl.GetCmt: String;
begin
  Result:=FComment;
end;

function THeaderSectionImpl.GetUnit: String;
begin
  Result:=FUnitName;
end;

procedure THeaderSectionImpl.SetCmt(const AValue: String);
begin
  FComment:=AValue;
end;

procedure THeaderSectionImpl.SetUnit(const AValue: String);
begin
  FUnitName:=AValue;
end;

function THeaderSectionImpl.DoWrite(const AObjects: TJ2PasObjects; out Content,
  Error: String): Boolean;
begin
  Result:=False;
  Content:='unit ' + FUnitName + ';' + sLineBreak +
    sLineBreak + '{$mode delphi}' + sLineBreak;

  //add the comment if it's not empty
  if not FComment.IsEmpty then
    Content:=FComment + sLineBreak + Content;
  Result:=True;
end;

end.

