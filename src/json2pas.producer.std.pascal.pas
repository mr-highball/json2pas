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

implementation

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
begin
  Result:=False;
  try
    //todo - depending on what include (intf/impl)  write the interface
    //and/or implementation definitions to content

    if FIncIntf then
    begin
      //first pass write all of the interface definitions replacing the
      //first 'T' (if any) and prefixing 'I' to the object name
      //...
    end;

    if FIncImpl then
    begin
      //next write, the implementation definition, and append 'Impl'
      //to the end of the object name
      //...
    end;

    //success
    //Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TTypeSectionImpl.Create(const ASectionIdentifer: String);
begin
  inherited Create(ASectionIdentifer);
  FIncImpl:=True;
  FIncIntf:=True;
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
      Content:=Content + 'uses' + sLineBreak + FUnits.Text;
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TImplUsesSectionImpl.Create(Const ASectionIdentifer:String);
begin
  inherited Create(ASectionIdentifer);
  FUnits:=TStringList.Create;

  //we ignore the 'interface' line for indents
  IndentIgnoreLines.Add(0);

  //ignore line break after interface, and ignore 'uses'
  IndentIgnoreLines.Add(1);
  IndentIgnoreLines.Add(2);
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
      Content:=Content + 'uses' + sLineBreak + FUnits.Text;
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

  //ignore line break after interface, and ignore 'uses'
  IndentIgnoreLines.Add(1);
  IndentIgnoreLines.Add(2);
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

