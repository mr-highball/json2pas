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
unit json2pas.producer.std;

{$mode delphi}

interface

uses
  Classes, SysUtils, json2pas.producer, json2pas, fgl;

type

  { IStandardProducer }
  (*
    base interface for standard unit producers
  *)
  IStandardProducer = interface(IUnitProducer)
    ['{79735CE1-B7CA-4887-9EE1-95355DF201F8}']
  end;

  TIgnoreLines = TFPGList<Integer>;

  { ISectionWriter }
  (*
    base interface for source code section writers
  *)
  ISectionWriter = interface
    ['{BB804503-CAF9-4688-82E9-0937BB968C14}']
    //property methods
    function GetID: String;
    function GetIgnoreLines: TIgnoreLines;
    function GetIndention: String;
    function GetIndentLevel: Integer;
    procedure SetID(Const AValue: String);
    procedure SetIndention(Const AValue: String);
    procedure SetIndentLevel(Const AValue: Integer);

    //properties
    property SectionIdentifier : String read GetID write SetID;

    (*
      a multiplier used in conjunction with the indention string
    *)
    property IndentLevel : Integer read GetIndentLevel write SetIndentLevel;

    (*
      the string used as indention
    *)
    property Indention : String read GetIndention write SetIndention;

    (*
      these line numbers (0 index) will not get indented
    *)
    property IndentIgnoreLines : TIgnoreLines read GetIgnoreLines;

    //methods
    function Write(Const AObjects:TJ2PasObjects;
      Out Content,Error:String):Boolean;
  end;

  //section writer collections
  TSectionWriters = TFPGInterfacedObjectList<ISectionWriter>;
  TSectionWriterMap = TFPGMapInterfacedObjectData<String,ISectionWriter>;

  { TSectionWriterImpl }

  TSectionWriterImpl = class(TInterfacedObject,ISectionWriter)
  strict private
    FID,
    FIndention: String;
    FIndentLevel: Integer;
    FIgnoreLines: TIgnoreLines;
    function GetID: String;
    function GetIgnoreLines: TIgnoreLines;
    procedure SetID(Const AValue: String);
    procedure SetIndention(Const AValue: String);
    procedure SetIndentLevel(Const AValue: Integer);
    function GetIndention: String;
    function GetIndentLevel: Integer;
  strict protected
    function DoWrite(Const AObjects:TJ2PasObjects;
      Out Content,Error:String):Boolean;virtual;abstract;
    procedure Indent(Const AInput:String;Out Output:String);
  public
    property IndentLevel : Integer read GetIndentLevel write SetIndentLevel;
    property Indention : String read GetIndention write SetIndention;
    property IndentIgnoreLines : TIgnoreLines read GetIgnoreLines;
    property SectionIdentifier : String read GetID write SetID;
    function Write(Const AObjects:TJ2PasObjects;
      Out Content,Error:String):Boolean;
    constructor Create;overload;
    constructor Create(Const ASectionIdentifer:String);virtual;overload;
    destructor Destroy; override;
  end;

  { TStandardProducerImpl }
  (*
    this is the standard unit producer tailored to well... my preferences,
    use it as a reference for implementing your own.
    the main functionality comes from the section writers which are responsible
    for breaking a source file into consecutive pieces and tackling the code generation
    (ie. interface section, implementation, etc...)
  *)
  TStandardProducerImpl = class(TUnitProducerImpl)
  strict private
    FWriters: TSectionWriters;
  strict protected
    (*
      overidden by children to add writers to this producer
    *)
    procedure DoAddWriters(Const AWriters:TSectionWriters);virtual;abstract;
    function PrepareSource(Const AUnitName:String;const AObjects: TJ2PasObjects; out Source,
      Error: String): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  strutils;

{ TSectionWriterImpl }

function TSectionWriterImpl.GetID: String;
begin
  Result:=FID;
end;

function TSectionWriterImpl.GetIgnoreLines: TIgnoreLines;
begin
  Result:=FIgnoreLines;
end;

procedure TSectionWriterImpl.SetID(const AValue: String);
begin
  FID:=AValue;
end;

procedure TSectionWriterImpl.SetIndention(const AValue: String);
begin
  FIndention:=AValue;
end;

procedure TSectionWriterImpl.SetIndentLevel(const AValue: Integer);
begin
  FIndentLevel:=AValue;
end;

function TSectionWriterImpl.GetIndention: String;
begin
  Result:=FIndention;
end;

function TSectionWriterImpl.GetIndentLevel: Integer;
begin
  Result:=FIndentLevel;
end;

procedure TSectionWriterImpl.Indent(const AInput: String; out Output: String);
var
  I:Integer;
  LTmp:TStringList;
begin
  //if we don't have an indention level or our indent character is empty exit
  if (FIndentLevel <= 0) or (FIndention.Length < 1) then
  begin
    Output:=AInput;
    Exit;
  end;

  //create a temp string list to iterate over and indent
  LTmp:=TStringList.Create;
  try
    LTmp.Text:=AInput;
    for I := 0 to Pred(LTmp.Count) do
    begin
      //if we are ignoring indent of a particular line index
      //the line doesn't need to change
      if FIgnoreLines.IndexOf(I) < 0 then
        LTmp[I]:=DupeString(FIndention,FIndentLevel) + LTmp[I];
    end;
  finally
    LTmp.Free;
  end;
end;

function TSectionWriterImpl.Write(const AObjects: TJ2PasObjects; out Content,
  Error: String): Boolean;
begin
  Result:=False;
  try
    //first generate the content
    if not DoWrite(AObjects,Content,Error) then
      Exit;

    //next, handle the indention of the content
    Indent(Content,Content);

    //success
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TSectionWriterImpl.Create;
begin
  Create('not set');
end;

constructor TSectionWriterImpl.Create(const ASectionIdentifer: String);
begin
  FIgnoreLines:=TIgnoreLines.Create;
  FID:=ASectionIdentifer;

  //two spaces is default indention
  FIndention:=#32#32;

  //default to not indent
  FIndentLevel:=0;
end;

destructor TSectionWriterImpl.Destroy;
begin
  FIgnoreLines.Free;
  inherited Destroy;
end;

{ TStandardProducerImpl }

function TStandardProducerImpl.PrepareSource(Const AUnitName:String;const AObjects: TJ2PasObjects; out
  Source, Error: String): Boolean;
var
  I:Integer;
  LContent:String;
begin
  Result:=False;
  Source:='';
  try
    //clear old writers
    FWriters.Clear;

    //add all the section writers
    DoAddWriters(FWriters);

    //for each writer, append to source
    for I := 0 to Pred(FWriters.Count) do
      if FWriters[I].Write(AObjects,LContent,Error) then
        Source:=Source + LContent
      else
        Exit;

    //success
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

constructor TStandardProducerImpl.Create;
begin
  inherited Create;
  FWriters:=TSectionWriters.Create;
end;

destructor TStandardProducerImpl.Destroy;
begin
  FWriters.Free;
  inherited Destroy;
end;

end.

