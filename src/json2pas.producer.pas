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
unit json2pas.producer;

{$mode delphi}

interface

uses
  Classes, SysUtils, json2pas, fgl;

type

  { TUnit }
  (*
    represents a finalized unit
  *)
  TUnit = record
  private
    FFileName: String;
    FSource: String;
  public
    property Source:String read FSource write FSource;
    property Filename:String read FFileName write FFileName;
  end;

  (*
    multiple units
  *)
  TUnits = array of TUnit;

  { IUnitProducer }
  (*
    common interface for unit producers
  *)
  IUnitProducer = interface
    ['{BCB30CEF-E041-437A-A559-8BCB9923DF27}']
    //property methods
    function GetUnits: TUnits;
    procedure SetRoot(Const AValue: String);
    function GetRoot: String;

    //properties
    (*
      array of finalized units
    *)
    property Units : TUnits read GetUnits;

    (*
      the root output folder
    *)
    property RootFolder : String read GetRoot write SetRoot;

    //methods
    (*
      unit initialization
    *)
    function NewUnit(Const AUnitName:String;Out Error:String):Boolean;overload;
    function NewUnit(Const AUnitName:String;
      Const AObjects:TJ2PasObjects;Out Error:String):Boolean;overload;
    function NewUnit(Const AUnitName:String;
      Const AObject:TJ2PasObject;Out Error:String):Boolean;overload;

    (*
      unit modification
    *)
    function AddToUnit(Const AUnitName:String;
      Const AObject:TJ2PasObject;Out Error:String):Boolean;
    procedure Remove(Const AUnitName:String);

    (*
      finalize units
    *)
    function Finalize(Out Error:String):Boolean;
    function SaveToFiles(Out Error:String):Boolean;
  end;

  (*
    base implementation of a unit producer
  *)

  { TUnitProducerImpl }

  TUnitProducerImpl = class(TInterfacedObject,IUnitProducer)
  strict protected
    type
      (*
        maps objects to a unit name
      *)
      TUnitMap = TFPGMapObject<String,TJ2PasObjects>;
  strict private
    FFinalized: Boolean;
    FUnits: TUnits;
    FRoot: String;
    FMap: TUnitMap;
    function GetUnits: TUnits;
    procedure SetRoot(Const AValue: String);
    function GetRoot: String;
  strict protected
    (*
      children override this method to prepare the source for
      a given file, by being provided objects
    *)
    function PrepareSource(Const AObjects:TJ2PasObjects;
      Out Source,Error:String):Boolean;virtual;abstract;
  public
    property Units : TUnits read GetUnits;
    property RootFolder : String read GetRoot write SetRoot;
    function NewUnit(Const AUnitName:String;Out Error:String):Boolean;overload;
    function NewUnit(Const AUnitName:String;
      Const AObjects:TJ2PasObjects;Out Error:String):Boolean;overload;
    function NewUnit(Const AUnitName:String;
      Const AObject:TJ2PasObject;Out Error:String):Boolean;overload;
    function AddToUnit(Const AUnitName:String;
      Const AObject:TJ2PasObject;Out Error:String):Boolean;
    procedure Remove(Const AUnitName:String);
    function Finalize(Out Error:String):Boolean;
    function SaveToFiles(Out Error:String):Boolean;
    constructor Create;virtual;
    destructor Destroy; override;
  end;

implementation

{ TUnitProducerImpl }

function TUnitProducerImpl.GetUnits: TUnits;
begin
  Result:=FUnits;
end;

procedure TUnitProducerImpl.SetRoot(const AValue: String);
begin
  FRoot:=AValue;
end;

function TUnitProducerImpl.GetRoot: String;
begin
  Result:=FRoot;
end;

function TUnitProducerImpl.NewUnit(const AUnitName: String;
  out Error: String): Boolean;
begin
  Result:=False;
  try
    //check the index to make sure we don't already have one created
    //also this object list doesn't own the j2pas objects, up to caller to
    //figure that out (if using global objects this is not a problem)
    if FMap.IndexOf(AUnitName) < 0 then
      FMap.Add(AUnitName,TJ2PasObjects.Create(False));

    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

function TUnitProducerImpl.NewUnit(const AUnitName: String;
  const AObjects: TJ2PasObjects; out Error: String): Boolean;
var
  I:Integer;
begin
  Result:=False;
  if not NewUnit(AUnitName,Error) then
    Exit;

  //add each object to the supplied unit
  for I := 0 to Pred(AObjects.Count) do
    if not AddToUnit(AUnitName,AObjects[I],Error) then
      Exit;

  //everything added correctly
  Result:=True;
end;

function TUnitProducerImpl.NewUnit(const AUnitName: String;
  const AObject: TJ2PasObject; out Error: String): Boolean;
begin
  Result:=False;
  if not NewUnit(AUnitName,Error) then
    Exit;

  //add the object to the unit
  if not AddToUnit(AUnitName,AObject,Error) then
    Exit;

  //success
  Result:=True;
end;

function TUnitProducerImpl.AddToUnit(const AUnitName: String;
  const AObject: TJ2PasObject; out Error: String): Boolean;
begin
  Result:=False;
  try
    //check if unit exists, add if not as convenience
    if FMap.IndexOf(AUnitName) < 0 then
      if not NewUnit(AUnitName,Error) then
        Exit;

    //add the object to the map's object list (this list does not own the object)
    FMap.Data[FMap.IndexOf(AUnitName)].Add(AObject);

    //success
    Result:=True;
  except on E:Exception do
    Error:=E.Message;
  end;
end;

procedure TUnitProducerImpl.Remove(const AUnitName: String);
begin
  //remove if exists
  if FMap.IndexOf(AUnitName) > 0 then
    FMap.Remove(AUnitName);
end;

function TUnitProducerImpl.Finalize(out Error: String): Boolean;
var
  I:Integer;
  LSource:String;
  LUnit:TUnit;
begin
  Result:=False;
  FFinalized:=False;
  SetLength(FUnits,0);

  //make sure we have some units
  if FMap.Count < 1 then
  begin
    Error:='no units to finalize';
    Exit;
  end;

  //iterate map and call to virtual method to prepare source code
  for I := 0 to Pred(FMap.Count) do
  begin
    //call virtual prepare to generate the source
    if not PrepareSource(FMap.Data[I],LSource,Error) then
      Exit;

    //update source
    LUnit.Source:=LSource;

    //set the filename to the root and unit name
    LUnit.Filename:=FRoot + DirectorySeparator + FMap.Keys[I];

    //add the unit the units array to the last index
    SetLength(FUnits,Succ(Length(FUnits)));
    FUnits[High(FUnits)]:=LUnit;
  end;

  //mark that we are finalized and that we succeeded
  FFinalized:=True;
  Result:=True;
end;

function TUnitProducerImpl.SaveToFiles(out Error: String): Boolean;
var
  I:Integer;
  LSource:TStringList;
begin
  Result:=False;

  //if a call to finalize hasn't been made, do that now for convenience
  if not FFinalized then
    if not Finalize(Error) then
      Exit;
  LSource:=TStringList.Create;
  try
    try
      //attempt to save all units to files
      for I := 0 to High(FUnits) do
      begin
        LSource.Text:=FUnits[I].Source;
        LSource.SaveToFile(FUnits[I].Filename);
      end;

      //everything succeeded
      Result:=True;
    except on E:Exception do
      Error:=E.Message;
    end;
  finally
    LSource.Free;
  end;
end;

constructor TUnitProducerImpl.Create;
begin
  FRoot:='.';
  FMap:=TUnitMap.Create(True);
  FFinalized:=False;
end;

destructor TUnitProducerImpl.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

end.

