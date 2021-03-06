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
program json2pas_util;

{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, strutils, json2pas, json2pas.producer,
  json2pas.producer.std, json2pas.producer.std.pascal;

type

  { TJSON2PasUtil }

  TJSON2PasUtil = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure TestSomeJSON;
var
  LObj:TJ2PasObject;
  LError:String;
  I:Integer;
  LProducer:IStandardProducer;
  LUnits:TUnits;
begin
  //attempt to parse a simple json object with one property
  if not TJ2PasObject.Parse('{"test_property":["hello world"], "test_strings":[""]}',LObj,LError) then
  begin
    WriteLn(LError);
    Exit;
  end;

  //add the object to global
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestObj');

  //now try to add parse an object which has a object property
  if not TJ2PasObject.Parse('{"test_object_property":{"test_property":["hello world"], "test_strings":[""]}}',LObj,LError) then
  begin
    WriteLn(LError);
    Exit;
  end;

  //add the compound object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestCompoundObj');

  //now try to parse an object which has an array of compound objects
  if not TJ2PasObject.Parse('{"test_array_of_objects_property":[{"test_object_property":{"test_property":["hello world"], "test_strings":[""]}}]}',LObj,LError) then
  begin
    WriteLn(LError);
    Exit;
  end;

  //add the array of object object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestArrayObject');

  WriteLn('Showing Object MetaData',sLineBreak,DupeString('-',30));

  //show user output
  for I:=0 to Pred(TJ2PasObject.Objects.Count) do
    WriteLn(TJ2PasObject.Objects[I].ToJSON);

  WriteLn(sLineBreak,'Generated Source Code',sLineBreak,DupeString('-',30));

  //make a code producer of your choice, in this case we use std.pascal
  LProducer:=TPascalProducerImpl.Create;//todo - change this to pascal

  //try to make a unit called testunit
  if not LProducer.NewUnit('testunit',TJ2PasObject.Objects,LError) then
  begin
    WriteLn('an error occurred: ',LError);
    Exit;
  end;

  //attempt to finalize the units
  if not LProducer.Finalize(LError) then
  begin
    WriteLn('an error occurred: ',LError);
    Exit;
  end;

  //copy the units and output to screen
  LUnits:=LProducer.Units;
  WriteLn('Generated Unit Count: ',Length(LUnits));
  for I := 0 to High(LUnits) do
    WriteLn(
      LUnits[I].Filename,sLineBreak,
      DupeString('-',10),sLineBreak,
      LUnits[I].Source,sLineBreak
    );
end;

{ TJSON2PasMain }

procedure TJSON2PasUtil.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('ht', 'help test');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  if HasOption('t','test') then
    TestSomeJSON;

  // stop program loop
  Terminate;
end;

constructor TJSON2PasUtil.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TJSON2PasUtil.Destroy;
begin
  inherited Destroy;
end;

procedure TJSON2PasUtil.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  WriteLn('Test: ', ExeName, ' -t');
end;

var
  Application: TJSON2PasUtil;
begin
  Application:=TJSON2PasUtil.Create(nil);
  Application.Title:='JSON2Pas';
  Application.Run;
  Application.Free;
end.

