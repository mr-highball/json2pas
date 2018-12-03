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
  Classes, SysUtils, CustApp, json2pas, json2pas.producer,
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

(*
  an example showing a name formatting method for properties
  turning something this 'hello_world' -> 'HelloWorld'
*)
procedure FormatProp(Var AName:String);
var
  I:Integer;
begin
  //uppercase first letter
  if AName.Length >= 1 then
    AName[1]:=UpperCase(AName[1])[1];

  //remove _ chars and uppercase following letter
  while Pos('_',AName) > 0 do
  begin
    I:=Pos('_',AName);

    //uppercase following
    if I < AName.Length then
      AName[Succ(I)]:=UpperCase(AName[Succ(I)])[1];

    //remove the underscore
    if (I > 1) and (I < AName.Length) then
      AName:=Copy(AName,1,Pred(I)) + Copy(AName,Succ(I),AName.Length - I)
    else if I = 1 then
      AName:=Copy(AName,Succ(I),AName.Length - 1)
    else
      AName:=Copy(AName,1,Pred(I));
  end;
end;

procedure TestSomeJSON;
var
  LObj:TJ2PasObject;
  LError:String;
  I:Integer;
begin
  //attempt to parse a simple json object with one property
  if not TJ2PasObject.Parse('{"test_property":["hello world"]}',LObj,LError) then
  begin
    WriteLn(LError);
  end;

  //add the object to global
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestObj');

  //now try to add parse an object which has a object property
  if not TJ2PasObject.Parse('{"test_object_property":{"test_property":["hello world"]}}',LObj,LError) then
  begin
    WriteLn(LError);
  end;

  //add the compound object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestCompoundObj');

  //now try to parse an object which has an array of compound objects
  if not TJ2PasObject.Parse('{"test_array_of_objects_property":[{"test_object_property":{"test_property":["hello world"]}}]}',LObj,LError) then
  begin
    WriteLn(LError);
  end;

  //add the array of object object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestArrayObject');

  //show user output
  for I:=0 to Pred(TJ2PasObject.Objects.Count) do
    WriteLn(TJ2PasObject.Objects[I].ToJSON);
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
  DefaultPropertyNameFormat:=FormatProp;
  Application:=TJSON2PasUtil.Create(nil);
  Application.Title:='JSON2Pas';
  Application.Run;
  Application.Free;
end.

