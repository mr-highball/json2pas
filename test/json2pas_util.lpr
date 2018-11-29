program json2pas_util;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, json2pas;

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
begin
  if not TJ2PasObject.Parse('{"test_property":["hello world"]}',LObj,LError) then
  begin
    WriteLn(LError);
  end;

  //add the object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestObj');

  //now try to add parse an object which has a object property
  if not TJ2PasObject.Parse('{"test_object_property":{"test_property":["hello world"]}}',LObj,LError) then
  begin
    WriteLn(LError);
  end;

  //add the compound object
  TJ2PasObject.ObjectExists(LObj.Properties,I,True,'TTestCompoundObj');
end;

{ TJSON2PasMain }

procedure TJSON2PasUtil.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
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
end;

var
  Application: TJSON2PasUtil;
begin
  Application:=TJSON2PasUtil.Create(nil);
  Application.Title:='JSON2Pas';
  Application.Run;
  Application.Free;
end.

