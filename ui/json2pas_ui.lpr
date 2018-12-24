program json2pas_ui;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Json2Pas';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TJson2PasMain, Json2PasMain);
  Application.Run;
end.

