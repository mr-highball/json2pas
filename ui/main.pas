unit main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type

  { TJson2PasMain }

  TJson2PasMain = class(TForm)
    btn_new: TButton;
    btn_delete: TButton;
    btn_save: TButton;
    edit_obj_name: TEdit;
    lbl_obj_name: TLabel;
    pnl_bottom: TPanel;
    pnl_right: TPanel;
    pnl_left: TPanel;
    splitter: TSplitter;
    syn_edit: TSynEdit;
    syn_pas: TSynPasSyn;
    tree_main: TTreeView;
    procedure btn_deleteClick(Sender: TObject);
    procedure btn_newClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure NewUnitOrObject;
    procedure DeleteUnitOrObject;
  public

  end;

var
  Json2PasMain: TJson2PasMain;

implementation

{$R *.lfm}

{ TJson2PasMain }

procedure TJson2PasMain.FormCreate(Sender: TObject);
begin
  pnl_left.Width:=Self.Width div 2;
  pnl_right.Width:=Self.Width div 2;
end;

procedure TJson2PasMain.NewUnitOrObject;
begin
  //todo - depending on selection prompt for a new unit or object to a unit
end;

procedure TJson2PasMain.DeleteUnitOrObject;
begin
  //todo - depending on selection prompt to delete a unit or object
end;

procedure TJson2PasMain.btn_newClick(Sender: TObject);
begin
  NewUnitOrObject;
end;

procedure TJson2PasMain.btn_deleteClick(Sender: TObject);
begin
  DeleteUnitOrObject;
end;

end.

