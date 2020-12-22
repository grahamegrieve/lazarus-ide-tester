unit idetester_debug_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Clipbrd,
  LCLIntf,
  idetester_strings;

type

  { TIDETesterDebugForm }

  TIDETesterDebugForm = class(TForm)
    btnCopyLoad: TButton;
    btnCopyRunSelected: TButton;
    btnClose: TButton;
    btnCopyRunChecked: TButton;
    btnOpenFolder: TButton;
    edtParamsChecked: TEdit;
    edtExecutable: TEdit;
    edtParamsLoad: TEdit;
    edtParamsRunSelected: TEdit;
    lblhelp: TLabel;
    lblParamsRunChecked: TLabel;
    lblParamsLoad: TLabel;
    lblParamsRunSelected: TLabel;
    lblExplanation: TLabel;
    lblExecutable: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlKillTime: TPanel;
    pnlKillTime1: TPanel;
    pnlKillTime2: TPanel;
    pnlParameters: TPanel;
    procedure btnCopyLoadClick(Sender: TObject);
    procedure btnCopyRunCheckedClick(Sender: TObject);
    procedure btnCopyRunSelectedClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblhelpClick(Sender: TObject);
  private

  public

  end;

var
  IDETesterDebugForm: TIDETesterDebugForm;

implementation

{$R *.lfm}

{ TIDETesterDebugForm }

procedure TIDETesterDebugForm.FormCreate(Sender: TObject);
begin
  Caption := rs_IdeTester_Caption_Debug;
  btnCopyRunSelected.Caption := rs_IdeTester_Caption_Debug_Copy;
  btnCopyRunChecked.Caption := rs_IdeTester_Caption_Debug_Copy;
  btnCopyLoad.Caption := rs_IdeTester_Caption_Debug_Copy;
  btnOpenFolder.Caption := rs_IdeTester_Caption_Debug_Open;
  btnClose.Caption := rs_IdeTester_Caption_Close;
  lblParamsRunSelected.Caption := rs_IdeTester_Caption_Debug_Label_Run_Selection;
  lblParamsRunChecked.Caption := rs_IdeTester_Caption_Debug_Label_Run_Checked;
  lblParamsLoad.Caption := rs_IdeTester_Caption_Debug_Label_Load;
  lblExplanation.Caption := rs_IdeTester_Caption_Debug_Label_Main;
  lblExecutable.Caption := rs_IdeTester_Caption_Debug_Label_Exec;
  lblhelp.caption := rs_IdeTester_Caption_Help;
end;

procedure TIDETesterDebugForm.lblhelpClick(Sender: TObject);
begin
  OpenURL(helpUrl);
end;

procedure TIDETesterDebugForm.btnCopyRunSelectedClick(Sender: TObject);
begin
  Clipboard.AsText := edtParamsRunSelected.text;
end;

procedure TIDETesterDebugForm.btnOpenFolderClick(Sender: TObject);
begin
  OpenDocument(ExtractFileDir(edtExecutable.text));
end;

procedure TIDETesterDebugForm.btnCopyRunCheckedClick(Sender: TObject);
begin
  Clipboard.AsText := edtParamsChecked.text;
end;

procedure TIDETesterDebugForm.btnCopyLoadClick(Sender: TObject);
begin
  Clipboard.AsText := edtParamsLoad.text;
end;

end.

