unit idetester_options;

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf,
  idetester_strings;

type
  { TIDETesterSettings }

  TIDETesterSettings = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Button1: TButton;
    CheckBox1: TCheckBox;
    edtParameters: TEdit;
    editWaitTime: TEdit;
    edtTester: TEdit;
    lblTester: TLabel;
    lblhelp: TLabel;
    lblTimeout: TLabel;
    lblExecutionParameters: TLabel;
    odProject: TOpenDialog;
    Panel1: TPanel;
    pnlKillTime: TPanel;
    pnlParameters: TPanel;
    pnlTester: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblhelpClick(Sender: TObject);
  private

  public

  end;

var
  IDETesterSettings: TIDETesterSettings;

implementation

{$R *.lfm}

procedure switchButtons(btn1, btn2 : TControl);
var
  l : integer;
begin
  l := btn1.left;
  btn1.left := btn2.left;
  btn2.left := l;
end;

{ TIDETesterSettings }

procedure TIDETesterSettings.lblhelpClick(Sender: TObject);
begin
  OpenURL(helpUrl);
end;

procedure TIDETesterSettings.FormCreate(Sender: TObject);
begin
  btnOk.Caption := rs_IdeTester_Caption_OK;
  btnCancel.Caption := rs_IdeTester_Caption_Cancel;
  lblhelp.caption := rs_IdeTester_Caption_Help;
  lblTester.caption := rs_IdeTester_Caption_Options_Label_Tester;
  lblExecutionParameters.caption := rs_IdeTester_Caption_Options_Label_Parameters;
  lblTimeout.Caption := rs_IdeTester_Caption_Options_Label_Timeout;
  {$IFNDEF WINDOWS}
  switchButtons(btnOk, btnCancel);
  {$ENDIF}
end;

procedure TIDETesterSettings.Button1Click(Sender: TObject);
begin
  if odProject.execute then
    edtTester.Text := odProject.filename;
end;

end.

