unit idetester_options;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf,
  idetester_strings;

type
  { TIDETesterSettings }

  TIDETesterSettings = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    Button1: TButton;
    chkAutoSave: TCheckBox;
    edtParameters: TEdit;
    editWaitTime: TEdit;
    edtTester: TEdit;
    lblTester: TLabel;
    lblhelp: TLabel;
    lblTimeout: TLabel;
    lblExecutionParameters: TLabel;
    odProject: TOpenDialog;
    pnlBottom: TPanel;
    pnlKillTime: TPanel;
    pnlParameters: TPanel;
    pnlTester: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure edtTesterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  chkAutoSave.Caption := rs_IdeTester_Caption_Options_Label_AutoSave;
  {$IFNDEF WINDOWS}
  switchButtons(btnOk, btnCancel);
  {$ENDIF}
end;

procedure TIDETesterSettings.FormShow(Sender: TObject);
var
  h : integer;
begin
  h := pnlBottom.Height;
  if pnlTester.Visible then
    inc(h, pnlTester.Height);
  if pnlParameters.Visible then
    inc(h, pnlParameters.Height);
  if pnlKillTime.Visible then
    inc(h, pnlKillTime.Height);
  Height := h;
end;

procedure TIDETesterSettings.Button1Click(Sender: TObject);
begin
  if odProject.execute then
    edtTester.Text := odProject.filename;
end;

procedure TIDETesterSettings.edtTesterChange(Sender: TObject);
begin
  chkAutoSave.enabled := edtTester.text <> '';
end;

end.

