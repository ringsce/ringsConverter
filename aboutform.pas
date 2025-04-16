unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    lblTitle: TLabel;
    label2: TLabel;
    Memo1: TMemo;
    Label3: TLabel;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure lblTitleOnClick(Sender: TObject);

  end;

implementation

uses
  LCLIntf; // For OpenURL

{$R *.lfm}

procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.Label3Click(Sender: TObject);
begin
  OpenURL(Label3.Caption);
end;

procedure TAboutForm.lblTitleOnClick(Sender: TObject);
begin
  lblTitle.Caption := 'Rings Converter';
end;

end.

