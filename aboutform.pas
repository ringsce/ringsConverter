unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    imgLogo: TImage;
    lblTitle: TLabel;
    lblVersion: TLabel;
    memoDesc: TMemo;
    lblLink: TLabel;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
  end;

implementation

uses
  LCLIntf; // For OpenURL

{$R *.lfm}

procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.lblLinkClick(Sender: TObject);
begin
  OpenURL(lblLink.Caption);
end;

end.

