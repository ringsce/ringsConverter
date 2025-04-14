program converter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  CppToPas in '../units/CppToPas.pas',
  ObjCToPas in '../units/ObjCToPas.pas',
  RProject in '../units/RProject.pas',
  CTopas in '../units/CtoPas.pas',
  ConvertCMakeToPas in '../units/CovertCMaketoPas.pas',
  pastoswift in '../units/pastoswift.pas',
  Startup in '../units/Startup.pas',
  AboutForm in '../units/AboutForm.pas'
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.{%H-}MainFormOnTaskbar:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  //Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

