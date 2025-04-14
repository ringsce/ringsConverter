unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  CppToPas, ObjCToPas,Ctopas, ConvertCmakeToPas, RProject, pastoswift;  // <-- Added RProject unit

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ButtonSaveProject: TButton; // Add a button to save the Edit
    MainMenu1: TMainMenu;
    MemoSwift: TMemo;
    MemoC: TMemo;
    MemoPas: TMemo;
    MemoCmake: TMemo;
    Index: TMenuItem;
    Convert: TMenuItem;
    CtoPascal: TMenuItem;
    CMake: TMenuItem;
    Swift: TMenuItem;
    Save: TMenuItem;
    Objc: TMenuItem;
    MenuItemConvert: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure ConvertClick(Sender: TObject);
    procedure MemoSwiftChange(Sender: TObject);
    procedure MemoCChange(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure CtoPascalClick(Sender: TObject);
    procedure CMakeClick(Sender: TObject);
    procedure SwiftClick(Sender: TObject);
    procedure ObjcClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    // Convert CMakeLists.txt in the project root to BuildOptions.pas
    ConvertCMakeListsToPas('CMakeLists.txt', 'BuildOptions.pas');
    WriteLn('Conversion complete. See BuildOptions.pas');
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
  //ConvertCMakeListsToPas(MemoC.Text);
end;

procedure TForm1.ConvertClick(Sender: TObject);
begin
  MemoPas.Text := ConvertCToPascal(MemoC.Text);
end;

procedure TForm1.MemoSwiftChange(Sender: TObject);
begin
     MemoSwift.Text := ConvertPascalToSwift(MemoSwift.Text);
end;

procedure TForm1.MemoCChange(Sender: TObject);
begin

end;

procedure TForm1.EditClick(Sender: TObject);
var
  REdit: TRProject;
begin
  REdit := TRProject.Create;
  try
    // Simulate adding opened files
    REdit.AddFile('Unit1.pas');
    REdit.AddFile('MainForm.lfm');
    REdit.AddFile('RProject.pas');

    // Save Edit file as XML with .rproj extension
    //Edit.SaveToFile('MyProject.rproj');
    ShowMessage('Project saved as MyProject.rproj');
  finally
    REdit.Free;
  end;
end;

procedure TForm1.CtoPascalClick(Sender: TObject);
begin
   MemoPas.Text := ConvertCToPascal(MemoC.Text);

end;

procedure TForm1.CMakeClick(Sender: TObject);
begin
     MemoPas.Text := ConvertObjCToPascal(MemoC.Text);
end;

procedure TForm1.SwiftClick(Sender: TObject);
begin
     MemoSwift.Text := ConvertPascalToSwift(MemoSwift.Text);

end;

procedure TForm1.ObjcClick(Sender: TObject);
begin
  try
    // Convert CMakeLists.txt in the project root to BuildOptions.pas
    ConvertCMakeListsToPas('CMakeLists.txt', 'BuildOptions.pas');
    WriteLn('Conversion complete. See BuildOptions.pas');
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
  //ConvertCMakeListsToPas(MemoC.Text);
end;

procedure TForm1.SaveClick(Sender: TObject);
var
  TEdit: TRProject;
begin
  TEdit := TRProject.Create;
  try
    // Simulate adding opened files
    TEdit.AddFile('Unit1.pas');
    TEdit.AddFile('MainForm.lfm');
    TEdit.AddFile('RProject.pas');

    // Save Edit file as XML with .rproj extension
    //Edit.SaveToFile('MyProject.rproj');
    ShowMessage('Project saved as MyProject.rproj');
  finally
    TEdit.Free;
  end;
end;

end.

