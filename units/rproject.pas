unit RProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite;

type
  TRProject = class
  private
    FOpenedFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const AFileName: string);
    procedure SaveToFile;
    procedure SaveAsLPR;
    procedure SaveAsXcodeProj;
    property OpenedFiles: TStringList read FOpenedFiles;
  end;

implementation

{ TRProject }

constructor TRProject.Create;
begin
  inherited Create;
  FOpenedFiles := TStringList.Create;
end;

destructor TRProject.Destroy;
begin
  FOpenedFiles.Free;
  inherited Destroy;
end;

procedure TRProject.AddFile(const AFileName: string);
begin
  if FOpenedFiles.IndexOf(AFileName) = -1 then
    FOpenedFiles.Add(AFileName);
end;

procedure TRProject.SaveToFile;
var
  Doc: TXMLDocument;
  RootNode, FilesNode, FileNode: TDOMElement;
  i: Integer;
  XMLStream: TFileStream;
  ProjectPath, FullFileName: string;
begin
  ProjectPath := GetUserDir + 'Documents' + DirectorySeparator + 'MyProj';
  if not DirectoryExists(ProjectPath) then
    ForceDirectories(ProjectPath);

  FullFileName := ProjectPath + DirectorySeparator + 'MyProject.rproj';

  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('Project');
    Doc.Appendchild(RootNode);

    FilesNode := Doc.CreateElement('OpenedFiles');
    RootNode.Appendchild(FilesNode);

    for i := 0 to FOpenedFiles.Count - 1 do
    begin
      FileNode := Doc.CreateElement('File');
      FileNode.AppendChild(Doc.CreateTextNode(FOpenedFiles[i]));
      FilesNode.AppendChild(FileNode);
    end;

    XMLStream := TFileStream.Create(FullFileName, fmCreate);
    try
      WriteXMLFile(Doc, XMLStream);
    finally
      XMLStream.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TRProject.SaveAsLPR;
var
  ProjectPath, LPRFile: string;
  i: Integer;
  LPR: TStringList;
begin
  ProjectPath := GetUserDir + 'Documents' + DirectorySeparator + 'MyProj';
  if not DirectoryExists(ProjectPath) then
    ForceDirectories(ProjectPath);

  LPRFile := ProjectPath + DirectorySeparator + 'MyProject.lpr';
  LPR := TStringList.Create;
  try
    LPR.Add('program MyProject;');
    LPR.Add('');
    LPR.Add('uses');
    for i := 0 to FOpenedFiles.Count - 1 do
    begin
      LPR.Add('  ' + ChangeFileExt(ExtractFileName(FOpenedFiles[i]), '') + ',');
    end;
    LPR[LPR.Count - 1] := TrimRight(LPR[LPR.Count - 1]); // remove comma
    LPR.Add('');
    LPR.Add('begin');
    LPR.Add('  // TODO: your app starts here');
    LPR.Add('end.');

    LPR.SaveToFile(LPRFile);
  finally
    LPR.Free;
  end;
end;

procedure TRProject.SaveAsXcodeProj;
var
  ProjectPath, PBXFile: string;
  PBX: TStringList;
begin
  ProjectPath := GetUserDir + 'Documents' + DirectorySeparator + 'MyProj';
  if not DirectoryExists(ProjectPath) then
    ForceDirectories(ProjectPath);

  PBXFile := ProjectPath + DirectorySeparator + 'MyProject.xcodeproj.pbxproj';
  PBX := TStringList.Create;
  try
    PBX.Add('// !Placeholder Xcode project structure');
    PBX.Add('// Real generation requires UUIDs, targets, and schemes');
    PBX.Add('// For now, this serves as a stub');

    PBX.SaveToFile(PBXFile);

    // Optionally create folder structure:
    CreateDir(ProjectPath + DirectorySeparator + 'MyProject.xcodeproj');
    RenameFile(PBXFile, ProjectPath + DirectorySeparator + 'MyProject.xcodeproj' + DirectorySeparator + 'project.pbxproj');
  finally
    PBX.Free;
  end;
end;

end.

