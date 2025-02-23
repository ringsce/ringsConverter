unit RProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite;

type
  // TRProject is a simple project container with a list of file names.
  TRProject = class
  private
    FOpenedFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const AFileName: string);
    // Instead of receiving a file name, SaveToFile now automatically uses
    // ~/Documents/MyProj/MyProject.rproj. Adjust as you see fit.
    procedure SaveToFile;
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
  // Create ~/Documents/MyProj if it doesn't exist
  ProjectPath := GetUserDir + 'Documents' + DirectorySeparator + 'MyProj';
  if not DirectoryExists(ProjectPath) then
    ForceDirectories(ProjectPath);

  // Full path to the .rproj file
  FullFileName := ProjectPath + DirectorySeparator + 'MyProject.rproj';

  // Create an XML document and build a simple structure
  Doc := TXMLDocument.Create;
  try
    // Root element
    RootNode := Doc.CreateElement('Project');
    Doc.Appendchild(RootNode);

    // Files element
    FilesNode := Doc.CreateElement('OpenedFiles');
    RootNode.Appendchild(FilesNode);

    // Add each opened file as an XML element
    for i := 0 to FOpenedFiles.Count - 1 do
    begin
      FileNode := Doc.CreateElement('File');
      FileNode.AppendChild(Doc.CreateTextNode(FOpenedFiles[i]));
      FilesNode.AppendChild(FileNode);
    end;

    // Save XML to file
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

end.

