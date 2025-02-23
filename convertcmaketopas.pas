unit ConvertCMakeToPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

procedure ConvertCMakeListsToPas(const CMakeListsFile, OutputPasFile: string);

implementation

procedure ConvertCMakeListsToPas(const CMakeListsFile, OutputPasFile: string);
var
  InputLines, OutputLines: TStringList;
  i: Integer;
  Line, OptionName, OptionValue: string;
  Regex: TRegExpr;
begin
  InputLines := TStringList.Create;
  OutputLines := TStringList.Create;
  try
    InputLines.LoadFromFile(CMakeListsFile);

    // Begin the Pascal unit header
    OutputLines.Add('unit BuildOptions;');
    OutputLines.Add('');
    OutputLines.Add('{$mode objfpc}{$H+}');
    OutputLines.Add('');
    OutputLines.Add('interface');
    OutputLines.Add('');
    OutputLines.Add('const');

    // Prepare a regex to capture CMake set() commands.
    // For example: set(CMAKE_BUILD_TYPE "Release")
    Regex := TRegExpr.Create;
    try
      Regex.Expression := '^[ \t]*set\(([A-Za-z0-9_]+)[ \t]+"([^"]+)"\)';
      for i := 0 to InputLines.Count - 1 do
      begin
        Line := InputLines[i];
        if Regex.Exec(Line) then
        begin
          OptionName := Regex.Match[1];
          OptionValue := Regex.Match[2];
          // Write the option as a Pascal constant definition:
          OutputLines.Add('  ' + OptionName + ' = ''' + OptionValue + ''';');
        end;
      end;
    finally
      Regex.Free;
    end;

    OutputLines.Add('');
    OutputLines.Add('implementation');
    OutputLines.Add('');
    OutputLines.Add('end.');

    // Save the generated Pascal unit
    OutputLines.SaveToFile(OutputPasFile);
  finally
    InputLines.Free;
    OutputLines.Free;
  end;
end;

end.

