unit PastoSwift;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

function ConvertPascalToSwift(const PascalCode: string): string;

implementation

function MapPascalTypeToSwift(const PascalType: string): string;
begin
  case LowerCase(PascalType) of
    'integer': Result := 'Int';
    'string':  Result := 'String';
    'boolean': Result := 'Bool';
    'char':    Result := 'Character';
    'real':    Result := 'Double';
    'double':  Result := 'Double';
    'byte':    Result := 'UInt8';
  else
    Result := PascalType;
  end;
end;

function ConvertPascalToSwift(const PascalCode: string): string;
var
  SwiftCode, Line, ParamPart, ParamName, ParamType, MappedType: string;
  Regex: TRegExpr;
  Lines: TStringList;
  i: Integer;
begin
  SwiftCode := PascalCode;

  // 1. Convert procedures
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'procedure\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\);';
    SwiftCode := Regex.Replace(SwiftCode, 'func \1(\2) {', True);
  finally
    Regex.Free;
  end;

  // 2. Convert functions with return types
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'function\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*:\s*([a-zA-Z_][a-zA-Z0-9_]*);';
    while Regex.Exec(SwiftCode) do
    begin
      MappedType := MapPascalTypeToSwift(Regex.Match[3]);
      SwiftCode := Regex.Replace(SwiftCode, 'func \1(\2) -> ' + MappedType + ' {', True);
    end;
  finally
    Regex.Free;
  end;

  // 3. Convert records to Swift structs
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'type\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*record';
    SwiftCode := Regex.Replace(SwiftCode, 'struct \1 {', True);
    SwiftCode := StringReplace(SwiftCode, 'end;', '}', [rfReplaceAll]);
  finally
    Regex.Free;
  end;

  // 4. Convert var/const declarations
  Lines := TStringList.Create;
  try
    Lines.Text := SwiftCode;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line.StartsWith('var ') then
      begin
        Line := Copy(Line, 5, Length(Line) - 4);
        ParamPart := Trim(Line);
        if Pos(':', ParamPart) > 0 then
        begin
          ParamName := Trim(LeftStr(ParamPart, Pos(':', ParamPart) - 1));
          ParamType := Trim(Copy(ParamPart, Pos(':', ParamPart) + 1, Length(ParamPart)));
          ParamType := StringReplace(ParamType, ';', '', []);
          Lines[i] := Format('var %s: %s', [ParamName, MapPascalTypeToSwift(ParamType)]);
        end;
      end
      else if Line.StartsWith('const ') then
      begin
        Line := Copy(Line, 7, Length(Line) - 6);
        ParamPart := Trim(Line);
        if Pos(':', ParamPart) > 0 then
        begin
          ParamName := Trim(LeftStr(ParamPart, Pos(':', ParamPart) - 1));
          ParamType := Trim(Copy(ParamPart, Pos(':', ParamPart) + 1, Length(ParamPart)));
          ParamType := StringReplace(ParamType, ';', '', []);
          Lines[i] := Format('let %s: %s', [ParamName, MapPascalTypeToSwift(ParamType)]);
        end;
      end;
    end;
    SwiftCode := Lines.Text;
  finally
    Lines.Free;
  end;

  // 5. Control structures

  // if/else
  SwiftCode := StringReplace(SwiftCode, 'if ', 'if ', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, ' then', ' {', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, 'else', '} else {', [rfReplaceAll]);

  // while
  SwiftCode := StringReplace(SwiftCode, 'while ', 'while ', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, ' do', ' {', [rfReplaceAll]);

  // repeat...until
  SwiftCode := StringReplace(SwiftCode, 'repeat', 'repeat {', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, 'until ', '} while !(', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, ';', ')', [rfReplaceAll]);

  // for x := a to b do → for x in a...b {
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*:=\s*(\d+)\s+to\s+(\d+)\s+do';
    SwiftCode := Regex.Replace(SwiftCode, 'for \1 in \2...\3 {', True);
  finally
    Regex.Free;
  end;

  // for-in
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'for\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+in\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+do';
    SwiftCode := Regex.Replace(SwiftCode, 'for \1 in \2 {', True);
  finally
    Regex.Free;
  end;

  // 6. Clean up Pascal-specific keywords
  SwiftCode := StringReplace(SwiftCode, 'begin', '', [rfReplaceAll]);
  SwiftCode := StringReplace(SwiftCode, 'end;', '}', [rfReplaceAll]);

  // 7. Remove Pascal semicolons (Swift doesn’t need them)
  SwiftCode := StringReplace(SwiftCode, ';', '', [rfReplaceAll]);

  // 8. Basic indentation (just a visual indentation level for braces)
  Lines := TStringList.Create;
  try
    Lines.Text := SwiftCode;
    var IndentLevel: Integer;
    IndentLevel := 0;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then Continue;

      if Line.StartsWith('}') then Dec(IndentLevel);
      Lines[i] := DupeString('  ', IndentLevel) + Line;
      if Line.EndsWith('{') then Inc(IndentLevel);
    end;
    SwiftCode := Lines.Text;
  finally
    Lines.Free;
  end;

  Result := Trim(SwiftCode);
end;

end.

