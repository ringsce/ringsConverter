unit CTopas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

function ConvertCToPas(const CCode: string): string;

implementation

function ConvertCToPas(const CCode: string): string;
var
  ConvertedCode: string;
  Regex: TRegExpr;
begin
  ConvertedCode := CCode;

  // 1) Convert C-style block comments /* ... */ to Pascal-style (* ... *)
  Regex := TRegExpr.Create;
  try
    Regex.ModifierM := True; // Multiline mode for comments spanning multiple lines
    Regex.Expression := '/\*(.*?)\*/';
    ConvertedCode := Regex.Replace(ConvertedCode, '(*\1*)', True);
  finally
    Regex.Free;
  end;

  // 2) Convert C++-style line comments (// ...) to Pascal-style comments
  // If you want to change them to (* ... *), uncomment the block below.
  {
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '//(.*)';
    ConvertedCode := Regex.Replace(ConvertedCode, '(*\1*)', True);
  finally
    Regex.Free;
  end;
  }
  // Otherwise, keep them as-is since Pascal also supports `//` comments.

  // 3) Convert #include <filename> to "uses filename;"
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '#include\s*<([^>]+)>';
    ConvertedCode := Regex.Replace(ConvertedCode, 'uses \1;', True);
  finally
    Regex.Free;
  end;

  // 4) Convert structs -> Pascal records
  ConvertedCode := StringReplace(ConvertedCode, 'struct ', 'type ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '{', 'record', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '};', 'end;', [rfReplaceAll]);

  // 5) Convert enums -> Pascal enums
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'enum\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{([^}]*)\};';
    ConvertedCode := Regex.Replace(ConvertedCode, 'type \1 = (\2);', True);
  finally
    Regex.Free;
  end;

  // 6) Convert "int main(...)" to Pascal's "begin ... end."
  Regex := TRegExpr.Create;
  try
    Regex.Expression := 'int\s+main\s*\([^)]*\)\s*\{';
    ConvertedCode := Regex.Replace(ConvertedCode, 'begin', True);
  finally
    Regex.Free;
  end;
  if RightStr(ConvertedCode, 1) = '}' then
    ConvertedCode := LeftStr(ConvertedCode, Length(ConvertedCode) - 1) + 'end.';

  // 7) Convert Function Declarations
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\bvoid\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(';
    ConvertedCode := Regex.Replace(ConvertedCode, 'procedure \1(', True);
  finally
    Regex.Free;
  end;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\bint\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(', True);
    ConvertedCode := StringReplace(ConvertedCode, ')', '): Integer)', [rfReplaceAll]);
  finally
    Regex.Free;
  end;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\bfloat\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(', True);
    ConvertedCode := StringReplace(ConvertedCode, ')', '): Real)', [rfReplaceAll]);
  finally
    Regex.Free;
  end;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\bdouble\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(', True);
    ConvertedCode := StringReplace(ConvertedCode, ')', '): Double)', [rfReplaceAll]);
  finally
    Regex.Free;
  end;

  // 8) Convert If-Else Statements
  ConvertedCode := StringReplace(ConvertedCode, 'if (', 'if ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' then', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} else {', 'else', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} else', 'else', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '}', '', [rfReplaceAll]);

  // 9) Convert Loops
  ConvertedCode := StringReplace(ConvertedCode, 'for (int ', 'for ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ' ; ', ' to ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '; ++', ' do', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' do', [rfReplaceAll]);

  ConvertedCode := StringReplace(ConvertedCode, 'while (', 'while ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' do', [rfReplaceAll]);

  ConvertedCode := StringReplace(ConvertedCode, 'do {', 'repeat', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} while (', 'until ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ');', ';', [rfReplaceAll]);

  // 10) Convert Pointers
  ConvertedCode := StringReplace(ConvertedCode, 'int* ', 'var ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'char* ', 'PChar ', [rfReplaceAll]);

  // 11) Convert Print Statements
  ConvertedCode := StringReplace(ConvertedCode, 'printf', 'writeln(', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'sprintf <<', 'writeln(', [rfReplaceAll]);

  // 12) Convert Return Statements
  ConvertedCode := StringReplace(ConvertedCode, 'return 0;', 'Exit;', [rfReplaceAll]);

  Result := ConvertedCode;
end;

end.

