unit CppToPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

function ConvertCToPascal(const CppCode: string): string;

implementation

function ConvertCToPascal(const CppCode: string): string;
var
  ConvertedCode: string;
  Regex: TRegExpr;
begin
  ConvertedCode := CppCode;

  // Convert C/C++ block comments: /* ... */  -->  (* ... *)
  Regex := TRegExpr.Create;
  try
    Regex.ModifierM := True; // multiline
    Regex.Expression := '/\*(.*?)\*/';
    ConvertedCode := Regex.Replace(ConvertedCode, '(*\1*)', True);
  finally
    Regex.Free;
  end;

  // Optionally, convert C/C++ line comments starting with // to Pascal style comments
  // If you want to change them to Pascal-style "(* ... *)" comments, uncomment the block below.
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '//(.*)';
    ConvertedCode := Regex.Replace(ConvertedCode, '(*\1*)', True);
  finally
    Regex.Free;
  end; // Otherwise, leave // comments unchanged.

  // Convert #include <filename> to Pascal comments
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '#include\s*<([^>]+)>';
    if Regex.Exec(ConvertedCode) then
      ConvertedCode := Regex.Replace(ConvertedCode, '// Uses: \1', True);
  finally
    Regex.Free;
  end;

  // Handle Structs -> Pascal Records
  ConvertedCode := StringReplace(ConvertedCode, 'struct ', 'type ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '{', 'record', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '};', 'end;', [rfReplaceAll]);

  // Handle Classes -> Pascal Classes
  ConvertedCode := StringReplace(ConvertedCode, 'class ', 'type ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'public:', 'public', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'private:', 'private', [rfReplaceAll]);

  // Convert Class Functions to Pascal Procedures
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '([a-zA-Z_][a-zA-Z0-9_]*)\s*::\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)';
    if Regex.Exec(ConvertedCode) then
      ConvertedCode := Regex.Replace(ConvertedCode, 'procedure \1.\2(\3);', True);
  finally
    Regex.Free;
  end;

  // Convert If-Else Statements
  ConvertedCode := StringReplace(ConvertedCode, 'if (', 'if ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' then', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} else {', 'else', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} else', 'else', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '}', '', [rfReplaceAll]);

  // Convert For Loops
  ConvertedCode := StringReplace(ConvertedCode, 'for (int ', 'for ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ' ; ', ' to ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '; ++', ' do', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' do', [rfReplaceAll]);

  // Convert While Loops
  ConvertedCode := StringReplace(ConvertedCode, 'while (', 'while ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ') {', ' do', [rfReplaceAll]);

  // Convert Do-While Loops
  ConvertedCode := StringReplace(ConvertedCode, 'do {', 'repeat', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '} while (', 'until ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ');', ';', [rfReplaceAll]);

  // Convert Pointers
  ConvertedCode := StringReplace(ConvertedCode, 'int* ', 'var ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'char* ', 'PChar ', [rfReplaceAll]);

  // Handle Printing (cout -> writeln)
  ConvertedCode := StringReplace(ConvertedCode, 'cout <<', 'writeln(', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'std::cout <<', 'writeln(', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'endl;', ');', [rfReplaceAll]);

  // Handle Return Statements
  ConvertedCode := StringReplace(ConvertedCode, 'return 0;', 'Exit;', [rfReplaceAll]);

  Result := ConvertedCode;
end;

end.

