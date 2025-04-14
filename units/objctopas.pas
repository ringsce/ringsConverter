unit ObjCToPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

function ConvertObjCToPascal(const ObjCCode: string): string;

implementation

function ConvertObjCToPascal(const ObjCCode: string): string;
var
  ConvertedCode: string;
  Regex: TRegExpr;
begin
  ConvertedCode := ObjCCode;

  // Convert #import <filename> to Pascal comments
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '#import\s*<([^>]+)>';
    if Regex.Exec(ConvertedCode) then
      ConvertedCode := Regex.Replace(ConvertedCode, '// Uses: \1', True);
  finally
    Regex.Free;
  end;

  // Convert protocols: @protocol ProtocolName -> type ProtocolName = interface
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '@protocol\s+([a-zA-Z_][a-zA-Z0-9_]*)';
    if Regex.Exec(ConvertedCode) then
      ConvertedCode := Regex.Replace(ConvertedCode, 'type \1 = interface', True);
  finally
    Regex.Free;
  end;

  // Convert categories: @interface ClassName(Category) -> add comment about category
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '(@interface\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^\)]+)\))';
    if Regex.Exec(ConvertedCode) then
      ConvertedCode := Regex.Replace(ConvertedCode, '// Category: \3 for class \2', True);
  finally
    Regex.Free;
  end;

  // Standard conversion of @interface for classes
  ConvertedCode := StringReplace(ConvertedCode, '@interface ', 'type ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ' : ', ' = class(', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, 'NSObject', 'TObject', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '{', '', [rfReplaceAll]);

  // Convert @end to Pascal "end;"
  ConvertedCode := StringReplace(ConvertedCode, '@end', 'end;', [rfReplaceAll]);

  // Convert @implementation to Pascal "implementation"
  ConvertedCode := StringReplace(ConvertedCode, '@implementation ', '', [rfReplaceAll]);

  // --- Convert Objective-C instance methods ---
  // First, convert methods with 'void' return to procedures:
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '-\s*\(void\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'procedure \1(\2);', True);
  finally
    Regex.Free;
  end;

  // Now, convert methods that return int
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '-\s*\(int\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(\2): Integer;', True);
  finally
    Regex.Free;
  end;

  // Convert methods that return bool (Objective-C uses BOOL)
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '-\s*\(BOOL\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(\2): Boolean;', True);
  finally
    Regex.Free;
  end;

  // If you have other types like float, double etc, add similar Regex passes.
  // For example, for float:
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '-\s*\(float\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'function \1(\2): Real;', True);
  finally
    Regex.Free;
  end;

  // --- Convert Objective-C class methods ---
  // Convert void class methods:
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\+\s*\(void\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'class procedure \1(\2);', True);
  finally
    Regex.Free;
  end;

  // Convert int class methods:
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\+\s*\(int\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'class function \1(\2): Integer;', True);
  finally
    Regex.Free;
  end;

  // Convert BOOL class methods:
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '\+\s*\(BOOL\)\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*:(\s*\([^)]+\)\s*[a-zA-Z_][a-zA-Z0-9_]*)';
    ConvertedCode := Regex.Replace(ConvertedCode, 'class function \1(\2): Boolean;', True);
  finally
    Regex.Free;
  end;

  // --- Convert properties ---
  ConvertedCode := StringReplace(ConvertedCode, '@property ', 'var ', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ';', '', [rfReplaceAll]);

  // Convert alloc/init to Pascal object creation
  ConvertedCode := StringReplace(ConvertedCode, 'alloc] init]', 'Create;', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '[', '', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ']', '', [rfReplaceAll]);

  // Convert NSString * to PChar
  ConvertedCode := StringReplace(ConvertedCode, 'NSString *', 'PChar', [rfReplaceAll]);

  // Convert NSLog to writeln
  ConvertedCode := StringReplace(ConvertedCode, 'NSLog(@"', 'writeln(''', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '");', ''');', [rfReplaceAll]);

  // Remove ARC-related keywords
  ConvertedCode := StringReplace(ConvertedCode, '__strong', '', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '__weak', '', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, '__autoreleasing', '', [rfReplaceAll]);

  // Convert Return statements: e.g., return something; to Exit(something);
  ConvertedCode := StringReplace(ConvertedCode, 'return ', 'Exit(', [rfReplaceAll]);
  ConvertedCode := StringReplace(ConvertedCode, ';', ');', [rfReplaceAll]);

  Result := ConvertedCode;
end;

end.

