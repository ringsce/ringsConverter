program realms;

{$MODE OBJFPC}  // Modern Object Pascal mode

// Include the translated files
{$INCLUDE optimize_source.pas}
{$INCLUDE write_collision_model.pas}

begin
  // Example usage of included functions
  Writeln('Realms Engine Initialized');

  // Call an optimization function from optimize_source.pas
  AlwaysOptimizeSourceFile('stb_image.pas');
  AlwaysOptimizeSourceFile('miniz.pas');

  // Call the collision model function from write_collision_model.pas
  WriteCollisionModelForMapEntity('mapEntityData', 'collision_model', True);

  Writeln('Execution Complete.');
end.

