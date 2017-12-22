program FTSGeneratorTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  GeneratorTests in 'GeneratorTests.pas',
  Generator in '..\Generator.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

