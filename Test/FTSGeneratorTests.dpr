program FTSGeneratorTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  System.SysUtils,
  DunitXTestRunner,
  GeneratorTests in 'GeneratorTests.pas',
  Generator in '..\Generator.pas';

{$R *.RES}

begin
  try
    TDUnitXTestRunner.Execute;
  except
    on E: Exception do
      TDUnitXTestRunner.MostrarErro(E);
  end;
end.

