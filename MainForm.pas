unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    MemoFields: TMemo;
    EditTable: TLabeledEdit;
    Label1: TLabel;
    Button1: TButton;
    MemoResults: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Generator;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Gen: TFTSGenerator;
begin
  Gen := TFTSGenerator.Create;
  try
    MemoResults.Text := Gen.GenerateData(EditTable.Text, MemoFields.Text);
  finally
    Gen.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
const
  StrLineFmt = '    ''%s''+';
var
  Lines, Results: TArray<string>;
  Line: string;
  LastPos: Integer;
begin
  Lines := MemoResults.Lines.ToStringArray;

  Results := ['  StrTriggerFTS = '];
  for Line in Lines do
  begin
    if Line.Trim.IsEmpty or Line.StartsWith('--') then
      Continue;

    Results := Results + [Format(StrLineFmt, [Line.Replace('''', '''''')])];
  end;

  if Length(Results) = 0 then
    Exit;

  LastPos := Pred(Length(Results));
  Results[LastPos] := Results[LastPos].Replace('+', ';');

  MemoResults.Text := ''.Join(sLineBreak, Results);
end;

end.
