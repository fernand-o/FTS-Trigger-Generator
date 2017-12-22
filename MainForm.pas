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
    procedure Button1Click(Sender: TObject);
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

end.
