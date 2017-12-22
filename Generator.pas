unit Generator;

interface

type
  TFTSFieldType = (ftText, ftNumeric);

  TFTSColumn = record
    name: string;
    typ: TFTSFieldType;
    weight: string;
  end;

  TFTSGenerator = class
  private
    FTable: string;
    FRawColumns: string;
    FColumns: TArray<TFTSColumn>;
    FResults: TArray<string>;
    FUseTempTable: Boolean;
    procedure AddResult(const ValueFmt: string; const Args: array of const);
    procedure DefineColumns;
    procedure GenerateMetadata;
    procedure GenerateTrigger;
    function TempTableDefinition: string;
    function SelectWithWeights: string;
    function ProcessColumns(Weights: TArray<TArray<TFTSColumn>>): string;
  public
    function GenerateData(Table, RawColumns: string): string;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

procedure TFTSGenerator.AddResult(const ValueFmt: string; const Args: array of const);
begin
  FResults := FResults + [Format(ValueFmt, Args)];
end;

function TFTSGenerator.GenerateData(Table, RawColumns: string): string;
begin
  FTable := Table;
  FRawColumns := RawColumns;

  GenerateMetadata;
  GenerateTrigger;

  Result := ''.Join(sLineBreak, FResults);
end;

procedure TFTSGenerator.GenerateMetadata;
begin
  AddResult('ALTER TABLE %s ADD COLUMN fts_document tsvector;', [FTable]);
  AddResult('CREATE INDEX %s_fts_index ON %s USING gist(fts_document);', [FTable, FTable]);
end;

procedure TFTSGenerator.GenerateTrigger;
const
  TriggerWrapperFmt =
    'CREATE OR REPLACE FUNCTION %s_fts_document_trigger() RETURNS TRIGGER AS $$'+ sLineBreak +
    'BEGIN'+ sLineBreak +
    '  NEW.FTS_DOCUMENT = (%s);'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END;';
var
  TriggerContent, TempTable: string;
begin
  DefineColumns;

  TempTable := TempTableDefinition;
  FUseTempTable := not TempTable.IsEmpty;

  TriggerContent := TempTableDefinition + SelectWithWeights;
  AddResult(TriggerWrapperFmt, [FTable, TriggerContent]);
end;

function TFTSGenerator.ProcessColumns(Weights: TArray<TArray<TFTSColumn>>): string;
const
  ItemTextFmt = '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',%s), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''%s'')';
  ItemNumFmt =  '    setweight(to_tsvector(''pt'', coalesce(concat_ws('' '',%s),'''')), ''%s'')';
var
  Numerics, Texts: TArray<TFTSColumn>;
  Column: TFTSColumn;
  I: Integer;
  Results: TArray<string>;

  procedure AddNumericColumns;
  const
    NumericFmt = '%s_temp.%s';
    ReverseFmt = 'reverse(%s)';
  var
    NumericColumn: TFTSColumn;
    Cols: TArray<string>;
    Col: string;
  begin
    if Length(Numerics) = 0 then
      Exit;

    for NumericColumn in Numerics do
    begin
      Col := Format(NumericFmt, [FTable, NumericColumn.name]);
      Cols := Cols + [Col];
      Cols := Cols + [Format(ReverseFmt, [Col])];
    end;
    Cols[0] := sLineBreak +'      '+ Cols[0];

    Results := Results + [Format(ItemNumFmt, [''.Join(',' + sLineBreak  + '      ', Cols), NumericColumn.weight])];
  end;

  procedure AddTextColumns;
  var
    TextColumn: TFTSColumn;
    Cols: TArray<string>;
    ColsStr: string;
  begin
    if Length(Texts) = 0 then
      Exit;

    for TextColumn in Texts do
      Cols := Cols + [TextColumn.name];

    if Length(Cols) > 1 then
      Cols[0] := sLineBreak +'      '+ Cols[0];

    Results := Results + [Format(ItemTextFmt, [''.Join(',' + sLineBreak + '      ', Cols), TextColumn.weight])];
  end;

begin
  Results := [];
  for I := 0 to Pred(Length(Weights)) do
  begin
    if Length(Weights[I]) = 0 then
      Continue;

    Numerics := [];
    Texts := [];
    for Column in Weights[I] do
    begin
      case Column.typ of
        ftNumeric: Numerics := Numerics + [Column];
        ftText: Texts := Texts + [Column];
      end;
    end;

    AddNumericColumns;
    AddTextColumns;
  end;

  Result := ''.Join(' ||' + sLineBreak, Results);
end;

function TFTSGenerator.SelectWithWeights: string;
const
  SelectTempTableFmt = sLineBreak + '    SELECT' + sLineBreak + '%s' + sLineBreak + '    FROM %s_temp';
var
  SelectColumns: string;
  Weights: TArray<TArray<TFTSColumn>>;

  function GetColumnsByWeight(Weight: string): TArray<TFTSColumn>;
  var
    WColumn: TFTSColumn;
  begin
    for WColumn in FColumns do
      if WColumn.weight = Weight then
        Result := Result + [WColumn];
  end;

begin
  SetLength(Weights, 4);
  Weights[0] := GetColumnsByWeight('A');
  Weights[1] := GetColumnsByWeight('B');
  Weights[2] := GetColumnsByWeight('C');
  Weights[3] := GetColumnsByWeight('D');

  SelectColumns := ProcessColumns(Weights);

  if FUseTempTable then
    Result := Format(SelectTempTableFmt, [SelectColumns, FTable])
  else
    Result := sLineBreak + SelectColumns;
end;

function TFTSGenerator.TempTableDefinition: string;
const
  TempTableFmt = sLineBreak + '    WITH %s_temp(%s) AS (VALUES ('+ sLineBreak + '%s))';
  NumRegexFmt = '      regexp_replace(NEW.%s, ''[^0-9]'', '''', ''g'')';
var
  Column: TFTSColumn;
  NumericColumns: TArray<string>;
  ColumnNames: string;
  I: Integer;
begin
  NumericColumns := [];
  for Column in FColumns do
    if Column.typ = ftNumeric then
      NumericColumns := NumericColumns + [Column.name];

  if Length(NumericColumns) = 0 then
    Exit('');

  ColumnNames := ''.Join(',', NumericColumns);

  for I := 0 to Pred(Length(NumericColumns)) do
    NumericColumns[I] := Format(NumRegexFmt, [NumericColumns[I]]);

  Result := Format(TempTableFmt, [FTable, ColumnNames, ''.Join(',' + sLineBreak, NumericColumns)]);
end;

procedure TFTSGenerator.DefineColumns;
var
  StringList: TStringList;
  Line: string;

  function ColumnType(typ: string): TFTSFieldType;
  begin
    if (typ.StartsWith('char')) or (typ.StartsWith('text')) or (typ.StartsWith('memo')) then
      Exit(ftText);

    Result := ftNumeric;
  end;

  function ColumnWeight: string;
  begin
    if Line.Contains('#') then
      Exit(Copy(Line, Pos('#', Line) + 1, 1));

    Result := 'D';
  end;

  procedure AddColumn;
  var
    Items: TArray<string>;
    Field: TFTSColumn;
  begin
    Items := Line.Trim.Split([' ']);

    Field.name := Items[0];
    Field.typ := ColumnType(Items[1]);
    Field.weight := ColumnWeight;

    FColumns := FColumns + [Field];
  end;

begin
  FColumns := [];

  StringList := TStringList.Create;
  try
    StringList.LineBreak := sLineBreak;
    StringList.Text := FRawColumns;

    for Line in StringList.ToStringArray do
      AddColumn;
  finally
    StringList.Free;
  end;
end;

end.
