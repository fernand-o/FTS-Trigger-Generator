unit Generator;

interface

type
  TFTSFieldType = (ftText, ftTextAsNumeric, ftInteger);

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
    procedure GenerateFTSFunction;
    procedure GenerateTrigger;
    procedure GenerateUpdateCommand;
    function TempTableDefinition: string;
    function SelectWithWeights: string;
    function ProcessColumns(Weights: TArray<TArray<TFTSColumn>>): string;
  public
    function GenerateData(Table, RawColumns: string): string;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils;

procedure TFTSGenerator.AddResult(const ValueFmt: string; const Args: array of const);
begin
  if Length(FResults) > 0 then
    FResults := FResults + [''];

  FResults := FResults + ['-----------------------------------------------------------------'];
  FResults := FResults + [Format(ValueFmt, Args)];
end;

function TFTSGenerator.GenerateData(Table, RawColumns: string): string;
begin
  FTable := Table;
  FRawColumns := RawColumns;

  GenerateMetadata;
  GenerateFTSFunction;
  GenerateTrigger;
  GenerateUpdateCommand;

  Result := ''.Join(sLineBreak, FResults);
end;

procedure TFTSGenerator.GenerateMetadata;
begin
  AddResult('ALTER TABLE %s ADD COLUMN fts_document tsvector;', [FTable]);
  AddResult('CREATE INDEX %s_fts_index ON %s USING gist(fts_document);', [FTable, FTable]);
end;

procedure TFTSGenerator.GenerateTrigger;
const
  TriggerFmt = 'CREATE TRIGGER %s_fts_update_trigger BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE PROCEDURE %s_fts_document_trigger();';
begin
  AddResult(TriggerFmt, [FTable, FTable, FTable]);
end;

procedure TFTSGenerator.GenerateUpdateCommand;
const
  UpdateFmt = 'UPDATE %s SET fts_document = '''';';
begin
  AddResult(UpdateFmt, [FTable]);
end;

procedure TFTSGenerator.GenerateFTSFunction;
const
  TriggerWrapperFmt =
    'CREATE OR REPLACE FUNCTION %s_fts_document_trigger() RETURNS TRIGGER AS $$'+ sLineBreak +
    'BEGIN'+ sLineBreak +
    '  NEW.FTS_DOCUMENT = (%s);'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END; $$ LANGUAGE plpgsql;';
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
  ItemTextFmt = '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',%s), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''%s'')';
  ItemNumFmt =  '    setweight(to_tsvector(''pt'', coalesce(concat_ws('' '',%s),'''')), ''%s'')';
var
  Numerics, Texts: TArray<TFTSColumn>;
  Column: TFTSColumn;
  I: Integer;
  Results: TArray<string>;

  procedure AddNumericColumns;
  const
    NumericAsTextFmt = '%s_temp.%s';
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
      Col := Format(NumericAsTextFmt, [FTable, NumericColumn.name]);
      Cols := Cols + [Col];
      Cols := Cols + [Format(ReverseFmt, [Col])];
    end;
    Cols[0] := sLineBreak +'      '+ Cols[0];

    Results := Results + [Format(ItemNumFmt, [''.Join(',' + sLineBreak  + '      ', Cols), NumericColumn.weight])];
  end;

  procedure AddTextColumns;
  const
    ColumnFmt = 'NEW.%s';
  var
    TextColumn: TFTSColumn;
    Cols: TArray<string>;
  begin
    if Length(Texts) = 0 then
      Exit;

    for TextColumn in Texts do
      Cols := Cols + [Format(ColumnFmt, [TextColumn.name])];

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
        ftTextAsNumeric, ftInteger: Numerics := Numerics + [Column];
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
  NumAsTextRegexFmt = '      regexp_replace(NEW.%s, ''[^0-9]'', '''', ''g'')';
  IntFmt = '      cast(NEW.%s as varchar(11))';
var
  Column: TFTSColumn;
  NumericColumns: TArray<TFTSColumn>;
  ColumnNames, ColumnFormattedNames: TArray<string>;
  NewName: string;
begin
  NumericColumns := [];
  for Column in FColumns do
    if Column.typ in [ftTextAsNumeric, ftInteger] then
      NumericColumns := NumericColumns + [Column];

  if Length(NumericColumns) = 0 then
    Exit('');

  ColumnNames := [];
  ColumnFormattedNames := [];
  for Column in NumericColumns do
  begin
    ColumnNames := ColumnNames + [Column.name];

    case Column.typ of
      ftTextAsNumeric: NewName := Format(NumAsTextRegexFmt, [Column.name]);
      ftInteger: NewName := Format(IntFmt, [Column.name]);
    end;
    ColumnFormattedNames := ColumnFormattedNames + [NewName];
  end;

  Result := Format(TempTableFmt, [FTable,
    ''.Join(',', ColumnNames),
    ''.Join(',' + sLineBreak, ColumnFormattedNames)
  ]);
end;

procedure TFTSGenerator.DefineColumns;
var
  StringList: TStringList;
  Line: string;

  function ColumnType(typ: string): TFTSFieldType;
  begin
    if typ.StartsWith('astext') then
      Exit(ftTextAsNumeric);

    if typ.StartsWith('int') then
      Exit(ftInteger);

    Result := ftText;
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
