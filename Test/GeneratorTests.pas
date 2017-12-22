unit GeneratorTests;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Generator;

type
  [TestFixture]
  TFTSGeneratorTests = class
  strict private
    FFTSGenerator: TFTSGenerator;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  published
    procedure GenerateData_FewTextColumns_DataGenerated;
    procedure GenerateData_SeveralTextColumns_DataGenerated;
  end;

implementation

procedure TFTSGeneratorTests.GenerateData_SeveralTextColumns_DataGenerated;
var
  RawColumns, ReturnValue, Expected: string;
begin
  RawColumns :=
'    nome character varying(50) NOT NULL, #A'+ sLineBreak +
'    endereco character varying(50), #B'+ sLineBreak +
'    numero character varying(10), #B'+ sLineBreak +
'    complemento character varying(20), #B'+ sLineBreak +
'    bairro character varying(30), #B'+ sLineBreak +
'    cep character(8), #B'+ sLineBreak +
'    email character varying(200), #B'+ sLineBreak +
'    observacoes memo, #B'+ sLineBreak +
'    cpf character(11), #B'+ sLineBreak +
'    rg character varying(20), #B'+ sLineBreak +
'    numero_passaporte character varying(20), #B'+ sLineBreak +
'    fone_comercial character varying(20), #C'+ sLineBreak +
'    fone_residencial character varying(20), #C'+ sLineBreak +
'    fone_celular character varying(20), #C'+ sLineBreak +
'    orgao_emissor_rg character varying(10), #D'+ sLineBreak +
'    estado_civil character(2), #C'+ sLineBreak +
'    razao_social character varying(100), #A'+ sLineBreak +
'    ie character varying(20), #C'+ sLineBreak +
'    fone character varying(20), #C'+ sLineBreak +
'    fax character varying(20), #C'+ sLineBreak +
'    website character varying(50), #B'+ sLineBreak +
'    cnpj character(14), #B';

  ReturnValue := FFTSGenerator.GenerateData('pessoa', RawColumns);

  Expected :=
    'ALTER TABLE pessoa ADD COLUMN fts_document tsvector;'+ sLineBreak +
    'CREATE INDEX pessoa_fts_index ON pessoa USING gist(fts_document);'+ sLineBreak +
    'CREATE OR REPLACE FUNCTION pessoa_fts_document_trigger() RETURNS TRIGGER AS $$'+ sLineBreak +
    'BEGIN'+ sLineBreak +
    '  NEW.FTS_DOCUMENT = ('+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      nome,'+ sLineBreak +
    '      razao_social), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''A'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',' + sLineBreak +
    '      endereco,'+ sLineBreak +
    '      numero,'+ sLineBreak +
    '      complemento,'+ sLineBreak +
    '      bairro,'+ sLineBreak +
    '      cep,'+ sLineBreak +
    '      email,'+ sLineBreak +
    '      observacoes,'+ sLineBreak +
    '      cpf,'+ sLineBreak +
    '      rg,'+ sLineBreak +
    '      numero_passaporte,'+ sLineBreak +
    '      website,'+ sLineBreak +
    '      cnpj), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''B'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      fone_comercial,'+ sLineBreak +
    '      fone_residencial,'+ sLineBreak +
    '      fone_celular,'+ sLineBreak +
    '      estado_civil,'+ sLineBreak +
    '      ie,'+ sLineBreak +
    '      fone,'+ sLineBreak +
    '      fax), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''C'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',orgao_emissor_rg), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''D''));'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END;';

  Assert.AreEqual(Expected, ReturnValue);
end;

procedure TFTSGeneratorTests.SetUp;
begin
  FFTSGenerator := TFTSGenerator.Create;
end;

procedure TFTSGeneratorTests.TearDown;
begin
  FFTSGenerator.Free;
  FFTSGenerator := nil;
end;

procedure TFTSGeneratorTests.GenerateData_FewTextColumns_DataGenerated;
var
  RawColumns, ReturnValue: string;
begin
  RawColumns :=
    '    nome character varying(50) NOT NULL, #A'+ sLineBreak +
    '    endereco character varying(50),';

  ReturnValue := FFTSGenerator.GenerateData('pessoa', RawColumns);

  Assert.AreEqual(
    'ALTER TABLE pessoa ADD COLUMN fts_document tsvector;'+ sLineBreak +
    'CREATE INDEX pessoa_fts_index ON pessoa USING gist(fts_document);'+ sLineBreak +
    'CREATE OR REPLACE FUNCTION pessoa_fts_document_trigger() RETURNS TRIGGER AS $$'+ sLineBreak +
    'BEGIN'+ sLineBreak +
    '  NEW.FTS_DOCUMENT = ('+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',nome), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''A'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',endereco), ''[^a-zA-ZÀ-ÿ0-9\s]'', '' '', ''g'')), ''D''));'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END;', ReturnValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TFTSGeneratorTests);
end.

