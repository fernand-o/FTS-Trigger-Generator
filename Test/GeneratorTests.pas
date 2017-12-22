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
    procedure GenerateData_NumericAndTextColumns_DataGenerated;
  end;

implementation

procedure TFTSGeneratorTests.GenerateData_NumericAndTextColumns_DataGenerated;
var
  RawColumns, ReturnValue, Expected: string;
begin
  RawColumns :=
    '    nome character varying(50) NOT NULL, #A'+ sLineBreak +
    '    endereco character varying(50), #B'+ sLineBreak +
    '    numero integer varying(10), #B'+ sLineBreak +
    '    complemento character varying(20), #B'+ sLineBreak +
    '    bairro character varying(30), #B'+ sLineBreak +
    '    cep integer(8), #B'+ sLineBreak +
    '    email character varying(200), #B'+ sLineBreak +
    '    observacoes memo, #B'+ sLineBreak +
    '    cpf integer(11), #B'+ sLineBreak +
    '    rg integer varying(20), #B'+ sLineBreak +
    '    numero_passaporte character varying(20), #B'+ sLineBreak +
    '    fone_comercial integer varying(20), #C'+ sLineBreak +
    '    fone_residencial integer varying(20), #C'+ sLineBreak +
    '    fone_celular integer varying(20), #C'+ sLineBreak +
    '    orgao_emissor_rg character varying(10), #D'+ sLineBreak +
    '    estado_civil character(2), #C'+ sLineBreak +
    '    razao_social character varying(100), #A'+ sLineBreak +
    '    ie character varying(20), #C'+ sLineBreak +
    '    fone integer varying(20), #C'+ sLineBreak +
    '    fax integer varying(20), #C'+ sLineBreak +
    '    website character varying(50), #B'+ sLineBreak +
    '    cnpj integer(14), #B';

  ReturnValue := FFTSGenerator.GenerateData('pessoa', RawColumns);

  Expected :=
    'ALTER TABLE pessoa ADD COLUMN fts_document tsvector;'+ sLineBreak +
    'CREATE INDEX pessoa_fts_index ON pessoa USING gist(fts_document);'+ sLineBreak +
    'CREATE OR REPLACE FUNCTION pessoa_fts_document_trigger() RETURNS TRIGGER AS $$'+ sLineBreak +
    'BEGIN'+ sLineBreak +
    '  NEW.FTS_DOCUMENT = ('+ sLineBreak +
    '    WITH pessoa_temp(numero,cep,cpf,rg,fone_comercial,fone_residencial,fone_celular,fone,fax,cnpj) AS (VALUES ('+ sLineBreak +
    '      regexp_replace(NEW.numero, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.cep, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.cpf, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.rg, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.fone_comercial, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.fone_residencial, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.fone_celular, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.fone, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.fax, ''[^0-9]'', '''', ''g''),'+ sLineBreak +
    '      regexp_replace(NEW.cnpj, ''[^0-9]'', '''', ''g'')))'+ sLineBreak +
    '    SELECT'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      nome,'+ sLineBreak +
    '      razao_social), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''A'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', coalesce(concat_ws('' '','+ sLineBreak +
    '      pessoa_temp.numero,'+ sLineBreak +
    '      reverse(pessoa_temp.numero),'+ sLineBreak +
    '      pessoa_temp.cep,'+ sLineBreak +
    '      reverse(pessoa_temp.cep),'+ sLineBreak +
    '      pessoa_temp.cpf,'+ sLineBreak +
    '      reverse(pessoa_temp.cpf),'+ sLineBreak +
    '      pessoa_temp.rg,'+ sLineBreak +
    '      reverse(pessoa_temp.rg),'+ sLineBreak +
    '      pessoa_temp.cnpj,'+ sLineBreak +
    '      reverse(pessoa_temp.cnpj)),'''')), ''B'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      endereco,'+ sLineBreak +
    '      complemento,'+ sLineBreak +
    '      bairro,'+ sLineBreak +
    '      email,'+ sLineBreak +
    '      observacoes,'+ sLineBreak +
    '      numero_passaporte,'+ sLineBreak +
    '      website), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''B'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', coalesce(concat_ws('' '','+ sLineBreak +
    '      pessoa_temp.fone_comercial,'+ sLineBreak +
    '      reverse(pessoa_temp.fone_comercial),'+ sLineBreak +
    '      pessoa_temp.fone_residencial,'+ sLineBreak +
    '      reverse(pessoa_temp.fone_residencial),'+ sLineBreak +
    '      pessoa_temp.fone_celular,'+ sLineBreak +
    '      reverse(pessoa_temp.fone_celular),'+ sLineBreak +
    '      pessoa_temp.fone,'+ sLineBreak +
    '      reverse(pessoa_temp.fone),'+ sLineBreak +
    '      pessoa_temp.fax,'+ sLineBreak +
    '      reverse(pessoa_temp.fax)),'''')), ''C'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      estado_civil,'+ sLineBreak +
    '      ie), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''C'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',orgao_emissor_rg), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''D'')'+ sLineBreak +
    '    FROM pessoa_temp);'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END;';

  Assert.AreEqual(Expected, ReturnValue);
end;

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
    '      razao_social), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''A'') ||'+ sLineBreak +
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
    '      cnpj), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''B'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '','+ sLineBreak +
    '      fone_comercial,'+ sLineBreak +
    '      fone_residencial,'+ sLineBreak +
    '      fone_celular,'+ sLineBreak +
    '      estado_civil,'+ sLineBreak +
    '      ie,'+ sLineBreak +
    '      fone,'+ sLineBreak +
    '      fax), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''C'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',orgao_emissor_rg), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''D''));'+ sLineBreak +
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
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',nome), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''A'') ||'+ sLineBreak +
    '    setweight(to_tsvector(''pt'', regexp_replace(concat_ws('' '',endereco), ''[^a-zA-Z�-�0-9\s]'', '' '', ''g'')), ''D''));'+ sLineBreak +
    '  RETURN NEW;'+ sLineBreak +
    'END;', ReturnValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TFTSGeneratorTests);
end.

