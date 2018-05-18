unit ProblemGerator;

interface
uses SysUtils,Classes,Generics.Collections,AG_Comuns,BitArray;

Type
  TIndividuo = LongWord;
  TProblemGerator =class
    private
     FValidIndividuosA: TList<TIndividuo>;
     FValidIndividuosB: TList<TIndividuo>;
     function bin2dec(value: string): LongWord;
    public
      property validPatternsA: TList<TIndividuo> read FValidIndividuosA write FValidIndividuosA;
      property validPatternsB: TList<TIndividuo> read FValidIndividuosB write FValidIndividuosB;
      constructor Create;
      destructor Destroy;override;
      function randomIndividual:TIndividuo;
      function individualToString(ind:LongWord):string;
  end;

implementation

{ TProblemGerator }

constructor TProblemGerator.Create;
var i:integer;
begin
  validPatternsA:= TList<LongWord>.Create;
  validPatternsA.Add(bin2dec('1100'));
  validPatternsA.Add(bin2dec('0011'));
  validPatternsA.Add(bin2dec('1001'));
  validPatternsB:= TList<LongWord>.Create;
  validPatternsB.Add(bin2dec('0111'));
  validPatternsB.Add(bin2dec('1011'));
  validPatternsB.Add(bin2dec('1101'));
  validPatternsB.Add(bin2dec('1110'));
end;

destructor TProblemGerator.Destroy;
begin
  validPatternsA.Free;
  validPatternsB.Free;
  inherited;
end;

function TProblemGerator.individualToString(ind: LongWord): string;
begin
  with TBitArray.Create(ind) do
  begin
    Result:= toBinString(28,4);
    free;
  end;
end;

function TProblemGerator.randomIndividual: TIndividuo;
var g1,g2,i:Integer;
    individual,ind:TIndividuo;
    s:string;
begin
   g1:= choice(validPatternsA);
   g2:= choice(validPatternsA);
   individual:= g1 + (g2 shl 4);
   for i := 2 to 6 do
   begin
     ind:= choice(validPatternsB);
     if ind = 15 then
       ind:= ind;
     individual:= individual or (ind shl (4*i));
   end;

   for i := 0 to 6 do
     if individual and ($F shl (4*i)) = 15 then
      with TBitArray.Create(individual) do
      begin
        s:= toBinString(28,4);
        free;
      end;


   Result:= individual;
   if Result = 15 then
     ;
end;

function TProblemGerator.bin2dec(value: string): LongWord;
var
  i, iValueSize: Integer;
begin
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
    if Value[i] = '1' then Result := Result + (1 shl (iValueSize - i));
end;

end.
