unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,AG_Otimizations,BitArray,Generics.Collections,
  Math, Vcl.StdCtrls,ProblemGerator, Vcl.ImgList, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.ComCtrls, Vcl.Samples.Spin, sLabel, sEdit, sSpinEdit, sComboBoxes,
  acAlphaImageList, sCheckBox, sSkinProvider, sSkinManager;

type

  TStringGrid = class(Vcl.grids.TStringGrid)
    private
      FImages: TImageList;
    public
      property Images: TImageList read FImages write FImages;
      function Substr(TagBg,TagEd,s:string):string;
  end;

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    sgResult: TStringGrid;
    ImageList1: TImageList;
    GroupBox2: TGroupBox;
    btRun: TButton;
    spPop: TsSpinEdit;
    Label1: TsLabel;
    Label2: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    spMaxGerations: TsSpinEdit;
    Label5: TsLabel;
    spElitism: TsSpinEdit;
    Label6: TsLabel;
    spDelay: TsSpinEdit;
    spCrossRate: TsDecimalSpinEdit;
    spMutateRate: TsDecimalSpinEdit;
    Label8: TsLabel;
    sLabel1: TsLabel;
    cbSelectMode: TsComboBoxEx;
    cbCrossMode: TsComboBoxEx;
    sAlphaImageList1: TsAlphaImageList;
    sLabel2: TsLabel;
    lbTorneio: TsLabel;
    ckStdDesv: TsCheckBox;
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure sgResultDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cbSelectModeChange(Sender: TObject);
    procedure ckStdDesvClick(Sender: TObject);
  private
    procedure updateGrid(Best: longword);
    procedure UpdateAGSettings(Ag: TAlgGenetic);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  POTENCIAS,DEMANDAS:TList<Integer>;
  POTENCIA_TOTAL:integer;
  EnableDesStd:boolean =true;


implementation

{$R *.dfm}

function sum(List:TList<Integer>):Integer;
var
  i: Integer;
begin
  Result:= 0;
  if List.Count = 0  then exit;

  for i := 0 to List.Count-1 do
    Result:= Result+ List[i];
end;

function Fitness(x: TIndividuo): Double;
var
  bits:TBitArray;
  potPerdida,pl: TList<Integer>;
  i,val,instante,gerador:LongWord;
  f,avg,variance,stdDev:Double;
begin
  bits := TBitArray.Create(x);
  potPerdida:= TList<Integer>.Create;
  pl:= TList<Integer>.Create;
  for i := 0 to 3 do
  begin
    potPerdida.Add(0);
    pl.add(0);
  end;

  for i := 0 to 27 do
  begin
    val:= bits[i];
    if val = 0 then
    begin
      instante := i mod 4;
      gerador  := i div 4;
      potPerdida[instante]:= potPerdida[instante]+POTENCIAS[gerador];
    end;
  end;

  f := 130;
  for i := 0 to 3 do
  begin
    pl[i]:= POTENCIA_TOTAL - potPerdida[i] - DEMANDAS[i];
    if pl[i] <= 0 then
      f:= f - 40;
  end;

  f:= f+sum(pl);

  if not EnableDesStd then
    exit(f);

  avg:= sum(pl)/pl.Count;

  variance:=0;
  for i := 0 to pl.Count-1 do
    variance:= variance+math.Power(avg-pl[i],2);

  variance:= variance/(pl.Count-1);

  stdDev := sqrt(variance);

  f:= f+(180-stdDev)*20;
  Result:= 0.5 * f;
end;

procedure EvalPot(Individuo:TIndividuo;PL,PT,PP,PD:TList<Integer>);
var
  i,j,k: Integer;
begin


 for i := 0 to 3 do
 begin
   PL.Add(0);
   PP.Add(0);
   PT.Add(0);
   PD.Add(DEMANDAS[i]);
 end;

 with TBitArray.Create(Individuo) do
 begin
  for i := 0 to 27 do
  begin
    j:= i mod 4;
    k:= i div 4;

    if(Item[i]) = 0 then
      PP[j]:= PP[j]+ POTENCIAS[k];
  end;
  Free;
 end;

 for i := 0 to 3 do
 begin
   PL[i]:= POTENCIA_TOTAL-PP[i]-PD[i];
 end;
end;

procedure TForm1.updateGrid(Best: longword);
var
  i,j,k:integer;
  PL,PT,PP,PD:Tlist<Integer>;
begin
 PL:= TList<Integer>.Create;
 PP:= TList<Integer>.Create;
 PT:= TList<Integer>.Create;
 PD:= TList<Integer>.Create;

 with TBitArray.Create(Best) do
 begin
  for i := 0 to 27 do
  begin
     j:= i mod 4;
     k:= i div 4;
     sgResult.Cells[j+1,k+1]:= format('<IMG%d/>',[item[i]]);
  end;
 end;

 EvalPot(Best,PL,PT,PP,PD);

 for i := 0 to 3 do
 begin
   sgResult.Cells[i+1,8]  := IntToStr(POTENCIA_TOTAL-PP[i]);
   sgResult.Cells[i+1,9]  := IntToStr(PD[i]);
   sgResult.Cells[i+1,10] := IntToStr(PL[i]);
   sgResult.Cells[i+1,0]    := (i+1).ToString();
 end;

 for i := 1 to 7 do
   sgResult.Cells[0,i]:= Format('G%d',[i]);

 sgResult.Cells[0,8]:= 'PT-PP';
 sgResult.Cells[0,9]:= 'PD';
 sgResult.Cells[0,10]:= 'PL';

 sgResult.Cells[0,0]:= 'INTER.:';



 sgResult.Paint;

 PL.Free;
 PT.Free;
 PP.Free;
 PD.Free;
end;


procedure TForm1.UpdateAGSettings(Ag:TAlgGenetic);
begin
  with AG do
  begin
    PopulationSize:= spPop.Value;
    ElitsmFactor:= spElitism.Value/PopulationSize;
    Pm:= spMutateRate.Value;
    pc:= spCrossRate.Value;
    TorneioFactor:= spTorneioSize.Value;

    case cbCrossMode.ItemIndex of
      0: CrossoverFn:= TBitAlgGenetic(Ag).crossoverSinglePoint;
      1: CrossoverFn:= TBitAlgGenetic(Ag).crossoverDoublePoint;
      2: CrossoverFn:= TBitAlgGenetic(Ag).crossoverMask;
    else
      CrossoverFn:= crossoverSinglePoint;
    end;

    case cbSelectMode.ItemIndex of
      0: SelectFn:= Roleta;
      1: SelectFn:= Ranking;
      2: SelectFn:= Torneio;
    else
      SelectFn:= Roleta;
    end;
  end;
end;

procedure TForm1.btRunClick(Sender: TObject);
var Ag:TBitAlgGenetic;
  i,j,k,delay,maxGeration: integer;
  Pop:TStringList;
  ind:TIndividuo;
  s:string;
  media:UInt64;
begin
 btRun.Enabled:= false;
 sgResult.Show;
 Ag:= TBitAlgGenetic.Create;
 Ag.Problem:= TProblemGerator.Create;
 Ag.NumeroBits:= 28;
 Ag.CrossoverFn:= Ag.crossoverSinglePoint;
 AG.FnFunction:= Fitness;
 delay:= spDelay.Value;
 maxGeration:= spMaxGerations.Value;
 Pop:=TStringList.Create;

 UpdateAGSettings(Ag);

 AG.CreatePopulation;
 Pop.Add('Geração Criada');
 for j := 0 to ag.PopulationSize-1 do
 begin
     ind:= Ag.Population[j];
     s:= ag.Problem.individualToString(ind);
     pop.Add(s);
 end;

 for i := 0 to maxGeration-1 do
 begin
   Pop.Add('Geração '+i.ToString());
   AG.Evolve();
   updateGrid(ag.BestIndividuo);

   media:= 0;
   for j := 0 to ag.PopulationSize-1 do
   begin
     ind:= Ag.Population[j];
     s:= ag.Problem.individualToString(ind);
     pop.Add(s);
     media:= media+ind;
   end;

   media:= media div ag.PopulationSize;

   for j := 0 to ag.PopulationSize-1 do
   begin

   end;


   sleep(delay);
   Application.ProcessMessages;
 end;



 Pop.SaveToFile(ChangeFileExt(ParamStr(0),'.txt'));
 Pop.Free;
 AG.Free;
 btRun.Enabled:= True;
end;


procedure TForm1.cbSelectModeChange(Sender: TObject);
begin
 lbTorneio.Enabled:= cbSelectMode.ItemIndex =2;
 spTorneioSize.Enabled:= lbTorneio.Enabled;
end;

procedure TForm1.ckStdDesvClick(Sender: TObject);
begin
  EnableDesStd:= ckStdDesv.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  POTENCIAS:= TList<Integer>.Create;
  POTENCIAS.Add(20);
  POTENCIAS.Add(15);
  POTENCIAS.Add(35);
  POTENCIAS.Add(40);
  POTENCIAS.Add(15);
  POTENCIAS.Add(15);
  POTENCIAS.Add(10);

  DEMANDAS:= TList<Integer>.Create;
  DEMANDAS.Add(80);
  DEMANDAS.Add(90);
  DEMANDAS.Add(65);
  DEMANDAS.Add(70);

  POTENCIA_TOTAL:= sum(POTENCIAS);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   POTENCIAS.Free;
   DEMANDAS.Free;
end;



procedure TForm1.sgResultDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  s: string;
  aCanvas: TCanvas;
  bmp:TBitmap;
  idx,x,y:integer;
  sg:TStringGrid;
begin
  sg:= (Sender as TStringGrid);
  s := sg.Cells[ACol, ARow];
  aCanvas := sg.Canvas;
//  aCanvas.FillRect(Rect);

  with aCanvas do
    begin
      Brush.Color := clBtnFace;
      Font.Style := [];
      if TryStrToInt(s,idx)and(idx<0) then
          Font.Color := clRed
      else
          Font.Color := clWindowText;
      Font.Name := sg.Font.Name;
      Font.Size := sg.Font.Size;
      FillRect(rect);
    end;

  x:= (Rect.Width - aCanvas.TextWidth(s)) div 2;
  y:= (Rect.Height - aCanvas.TextHeight(s)) div 2;
//  aCanvas.TextRect(Rect,x,y,s);



  if TryStrToInt(sg.Substr('<IMG','/>',s),idx) then
  begin
     bmp:= TBitmap.Create;
     ImageList1.GetBitmap(idx,bmp);
     aCanvas.Draw(Rect.Left, Rect.Top, bmp);
     bmp.Free;
  end
  else
  begin
   DrawText(acanvas.Handle, PChar(s),
             length(s), Rect,
             DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS);
  end;
    with aCanvas do
    begin
      Pen.Style := psSolid;
      Pen.Width := 1;
      Pen.Color := clWhite;
      Polyline([point(rect.left, rect.bottom),
               rect.TopLeft, point(rect.Right, rect.top)]);
      Pen.Color := clBtnShadow;
      Polyline([point(rect.left+1, rect.bottom-1),
               point(rect.right-1, rect.bottom-1),
               point(rect.Right-1, rect.Top+1)]);
    end;
//   aCanvas.TextRect(Rect,x,y,s);
end;


{ TStringGrid }

function TStringGrid.Substr(TagBg, TagEd, s: string): string;
var p1,p2:integer;
begin
  p1:= pos(TagBg,s);
  p2:= pos(TagEd,s);

  if p1*p2 = 0 then exit('');
  p1:= p1+Length(TagBg);

  Result:= Copy(s,p1,p2-p1);
end;

end.
