unit KM_GUIGameHouseArena;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes, KM_ResDevelopment,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HouseArena;

type

  TKMGuiGameArena = class(TKMPanel)
    private
      procedure Refresh(Arena : TKMHouseArena);

      procedure SelectType_Click(Sender: TObject);
      procedure Start_Click(Sender : TObject);
    protected
      Button_Points : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMButtonFlat;
        Button_FestivalType : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX_ALL] of TKMButtonFlat;
        WareRow_Cost : array[0..1] of TKMWaresRow;
        Button_StartFestival : TKMButton;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;

implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts;


constructor TKMGuiGameArena.Create(aParent: TKMPanel);
var dtt: TKMDevelopmentTreeType;
  I, top : Integer;
  WT : TKMWareType;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

  TKMLabel.Create(self, 0, 5, Width, 20, gResTexts[2302], fntMetal, taCenter);
  for dtt := Low(Button_Points) to High(Button_Points) do
  begin
    I := byte(dtt);
    Button_Points[dtt] := TKMButtonFlat.Create(self, I * 39 - 5, 30, 37, 35, TREE_TYPE_ICON[dtt]);
    Button_Points[dtt].Caption := '';
    Button_Points[dtt].Hint := gResTexts[2300]+ ' ' + gResTexts[TREE_TYPE_HINT[dtt]];
    Button_Points[dtt].Tag := I;
  end;

  top := Button_Points[DEVELOPMENT_MIN].Bottom + 5;
  TKMLabel.Create(self, 0, top, Width, 20, gResTexts[2303], fntMetal, taCenter);
  for dtt := Low(Button_FestivalType) to High(Button_FestivalType) do
  begin
    I := byte(dtt) - 1;
    Button_FestivalType[dtt] := TKMButtonFlat.Create(self, I * 39 + 10 + 5, top + 20, 37, 35, TREE_TYPE_ICON[dtt]);
    case dtt of
      dttAll : Button_FestivalType[dtt].Hint := gResTexts[2300]+ ' ' + gResTexts[TREE_TYPE_HINT[dtt]] + ' x1';
      else Button_FestivalType[dtt].Hint := gResTexts[2300]+ ' ' + gResTexts[TREE_TYPE_HINT[dtt]] + ' x3';
    end;
    Button_FestivalType[dtt].Tag := I + 1;
    Button_FestivalType[dtt].OnClick := SelectType_Click;

  end;
  top := Button_FestivalType[DEVELOPMENT_MIN].Bottom + 5;
  TKMLabel.Create(self, 0, top, Width, 20, gResTexts[154], fntMetal, taLeft);
  Inc(top, 20);
  for I := Low(WareRow_Cost) to High(WareRow_Cost) do
  begin
    If I = 0 then
      WT := wtFood
    else
      WT := wtWarfare;
    WareRow_Cost[I] := TKMWaresRow.Create(self, 0, top + 28 * I, Width);
    WareRow_Cost[I].WareCntAsNumber := true;
    WareRow_Cost[I].TexID := gRes.Wares[WT].GUIIcon;
    WareRow_Cost[I].Hint := gResTexts[1907] + ': ' + gRes.Wares[WT].Title;
  end;

  inc(top, 55);

  Button_StartFestival := TKMButton.Create(self, 0, top, Width, 25, gResTexts[2301], bsGame);
  Button_StartFestival.OnClick := Start_Click;

end;

procedure TKMGuiGameArena.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseArena(aHouse));
end;

procedure TKMGuiGameArena.Refresh(Arena: TKMHouseArena);
var dtt: TKMDevelopmentTreeType;
  I : integer;
begin

  for dtt := Low(Button_Points) to High(Button_Points) do
  begin
    Button_Points[dtt].Caption := gMySpectator.Hand.DevPoints(dtt).ToString;
  end;

  for dtt := Low(Button_FestivalType) to High(Button_FestivalType) do
  begin
    Button_FestivalType[dtt].Enabled := not Arena.FestivalStarted;
    Button_FestivalType[dtt].BackBevelColor := IfThen(Arena.FestivalType = dtt, $A5FFAF00, $00000000)
  end;

  for I := Low(WareRow_Cost) to High(WareRow_Cost) do
    WareRow_Cost[I].Enabled := not Arena.FestivalStarted;
  WareRow_Cost[0].WareCount := Arena.FoodCost;
  WareRow_Cost[1].WareCount := Arena.WarfareCost;
  Button_StartFestival.Enabled := not Arena.FestivalStarted and Arena.CanStartFestival;
end;

procedure TKMGuiGameArena.SelectType_Click(Sender: TObject);
var H : TKMHouseArena;
begin
  H := TKMHouseArena(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicArenaSelectFestival, H, TKMControl(Sender).Tag);
  //H.FestivalType := TKMDevelopmentTreeType(TKMControl(Sender).Tag);
  Refresh(H);
end;

procedure TKMGuiGameArena.Start_Click(Sender: TObject);
var H : TKMHouseArena;
begin
  H := TKMHouseArena(gMySpectator.Selected);
  //H.StartFestival;
  gGame.GameInputProcess.CmdHouse(gicArenaStartFestival, H);
  Refresh(H);
end;



end.

