unit KM_GUIMapEdPlayerBlockUnit;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Controls, KM_ControlsBase,
  KM_Pics, KM_InterfaceGame;

type
  TKMMapEdPlayerBlockUnit = class
  private
    procedure Player_BlockUnitClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_Block_Over(Sender: TObject; Shift: TShiftState);
    procedure Player_BlockRefresh;
  protected
    Panel_BlockUnit: TKMPanel;
    Button_BlockUnit,
    Button_BlockBarracksWarriors,
    Button_BlockTHWarriors,
    Button_BlockSiegeMachines,
    Button_BlockPalaceUnits: array of TKMButtonFlat;

    Image_BlockUnit,
    Image_BlockBarracksWarriors,
    Image_BlockTHWarriors,
    Image_BlockSiegeMachines,
    Image_BlockPalaceUnits: array of TKMImage;
  public
    constructor Create(aParent: TKMPanel);
    procedure Show;
    function Visible: Boolean;
    procedure Hide;
    procedure UpdatePlayerColor;
  end;


implementation
uses
  KM_HandsCollection,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts, KM_ResTypes,
  KM_HandTypes,
  KM_Defaults;


{ TKMMapEdPlayerBlockUnit }
constructor TKMMapEdPlayerBlockUnit.Create(aParent: TKMPanel);
var
  I: Integer;
  top : Integer;
begin
  inherited Create;

  SetLength(Button_BlockUnit, length(SCHOOL_GAME_ORDER));
  SetLength(Button_BlockBarracksWarriors, length(BARRACKS_GAME_ORDER));
  SetLength(Button_BlockTHWarriors, length(TH_GAME_ORDER));
  SetLength(Button_BlockSiegeMachines, length(SIEGE_GAME_ORDER));
  SetLength(Button_BlockPalaceUnits, length(PALACE_UNITS_ORDER));

  SetLength(Image_BlockUnit, length(SCHOOL_GAME_ORDER));
  SetLength(Image_BlockBarracksWarriors, length(BARRACKS_GAME_ORDER));
  SetLength(Image_BlockTHWarriors, length(TH_GAME_ORDER));
  SetLength(Image_BlockSiegeMachines, length(SIEGE_GAME_ORDER));
  SetLength(Image_BlockPalaceUnits, length(PALACE_UNITS_ORDER));

  Panel_BlockUnit := TKMPanel.Create(aParent, 9, 28, aParent.Width - 9, 400);
  with TKMLabel.Create(Panel_BlockUnit, 0, PAGE_TITLE_Y, Panel_BlockUnit.Width, 0, gResTexts[TX_MAPED_BLOCK_UNITS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  for I := 0 to High(Button_BlockUnit) do
  begin
    Button_BlockUnit[I] := TKMButtonFlat.Create(Panel_BlockUnit, 9 + (I mod 5)*37,30+(I div 5)*37,33,33,gRes.Units[SCHOOL_GAME_ORDER[I]].GUIIcon);
    Button_BlockUnit[I].OnMouseDown := Player_BlockUnitClick;
    Button_BlockUnit[I].OnMouseOver := Player_Block_Over;
    Button_BlockUnit[I].Tag := byte(SCHOOL_GAME_ORDER[I]);
    Button_BlockUnit[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    Button_BlockUnit[I].Hint := gRes.Units[SCHOOL_GAME_ORDER[I]].GUIName;

    Image_BlockUnit[I] := TKMImage.Create(Panel_BlockUnit, 9 + (I mod 5)*37 + 15,30+(I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockUnit[I].Hitable := False;
    Image_BlockUnit[I].ImageCenter;
  end;
  top := Button_BlockUnit[high(Button_BlockUnit)].Bottom + 10;

  TKMLabel.Create(Panel_BlockUnit, 9, top, Panel_BlockUnit.Width - 9, 0, gResTexts[TX_MAPED_BLOCK_UNITS_IN_BARRACKS], fntMetal, taLeft);
  for I := 0 to High(Button_BlockBarracksWarriors) do
  begin
    Button_BlockBarracksWarriors[I] := TKMButtonFlat.Create(Panel_BlockUnit,9 + (I mod 5)*37,top + 20 +(I div 5)*37,33,33,
                                                            gRes.Units[BARRACKS_GAME_ORDER[I]].GUIIcon, rxGui);
    Button_BlockBarracksWarriors[I].Hint := gRes.Units[BARRACKS_GAME_ORDER[I]].GUIName;
    Button_BlockBarracksWarriors[I].Tag := byte(BARRACKS_GAME_ORDER[I]);
    Button_BlockBarracksWarriors[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    Button_BlockBarracksWarriors[I].OnMouseDown := Player_BlockUnitClick;
    Button_BlockBarracksWarriors[I].OnMouseOver := Player_Block_Over;
    Image_BlockBarracksWarriors[I] := TKMImage.Create(Panel_BlockUnit, 9 + (I mod 5)*37 + 15,top + 20 +(I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockBarracksWarriors[I].Hitable := False;
    Image_BlockBarracksWarriors[I].ImageCenter;
  end;
  top := Button_BlockBarracksWarriors[high(Button_BlockBarracksWarriors)].Bottom + 10;

  TKMLabel.Create(Panel_BlockUnit, 9, top, Panel_BlockUnit.Width - 9, 0, gResTexts[TX_MAPED_BLOCK_UNITS_IN_TOWNHALL], fntMetal, taLeft);
  for I := 0 to High(Button_BlockTHWarriors) do
  begin
    Button_BlockTHWarriors[I] := TKMButtonFlat.Create(Panel_BlockUnit, 9 + (I mod 5)*37,top + 20 +(I div 5)*37,33,33, gRes.Units[TH_GAME_ORDER[I]].GUIIcon, rxGui);
    Button_BlockTHWarriors[I].Hint := gRes.Units[TH_GAME_ORDER[I]].GUIName;
    Button_BlockTHWarriors[I].Tag := byte(TH_GAME_ORDER[I]);
    Button_BlockTHWarriors[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    Button_BlockTHWarriors[I].OnMouseDown := Player_BlockUnitClick;
    Button_BlockTHWarriors[I].OnMouseOver := Player_Block_Over;
    Image_BlockTHWarriors[I] := TKMImage.Create(Panel_BlockUnit, 9 + (I mod 5)*37 + 15,top + 20 +(I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockTHWarriors[I].Hitable := False;
    Image_BlockTHWarriors[I].ImageCenter;
  end;
  top := Button_BlockTHWarriors[high(Button_BlockTHWarriors)].Bottom + 10;

  TKMLabel.Create(Panel_BlockUnit, 9, top, Panel_BlockUnit.Width - 9, 0, gResTexts[1841], fntMetal, taLeft);
  for I := 0 to High(Button_BlockSiegeMachines) do
  begin
    Button_BlockSiegeMachines[I] := TKMButtonFlat.Create(Panel_BlockUnit, 9 + (I mod 5)*37,top + 20 +(I div 5)*37,33,33, gRes.Units[SIEGE_GAME_ORDER[I]].GUIIcon, rxGui);
    Button_BlockSiegeMachines[I].Hint := gRes.Units[SIEGE_GAME_ORDER[I]].GUIName;
    Button_BlockSiegeMachines[I].Tag := byte(SIEGE_GAME_ORDER[I]);
    Button_BlockSiegeMachines[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    Button_BlockSiegeMachines[I].OnMouseDown := Player_BlockUnitClick;
    Button_BlockSiegeMachines[I].OnMouseOver := Player_Block_Over;

    Image_BlockSiegeMachines[I] := TKMImage.Create(Panel_BlockUnit, 9 + (I mod 5)*37 + 15,top + 20 +(I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockSiegeMachines[I].Hitable := False;
    Image_BlockSiegeMachines[I].ImageCenter;
  end;
  top := Button_BlockSiegeMachines[high(Button_BlockSiegeMachines)].Bottom + 10;

  TKMLabel.Create(Panel_BlockUnit, 9, top, Panel_BlockUnit.Width - 9, 0, gResTexts[1882], fntMetal, taLeft);
  for I := 0 to High(Button_BlockPalaceUnits) do
  begin
    Button_BlockPalaceUnits[I] := TKMButtonFlat.Create(Panel_BlockUnit, 9 + (I mod 5)*37,top + 20 +(I div 5)*37,33,33, gRes.Units[PALACE_UNITS_ORDER[I]].GUIIcon, rxGui);
    Button_BlockPalaceUnits[I].Hint := gRes.Units[PALACE_UNITS_ORDER[I]].GUIName;
    Button_BlockPalaceUnits[I].Tag := byte(PALACE_UNITS_ORDER[I]);
    Button_BlockPalaceUnits[I].Tag2 := BUTTON_BLOCK_Unit_TAG_2;
    Button_BlockPalaceUnits[I].OnMouseDown := Player_BlockUnitClick;
    Button_BlockPalaceUnits[I].OnMouseOver := Player_Block_Over;

    Image_BlockPalaceUnits[I] := TKMImage.Create(Panel_BlockUnit, 9 + (I mod 5)*37 + 15,top + 20 + (I div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockPalaceUnits[I].Hitable := False;
    Image_BlockPalaceUnits[I].ImageCenter;
  end;
end;


procedure TKMMapEdPlayerBlockUnit.Player_BlockUnitClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I: Integer;
  U: TKMUnitType;
  UL : TKMHandUnitLock;
begin
  I := TKMButtonFlat(Sender).Tag;
  U := TKMUnitType(I);
  UL := gMySpectator.Hand.Locks.GetUnitBlocked(U);
  if ssRight in Shift then
    gMySpectator.Hand.Locks.SetUnitBlocked(TKMHandUnitLock( (1 + Ord(UL) + 1) mod 3), U)
  else
    gMySpectator.Hand.Locks.SetUnitBlocked(TKMHandUnitLock( (1 + Ord(UL)) mod 3), U);


  Player_BlockRefresh;
end;

procedure TKMMapEdPlayerBlockUnit.Player_BlockRefresh;
var
  K: Integer;
  W: TKMUnitType;
begin
  for K := 0 to High(TH_GAME_ORDER) do
  begin
    W := TH_GAME_ORDER[K];

    case gMySpectator.Hand.Locks.GetUnitBlocked(W) of
      ulBlocked : Image_BlockTHWarriors[K].TexID := 32;
      ulNotVisible : Image_BlockTHWarriors[K].TexID := 91;
      else Image_BlockTHWarriors[K].TexID := 0;
    end;
  end;

  for K := 0 to High(SIEGE_GAME_ORDER) do
  begin
    W := SIEGE_GAME_ORDER[K];
    case gMySpectator.Hand.Locks.GetUnitBlocked(W) of
      ulBlocked : Image_BlockSiegeMachines[K].TexID := 32;
      ulNotVisible : Image_BlockSiegeMachines[K].TexID := 91;
      else Image_BlockSiegeMachines[K].TexID := 0;
    end;
  end;

  for K := 0 to High(PALACE_UNITS_ORDER) do
  begin
    W := PALACE_UNITS_ORDER[K];
    case gMySpectator.Hand.Locks.GetUnitBlocked(W) of
      ulBlocked : Image_BlockPalaceUnits[K].TexID := 32;
      ulNotVisible : Image_BlockPalaceUnits[K].TexID := 91;
      else Image_BlockPalaceUnits[K].TexID := 0;
    end;
  end;

  for K := 0 to High(BARRACKS_GAME_ORDER) do
  begin
    W := BARRACKS_GAME_ORDER[K];
    case gMySpectator.Hand.Locks.GetUnitBlocked(W) of
      ulBlocked : Image_BlockBarracksWarriors[K].TexID := 32;
      ulNotVisible : Image_BlockBarracksWarriors[K].TexID := 91;
      else Image_BlockBarracksWarriors[K].TexID := 0;
    end;
  end;

  for K := low(Image_BlockUnit) to high(Image_BlockUnit) do
  begin
    W := SCHOOL_GAME_ORDER[K];
    case gMySpectator.Hand.Locks.GetUnitBlocked(W) of
      ulBlocked : Image_BlockUnit[K].TexID := 32;
      ulNotVisible : Image_BlockUnit[K].TexID := 91;
      else Image_BlockUnit[K].TexID := 0;
    end;
  end;

end;


procedure TKMMapEdPlayerBlockUnit.UpdatePlayerColor;
var
  I: Integer;
  col: Cardinal;
begin
  col := gMySpectator.Hand.FlagColor;

  for I := Low(Button_BlockUnit) to High(Button_BlockUnit) do
    Button_BlockUnit[I].FlagColor := col;
  for I := Low(Button_BlockBarracksWarriors) to High(Button_BlockBarracksWarriors) do
    Button_BlockBarracksWarriors[I].FlagColor := col;
  for I := Low(Button_BlockTHWarriors) to High(Button_BlockTHWarriors) do
    Button_BlockTHWarriors[I].FlagColor := col;

  for I := Low(Button_BlockSiegeMachines) to High(Button_BlockSiegeMachines) do
    Button_BlockSiegeMachines[I].FlagColor := col;

  for I := 0 to High(PALACE_UNITS_ORDER) do
    Button_BlockPalaceUnits[I].FlagColor := col;
end;

procedure TKMMapEdPlayerBlockUnit.Player_Block_Over(Sender: TObject; Shift: TShiftState);
var ctrlDown : TKMControl;
  ctrlOver : TKMControl;
  UT1, UT2 : TKMUnitType;
begin
  if Sender = nil then
    Exit;

  if not (sender is TKMButtonFlat) then
    Exit;

  if not (TKMButtonFlat(Sender).Tag2 = BUTTON_BLOCK_Unit_TAG_2) then
    Exit;

  ctrlDown := TKMButtonFlat(Sender).MasterPanel.MasterControl.CtrlDown;
  ctrlOver := TKMControl(Sender);
  if (ctrlDown = nil) or not (ctrlDown is TKMButtonFlat) then
    Exit;
  if ctrlDown = ctrlOver then
    Exit;

  if (TKMButtonFlat(ctrlDown).Tag2 = BUTTON_BLOCK_Unit_TAG_2) then
  begin
    UT1 := TKMUnitType(ctrlDown.Tag);
    UT2 := TKMUnitType(ctrlOver.Tag);
    if gMySpectator.Hand.Locks.GetUnitBlocked(UT1) <> gMySpectator.Hand.Locks.GetUnitBlocked(UT2) then
    begin
      //gMySpectator.Hand.Locks.HouseLock[H2] :=gMySpectator.Hand.Locks.HouseLock[H1];
      gMySpectator.Hand.Locks.SetUnitBlocked(gMySpectator.Hand.Locks.GetUnitBlocked(UT1), UT2);
      Player_BlockRefresh;
    end;
  end;


end;


procedure TKMMapEdPlayerBlockUnit.Show;
begin
  Player_BlockRefresh;
  Panel_BlockUnit.Show;
end;


procedure TKMMapEdPlayerBlockUnit.Hide;
begin
  Panel_BlockUnit.Hide;
end;


function TKMMapEdPlayerBlockUnit.Visible: Boolean;
begin
  Result := Panel_BlockUnit.Visible;
end;

end.
