unit KM_GUIMapEdPlayerBlockHouse;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsScroll,
   KM_Pics, KM_InterfaceGame, KM_Defaults;

type
  TKMMapEdPlayerBlockHouse = class
  private
    fLevels : Boolean;
    procedure Player_BlockHouseClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_BlockHouseRefresh;
    procedure Player_ChangeToLvl(Sender : TObject);
    procedure Player_BlockHouseOver(Sender : TObject; X, Y : Integer; Shift : TShiftState);
    procedure Player_BlockFieldOver(Sender : TObject; X, Y : Integer; Shift : TShiftState);
  protected
    Panel_BlockHouse: TKMScrollPanel;
    Button_SwitchToLvl: TKMButton;
    Label_BlockHouse: array of TKMlabel;
    {Button_BlockHouse: array of TKMButtonFlat;
    Image_BlockHouse: array of TKMImage;}
    Button_BlockHouse: array of TKMButtonFlatBlock;
    Button_BlockField: array[TKMLockFieldType] of TKMButtonFlatBlock;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_HandLocks,
  KM_Resource, KM_ResHouses, KM_RenderUI, KM_ResFonts,
  KM_ResTypes, KM_HandTypes,
  KM_UtilsExt, KM_CommonUtils,
  SysUtils, Math;

{ TKMMapEdPlayerBlockHouse }
constructor TKMMapEdPlayerBlockHouse.Create(aParent: TKMPanel);
var
  I, J, K, L, top, lastID: Integer;
  H : TKMHouseType;
  FT : TKMLockFieldType;
begin
  inherited Create;

  Panel_BlockHouse := TKMScrollPanel.Create(aParent, TB_PAD, 28, aParent.Width - 9, aParent.MasterParent.Height - 350, [saVertical], bsMenu, ssCommon);
  Panel_BlockHouse.ScrollV.Left := Panel_BlockHouse.ScrollV.Left + 20;
  Panel_BlockHouse.Padding.SetBottom(10);
  Panel_BlockHouse.ScrollV_PadTop := 10;
  Panel_BlockHouse.ScrollV_PadBottom := 10;
  Panel_BlockHouse.ScrollV_PadLeft := -20;

  with TKMLabel.Create(Panel_BlockHouse, 0, PAGE_TITLE_Y, Panel_BlockHouse.Width, 0, gResTexts[TX_MAPED_BLOCK_HOUSES], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  fLevels := false;



  SetLength(Label_BlockHouse, length(HOUSE_GUI_TAB_ORDER));
  SetLength(Button_BlockHouse, 0);
  //SetLength(Image_BlockHouse, 0);
  top := 30;

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
  begin
    Button_BlockField[FT] := TKMButtonFlatBlock.Create(Panel_BlockHouse, 9 + byte(FT) mod 5 * 37, top + (byte(FT) div 5) * 37,33,33, LOCK_FIELD_GUI[FT]);
    Button_BlockField[FT].Tag := byte(FT);
    Button_BlockField[FT].Tag2 := BUTTON_BLOCK_FIELD_TAG_2;
    Button_BlockField[FT].OnMouseDown := Player_BlockHouseClick;

    case FT of
      lftRoadStone: Button_BlockField[FT].Hint := gResTexts[1992];
      lftRoadWooden: Button_BlockField[FT].Hint := gResTexts[1993];
      lftRoadClay: Button_BlockField[FT].Hint := gResTexts[1994];
      lftRoadExclusive: Button_BlockField[FT].Hint := gResTexts[1995];
      lftPalisade: Button_BlockField[FT].Hint := gResTexts[1633];
      lftField: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_FIELD];
      lftGrassField: Button_BlockField[FT].Hint := gResTexts[1990];
      lftVegetablesField: Button_BlockField[FT].Hint := gResTexts[1991];
      lftWineField: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_WINE];
      lftRemove: Button_BlockField[FT].Hint := gResTexts[TX_BUILD_DEMOLISH];
    end;

  end;

  top := Button_BlockField[high(TKMLockFieldType)].Bottom + 3;


  Button_SwitchToLvl := TKMButton.Create(Panel_BlockHouse, 9, top, 33, 33, 748, rxGui, bsGame);
  Button_SwitchToLvl.OnClick := Player_ChangeToLvl;
  Button_SwitchToLvl.Hint := gResTexts[1859];

  top := Button_SwitchToLvl.Bottom + 3;
  J := 0;
  Panel_BlockHouse.MasterControl.AddMouseMoveCtrlSub(Player_BlockHouseOver);
  Panel_BlockHouse.MasterControl.AddMouseMoveCtrlSub(Player_BlockFieldOver);
  for I := 0 to high(HOUSE_GUI_TAB_ORDER) do
  begin
    if I > 0 then
      top := Button_BlockHouse[J - 1].Bottom + 3;

    Label_BlockHouse[I] := TKMLabel.Create(Panel_BlockHouse, 0, top - 3, Panel_BlockHouse.Width, 20, gResTexts[HOUSE_GUI_TAB_ORDER[I].TextID], fntOutline, taCenter);
    Inc(Top, 15);
    for K := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
    begin
      LastID := 0;

      for L := 0 to High(HOUSE_GUI_TAB_ORDER[I].H[K]) do
      begin
        H := HOUSE_GUI_TAB_ORDER[I].H[K, L];
        SetLength(Button_BlockHouse, Length(Button_BlockHouse) + 1);//add new element

        Button_BlockHouse[J] := TKMButtonFlatBlock.Create(Panel_BlockHouse, 9 + lastID mod 5 * 37, top + (lastID div 5) * 37,33,33,gRes.Houses[H].GUIIcon);
        Button_BlockHouse[J].Hint := gRes.Houses[H].HouseName;
        Button_BlockHouse[J].Tag := byte(H);
        Button_BlockHouse[J].Tag2 := BUTTON_BLOCK_HOUSE_TAG_2;
        Button_BlockHouse[J].Hint := gRes.Houses[H].HouseName;
        Button_BlockHouse[J].OnMouseDown := Player_BlockHouseClick;

        Inc(J);
        Inc(lastID);
      end;
      top := Button_BlockHouse[J - 1].Bottom + 3;
    end;
  end;

end;

procedure TKMMapEdPlayerBlockHouse.Player_ChangeToLvl(Sender: TObject);
var I, J : Integer;
  H : TKMHouseType;

begin
  fLevels := not fLevels;
  if fLevels then
    Button_SwitchToLvl.TexID := 747
  else
    Button_SwitchToLvl.TexID := 748;


  for I := 0 to high(Button_BlockHouse) do
  begin
    Button_BlockHouse[I].Enable;
    Button_BlockHouse[I].Caption := '';
    Button_BlockHouse[I].SetBlock(0);
    //Image_BlockHouse[I].Visible := not fLevels;
  end;

  if fLevels then
  begin
    for I := 0 to High(Button_BlockHouse) do
    begin
      H := TKMHouseType(Button_BlockHouse[I].Tag);
      Button_BlockHouse[I].Disable;
      if length(gRes.Houses[H].Levels) > 0 then
      begin
        Button_BlockHouse[I].Enable;

        if gMySpectator.Hand.Locks.HouseMaxLvl[H] > 0 then
        begin
          if gMySpectator.Hand.Locks.HouseMaxLvl[H] = (length(gRes.Houses[H].Levels) + 1) then
            Button_BlockHouse[I].Caption := 'max'
          else
            Button_BlockHouse[I].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H] - 1)
        end else
          Button_BlockHouse[I].Caption := '--';

      end;
    end;
  end else
    Player_BlockHouseRefresh;

end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I, J, K: Integer;
  H: TKMHouseType;
  locks: TKMHandLocks;
  FT : TKMLockFieldType;
begin
  locks := gMySpectator.Hand.Locks;

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if Sender = Button_BlockField[FT] then
    begin
      locks.SetFieldLocked(FT, not locks.FieldLocked(FT));
      Player_BlockHouseRefresh;
      Exit;
    end;

  if fLevels then
  begin
    I := TKMButtonFlatBlock(Sender).Tag;
    H := TKMHouseType(I);
    K := -1;
    for J := 0 to High(Button_BlockHouse) do
      if Sender = Button_BlockHouse[J]  then
      begin
        K := J;
        Break;
      end;
    if K = -1 then
    Exit;

    if ssLeft in Shift then
    begin
      J := locks.HouseMaxLvl[H];
      IncLoop(J, 0, length(gRes.Houses[H].Levels) + 1);
      locks.HouseMaxLvl[H] := J;
      //locks.HouseMaxLvl[H] := EnsureRange(locks.HouseMaxLvl[H] + 1, 0, length(gRes.Houses[H].Levels) + 1)
    end else
    if ssRight in Shift then
    begin
      J := locks.HouseMaxLvl[H];
      IncLoop(J, 0, length(gRes.Houses[H].Levels) + 1, -1);
      locks.HouseMaxLvl[H] := J;
    end;
      //locks.HouseMaxLvl[H] := EnsureRange(locks.HouseMaxLvl[H] - 1, 0, length(gRes.Houses[H].Levels) + 1);

    if gMySpectator.Hand.Locks.HouseMaxLvl[H] > 0 then
    begin
      if gMySpectator.Hand.Locks.HouseMaxLvl[H] = (length(gRes.Houses[H].Levels) + 1) then
        Button_BlockHouse[K].Caption := 'max'
      else
        Button_BlockHouse[K].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H] - 1)
    end else
      Button_BlockHouse[K].Caption := '--';
  end else
  begin
    I := TKMButtonFlatBlock(Sender).Tag;
    H := TKMHouseType(I);

    locks := gMySpectator.Hand.Locks;

    // Circling through 4 house lock values
    if ssLeft in Shift then
      // Straight direction
      locks.HouseLock[H] := TKMHandHouseLock(1 + ((Ord(locks.HouseLock[H])) mod 4))
    else
    if ssRight in Shift then
      // Reverse direction
      locks.HouseLock[H] := TKMHandHouseLock(1 + ((Ord(locks.HouseLock[H]) + 2) mod 4));

    Player_BlockHouseRefresh;
  end;
end;


procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseRefresh;
var
  I: Integer;
  H: TKMHouseType;
  locks: TKMHandLocks;
  FT : TKMLockFieldType;
begin
  locks := gMySpectator.Hand.Locks;
  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if locks.FieldLocked(FT) then
      Button_BlockField[FT].SetBlock(4)
    else
      Button_BlockField[FT].SetBlock(0);
  if fLevels then Exit;


  for I := 0 to high(Button_BlockHouse) do
  begin
    H := TKMHouseType(Button_BlockHouse[I].Tag);
    Button_BlockHouse[I].SetBlock(byte(locks.HouseLock[H]));
    {case locks.HouseLock[H] of
      hlBlocked:  Image_BlockHouse[I].TexID := 32;
      hlGranted:  Image_BlockHouse[I].TexID := 33;
      hlNotVisible:  Image_BlockHouse[I].TexID := 91;
      else        Image_BlockHouse[I].TexID := 0;
    end;}
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockHouseOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
var ctrlDown : TKMControl;
  H1, H2 : TKMHouseType;
  K : Integer;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_HOUSE_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;
  if ctrlDown = nil then
    Exit;
  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = BUTTON_BLOCK_HOUSE_TAG_2) then
  begin
    H1 := TKMHouseType(ctrlDown.Tag);
    H2 := TKMHouseType(TKMButtonFlatBlock(Sender).Tag);
    if fLevels then
    begin
      if gMySpectator.Hand.Locks.HouseMaxLvl[H1] <> gMySpectator.Hand.Locks.HouseMaxLvl[H2] then
      begin
        gMySpectator.Hand.Locks.HouseMaxLvl[H2] := Min(gMySpectator.Hand.Locks.HouseMaxLvl[H1], length(gRes.Houses[H2].Levels) + 1);
        Player_BlockHouseRefresh;
        for K := 0 to High(Button_BlockHouse) do
          if Button_BlockHouse[K] = Sender then
          begin
            if gMySpectator.Hand.Locks.HouseMaxLvl[H2] > 0 then
            begin
              if gMySpectator.Hand.Locks.HouseMaxLvl[H2] = (length(gRes.Houses[H2].Levels) + 1) then
                Button_BlockHouse[K].Caption := 'max'
              else
                Button_BlockHouse[K].Caption := IntToStr(gMySpectator.Hand.Locks.HouseMaxLvl[H2] - 1)
            end else
              Button_BlockHouse[K].Caption := '--';
          end;
      end;

    end else
    if gMySpectator.Hand.Locks.HouseLock[H1] <> gMySpectator.Hand.Locks.HouseLock[H2] then
    begin
      gMySpectator.Hand.Locks.HouseLock[H2] :=gMySpectator.Hand.Locks.HouseLock[H1];
      Player_BlockHouseRefresh;
    end;
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Player_BlockFieldOver(Sender: TObject; X, Y: Integer; Shift : TShiftState);
var ctrlDown : TKMControl;
  F1, F2 : TKMLockFieldType;
begin
  if Sender = nil then
    Exit;
  if not (Sender is TKMButtonFlatBlock) then
    Exit;

  if not (TKMButtonFlatBlock(Sender).Tag2 = BUTTON_BLOCK_FIELD_TAG_2) then
    Exit;
  ctrlDown := TKMButtonFlatBlock(Sender).MasterPanel.MasterControl.CtrlDown;
  if ctrlDown = nil then
    Exit;
  if (ctrlDown is TKMButtonFlatBlock) and (TKMButtonFlatBlock(ctrlDown).Tag2 = BUTTON_BLOCK_FIELD_TAG_2) then
  begin
    F1 := TKMLockFieldType(ctrlDown.Tag);
    F2 := TKMLockFieldType(TKMButtonFlatBlock(Sender).Tag);
    if gMySpectator.Hand.Locks.FieldLocked(F1) <> gMySpectator.Hand.Locks.FieldLocked(F2) then
    begin
      gMySpectator.Hand.Locks.SetFieldLocked(F2, gMySpectator.Hand.Locks.FieldLocked(F1));
      Player_BlockHouseRefresh;
    end;
  end;
end;

procedure TKMMapEdPlayerBlockHouse.Hide;
begin
  Panel_BlockHouse.Hide;
end;


procedure TKMMapEdPlayerBlockHouse.Show;
begin
  Player_BlockHouseRefresh;
  Panel_BlockHouse.Show;
end;


function TKMMapEdPlayerBlockHouse.Visible: Boolean;
begin
  Result := Panel_BlockHouse.Visible;
end;


end.
