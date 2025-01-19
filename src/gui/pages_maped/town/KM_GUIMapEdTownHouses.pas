unit KM_GUIMapEdTownHouses;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_InterfaceDefaults,
   KM_Controls, KM_ControlsBase, KM_ControlsScroll,
   KM_Defaults, KM_InterfaceGame, KM_ControlsTrackBar;

type
  TKMMapEdTownHouses = class(TKMMapEdSubMenuPage)
  private
    oldHT : Byte;
    procedure UpdateField(Sender: TObject; aIncrement: Integer);

    procedure Town_FieldShiftClick(Sender: TObject; Shift: TShiftState);
    procedure Town_FieldMWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
    procedure Town_BuildChange(Sender: TObject);
    procedure Town_BuildRefresh;
    procedure Town_SetStyle(Sender : TObject);
    procedure Town_SetLevel(Sender: TObject; Shift: TShiftState);
    procedure Town_ChangeFillType(Sender: TObject; Shift: TShiftState);
  protected
    Panel_Build: TKMScrollPanel;
      Button_BuildField: array[TKMLockFieldType] of TKMButtonFlatStack;
      Button_HouseRandomRes,
      Button_HouseNoRes,
      Button_HouseFW,
      Button_HouseStyle,
      Button_HouseSite:TKMButton;

      Button_HouseLevel: TKMButtonFlat;

      Label_BuildType: array of TKMLabel;
      Button_Build: array of TKMButtonFlat;
      BrushSize: TKMTrackBar;
  public
    constructor Create(aParent: TKMPanel);

    procedure BuildRoad;
    procedure BuildField;
    procedure BuildWine;
    procedure BuildCancel;

    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdateHotkeys;
    procedure UpdateState;
    procedure UpdateStateIdle;
  end;


implementation
uses
  KM_GameSettings,
  KM_ResTexts, KM_Cursor, KM_Resource, KM_ResHouses, KM_ResFonts,
  KM_RenderUI, KM_Terrain, KM_Points, KM_Utils, KM_UtilsExt,
  KM_ResTypes, KM_ResMapElements;


{ TKMMapEdTownHouses }
constructor TKMMapEdTownHouses.Create(aParent: TKMPanel);
var
  I, J, K, L, lastID, top : Integer;
  //RT : TKMRoadType;
  FT : TKMLockFieldType;
  H : TKMHouseType;
begin
  inherited Create;
  gCursor.MapEd_HouseFill := 0;

  Panel_Build := TKMScrollPanel.Create(aParent, 9, 30, aParent.Width - 9, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  Panel_Build.ScrollV.Left := Panel_Build.ScrollV.Left + 20;
  Panel_Build.AnchorsStretch;

  with TKMLabel.Create(Panel_Build,0,PAGE_TITLE_Y,Panel_Build.Width,0,gResTexts[TX_MAPED_ROAD_TITLE],fntOutline,taCenter) do
    Anchors := [anLeft, anTop, anRight];

  top := 20 + PAGE_TITLE_Y;
      for FT := low(TKMLockFieldType) to High(TKMLockFieldType) do
    begin
      Button_BuildField[FT] := TKMButtonFlatStack.Create(Panel_Build, 37 * (byte(FT) mod 5), top +  37 * (byte(FT) div 5), 33, 33, [LOCK_FIELD_GUI[FT]]);
      Button_BuildField[FT].OnClickShift   := Town_FieldShiftClick;
      Button_BuildField[FT].OnMouseWheel := Town_FieldMWheel;
      Button_BuildField[FT].Tag := byte(FT);
      Button_BuildField[FT].CapColor := clMapEdBtnField;
      case FT of
        lftRoadStone: Button_BuildField[FT].Hint := gResTexts[1992];
        lftRoadWooden: Button_BuildField[FT].Hint := gResTexts[1993];
        lftRoadClay: Button_BuildField[FT].Hint := gResTexts[1994];
        lftRoadExclusive: Button_BuildField[FT].Hint := gResTexts[1995];
        lftPalisade: Button_BuildField[FT].Hint := gResTexts[1633];
        lftField: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_FIELD];
        lftGrassField: Button_BuildField[FT].Hint := gResTexts[1990];
        lftVegetablesField: Button_BuildField[FT].Hint := gResTexts[1991];
        lftWineField: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_WINE];
        lftRemove: Button_BuildField[FT].Hint := gResTexts[TX_BUILD_DEMOLISH];
      end;
    end;
  //Button_BuildField[lftVegetablesField].Disable;
  Button_BuildField[lftRemove].Left := 0;
  Button_BuildField[lftRemove].Top := Button_BuildField[lftWineField].Top + 37;
  {for RT := rtStone to High(Button_BuildRoad) do
  begin
    Button_BuildRoad[RT]          := TKMButtonFlat.Create(Panel_Build,  -28 +  byte(RT) * 37,top,33,33,ROAD_GUI_PIC[RT]);
    Button_BuildRoad[RT].OnClick  := Town_BuildChange;
    Button_BuildRoad[RT].Tag      := byte(RT);
  end;

  Button_BuildField  := TKMButtonFlatStack.Create(Panel_Build, 9,top + 37,33,33,[337, GRAIN_GUI_PIC[GRAIN_GUI_ORDER[0]]]);
  Button_BuildField.Tag := byte(GRAIN_GRAIN_MIN);
  Button_BuildField.CapColor := clMapEdBtnField;
  Button_BuildField.Caption := IntToStr(gCursor.MapEdFieldAge + 1);
  Button_BuildField.CapOffsetY := 0;
  Button_BuildField.TexOffsetY := 0;
  Button_BuildField.OnClickShift := Town_FieldShiftClick;
  Button_BuildField.OnMouseWheel := Town_FieldMWheel;

  Button_BuildGrassField := TKMButtonFlatStack.Create(Panel_Build, 46,top + 37,33,33, [337, GRAIN_GUI_PIC[gftGrass]]);
  Button_BuildGrassField.Tag := byte(GRAIN_GRASS_MIN);
  Button_BuildGrassField.CapColor := clMapEdBtnField;
  Button_BuildGrassField.Caption := IntToStr(gCursor.MapEdFieldAge + 1);
  Button_BuildGrassField.CapOffsetY := 0;
  Button_BuildGrassField.TexOffsetY := 0;
  Button_BuildGrassField.OnClickShift := Town_FieldShiftClick;
  Button_BuildGrassField.OnMouseWheel := Town_FieldMWheel;

  Button_BuildWine   := TKMButtonFlat.Create(Panel_Build, 83,top + 37,33,33,336);
  Button_BuildPalisade   := TKMButtonFlat.Create(Panel_Build, 120,top + 37,33,33,373);
  Button_BuildPalisade.Hint := gResTexts[1633];

  Button_BuildCancel := TKMButtonFlat.Create(Panel_Build,9,top + 74,33,33,340);}

  Button_BuildField[lftField].Tag := ord(GRAIN_GUI_ORDER[0]);
  Button_BuildField[lftGrassField].Tag := ord(GRASS_GUI_ORDER[0]);
  Button_BuildField[lftVegetablesField].Tag := ord(VEGE_GUI_ORDER[0]);

  Button_BuildField[lftField].TextIds := [LOCK_FIELD_GUI[lftField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftField].Tag)]];
  Button_BuildField[lftGrassField].TextIds := [LOCK_FIELD_GUI[lftField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftGrassField].Tag)]];
  Button_BuildField[lftVegetablesField].TextIds := [LOCK_FIELD_GUI[lftField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftVegetablesField].Tag)]];


  BrushSize   := TKMTrackBar.Create(Panel_Build, Button_BuildField[lftRemove].Right + 5, Button_BuildField[lftRemove].Top + 7, Panel_Build.Width - 68 - 33, 1, 5);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.Hint := gResTexts[1839];
  BrushSize.Position := 1;


  {Button_BuildWine.CapColor := clMapEdBtnWine;

  Button_BuildWine.Caption  := IntToStr(gCursor.MapEdWineFieldAge + 1);


  Button_BuildWine.CapOffsetY := -10;
  Button_BuildWine.TexOffsetY := 6;

  Button_BuildWine.OnClickShift  := Town_FieldShiftClick;
  Button_BuildWine.OnMouseWheel := Town_FieldMWheel;

  Button_BuildCancel.OnClick:= Town_BuildChange;
  Button_BuildPalisade.OnClick  := Town_BuildChange;}
  BrushSize.OnChange := Town_BuildChange;

  //Panels_Build[J].Height := Button_BuildCancel.Bottom + 5;


  SetLength(Button_Build, 0);//just in case
  SetLength(Label_BuildType, length(HOUSE_GUI_TAB_ORDER));

  top := BrushSize.Bottom + 10;


  Button_HouseStyle := TKMButton.Create(Panel_Build, 0, top, 30, 30, 389, rxGui, bsGame);
  Button_HouseStyle.Tag := 1;
  Button_HouseStyle.Hint := gResTexts[1812];
  Button_HouseStyle.MobilHint := true;
  Button_HouseStyle.OnClick := Town_SetStyle;


  Button_HouseNoRes := TKMButton.Create(Panel_Build, 30, top, 30, 30, 717, rxGui, bsGame);
  Button_HouseNoRes.Hint := gResTexts[1868];
  Button_HouseNoRes.MobilHint := true;
  Button_HouseNoRes.OnClick := Town_SetStyle;


  Button_HouseFW := TKMButton.Create(Panel_Build, 60, top, 30, 30, 769, rxGui, bsGame);
  Button_HouseFW.Hint := gResTexts[1818];
  Button_HouseFW.MobilHint := true;
  Button_HouseFW.OnClick := Town_SetStyle;

  Button_HouseRandomRes := TKMButton.Create(Panel_Build, 90, top, 40, 30, '--', bsGame);
  Button_HouseRandomRes.Hint := gResTexts[1869];
  Button_HouseRandomRes.MobilHint := true;
  Button_HouseRandomRes.OnClickShift := Town_ChangeFillType;
  Button_HouseRandomRes.Font := fntMetal;

  Button_HouseSite := TKMButton.Create(Panel_Build, 130, top, 30, 30, 389, rxGui, bsGame);
  Button_HouseSite.Hint := gResTexts[2020];
  Button_HouseSite.MobilHint := true;
  Button_HouseSite.OnClick := Town_SetStyle;

  Button_HouseLevel := TKMButtonFlat.Create(Panel_Build, 160, top,33,33,748);
  Button_HouseLevel.OnClickShift := Town_SetLevel;
  Button_HouseLevel.Tag := 0;



  top := Button_HouseRandomRes.Bottom + 5;
    
  J := 0;
  for I := 0 to high(HOUSE_GUI_TAB_ORDER) do
  begin
    if I > 0 then
      top := Button_Build[J - 1].Bottom + 3;

    Label_BuildType[I] := TKMLabel.Create(Panel_Build, 0, top - 3, Panel_Build.Width, 20, gResTexts[HOUSE_GUI_TAB_ORDER[I].TextID], fntOutline, taCenter);
    Label_BuildType[I].Hitable := false;

    Inc(Top, 15);
    lastID := 0;
    for K := 0 to High(HOUSE_GUI_TAB_ORDER[I].H) do
    begin
      for L := 0 to high(HOUSE_GUI_TAB_ORDER[I].H[K]) do
      begin
        H := HOUSE_GUI_TAB_ORDER[I].H[K,L];
        if H = htNone then
          Continue;
        SetLength(Button_Build, J + 1);//add new elements
        Button_Build[J] := TKMButtonFlat.Create(Panel_Build, lastID mod 5 * 37, top + (lastID div 5) * 37,33,33,gRes.Houses[H].GUIIcon);
        Button_Build[J].OnClick := Town_BuildChange;
        Button_Build[J].Tag := Byte(H);
        Button_Build[J].Hint := gRes.Houses[H].HouseName;
        Inc(J);
        Inc(lastID);
      end;
    end;
    top := Button_Build[J - 1].Bottom + 3;
  end;


  for I := 0 to High(fSubMenuActionsEvents) do
    fSubMenuActionsEvents[I] := Town_BuildChange;

  {fSubMenuActionsCtrls[0,0] := Button_BuildRoad[rtStone];
  fSubMenuActionsCtrls[1,0] := Button_BuildField;
  fSubMenuActionsCtrls[2,0] := Button_BuildWine;
  fSubMenuActionsCtrls[3,0] := Button_BuildCancel;}

  //for I := 4 to High(fSubMenuActionsCtrls) do
  //  fSubMenuActionsCtrls[I, 0] := Button_Build[I-3];
end;


procedure TKMMapEdTownHouses.BuildRoad;
begin
  //Button_BuildRoad[rtStone].Down := True;
  //Town_BuildChange(Button_BuildRoad[rtStone]);
end;


procedure TKMMapEdTownHouses.BuildField;
begin
  //Button_BuildField.Down := True;
  //Town_BuildChange(Button_BuildField);
end;


procedure TKMMapEdTownHouses.BuildWine;
begin
  //Button_BuildWine.Down := True;
  //Town_BuildChange(Button_BuildWine);
end;


procedure TKMMapEdTownHouses.BuildCancel;
begin
  //Button_BuildCancel.Down := True;
  //Town_BuildChange(Button_BuildCancel);
end;


procedure TKMMapEdTownHouses.UpdateField(Sender: TObject; aIncrement: Integer);
var
  done : Boolean;
  maxAge : Byte;
begin
  done := false;
  if Sender = Button_BuildField[lftField] then
  begin
    done := true;
    gCursor.GrainType := TKMGrainType(Button_BuildField[lftField].Tag);

    maxAge := gFieldGrains[gCursor.GrainType].StagesCount;

    if gCursor.MapEdFieldAge <> -1 then
    gCursor.MapEdFieldAge := Min(gCursor.MapEdFieldAge, maxAge);
    if aIncrement <> 0 then
      if (gCursor.Mode = cmField)  then
        if gCursor.MapEdFieldAge = -1 then
          gCursor.MapEdFieldAge := (IfThen(aIncrement > 0, 0, -1) + maxAge) mod maxAge
        else
        if (gCursor.MapEdFieldAge + aIncrement >= maxAge) or  (gCursor.MapEdFieldAge + aIncrement = -1) then
          gCursor.MapEdFieldAge := -1
        else
          gCursor.MapEdFieldAge := (gCursor.MapEdFieldAge + aIncrement + maxAge) mod maxAge;

    gCursor.Mode := cmField;
    if gCursor.MapEdFieldAge = -1 then
      Button_BuildField[lftField].Caption := 'RN'
    else
      Button_BuildField[lftField].Caption := IntToStr(gCursor.MapEdFieldAge + 1);
  end;


  if not done then
    if Sender = Button_BuildField[lftGrassField] then
    begin
      done := true;
      gCursor.GrainType := TKMGrainType(Button_BuildField[lftGrassField].Tag);

      maxAge := gFieldGrains[gCursor.GrainType].StagesCount;
      if gCursor.MapEdGrassFieldAge <> -1 then
        gCursor.MapEdGrassFieldAge := Min(gCursor.MapEdGrassFieldAge, maxAge);

      if aIncrement <> 0 then
        if (gCursor.Mode = cmGrassLand) then
          if gCursor.MapEdGrassFieldAge = -1 then
            gCursor.MapEdGrassFieldAge := (aIncrement + maxAge) mod maxAge
          else
          if (gCursor.MapEdGrassFieldAge + aIncrement >= maxAge) or  (gCursor.MapEdGrassFieldAge + aIncrement = -1) then
            gCursor.MapEdGrassFieldAge := -1
          else
            gCursor.MapEdGrassFieldAge := (gCursor.MapEdGrassFieldAge + aIncrement + maxAge) mod maxAge;

      gCursor.Mode := cmGrassLand;

      if gCursor.MapEdGrassFieldAge = -1 then
        Button_BuildField[lftGrassField].Caption := 'RN'
      else
        Button_BuildField[lftGrassField].Caption := IntToStr(gCursor.MapEdGrassFieldAge + 1);
    end;


  if not done then
    if Sender = Button_BuildField[lftVegetablesField] then
    begin
      done := true;
      gCursor.GrainType := TKMGrainType(Button_BuildField[lftVegetablesField].Tag);

      maxAge := gFieldGrains[gCursor.GrainType].StagesCount;
      if gCursor.MapEdGrassFieldAge <> -1 then
        gCursor.MapEdGrassFieldAge := Min(gCursor.MapEdGrassFieldAge, maxAge);

      if aIncrement <> 0 then
        if (gCursor.Mode = cmVegeField) then
          if gCursor.MapEdVegeFieldAge = -1 then
            gCursor.MapEdVegeFieldAge := (aIncrement + maxAge) mod maxAge
          else
          if (gCursor.MapEdVegeFieldAge + aIncrement >= maxAge) or  (gCursor.MapEdVegeFieldAge + aIncrement = -1) then
            gCursor.MapEdVegeFieldAge := -1
          else
            gCursor.MapEdVegeFieldAge := (gCursor.MapEdVegeFieldAge + aIncrement + maxAge) mod maxAge;

      gCursor.Mode := cmVegeField;

      if gCursor.MapEdVegeFieldAge = -1 then
        Button_BuildField[lftVegetablesField].Caption := 'RN'
      else
        Button_BuildField[lftVegetablesField].Caption := IntToStr(gCursor.MapEdVegeFieldAge + 1);
    end;

  if not done then
    if Sender = Button_BuildField[lftWineField] then
    begin
      //gCursor.GrainType := TKMGrainType(Button_BuildField[lftWineField].Tag);
      maxAge := WINE_STAGES_COUNT;
      if gCursor.MapEdGrassFieldAge <> -1 then
        gCursor.MapEdGrassFieldAge := Min(gCursor.MapEdGrassFieldAge, maxAge);

      if aIncrement <> 0 then
        if (gCursor.Mode = cmWine) then
          if gCursor.MapEdWineFieldAge = -1 then
            gCursor.MapEdWineFieldAge := (aIncrement + maxAge) mod maxAge
          else
          if (gCursor.MapEdWineFieldAge + aIncrement >= maxAge) or  (gCursor.MapEdWineFieldAge + aIncrement = -1) then
            gCursor.MapEdWineFieldAge := -1
          else
            gCursor.MapEdWineFieldAge := (gCursor.MapEdWineFieldAge + aIncrement + maxAge) mod maxAge;

      {if gCursor.Mode = cmWine then
        gCursor.MapEdWineFieldAge :=  (gCursor.MapEdWineFieldAge + aIncrement + WINE_STAGES_COUNT) mod WINE_STAGES_COUNT;}
      gCursor.Mode := cmWine;

      if gCursor.MapEdWineFieldAge = -1 then
        Button_BuildField[lftWineField].Caption := 'RN'
      else
        Button_BuildField[lftWineField].Caption := IntToStr(gCursor.MapEdWineFieldAge + 1);
    end;
end;


procedure TKMMapEdTownHouses.Town_FieldShiftClick(Sender: TObject; Shift: TShiftState);
  procedure SetMode(aMode : TKMCursorMode; aTag : Integer; aRoadType : TKMRoadType = rtNone);
  begin
    gCursor.Mode := aMode;
    gCursor.Tag1 := aTag;
    gCursor.RoadType := aRoadType;
  end;
var
  stageInc: Integer;
  FT : TKMLockFieldType;
begin
  if ssRight in Shift then
    stageInc := -1
  else
    stageInc := 1;

  for FT := Low(TKMLockFieldType) to High(TKMLockFieldType) do
    if not (FT in [lftField, lftGrassField, lftWineFIeld, lftVegetablesField]) then
      if Sender = Button_BuildField[FT] then
      begin
        case FT of
          lftRoadStone: SetMode(cmRoad, 0, rtStone);
          lftRoadWooden: SetMode(cmRoad, 0, rtWooden);
          lftRoadClay: SetMode(cmRoad, 0, rtClay);
          lftRoadExclusive: SetMode(cmRoad, 0, rtExclusive);
          lftPalisade: gCursor.Mode := cmPalisade;
          lftField: gCursor.Mode := cmField;
          lftGrassField: gCursor.Mode := cmGrassLand;
          lftVegetablesField: gCursor.Mode := cmVegeField;
          lftWineField: gCursor.Mode := cmWine;
          lftRemove:  begin
                        gCursor.Mode := cmErase;
                        gCursor.MapEdSize := BrushSize.Position;
                      end;
        end;
        Exit;
      end;

  if (Sender = Button_BuildField[lftField]) and (gCursor.Mode = cmField) then
  begin
    Button_BuildField[lftField].Tag := Button_BuildField[lftField].Tag + stageInc;
    if Button_BuildField[lftField].Tag > ord(GRAIN_GRAIN_MAX) then
      Button_BuildField[lftField].Tag := ord(GRAIN_GRAIN_MIN)
    else
    if Button_BuildField[lftField].Tag < ord(GRAIN_GRAIN_MIN) then
      Button_BuildField[lftField].Tag := ord(GRAIN_GRAIN_MAX);

    Button_BuildField[lftField].TextIds := [LOCK_FIELD_GUI[lftField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftField].Tag)]];
    //stageInc := 0;
  end else
  if (Sender = Button_BuildField[lftGrassField]) and (gCursor.Mode = cmGrassLand) then
  begin
    Button_BuildField[lftGrassField].Tag := Button_BuildField[lftGrassField].Tag + stageInc;

    if Button_BuildField[lftGrassField].Tag > ord(GRAIN_GRASS_MAX) then
      Button_BuildField[lftGrassField].Tag := ord(GRAIN_GRASS_MIN)
    else
    if Button_BuildField[lftGrassField].Tag < ord(GRAIN_GRASS_MIN) then
      Button_BuildField[lftGrassField].Tag := ord(GRAIN_GRASS_MAX);

    Button_BuildField[lftGrassField].TextIds := [LOCK_FIELD_GUI[lftGrassField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftGrassField].Tag)]];
    //stageInc := 0;
  end else
  if (Sender = Button_BuildField[lftVegetablesField]) and (gCursor.Mode = cmVegeField) then
  begin
    Button_BuildField[lftVegetablesField].Tag := Button_BuildField[lftVegetablesField].Tag + stageInc;

    if Button_BuildField[lftVegetablesField].Tag > ord(GRAIN_VEGE_MAX) then
      Button_BuildField[lftVegetablesField].Tag := ord(GRAIN_VEGE_MIN)
    else
    if Button_BuildField[lftVegetablesField].Tag < ord(GRAIN_VEGE_MIN) then
      Button_BuildField[lftVegetablesField].Tag := ord(GRAIN_VEGE_MAX);

    Button_BuildField[lftVegetablesField].TextIds := [LOCK_FIELD_GUI[lftVegetablesField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftVegetablesField].Tag)]];
    //stageInc := 0;
  end else
  if (Sender = Button_BuildField[lftWineField]) and (gCursor.Mode = cmGrassLand) then
  begin
    {Button_BuildField[lftWineField].Tag := Button_BuildField[lftWineField].Tag + stageInc;

    if Button_BuildField[lftWineField].Tag > ord(GRAIN_WINE_MAX) then
      Button_BuildField[lftWineField].Tag := ord(GRAIN_WINE_MIN)
    else
    if Button_BuildField[lftWineField].Tag < ord(GRAIN_WINE_MIN) then
      Button_BuildField[lftWineField].Tag := ord(GRAIN_WINE_MAX);

    Button_BuildField[lftWineField].TextIds := [LOCK_FIELD_GUI[lftWineField], GRAIN_GUI_PIC[TKMGrainType(Button_BuildField[lftWineField].Tag)]];}
    //stageInc := 0;
  end;

  UpdateField(Sender, 0);
end;


procedure TKMMapEdTownHouses.Town_FieldMWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
begin
  UpdateField(Sender, WheelSteps);
  aHandled := True;
end;


procedure TKMMapEdTownHouses.Town_BuildChange(Sender: TObject);
  procedure SetMode(aMode : TKMCursorMode; aTag : Integer; aRoadType : TKMRoadType = rtNone);
  begin
    gCursor.Mode := aMode;
    gCursor.Tag1 := aTag;
    gCursor.RoadType := aRoadType;
  end;
var
  I: Integer;
  oldMode : TKMCursorMode;
begin
  //Reset cursor and see if it needs to be changed
  oldMode := gCursor.Mode;
  gCursor.Mode := cmNone;
  if (Sender = BrushSize) then
  begin
    If not (oldMode in [cmErase, cmRoad]) then
      gCursor.Mode := cmErase
    else
      gCursor.Mode := oldMode;
  end
  else

  {if (Sender = Button_BuildPalisade) then
    gCursor.Mode := cmPalisade
  else
  if (Sender = Button_BuildCancel) or ( Sender = BrushSize) then
    gCursor.Mode := cmErase
  else
  if Sender = Button_BuildRoad[rtStone] then
    SetMode(cmRoad, 0, rtStone)
  else
  if Sender = Button_BuildRoad[rtWooden] then
    SetMode(cmRoad, 0, rtWooden)
  else
  if Sender = Button_BuildRoad[rtClay] then
    SetMode(cmRoad, 0, rtClay)
  else
  if Sender = Button_BuildRoad[rtExclusive] then
    SetMode(cmRoad, 0, rtExclusive)
  else
  if Sender = Button_BuildWine then
    Town_FieldShiftClick(Sender, [])
  else
  if Sender = Button_BuildField then
    Town_FieldShiftClick(Sender, [])
  else
  if Sender = Button_BuildGrassField then
    Town_FieldShiftClick(Sender, [])
  else}

  for I := 0 to high(Button_Build) do
    if Sender = Button_Build[I] then
    begin
      gCursor.Mode := cmHouses;
      oldHT := gCursor.Tag1;
      gCursor.Tag1 := Button_Build[I].Tag;
      gCursor.MapEd_HouseStyle := Min(length(gRes.Houses[TKMHouseType(Button_Build[I].Tag)].Styles), gCursor.MapEd_HouseStyle);
    end;
    

  gCursor.MapEdSize := BrushSize.Position;
  Town_BuildRefresh;
end;


procedure TKMMapEdTownHouses.Town_BuildRefresh;
var
  I: Integer;
  HT : TKMHouseType;
  //RT : TKMRoadType;
begin
  if not Visible then
    Exit;

  Button_BuildField[lftRoadStone].Down := (gCursor.Mode = cmRoad) and (gCursor.RoadType = rtStone);
  Button_BuildField[lftRoadWooden].Down := (gCursor.Mode = cmRoad) and (gCursor.RoadType = rtWooden);
  Button_BuildField[lftRoadClay].Down := (gCursor.Mode = cmRoad) and (gCursor.RoadType = rtClay);
  Button_BuildField[lftRoadExclusive].Down := (gCursor.Mode = cmRoad) and (gCursor.RoadType = rtExclusive);
  Button_BuildField[lftPalisade].Down := (gCursor.Mode = cmPalisade);

  Button_BuildField[lftVegetablesField].Down := gCursor.Mode = cmVegeField;
  Button_BuildField[lftGrassField].Down := (gCursor.Mode = cmGrassLand);
  Button_BuildField[lftWineField].Down := (gCursor.Mode = cmWine);
  Button_BuildField[lftField].Down := (gCursor.Mode = cmField);
  Button_BuildField[lftRemove].Down := (gCursor.Mode = cmErase);
  BrushSize.Enabled := (gCursor.Mode in [cmErase, cmRoad]);

  for I := 0 to high(Button_Build) do
      Button_Build[I].Down := (gCursor.Mode = cmHouses) and (gCursor.Tag1 = Button_Build[I].Tag);



  Button_HouseNoRes.Disable;
  Button_HouseFW.Disable;
  Button_HouseStyle.Disable;
  Button_HouseLevel.Disable;
  Button_HouseRandomRes.Disable;

  if gCursor.MapEd_HouseSite then
    Button_HouseSite.TexID := 977
  else
    Button_HouseSite.TexID := 976;


  if (gCursor.Mode = cmHouses) then
  begin
    Button_HouseRandomRes.Enable;
    HT := TKMHouseType(gCursor.Tag1);

    if gRes.Houses[HT].AcceptsWares then
      Button_HouseNoRes.Enable;
    if gRes.Houses[HT].CanForceWork then
      Button_HouseFW.Enable;

    if (length(gRes.Houses[HT].Levels) > 0) then
    begin
      Button_HouseLevel.Enable;
      Button_HouseLevel.Tag := EnsureRange(Button_HouseLevel.Tag, 0, length(gRes.Houses[HT].Levels));
      Button_HouseLevel.Caption := IntToStr(Button_HouseLevel.Tag);
      gCursor.MapEd_HouseLevel := Button_HouseLevel.Tag;
    end;
    Button_HouseStyle.Enabled := length(gRes.Houses[HT].Styles) > 0;
    if (oldHT <> gCursor.Tag1) then
    begin
      Button_HouseStyle.Tag := 1;
      Button_HouseStyle.TexID :=  399;
      gCursor.MapEd_HouseStyle := 0;
    end;

    oldHT := gCursor.Tag1;

  end;
end;


procedure TKMMapEdTownHouses.Hide;
begin
  Panel_Build.Hide;
end;


procedure TKMMapEdTownHouses.Show;
begin
  Town_BuildRefresh;
  Panel_Build.Show;

  if gCursor.MapEdGrassFieldAge = -1 then
    Button_BuildField[lftGrassField].Caption := 'RN'
  else
    Button_BuildField[lftGrassField].Caption := IntToStr(gCursor.MapEdGrassFieldAge + 1);

  if gCursor.MapEdVegeFieldAge = -1 then
    Button_BuildField[lftVegetablesField].Caption := 'RN'
  else
    Button_BuildField[lftVegetablesField].Caption := IntToStr(gCursor.MapEdVegeFieldAge + 1);

  if gCursor.MapEdFieldAge = -1 then
    Button_BuildField[lftField].Caption := 'RN'
  else
    Button_BuildField[lftField].Caption := IntToStr(gCursor.MapEdFieldAge + 1);

  if gCursor.MapEdWineFieldAge = -1 then
    Button_BuildField[lftWineField].Caption := 'RN'
  else
    Button_BuildField[lftWineField].Caption := IntToStr(gCursor.MapEdWineFieldAge + 1);

end;


function TKMMapEdTownHouses.Visible: Boolean;
begin
  Result := Panel_Build.Visible;
end;


procedure TKMMapEdTownHouses.UpdateHotkeys;
begin
  {Button_BuildRoad[rtStone].Hint     := GetHintWHotkey(TX_BUILD_ROAD_HINT, kfMapedSubMenuAction1);
  Button_BuildField.Hint    := GetHintWHotkey(TX_BUILD_FIELD_HINT, kfMapedSubMenuAction2);
  Button_BuildWine.Hint     := GetHintWHotkey(TX_BUILD_WINE_HINT, kfMapedSubMenuAction3);
  Button_BuildCancel.Hint   := GetHintWHotkey(TX_BUILD_CANCEL_HINT, kfMapedSubMenuAction4);}
end;

procedure TKMMapEdTownHouses.Town_SetStyle(Sender: TObject);
var I : Integer;
  HT : TKMHouseType;
begin
  HT := TKMHouseType(gCursor.Tag1);

  if Sender = Button_HouseSite then
  begin
    gCursor.MapEd_HouseSite := not gCursor.MapEd_HouseSite;
    if gCursor.MapEd_HouseSite then
      Button_HouseSite.TexID := 389
    else
      Button_HouseSite.TexID := 718;
  end else
  if (Sender = Button_HouseNoRes) then
  begin
    If gCursor.SwitchMod(medHouseNoResource) then
      Button_HouseNoRes.TexID := 718
    else
      Button_HouseNoRes.TexID := 717;
  end else
  if (Sender = Button_HouseFW) then
  begin
    If gCursor.SwitchMod(medHouseForceWorking) then
      Button_HouseFW.TexID := 770
    else
      Button_HouseFW.TexID := 769;
  end else
  if (Sender = Button_HouseStyle) then
  begin
    I := Button_HouseStyle.Tag;
    gCursor.MapEd_HouseStyle := I;
    if I = 0 then
      Button_HouseStyle.TexID := 399
    else
      Button_HouseStyle.TexID := gRes.Houses[HT].Styles[I - 1].Icon;

    Inc(I);
    if I > length(gRes.Houses[HT].Styles) then
      I := 0;

    Button_HouseStyle.Tag := I;
  end;



  Town_BuildRefresh;
end;

procedure TKMMapEdTownHouses.Town_SetLevel(Sender: TObject; Shift :TShiftState);
var MaxCount, Count : Byte;
  HT : TKMHouseType;
begin
  HT := TKMHouseType(gCursor.Tag1);

  if ssShift in Shift then
    Count := 5
  else
    Count := 1;

  MaxCount := Length(gRes.Houses[HT].Levels);

  if ssLeft in Shift then
    Button_HouseLevel.Tag := EnsureRange(Button_HouseLevel.Tag + Count, 0, MaxCount)
  else
  if ssRight in Shift then
    Button_HouseLevel.Tag := EnsureRange(Button_HouseLevel.Tag - Count, 0, MaxCount);

  Button_HouseLevel.Caption := IntToStr(Button_HouseLevel.Tag);

  if Button_HouseLevel.Tag = MaxCount then
    Button_HouseLevel.TexID := 746
  else
    Button_HouseLevel.TexID := 748;

  gCursor.MapEd_HouseLevel := Button_HouseLevel.Tag;

end;

procedure TKMMapEdTownHouses.Town_ChangeFillType(Sender: TObject; Shift: TShiftState);
var I : Integer;
  finCap : String;
begin
  if ssShift in Shift then
    Button_HouseRandomRes.Tag2 := IfThen(Button_HouseRandomRes.Tag2 = 0, 1, 0)
  else
  begin
    I := Button_HouseRandomRes.Tag;

    Inc(I);
    if I > 6 then
      I := 0;

    Button_HouseRandomRes.Tag := I;
  end;

  gCursor.MapEd_HouseFill := 10 * Button_HouseRandomRes.Tag2 + Button_HouseRandomRes.Tag;
  finCap := '';

  if Button_HouseRandomRes.Tag2 = 1 then
    finCap := 'R';

  if Button_HouseRandomRes.Tag = 6 then
    finCap := 'max'
  else
  if Button_HouseRandomRes.Tag = 0 then
    finCap := '--'
  else
    finCap := finCap + IntToStr(Button_HouseRandomRes.Tag);

  Button_HouseRandomRes.Caption := finCap;
end;

procedure TKMMapEdTownHouses.UpdateState;
begin
  Town_BuildRefresh;
end;


procedure TKMMapEdTownHouses.UpdateStateIdle;
begin
  // Not used atm...
end;


end.
