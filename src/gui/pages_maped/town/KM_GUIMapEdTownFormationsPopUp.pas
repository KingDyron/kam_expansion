unit KM_GUIMapEdTownFormationsPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsEdit,
  KM_Defaults, KM_Pics;


type
  TKMMapEdTownFormations = class
  private
    fOwner: TKMHandID;
    procedure Formations_Close(Sender: TObject);
    procedure Formations_Change(Sender: TObject);
    procedure Formations_MouseOverInc(Sender: TObject; Shift : TShiftState);
    procedure Formations_MouseOverDec(Sender: TObject; Shift : TShiftState);
    procedure Formations_PanelOver(Sender: TObject; Shift : TShiftState);
    function GetVisible: Boolean;
  protected
    Panel_Formations: TKMPanel;
      Image_FormationsFlag: TKMImage;
      NumEdit_FormationsCount,
      NumEdit_FormationsColumns: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMNumericEdit;
      Button_Formations_Ok: TKMButton;
      Button_Formations_Cancel: TKMButton;
      BevelStack : TKMBevel;
      ImageStack_Army : TKMImageStack;
  public
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    procedure Show(aPlayer: TKMHandID);
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
  end;


implementation
uses
  KM_HandsCollection, KM_Hand,
  KM_RenderUI,
  KM_ResTexts, KM_ResFonts, KM_ResTypes;


{ TKMMapEdFormations }
constructor TKMMapEdTownFormations.Create(aParent: TKMPanel);
const
  T: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Integer = (TX_MAPED_AI_ATTACK_TYPE_MELEE,
                                                          TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE,
                                                          TX_MAPED_AI_ATTACK_TYPE_RANGED,
                                                          TX_MAPED_AI_ATTACK_TYPE_MOUNTED,
                                                          1643, 1883, 1963, 2022);
  SIZE_X = 670;
  SIZE_Y = 280;
var
  GT: TKMGroupType;
  img: TKMImage;
  I : Integer;
begin
  inherited Create;

  Panel_Formations := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Formations.AnchorsCenter;
  Panel_Formations.Hide;

  TKMBevel.Create(Panel_Formations, -2000,  -2000, 5000, 5000);
  img := TKMImage.Create(Panel_Formations, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
  img.ImageStretch;
  with TKMBevel.Create(Panel_Formations,   0,  0, SIZE_X, SIZE_Y) do
    OnMouseOver := Formations_PanelOver;

  TKMLabel.Create(Panel_Formations, SIZE_X div 2, 10, gResTexts[TX_MAPED_AI_FORMATIONS_TITLE], fntOutline, taCenter);

  Image_FormationsFlag := TKMImage.Create(Panel_Formations, 10, 10, 0, 0, 30, rxGuiMain);

  TKMLabel.Create(Panel_Formations, 20, 70, 80, 0, gResTexts[TX_MAPED_AI_FORMATIONS_COUNT], fntMetal, taLeft);
  TKMLabel.Create(Panel_Formations, 20, 95, 80, 0, gResTexts[TX_MAPED_AI_FORMATIONS_COLUMNS], fntMetal, taLeft);

  I := 0;
  for GT := gtMelee to GROUP_TYPE_MAX do
  begin
    with TKMLabel.Create(Panel_Formations, 50 + (I mod 4) * 140 + 32, 37 + (I div 4) * 90, 120, 35, gResTexts[T[GT]], fntMetal, taCenter) do
    begin
      WordWrap := true;
      Hitable := false;
      TextVAlign := tvaMiddle;
    end;

    NumEdit_FormationsCount[GT] := TKMNumericEdit.Create(Panel_Formations, 110 + (I mod 4) * 140, 75 + (I div 4) * 90, 1, 255);
    NumEdit_FormationsColumns[GT] := TKMNumericEdit.Create(Panel_Formations, 110 + (I mod 4) * 140, 100 + (I div 4) * 90, 1, 255);
    NumEdit_FormationsColumns[GT].OnChange := Formations_Change;
    NumEdit_FormationsCount[GT].OnChange := Formations_Change;
    NumEdit_FormationsColumns[GT].ButtonInc.OnMouseOver := Formations_MouseOverInc;
    NumEdit_FormationsColumns[GT].ButtonDec.OnMouseOver := Formations_MouseOverDec;
    NumEdit_FormationsCount[GT].ButtonInc.OnMouseOver := Formations_MouseOverInc;
    NumEdit_FormationsCount[GT].ButtonDec.OnMouseOver := Formations_MouseOverDec;
    inc(I);
  end;
  {I := 0;
  for GT := gtMachines to GROUP_TYPE_MAX do
  begin
    TKMLabel.Create(Panel_Formations, 150 + I * 170 + 32, 125, 0, 0, gResTexts[T[GT]], fntMetal, taCenter);
    NumEdit_FormationsCount[GT] := TKMNumericEdit.Create(Panel_Formations, 150 + I * 170, 145, 1, 255);
    NumEdit_FormationsColumns[GT] := TKMNumericEdit.Create(Panel_Formations, 150 + I * 170, 170, 1, 255);
    NumEdit_FormationsColumns[GT].OnChange := Formations_Change;
    NumEdit_FormationsCount[GT].OnChange := Formations_Change;

    NumEdit_FormationsColumns[GT].ButtonInc.OnMouseOver := Formations_MouseOverInc;
    NumEdit_FormationsColumns[GT].ButtonDec.OnMouseOver := Formations_MouseOverDec;
    NumEdit_FormationsCount[GT].ButtonInc.OnMouseOver := Formations_MouseOverInc;
    NumEdit_FormationsCount[GT].ButtonDec.OnMouseOver := Formations_MouseOverDec;
    inc(I);
  end;}

  {with TKMLabel.Create(Panel_Formations, 20, 205, SIZE_X - 20, 60, gResTexts[TX_MAPED_AI_FORMATIONS_AAI_INFO], fntMetal, taLeft) do
  begin
    FontColor := icGoldenYellow;
    WordWrap := True;
  end;}

  Button_Formations_Ok := TKMButton.Create(Panel_Formations, (SIZE_X div 2) - 160 - 20 , SIZE_Y-40, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
  Button_Formations_Ok.OnClick := Formations_Close;
  Button_Formations_Cancel := TKMButton.Create(Panel_Formations, (SIZE_X div 2) + 20, SIZE_Y-40, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
  Button_Formations_Cancel.OnClick := Formations_Close;

  BevelStack := TKMBevel.Create(Panel_Formations, 0, 0, 60, 44);
  ImageStack_Army    := TKMImageStack.Create(Panel_Formations, 0, 0, 56, 40, 43, 50);
  ImageStack_Army.SetCount(1, 1, 1 div 2);
  BevelStack.Hitable := false;
  ImageStack_Army.Hitable := false;
  BevelStack.Hide;
  ImageStack_Army.Hide;
end;


function TKMMapEdTownFormations.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_Formations_Cancel.IsClickable then
                begin
                  Formations_Close(Button_Formations_Cancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_Formations_Ok.IsClickable then
                begin
                  Formations_Close(Button_Formations_Ok);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdTownFormations.Show(aPlayer: TKMHandID);
var
  GT: TKMGroupType;
begin
  fOwner := aPlayer;

  //Fill UI
  Image_FormationsFlag.FlagColor := gHands[fOwner].FlagColor;

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    NumEdit_FormationsCount[GT].Value := gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].NumUnits;
    NumEdit_FormationsColumns[GT].Value := gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow;
  end;

  Panel_Formations.Show;
end;


function TKMMapEdTownFormations.GetVisible: Boolean;
begin
  Result := Panel_Formations.Visible;
end;


procedure TKMMapEdTownFormations.Formations_Close(Sender: TObject);
var
  GT: TKMGroupType;
begin
  Assert(Image_FormationsFlag.FlagColor = gHands[fOwner].FlagColor, 'Cheap test to see if active player didn''t changed');

  if Sender = Button_Formations_Ok then
    //Save settings
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    begin
      gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].NumUnits := NumEdit_FormationsCount[GT].Value;
      gHands[fOwner].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow := NumEdit_FormationsColumns[GT].Value;
    end;

  Panel_Formations.Hide;
end;

procedure TKMMapEdTownFormations.Formations_MouseOverInc(Sender: TObject; Shift: TShiftState);
var GT, GT2 : TKMGroupType;
begin
  GT2 := gtNone;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    if (Sender = NumEdit_FormationsColumns[GT].ButtonInc) or (Sender = NumEdit_FormationsCount[GT].ButtonInc) then
    begin
      GT2 := GT;
      Break;
    end;
  if GT2 = gtNone then
    Exit;
  BevelStack.Show;
  ImageStack_Army.Show;

  BevelStack.Left := NumEdit_FormationsCount[GT2].Left - BevelStack.Width - 4;
  BevelStack.Top := NumEdit_FormationsCount[GT2].Top;
  ImageStack_Army.Left := BevelStack.Left + 2;
  ImageStack_Army.Top := BevelStack.Top + 2;

  ImageStack_Army.SetCount(NumEdit_FormationsCount[GT2].Value, NumEdit_FormationsColumns[GT2].Value, NumEdit_FormationsColumns[GT2].Value div 2);
  //BevelStack.RePaint;
  //ImageStack_Army.Repaint;
end;

procedure TKMMapEdTownFormations.Formations_MouseOverDec(Sender: TObject; Shift: TShiftState);
var GT, GT2 : TKMGroupType;
begin

  GT2 := gtNone;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    if (Sender = NumEdit_FormationsColumns[GT].ButtonDec) or (Sender = NumEdit_FormationsCount[GT].ButtonDec) then
    begin
      GT2 := GT;
      Break;
    end;
  if GT2 = gtNone then
    Exit;
  BevelStack.Show;
  ImageStack_Army.Show;

  BevelStack.Left := NumEdit_FormationsCount[GT2].Left - BevelStack.Width - 4;
  BevelStack.Top := NumEdit_FormationsCount[GT2].Top;
  ImageStack_Army.Left := BevelStack.Left + 2;
  ImageStack_Army.Top := BevelStack.Top + 2;

  ImageStack_Army.SetCount(NumEdit_FormationsCount[GT2].Value, NumEdit_FormationsColumns[GT2].Value, NumEdit_FormationsColumns[GT2].Value div 2);
  //BevelStack.RePaint;
  //ImageStack_Army.Repaint;
end;

procedure TKMMapEdTownFormations.Formations_Change(Sender: TObject);
var GT, GT2 : TKMGroupType;
begin
  GT2 := gtNone;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    if (Sender = NumEdit_FormationsColumns[GT]) or (Sender = NumEdit_FormationsCount[GT]) then
    begin
      GT2 := GT;
      Break;
    end;

  if GT2 = gtNone then
    Exit;
  ImageStack_Army.SetCount(NumEdit_FormationsCount[GT2].Value, NumEdit_FormationsColumns[GT2].Value, NumEdit_FormationsColumns[GT2].Value div 2);

end;

procedure TKMMapEdTownFormations.Formations_PanelOver(Sender: TObject; Shift: TShiftState);
begin
  BevelStack.Hide;
  ImageStack_Army.Hide;
end;

end.
