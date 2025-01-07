unit KM_GUIMapEdMissionAlliances;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsPopUp, KM_ControlsSwitch, KM_ControlsGrid,
   KM_Defaults;

type
  TKMMapEdMissionAlliances = class
  private
    procedure CloseAlliances_Click(Sender: TObject);

    procedure Mission_AlliancesChange(Sender: TObject);
    procedure Mission_AlliancesUpdate;
    procedure CheckBoxGrid_Changed(Sender : TObject; X, Y : Integer; aItem : PKMGridItem);
    procedure CheckBoxGrid_Stop(Sender : TObject);
  protected
    PopUp_Alliances: TKMPopUpMenu;
    Panel_Alliances: TKMPanel;
      Bevel_Alliances: TKMBevel;
      Image_Alliances: TKMImage;
      CheckBox_Alliances: array [0..MAX_HANDS-1, 0..MAX_HANDS-1] of TKMCheckBox;

      Grid_Alliances : TKMCheckBoxGrid;
      Button_CloseAlliances: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_Hand,
  KM_RenderUI,
  KM_ResTexts, KM_ResFonts, KM_ResTypes;

const
  TB_CHB_A_L = 32; //Left CheckBox_Alliances
  TB_CHB_A_T = 40; //Top CheckBox_Alliances
  TB_CHB_A_W = 20; // Spacing between CheckBox_Alliances by width
  TB_CHB_A_H = 20; // Distance between CheckBox_Alliances by height
  TB_PANEL_A_L = 30; //Panel_Alliances.left
  TB_PANEL_A_T = 30; // Panel_Alliances.top
  TB_BUTTON_W = 200; // Button_CloseAlliances.Width
  TB_BUTTON_H = 30; // Button_CloseAlliances.Height

{ TKMMapEdMissionAlliances }
constructor TKMMapEdMissionAlliances.Create(aParent: TKMPanel);
var
  I, K: Integer;
begin
  inherited Create;

  PopUp_Alliances := TKMPopUpMenu.Create(aParent.MasterParent, 0);
  PopUp_Alliances.Width := (TB_PANEL_A_L * 2) + (TB_CHB_A_L * 2) + (TB_CHB_A_W * MAX_HANDS);
  PopUp_Alliances.Height := (TB_PANEL_A_T * 2) + (TB_CHB_A_T * 2) + (TB_CHB_A_H * MAX_HANDS) + TB_CHB_A_H + 10;
  PopUp_Alliances.AnchorsCenter;
  PopUp_Alliances.Left := (aParent.MasterParent.Width div 2) - (PopUp_Alliances.Width div 2);
  PopUp_Alliances.Top := (aParent.MasterParent.Height div 2) - (PopUp_Alliances.Height div 2);

    Bevel_Alliances := TKMBevel.Create(PopUp_Alliances, -2000,  -2000, 5000, 5000);
    Bevel_Alliances.BackAlpha := 0.7;
    Bevel_Alliances.EdgeAlpha := 0.9;

    Image_Alliances := TKMImage.Create(PopUp_Alliances, -25, -25, PopUp_Alliances.Width+50, PopUp_Alliances.Height+50, 18, rxGuiMain);
    Image_Alliances.ImageStretch;

  Panel_Alliances := TKMPanel.Create(PopUp_Alliances, TB_PANEL_A_L, TB_PANEL_A_T, PopUp_Alliances.Width - (TB_PANEL_A_L * 2), PopUp_Alliances.Height - (TB_PANEL_A_T * 2));

  TKMLabel.Create(Panel_Alliances, 0, 0, Panel_Alliances.Width, 0, gResTexts[TX_MAPED_ALLIANCE], fntOutline, taCenter);
  for I := 0 to MAX_HANDS - 1 do
  begin
    TKMLabel.Create(Panel_Alliances, 40 + I * TB_CHB_A_W, 20, IntToStr(I + 1), fntGrey, taCenter);
    TKMLabel.Create(Panel_Alliances, 10, 40 + I * TB_CHB_A_H, IntToStr(I + 1), fntGrey, taCenter);
    for K := 0 to MAX_HANDS - 1 do
    begin
      CheckBox_Alliances[I,K] := TKMCheckBox.Create(Panel_Alliances, TB_CHB_A_L + K * TB_CHB_A_W, TB_CHB_A_T + I * TB_CHB_A_H, 15, 15, '', fntMetal);
      CheckBox_Alliances[I,K].Tag       := I * MAX_HANDS + K;
      CheckBox_Alliances[I,K].OnClick   := Mission_AlliancesChange;
      CheckBox_Alliances[I,K].Hide;
    end;
  end;

  Grid_Alliances := TKMCheckBoxGrid.Create(Panel_Alliances, TB_CHB_A_L, TB_CHB_A_T, MAX_HANDS, MAX_HANDS);
  Grid_Alliances.OnChangePos := CheckBoxGrid_Changed;
  Grid_Alliances.OnMouseUp := CheckBoxGrid_Stop;

  // We don't have a rule to make alliances symmetrical yet
  TKMLabel.Create(Panel_Alliances, 0, Panel_Alliances.Height - 60, Panel_Alliances.Width, 20, gResTexts[TX_MAPED_ALLIANCE_SYMMETRIC_LBL], fntGrey, taCenter);

  Button_CloseAlliances := TKMButton.Create(Panel_Alliances, (Panel_Alliances.Width div 2) - (TB_BUTTON_W div 2), Panel_Alliances.Height - 30,
                                            TB_BUTTON_W, TB_BUTTON_H, gResTexts[TX_WORD_CLOSE], bsGame);
  Button_CloseAlliances.OnClick := CloseAlliances_Click;
end;


procedure TKMMapEdMissionAlliances.CloseAlliances_Click(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMissionAlliances.Mission_AlliancesChange(Sender: TObject);
const
  ALL: array [Boolean] of TKMAllianceType = (atEnemy, atAlly);
var
  I,K: Integer;
begin
  I := TKMCheckBox(Sender).Tag div gHands.Count;
  K := TKMCheckBox(Sender).Tag mod gHands.Count;

  gHands[I].Alliances[K] := ALL[CheckBox_Alliances[I,K].Checked or (I = K)];

  //Always copy status to symmetrical item, since we don't allow unsymmetrical alliances yet
  CheckBox_Alliances[K,I].Checked := CheckBox_Alliances[I,K].Checked;
  gHands[K].Alliances[I] := gHands[I].Alliances[K];

  Mission_AlliancesUpdate;
end;


procedure TKMMapEdMissionAlliances.Mission_AlliancesUpdate;
var
  I,K: Integer;
  HasAssetsI, HasAssetsK: Boolean;
begin
  {for I := 0 to gHands.Count - 1 do
  begin
    HasAssetsI := gHands[I].HasAssets;
    for K := 0 to gHands.Count - 1 do
    begin
      HasAssetsK := gHands[K].HasAssets;
      CheckBox_Alliances[I,K].Enabled := HasAssetsI and HasAssetsK and (I <> K);
      CheckBox_Alliances[I,K].Checked := HasAssetsI and HasAssetsK and (gHands[I].Alliances[K] = atAlly);
    end;
  end;}

  for I := 0 to gHands.Count - 1 do
  begin
    HasAssetsI := gHands[I].HasAssets;
    for K := 0 to gHands.Count - 1 do
    begin
      HasAssetsK := gHands[K].HasAssets;
      //Grid_Alliances.Item[I, K].Visible := (I <> K);
      Grid_Alliances.Item[I, K].Enabled := HasAssetsI and HasAssetsK and (I <> K);
      Grid_Alliances.Item[I, K].Checked := HasAssetsI and HasAssetsK and (gHands[I].Alliances[K] = atAlly);
    end;
  end;

end;

procedure TKMMapEdMissionAlliances.CheckBoxGrid_Changed(Sender: TObject; X: Integer; Y: Integer; aItem : PKMGridItem);
const
  ALL: array [Boolean] of TKMAllianceType = (atEnemy, atAlly);
begin
  if Sender <> Grid_Alliances then
    Exit;

  gHands[X].Alliances[Y] := ALL[aItem.Checked or (X = Y)];

  //Always copy status to symmetrical item, since we don't allow unsymmetrical alliances yet
  //CheckBox_Alliances[Y,I].Checked := CheckBox_Alliances[I,K].Checked;
  Grid_Alliances.Item[Y, X].Checked := aItem.Checked;

  gHands[Y].Alliances[X] := gHands[X].Alliances[Y];

  //Mission_AlliancesUpdate;
end;

procedure TKMMapEdMissionAlliances.CheckBoxGrid_Stop(Sender: TObject);
begin
  Mission_AlliancesUpdate;
end;


procedure TKMMapEdMissionAlliances.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  if (Key = VK_ESCAPE) and PopUp_Alliances.Visible then
  begin
    Hide;
    aHandled := True;
  end;
end;


procedure TKMMapEdMissionAlliances.Hide;
begin
  PopUp_Alliances.Hide;
end;


procedure TKMMapEdMissionAlliances.Show;
begin
  Mission_AlliancesUpdate;
  PopUp_Alliances.Show;
end;


function TKMMapEdMissionAlliances.Visible: Boolean;
begin
  Result := Panel_Alliances.Visible;
end;


end.
