unit KM_GUIMapEdPlayerColors;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsColor, KM_ControlsEdit, KM_ControlsSwitch;

type
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptMarkers);

  TKMMapEdPlayerColors = class
  private
    procedure UpdateColor(aColor: Cardinal; aIsBGR: Boolean = True);
    procedure ColorCodeChange(Sender: TObject);
    procedure Player_ColorClick(Sender: TObject);
    function GetColorCodeText(aColor: Cardinal; aConvertFromBGR: Boolean): String;
  protected
    Panel_Color: TKMPanel;
      ColorSwatch_Color: TKMColorSwatch;
      //Components for Show Code BGR Color
      Radio_ColorCodeType: TKMRadioGroup;
      Shape_Color: TKMShape;
      Edit_ColorCode: TKMEdit;
      Panel_TextColor: TKMPanel;
        Label_TextColor: TKMLabel;
        Edit_TextColorCode: TKMEdit;
        Shape_TextColor: TKMShape;
  public
    constructor Create(aParent: TKMPanel);

    procedure UpdatePlayer(aUpdateColorEdit: Boolean = False);
    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  StrUtils,
  KM_Hand, KM_HandsCollection,
  KM_Game, KM_InterfaceGame,
  KM_ControlsTypes,
  KM_RenderUI,
  KM_ResTexts, KM_ResFonts,
  KM_CommonUtils;


{ TKMMapEdPlayerColors }
constructor TKMMapEdPlayerColors.Create(aParent: TKMPanel);
const
  MAX_COL = 288;
  SCOLOR_C = 16;
  SCOLOR_R = 18;
  SCOLOR_S = 12;
  COLOR_TYPE_W = 70;
  COLOR_TYPE_H = 70;
var
  Hue, Sat, Bri, I, K: Integer;
  R, G, B: Byte;
  Col: array [0..MAX_COL-1] of Cardinal;
  XRCCode, YRCCode: Integer;
begin
  inherited Create;

  Panel_Color := TKMPanel.Create(aParent, 0, 28, aParent.Width, 400);
  with TKMLabel.Create(Panel_Color, 0, PAGE_TITLE_Y, Panel_Color.Width, 0, gResTexts[TX_MAPED_PLAYER_COLORS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  with TKMBevel.Create(Panel_Color, (9 + (((Panel_Color.Width - 9) div 2) - ((SCOLOR_C * SCOLOR_S) div 2))) - 2, 30, (SCOLOR_C * SCOLOR_S) + 2, (SCOLOR_R * SCOLOR_S) + 2) do
    Anchors := [anLeft, anTop, anRight];
  ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 9 + (((Panel_Color.Width - 9) div 2) - ((SCOLOR_C * SCOLOR_S) div 2)), 32, SCOLOR_C, SCOLOR_R, SCOLOR_S);

  //Show Color Code
  TKMLabel.Create(Panel_Color, 9, ColorSwatch_Color.Top + ColorSwatch_Color.Height + 8, gResTexts[TX_MAPED_PLAYER_COLOR_CODE], fntOutline, taLeft);
  with TKMBevel.Create(Panel_Color, 9, ColorSwatch_Color.Top + ColorSwatch_Color.Height + 8 + 20, COLOR_TYPE_W, COLOR_TYPE_H) do
  begin
    XRCCode := Left;
    YRCCode := Top;
  end;
  Radio_ColorCodeType := TKMRadioGroup.Create(Panel_Color, XRCCode + ((COLOR_TYPE_W div 2) - ((COLOR_TYPE_W - 8) div 2)), YRCCode + ((COLOR_TYPE_H div 2) - 25), COLOR_TYPE_W - 8, 50, fntMetal);
  Radio_ColorCodeType.Add('BGR', gResTexts[TX_MAPED_PLAYER_COLOR_BGR_HINT]); //No need to translate BGR / RGB
  Radio_ColorCodeType.Add('RGB', gResTexts[TX_MAPED_PLAYER_COLOR_RGB_HINT]);
  Radio_ColorCodeType.OnChange := ColorCodeChange;
  Radio_ColorCodeType.ItemIndex := 0;

  TKMBevel.Create(Panel_Color, XRCCode +  COLOR_TYPE_W + 5, YRCCode, 20, 20);
  Shape_Color := TKMShape.Create(Panel_Color, XRCCode + COLOR_TYPE_W + 7, YRCCode + 2, 17, 17);
  Edit_ColorCode := TKMEdit.Create(Panel_Color, XRCCode + COLOR_TYPE_W + 25, YRCCode, Panel_Color.Width - 9 - COLOR_TYPE_W - 25, 20, fntMetal, True);
  Edit_ColorCode.AutoFocusable := False; // No need to make too much attention on that field
  Edit_ColorCode.Anchors := [anLeft, anTop, anRight];
  Edit_ColorCode.AllowedChars := acHex;
  Edit_ColorCode.MaxLen := 6;
  Edit_ColorCode.OnChange := ColorCodeChange;


  Panel_TextColor := TKMPanel.Create(Panel_Color, XRCCode +  COLOR_TYPE_W + 5, YRCCode + COLOR_TYPE_H - 40, Panel_Color.Width - 9 - COLOR_TYPE_W - 5, 40);
  Panel_TextColor.Anchors := [anLeft, anTop, anRight];
  Panel_TextColor.Hint := gResTexts[TX_MAPED_PLAYER_COLOR_TEXT_COLOR_HINT];
    Label_TextColor := TKMLabel.Create(Panel_TextColor, 0, 0, gResTexts[TX_MAPED_PLAYER_COLOR_TEXT_COLOR], fntGrey, taLeft);
    //Edit to show text color code (desaturated) Could be used for scripts overlay
    with TKMBevel.Create(Panel_TextColor, 0, 20, 20, 20) do
      Hint := gResTexts[TX_MAPED_PLAYER_COLOR_TEXT_COLOR_HINT];
    Shape_TextColor := TKMShape.Create(Panel_TextColor, 2, 22, 17, 17);
    Shape_TextColor.Hint := gResTexts[TX_MAPED_PLAYER_COLOR_TEXT_COLOR_HINT];
    Edit_TextColorCode := TKMEdit.Create(Panel_TextColor, 20, 20, Panel_TextColor.Width - 20, 20, fntMetal, True);
    Edit_TextColorCode.BlockInput := True;
    Edit_TextColorCode.AutoFocusable := False; // No need to make too much attention on that field
    Edit_TextColorCode.Hint := gResTexts[TX_MAPED_PLAYER_COLOR_TEXT_COLOR_HINT];

  //Generate a palette using HSB so the layout is more intuitive
  I := 0;
  for Hue := 0 to 16 do //Less than 17 hues doesn't give a good solid yellow hue
    for Bri := 1 to 4 do
      for Sat := 4 downto 1 do //Reversed saturation looks more natural
      begin
        ConvertHSB2RGB(Hue/17, Sat/4, Bri/5, R, G, B);
        Col[I] := (B shl 16) or (G shl 8) or R or $FF000000;
        Inc(I);
      end;
  //Add greyscale at the bottom
  for I := 0 to 15 do
  begin
    K := I*16;
    Col[MAX_COL-16+I] := (K shl 16) or (K shl 8) or K or $FF000000;
  end;

  ColorSwatch_Color.SetColors(Col);

  ColorSwatch_Color.OnClick := Player_ColorClick;
end;


procedure TKMMapEdPlayerColors.UpdateColor(aColor: Cardinal; aIsBGR: Boolean = True);
begin
  if not aIsBGR then //RGB
    aColor := RGB2BGR(aColor);

  gMySpectator.Hand.FlagColor := aColor;
  Shape_Color.FillColor := aColor;

  Shape_TextColor.FillColor := FlagColorToTextColor(aColor);
  Label_TextColor.SetColor(Shape_TextColor.FillColor);

  //Update minimap
  gGame.ActiveInterface.SyncUI(False);
end;


procedure TKMMapEdPlayerColors.ColorCodeChange(Sender: TObject);
var
  C: Cardinal;
begin
  Edit_ColorCode.SetTextSilently(UpperCase(Edit_ColorCode.Text)); //Will not trigger OnChange event
  if Length(Edit_ColorCode.Text) > 0 then
  begin
    C := StrToInt('$' + Edit_ColorCode.Text);
    C := C or $FF000000;
    if Sender = Radio_ColorCodeType then
    begin
      if Radio_ColorCodeType.ItemIndex = 0 then
        C := RGB2BGR(C)
      else
        C := BGR2RGB(C);
      Edit_ColorCode.Text := GetColorCodeText(C, False);
      Edit_TextColorCode.Text := GetColorCodeText(FlagColorToTextColor(C), False);
    end else
      UpdateColor(C, Radio_ColorCodeType.ItemIndex = 0);
  end;
end;


function TKMMapEdPlayerColors.GetColorCodeText(aColor: Cardinal; aConvertFromBGR: Boolean): String;
begin
  if aConvertFromBGR then
  begin
    if Radio_ColorCodeType.ItemIndex = 1 then //RGB
      aColor := BGR2RGB(aColor);
  end;

  Result := Format('%.6x', [aColor and $FFFFFF]);
end;


procedure TKMMapEdPlayerColors.Player_ColorClick(Sender: TObject);
var
  C: Cardinal;
begin
  C := ColorSwatch_Color.GetColor;
  UpdateColor(C);
  Edit_ColorCode.Text := GetColorCodeText(C, True);
end;


procedure TKMMapEdPlayerColors.Hide;
begin
  Panel_Color.Hide;
end;


procedure TKMMapEdPlayerColors.UpdatePlayer(aUpdateColorEdit: Boolean = False);
var
  colorText: UnicodeString;
begin
  ColorSwatch_Color.SelectByColor(gMySpectator.Hand.FlagColor);
  Shape_Color.FillColor := gMySpectator.Hand.FlagColor;
  Shape_TextColor.FillColor := FlagColorToTextColor(Shape_Color.FillColor);

  colorText := GetColorCodeText(gMySpectator.Hand.FlagColor, True);
  if aUpdateColorEdit or not AnsiEndsText(Edit_ColorCode.Text, colorText) then
    Edit_ColorCode.UpdateText(colorText);

  Edit_TextColorCode.UpdateText(GetColorCodeText(Shape_TextColor.FillColor, True));
  Label_TextColor.SetColor(Shape_TextColor.FillColor);
end;


procedure TKMMapEdPlayerColors.Show;
begin
  UpdatePlayer(True);
  Panel_Color.Show;
end;


function TKMMapEdPlayerColors.Visible: Boolean;
begin
  Result := Panel_Color.Visible;
end;


end.
