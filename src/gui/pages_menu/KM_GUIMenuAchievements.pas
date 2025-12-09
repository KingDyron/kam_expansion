unit KM_GUIMenuAchievements;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  Classes,
  KM_Defaults,
  KM_Achievements,
  KM_Maps, KM_MapTypes, KM_GameTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsScroll,
  KM_CommonTypes, KM_InterfaceDefaults, KM_InterfaceTypes, KM_ControlsMemo;


type
  TKMAchievementBox = class(TKMControl)
  private
    fAchName, fAchText, fAchHint : String;
    fAchievement : TKMAchievement;
    procedure MakeTexts(aText : String);
    procedure ButtonClicked(Sender : TObject);
    procedure SetAchievement(aAch : TKMAchievement);
    procedure RefreshAchievement;
  public

    Button_Left, Button_Right : TKMButton;
    Progress : Single;
    ShowNotification : Boolean;
    CurrentLevel, MaxLevels : Byte;

    property Achievement: TKMAchievement read fAchievement write SetAchievement;
    constructor Create(aParent : TKMPanel; aTop : Integer);

    procedure SetText(aText : String);

    procedure Paint; override;
  end;


  TKMMenuAchievements = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;
    procedure RefreshList;
    procedure BackClick(Sender: TObject);
  protected
    Panel_Main: TKMPanel;
      Bevel_Back : TKMBevel;
      Button_Back : TKMButton;
      Panel_List: TKMScrollPanel;
        Ach_List : array of TKMAchievementBox;

  public

    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_ResFonts, KM_ResTypes, KM_Resource,
  KM_CommonUtils, KM_RenderUI, KM_GameSettings,
  KM_MapUtils, KM_MapUtilsExt,
  IOUtils;

{ TKMGUIMenuSingleMap }
constructor TKMMenuAchievements.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
var I : integer;
begin
  inherited Create(gpAchievements);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;
  Panel_Main := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);

    Bevel_Back := TKMBevel.Create(Panel_Main, 100, 30, Panel_Main.Width - 200, Panel_Main.Height - 150);

    Panel_List := TKMScrollPanel.Create(Panel_Main, Bevel_Back.Left + 2, Bevel_Back.Top + 2, Bevel_Back.Width - 4, Bevel_Back.Height - 4,
                                        [saVertical], bsMenu, ssCommon);

    Panel_List.ScrollV.WheelStep := 115 div 2;

    SetLength(Ach_List, gAchievements.SortedCount);
    for I := 0 to gAchievements.SortedCount - 1 do
    begin
      Ach_List[I] := TKMAchievementBox.Create(Panel_List, I * 115);
      Ach_List[I].Tag := I;
    end;

  Button_Back := TKMButton.Create(Panel_Main,337,700,350,30,gResTexts[TX_MENU_BACK],bsMenu);
  Button_Back.OnClick   := BackClick;

end;

procedure TKMMenuAchievements.RefreshList;
var I : Integer;
begin
  for I := 0 to gAchievements.SortedCount - 1 do
  begin
    Ach_List[I].Achievement         := gAchievements.Sorted[I];
    {Ach_List[I].SetText(gAchievements.Sorted[I].FullText);
    Ach_List[I].Progress            := gAchievements.Sorted[I].CurrentProgress;
    Ach_List[I].ShowNotification    := gAchievements.Sorted[I].AbsToShow;
    Ach_List[I].CurrentLevel        := gAchievements.Sorted[I].CurrentLevel;
    Ach_List[I].MaxLevels           := gAchievements.Sorted[I].LevelsCount;}
  end;

end;

procedure TKMMenuAchievements.Show;
begin
  Panel_Main.Show;
  RefreshList;
end;

procedure TKMMenuAchievements.Hide;
var oldVisible : Boolean;
begin
  oldVisible := Panel_Main.Visible;
  Panel_Main.Hide;

  if oldVisible then
    gAchievements.Visit;

end;


procedure TKMMenuAchievements.BackClick(Sender: TObject);
begin
  Hide;
  fOnPageChange(gpMainMenu);
end;






constructor TKMAchievementBox.Create(aParent: TKMPanel; aTop: Integer);
begin
  Inherited Create(aParent, 0, aTop, 750, 120);
  Left := (aParent.Width div 2) - (Width div 2);
  fAchName := '';
  fAchText := '';
  fAchHint := '';
  Button_Left := TKMButton.Create(aParent, Left + 40, Top + 53, 20, 20, '<', bsBone);
  Button_Right := TKMButton.Create(aParent, Right - 75, Top + 53, 20, 20, '>', bsBone);
  Button_Left.OnClick := ButtonClicked;
  Button_Right.OnClick := ButtonClicked;
end;

procedure TKMAchievementBox.MakeTexts(aText: string);
var I, K, count : Integer;
begin
  fAchName := '';
  fAchText := '';
  fAchHint := '';

  count := 0;
  K := 0;
  for I := 1 to length(aText) do
  if aText[I] = '|' then
  begin
    if fAchName = '' then
      fAchName := Copy(aText, K, count)
    else
    if fAchText = '' then
      fAchText := Copy(aText, K, count)
    else
    if fAchHint = '' then
      fAchHint := Copy(aText, K, count);

    count := 0;
    K := I + 1;
  end else
    Inc(count);

  if fAchName = '' then
    fAchName := Copy(aText, K, count + 1)
  else
  if fAchText = '' then
    fAchText := Copy(aText, K, count + 1)
  else
  if fAchHint = '' then
    fAchHint := Copy(aText, K, count + 1);

  fAchText := gRes.Fonts[fntMetal].WordWrap(fAchText, Width - 35 * 2, True, False)
end;

procedure TKMAchievementBox.SetText(aText: string);
begin
  MakeTexts(aText);
  Hint := fAchHint;
end;

procedure TKMAchievementBox.ButtonClicked(Sender: TObject);
begin
  if Sender = Button_Left then
    Achievement := fAchievement.Parent
  else
  if Sender = Button_Right then
    Achievement := fAchievement.Next;

end;

procedure TKMAchievementBox.SetAchievement(aAch: TKMAchievement);
begin
  fAchievement := aAch;
  RefreshAchievement;
end;

procedure TKMAchievementBox.RefreshAchievement;
begin

  SetText(fAchievement.FullText);
  Progress            := fAchievement.CurrentProgress;
  ShowNotification    := fAchievement.AbsToShow;
  CurrentLevel        := fAchievement.CurrentLevel;
  MaxLevels           := fAchievement.LevelsCount;

  Button_Left.Visible := fAchievement.Parent <> nil;
  Button_Right.Visible := (fAchievement.Next <> nil) and fAchievement.Completed;

end;

procedure TKMAchievementBox.Paint;
const BAR_MARGIN = 100;
      TEXT_MARGIN = 50;
begin
  Inherited;

  TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, [anLeft, anTop, anRight, anBottom],
                            rxGuiMain, 104, self.Enabled);//big scroll
  TKMRenderUI.WriteBevel(AbsLeft + BAR_MARGIN, AbsBottom - 29, Width - BAR_MARGIN * 2, 10, 1, 0.75);
  TKMRenderUI.WritePercentBar(AbsLeft + BAR_MARGIN, AbsBottom - 33, Width - BAR_MARGIN * 2, 10, Progress, 0, 0,
                              pboLeft, $FF00FF00);//progress bar


  TKMRenderUI.SetupClipX(AbsLeft, AbsRight);
  TKMRenderUI.SetupClipY(AbsTop, AbsBottom);

  TKMRenderUI.WriteText(AbsLeft + 35, AbsTop + 25, Width - 35 * 2, fAchName, fntOutline, taCenter);//name
  TKMRenderUI.WriteText(AbsLeft + TEXT_MARGIN, AbsTop + 42, Width - TEXT_MARGIN * 2, fAchText, fntMetal, taCenter);//text
  TKMRenderUI.WriteText(AbsRight - 100, AbsBottom - 38, 50, IntToStr(trunc(Progress * 100)) + '%', fntMetal, taCenter); //progress in %


  TKMRenderUI.WriteText(AbsLeft + 35, AbsTop + 25, 50, CurrentLevel.ToString + '/' + MaxLevels.ToString, fntOutline, taCenter); //levels progress

  TKMRenderUI.ReleaseClipY;
  TKMRenderUI.ReleaseClipX;

  if ShowNotification then
    TKMRenderUI.WritePicture(AbsRight - 53, AbsTop + 21, 35, 33, [anLeft, anTop, anRight, anBottom],
                              rxGuiMain, 105, Enabled);

end;

end.
