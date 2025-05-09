unit KM_GUIMenuCredits;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF WDC} ShellAPI, Windows, {$ENDIF} // Required for OpenURL in Delphi
  {$IFDEF FPC} LCLIntf, {$ENDIF} // Required for OpenURL in Lazarus
  Classes,
  Vcl.Forms,
  KM_Controls, KM_ControlsBase,
  KM_Defaults,
  KM_InterfaceDefaults, KM_InterfaceTypes;


type
  TKMMenuCredits = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;

    procedure LinkClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_Credits: TKMPanel;
    Label_Contact: TKMLabel;
    Label_Credits_KaM: TKMLabelScroll;
    Label_Credits_Remake: TKMLabelScroll;
    Button_CreditsHomepage: TKMButton;
    Button_CreditsDiscord: TKMButton;
    Button_CreditsDiscord2: TKMButton;
    Button_CreditsFacebook: TKMButton;
    Button_CreditsBack: TKMButton;
  public
    OnToggleLocale: TKMToggleLocaleEvent;

    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
    procedure Show;
  end;


implementation
uses
  KM_RenderUI,
  KM_System, 
  KM_ResTexts, KM_Resource, KM_ResFonts, KM_ResLocales, KM_ResTypes,
  KM_GameSettings, KM_CommonUtils;


{ TKMGUIMainCredits }
constructor TKMMenuCredits.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
const
  OFFSET = 312;
begin
  inherited Create(gpCredits);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;

  Panel_Credits := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_Credits.AnchorsStretch;

    TKMLabel.Create(Panel_Credits, aParent.Width div 2 - OFFSET, 70, gResTexts[TX_CREDITS],fntOutline,taCenter);
    Label_Credits_Remake := TKMLabelScroll.Create(Panel_Credits, aParent.Width div 2 - OFFSET, 110, 0, aParent.Height - 130,
      gResTexts[TX_CREDITS_PROGRAMMING] + '|Krom|Lewin|Rey|King Dyron||' +
      gResTexts[TX_CREDITS_ADDITIONAL_PROGRAMMING] + '|Alex|andreus|Danjb|ZblCoder||' +
      gResTexts[TX_CREDITS_ADDITIONAL_GRAPHICS] + '|StarGazer|Malin|H.A.H.|King Dyron|Kinghts Dzapan|Siegfried89||' +
      gResTexts[TX_CREDITS_ADDITIONAL_MUSIC] + '|Andre Sklenar - www.juicelab.cz|Jaimy - KaM Revisited Soundtrack||' +
      gResTexts[TX_CREDITS_ADDITIONAL_SOUNDS] + '|trb1914||' +
      gResTexts[TX_CREDITS_ADDITIONAL_TRANSLATIONS] + '|' + gResLocales.TranslatorCredits + '|' +
      gResTexts[TX_CREDITS_SPECIAL] + '|Memgor-for making this mod with me|KaM Community members',
      fntGrey,
      taCenter);
    Label_Credits_Remake.Anchors := [anLeft, anTop, anBottom];

    TKMLabel.Create(Panel_Credits, aParent.Width div 2 + OFFSET, 70, gResTexts[TX_CREDITS_ORIGINAL], fntOutline, taCenter);
    Label_Credits_KaM := TKMLabelScroll.Create(Panel_Credits, aParent.Width div 2 + OFFSET, 110, 0, aParent.Height - 130, gResTexts[TX_CREDITS_TEXT], fntGrey, taCenter);
    Label_Credits_KaM.Anchors := [anLeft,anTop,anBottom];

    Label_Contact := TKMLabel.Create(Panel_Credits,350,477,324,75, '[$F8A070]Contact with mod authors:[$FFFFFF]|e-mail: nox.studio2011@gmail|Discord: KingDyron#6907', fntMetal, taCenter);

    Button_CreditsHomepage := TKMButton.Create(Panel_Credits,350,538,324,30, '[$F8A070]www.kamremake.com[]', bsMenu);
    Button_CreditsHomepage.Anchors := [anLeft,anBottom];
    Button_CreditsHomepage.OnClick := LinkClick;

    Button_CreditsDiscord2 := TKMButton.Create(Panel_Credits,350,574,324,30, '[$F8A070]KaM Expansion Discord[]', bsMenu);
    Button_CreditsDiscord2.Anchors := [anLeft,anBottom];
    Button_CreditsDiscord2.OnClick := LinkClick;

    Button_CreditsDiscord := TKMButton.Create(Panel_Credits,350,610,324,30, '[$F8A070]KaM Remake Discord[]', bsMenu);
    Button_CreditsDiscord.Anchors := [anLeft,anBottom];
    Button_CreditsDiscord.OnClick := LinkClick;

    Button_CreditsFacebook := TKMButton.Create(Panel_Credits,350,646,324,30, '[$F8A070]KaM Remake Facebook[]', bsMenu);
    Button_CreditsFacebook.Anchors := [anLeft,anBottom];
    Button_CreditsFacebook.OnClick := LinkClick;

    Button_CreditsBack := TKMButton.Create(Panel_Credits,350,700,324,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_CreditsBack.Anchors := [anLeft,anBottom];
    Button_CreditsBack.OnClick := BackClick;
end;


procedure TKMMenuCredits.LinkClick(Sender: TObject);

  //This can't be moved to e.g. KM_CommonUtils because the dedicated server needs that, and it must be Linux compatible
  procedure GoToURL(const aUrl: string);
  begin
    {$IFDEF WDC}
    ShellExecute(Application.Handle, 'open', PChar(aUrl), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
    {$IFDEF FPC}
    OpenURL(aUrl);
    {$ENDIF}
  end;

begin
  if Sender = Button_CreditsHomepage then
    GoToURL('http://www.kamremake.com/');
  if Sender = Button_CreditsFacebook then
    GoToURL('https://www.facebook.com/KaMRemake/');
  if Sender = Button_CreditsDiscord then
    GoToURL('https://discord.gg/UkkYceR');
  if Sender = Button_CreditsDiscord2 then
    GoToURL('https://discord.gg/ZxdAhN9JXM');
end;


procedure TKMMenuCredits.Show;
begin
  // Load asian fonts, since there are some credits information on asian languages
  // No need to redraw all UI, as we do on the Options page, since there is no info rendered on the credits page yet
  if gRes.Fonts.LoadLevel <> fllFull then
  begin
    gSystem.Cursor := kmcAnimatedDirSelector;
    gRes.LoadLocaleFonts(gGameSettings.Locale, True);
    gSystem.Cursor := kmcDefault;
  end;

  //Set initial position
  Label_Credits_KaM.SmoothScrollToTop := TimeGet;
  Label_Credits_Remake.SmoothScrollToTop := TimeGet;

  Panel_Credits.Show;
end;


procedure TKMMenuCredits.BackClick(Sender: TObject);
begin
  fOnPageChange(gpMainMenu);
end;


end.
