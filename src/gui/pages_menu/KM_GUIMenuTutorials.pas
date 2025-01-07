unit KM_GUIMenuTutorials;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_Defaults,
  KM_Maps, KM_MapTypes, KM_GameTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsList, KM_ControlsSwitch,
  KM_CommonTypes, KM_InterfaceDefaults, KM_InterfaceTypes, KM_ControlsMemo;


const
  MAX_UI_GOALS = 7;


type
  TKMMenuTutorial = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;
    fIsBattle : Boolean;
    procedure BackClick(Sender: TObject);
    //procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDoubleClick(Sender: TObject);
    procedure RefreshList;
  protected
    Panel_Tutorials: TKMPanel;
      Button_T_Back : TKMButton;
      Button_Tutorial : array of TKMButton;
      //List_Tutorials : TKMListBox;
      Label_Name : TKMLabel;
      Memo_Desription : TKMMemo;
  public
    OnNewSingleMap: TKMNewSingleMapEvent;

    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

    procedure Show(aBattleTutorials : Boolean = false);
    procedure Hide;
  end;


implementation
uses
  KM_ResTexts, KM_ResFonts, KM_ResTypes, KM_Resource,
  KM_CommonUtils, KM_RenderUI, KM_GameSettings,
  KM_MapUtils, KM_MapUtilsExt,
  IOUtils;

const
  PAD_VERT = 44; //Padding from top/bottom
  PAD_SIDE = 40; //Padding from sides
  BUTTON_DIST = 6;
  FLAG_W = 22;


{ TKMGUIMenuSingleMap }
constructor TKMMenuTutorial.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
begin
  inherited Create(gpTutorial);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;
  Panel_Tutorials := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);


    Button_T_Back   := TKMButton.Create(Panel_Tutorials,337,630,350,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_T_Back.OnClick   := BackClick;

    {List_Tutorials := TKMListBox.Create(Panel_Tutorials, 337, 50, 350, 500, fntOutline, bsGame);
    List_Tutorials.OnClick := ListBoxClick;
    List_Tutorials.OnChange := ListBoxClick;
    List_Tutorials.OnDoubleClick := ListBoxDoubleClick;
    List_Tutorials.TextAlign := taCenter;
    //TKMBevel.Create(Panel_Tutorials, 517, 50, 350, 50);}

    Label_Name := TKMLabel.Create(Panel_Tutorials, 517, 60, 350, 50, '', fntOutline, taCenter);
    Memo_Desription := TKMMemo.Create(Panel_Tutorials, 517, 100, 350, 450, fntMetal, bsGame);
    Memo_Desription.AnchorsStretch;
    Memo_Desription.WordWrap := True;
    Memo_Desription.ScrollDown := True;

    Label_Name.Hide;
    Memo_Desription.Hide;
end;

procedure TKMMenuTutorial.RefreshList;
var I, K : Integer;
  S, name : String;//path, name
  mapInfo : TKMMapInfo;
begin
  for I := 0 to high(Button_Tutorial) do
    Button_Tutorial[I].Hide;

  //gRes.JsonData.RefreshTutorials; //no need to refresh list of tutorials
  K := 0;
  if fIsBattle then
  begin
    for I := 0 to gRes.JsonData.BattleTutorialCount - 1 do
    begin
      S := gRes.JsonData.GetBattleTutorialPath(I);
      name := Copy(TPath.GetFileName(S), 1, length(TPath.GetFileName(S)) - 4 );

      mapInfo := TKMMapInfo.Create(ExeDir + TPath.GetDirectoryName(S) + PathDelim,
                                   name, true);

      if K > high(Button_Tutorial) then //add new button if needed
      begin
        SetLength(Button_Tutorial, K + 1);
        Button_Tutorial[K] := TKMButton.Create(Panel_Tutorials, 337, 200 + 40 * K, 350, 30, '', bsPaper);
        Button_Tutorial[K].Font := fntOutline;
      end;
      Button_Tutorial[K].Caption := mapInfo.MapName;
      Button_Tutorial[K].Show;
      Button_Tutorial[K].Tag := K;
      Button_Tutorial[K].OnClick := ListBoxDoubleClick;
      inc(K);
    end;
    Exit;
  end;
  for I := 0 to gRes.JsonData.TutorialCount - 1 do
  begin
    S := gRes.JsonData.GetTutorialPath(I);
    name := Copy(TPath.GetFileName(S), 1, length(TPath.GetFileName(S)) - 4 );
    mapInfo := TKMMapInfo.Create(ExeDir + TPath.GetDirectoryName(S) + PathDelim,
                                 name, true);

    if K > high(Button_Tutorial) then //add new button if needed
    begin
      SetLength(Button_Tutorial, K + 1);
      Button_Tutorial[K] := TKMButton.Create(Panel_Tutorials, 337, 200 + 40 * K, 350, 30, '', bsPaper);
      Button_Tutorial[K].Font := fntOutline;
    end;
    Button_Tutorial[K].Caption := mapInfo.MapName;
    Button_Tutorial[K].Show;
    Button_Tutorial[K].Tag := K;
    Button_Tutorial[K].OnClick := ListBoxDoubleClick;
    inc(K);
  end;




end;

procedure TKMMenuTutorial.Show(aBattleTutorials : Boolean = false);
begin
  fIsBattle := aBattleTutorials;
  Panel_Tutorials.Show;
  RefreshList;
  Label_Name.Caption := '';
  Memo_Desription.Text := '';
end;

procedure TKMMenuTutorial.Hide;
begin
  Panel_Tutorials.Hide;
end;


procedure TKMMenuTutorial.BackClick(Sender: TObject);
begin
  fOnPageChange(gpSingleplayer);
end;

{procedure TKMMenuTutorial.ListBoxClick(Sender: TObject);
var I : Integer;
  S, name : String;//path, name
  mapInfo : TKMMapInfo;
begin
  {I := List_Tutorials.ItemIndex; }
{  I := TKMButton(Sender).Tag;
  if I = -1 then
    Exit;
  S := gRes.Paths.GetTutorialPath(I);
  name := Copy(TPath.GetFileName(S), 1, length(TPath.GetFileName(S)) - 4 );


  mapInfo := TKMMapInfo.Create(ExeDir + TPath.GetDirectoryName(S) + PathDelim,
                               name, true);


  Label_Name.Caption := mapInfo.Name;
  Memo_Desription.Text := mapInfo.BigDesc;

end;}

procedure TKMMenuTutorial.ListBoxDoubleClick(Sender: TObject);
var I : Integer;
  S : String;//path
begin
{  I := List_Tutorials.ItemIndex;}
  I := TKMButton(Sender).Tag;

  if I = -1 then
    Exit;
  if fIsBattle then
  begin
    S := gRes.JsonData.GetBattleTutorialPath(I);

    if Assigned(OnNewSingleMap) then
      OnNewSingleMap(ExeDir + S, gRes.JsonData.BattleTutorial[I]);
  end else
  begin
    S := gRes.JsonData.GetTutorialPath(I);

    if Assigned(OnNewSingleMap) then
      OnNewSingleMap(ExeDir + S, gRes.JsonData.Tutorial[I]);

  end;

end;

end.
