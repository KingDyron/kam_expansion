unit KM_GUIMenuDebug;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils, Classes,
  KM_Defaults,
  KM_Maps, KM_MapTypes, KM_GameTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsList, KM_ControlsSwitch, KM_ControlsScroll,
  KM_ControlsEdit,
  KM_CommonTypes, KM_InterfaceDefaults, KM_InterfaceTypes,
  KM_ResTypes, KM_ResHouses, KM_ResDevelopment,

  KM_GUICommonDevelopment;


const
  MAX_UI_GOALS = 7;


type
  TKMHouseViewer = class(TKMControl)
    private
      fType : TKMHouseType;
      fColor : TKMColor3f;
    public
      Caption : UnicodeString;
      AnimStep : Cardinal;
      ViewAsConstruction : Boolean;
      WorkAnim : set of TKMHouseActionType;
      DoAnimStep : Boolean;
      Constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
      procedure SetHouseType(aHouse : TKMHouseType);
      procedure UpdateState(aGlobalTickCount : Cardinal); override;
      procedure Paint; Override;
  end;
  TKMButtonFlatDevMenu = class(TKMButtonFlat)
  public
    Dev : PKMDevelopment;
    Cost : Byte;
    procedure Paint; override;
  end;
  TKMDevRepoInfo = class
  public
    Button : TKMButtonFlat;
    ID : Integer;
    Dev : PKMDevelopment;
    X, Y : Integer;  //new position
    Next : array of TKMDevRepoInfo;
    Parent : TKMDevRepoInfo;
    constructor Create(aParent : TKMPanel; aOnClickShift : TNotifyEventShift; aX, aY : Integer);
    function AddNext(aParent : TKMPanel; aOnClickShift : TNotifyEventShift; aX, aY : Integer) : TKMDevRepoInfo;
    destructor Destroy; override;
    procedure Delete(aNext : TKMDevRepoInfo);
  end;

  TKMDevRepoTree = class(TKMPanel)
    public
      List : TKMDevRepoInfo;
      procedure Refresh;
      procedure Paint; override;
  end;


  TKMGUIDevelopmentDebug = class(TKMGUICommonDevelopment)
    protected
      function CreateButton(aParent : TKMPanel) : TKMButtonFlat; override;
      procedure SetUpButton(B : TKMButtonFlat; aDev : PKMDevelopment); override;
      procedure RefreshSingle(B : TKMButtonFlat; aDev : PKMDevelopment);  override;
  end;

  TKMMenuDebug = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;

    fSelectedAnim : Integer;
    fHouse : TKMHouseType;

    fCopiedAnim : TKMAnimation;
    fSelectedDev : Pointer;

    procedure BackClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ListClicked(Sender: TObject);
    procedure SelectHouse(aHouseType : TKMHouseType);


    procedure AnimationChanged(Sender: TObject);
    procedure AnimationChangedShift(Sender: TObject; Shift : TShiftState);

    procedure PileOffsetChanged(Sender: TObject);

    procedure SwitchType(Sender: TObject);
    procedure SwitchButton(Sender: TObject);
    procedure RefreshViewer;
    procedure RefreshControls;


    function Spec : TKMHouseSpec;
    procedure EditAnimationPos;

    procedure TreeButtonClicked(Sender: TObject; Shift : TShiftState);
    procedure TreeDevelopmentChange(Sender : TObject);

    procedure TreeRepoButtonClicked(Sender: TObject; Shift : TShiftState);
    procedure TreeRepoChange(Sender : TObject);
    procedure SaveRepo;
  protected
    Panel_Debug: TKMPanel;
      Button_T_Back : TKMButton;
      Button_SaveRes : TKMButton;
      ColumnBox_Houses : TKMColumnBox;
      House_Viewer : TKMHouseViewer;

      Panel_Edit_Scroll : TKMScrollPanel;
        Switch_Type : TKMSwitch;
        Switch_Left, Switch_Right : TKMButton;
        Label_Type : TKMLabel;

        Panel_Type : array of TKMPanel;
          //animations
          Edit_X, Edit_Y, Edit_TextID, Edit_StepStart, Edit_StepsCount : TKMNumericEdit;
          Button_AddStep,
          Button_Clear,
          Button_SetAnim,
          Button_CopyAnim,
          Button_PasteAnim : TKMButton;
          List_AnimSteps : TKMListBox;
          Panel_CheckBoxScroll : TKMScrollPanel;
            CheckBox_AnimType : array[TKMHouseActionType] of TKMCheckBoxFlat;
          //supply pile offsets
            Pile_X, Pile_Y : array[0..2] of TKMNumericEdit;

          //development tree
          Tree : TKMGUIDevelopmentDebug;
            Dev_GuiIcon : TKMNumericEdit;
            Position_X : TKMNumericEdit;
            Cost : TKMNumericEdit;
            HintID : TKMNumericEdit;
            Dev_Special : TKMCheckBox;
            Button_AddDev : TKMButton;
            Button_DelDev : TKMButton;
            Button_SavDev : TKMButton;
          //Development reposition
          Button_SaveRepo : TKMButton;
          SelectedTreeType : TKMDevelopmentTreeType;
          Button_SelectType : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMButton;
          Tree_Repo : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMDevRepoTree;
            Repo_DevID,
            Repo_Position_X,
            Repo_Position_Y : TKMNumericEdit;
            Repo_Button_Add, Repo_Button_Del : TKMButton;




  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_Points,
  KM_ResTexts, KM_ResFonts, KM_Resource,
  KM_CommonUtils, KM_RenderUI, KM_Pics,
  KM_JsonHelpers,
  IOUtils;

const
  PAD_VERT = 44; //Padding from top/bottom
  PAD_SIDE = 40; //Padding from sides
  BUTTON_DIST = 6;
  FLAG_W = 22;


{ TKMGUIMenuSingleMap }
constructor TKMMenuDebug.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
  function AddNewPanel : TKMPanel;
  begin
    SetLength(Panel_Type, length(Panel_Type) + 1);
    Panel_Type[high(Panel_Type)] := TKMPanel.Create(Panel_Edit_Scroll, 0, 60, Panel_Edit_Scroll.Width, Panel_Edit_Scroll.Height - 60);
    Result := Panel_Type[high(Panel_Type)];
  end;

var HT : TKMHouseType;
  I, K, L : Integer;
  HA : TKMHouseActionType;
  P : TKMPanel;
  dtt : TKMDevelopmentTreeType;
begin
  inherited Create(gpDebug);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := BackClick;
  Panel_Debug := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);


    Button_T_Back   := TKMButton.Create(Panel_Debug,10,Panel_Debug.Bottom - 40 ,350,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_T_Back.OnClick   := BackClick;

    Button_SaveRes   := TKMButton.Create(Panel_Debug,Button_T_Back.Right + 10,Panel_Debug.Bottom - 40 ,350,30, 'Save to json [Export]',bsMenu);
    Button_SaveRes.OnClick  := SaveClick;

    ColumnBox_Houses := TKMColumnBox.Create(Panel_Debug, 720, 30, 265, 680, fntOutline, bsGame);
    ColumnBox_Houses.SetColumns(fntOutline, ['HouseType:', ''], [0, 200]);
    ColumnBox_Houses.OnChange := ListClicked;
    ColumnBox_Houses.ItemHeight := 30;
    ColumnBox_Houses.ShowLines := true;
    ColumnBox_Houses.ShowHintWhenShort := true;

    for I := low(HOUSE_GUI_TAB_ORDER) to High(HOUSE_GUI_TAB_ORDER) do
    for K := Low(HOUSE_GUI_TAB_ORDER[I].H) to High(HOUSE_GUI_TAB_ORDER[I].H) do
    for L := Low(HOUSE_GUI_TAB_ORDER[I].H[K]) to High(HOUSE_GUI_TAB_ORDER[I].H[K]) do
    begin
      HT :=HOUSE_GUI_TAB_ORDER[I].H[K, L];
      If not (HT in HOUSES_VALID) then
        Continue;
      ColumnBox_Houses.AddItem(MakeListRow([gres.Houses[HT].HouseName, ''],
                                [icWhite, icWhite],
                                [PIC_CLEAR, MakePic(rxGui, gRes.Houses[HT].GUIIcon)], byte(HT))
                                );
    end;
  House_Viewer := TKMHouseViewer.Create(Panel_Debug, 375, 134, 400, 450);
  House_Viewer.Left := (Panel_Debug.Width div 2) - (House_Viewer.Width div 2);
  House_Viewer.Top := (Panel_Debug.Height div 2) - (House_Viewer.Height div 2);


  Tree := TKMGUIDevelopmentDebug.Create(Panel_Debug, Panel_Debug.Width div 2 - 90, 50, 180, Panel_Debug.Height - 125);
  Tree.OnButtonClickedShift := TreeButtonClicked;

  TKMBevel.Create(Panel_Debug, 30, 30, 270, 680);
  Panel_Edit_Scroll := TKMScrollPanel.Create(Panel_Debug, 30, 30, 270, 680, [saVertical], bsGame, ssGame);

    Switch_Type := TKMSwitch.Create(Panel_Edit_Scroll, 0, 0, Panel_Edit_Scroll.Width, 30);
    Switch_Type.OnChange := SwitchType;
    Switch_Type.TexID := [37, 38, 39, 40, 41, 43, 51];
    Switch_Type.Offset := 80;
    Label_Type := TKMLabel.Create(Panel_Edit_Scroll, 0, 35, Panel_Edit_Scroll.Width, 20, 'Animation', fntOutline, taCenter);
    Switch_Left := TKMButton.Create(Panel_Edit_Scroll, 0, 0, 30, 30, 2, rxGui, bsGame);
    Switch_Right := TKMButton.Create(Panel_Edit_Scroll, Panel_Edit_Scroll.Width - 30, 0, 30, 30, 3, rxGui, bsGame);
    Switch_Left.OnClick := SwitchButton;
    Switch_Right.OnClick := SwitchButton;

    P := AddNewPanel;//animations
    TKMBevel.Create(P, 10, 0, 120, 20*15);
    Panel_CheckBoxScroll := TKMScrollPanel.Create(P, 10, 0, 120, 20*15, [saVertical], bsGame, ssGame);

      for HA := Low(TKMHouseActionType) to High(TKMHouseActionType) do
      begin
        CheckBox_AnimType[HA] := TKMCheckBoxFlat.Create(Panel_CheckBoxScroll, 0, byte(HA) * 23, 120, 20, HOUSE_ACTION_STR[HA], fntGrey);
        CheckBox_AnimType[HA].Checked := false;
        CheckBox_AnimType[HA].Tag := byte(HA);
        CheckBox_AnimType[HA].Tag2 := 1;
        CheckBox_AnimType[HA].LineWidth := 2;
        CheckBox_AnimType[HA].LineColor := $FFFF0055;
        CheckBox_AnimType[HA].OnClickShift := AnimationChangedShift;
        CheckBox_AnimType[HA].Checked := HA in [haFlag1, haFlag2, haFlag3, haFlagPole, haSmoke, haIdle];
      end;

    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, Panel_CheckBoxScroll.Top, 100, 20, 'Position:', fntMetal, taLeft);
    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, Panel_CheckBoxScroll.Top + 22, 20, 20, 'X:', fntMetal, taLeft);
      Edit_X := TKMNumericEdit.Create(P, Panel_CheckBoxScroll.Right + 45, Panel_CheckBoxScroll.Top + 20, -500, 500);
      Edit_X.OnChange := AnimationChanged;
      Edit_X.Width := 80;
    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, Panel_CheckBoxScroll.Top + 47, 20, 20, 'Y:', fntMetal, taLeft);
      Edit_Y := TKMNumericEdit.Create(P, Panel_CheckBoxScroll.Right + 45, Panel_CheckBoxScroll.Top + 45, -500, 500);
      Edit_Y.OnChange := AnimationChanged;
      Edit_Y.Width := 80;

    List_AnimSteps := TKMListBox.Create(P, Panel_CheckBoxScroll.Left, Panel_CheckBoxScroll.Bottom + 3,
                                        Panel_CheckBoxScroll.Width, Panel_CheckBoxScroll.Height, fntGrey, bsGame);
    List_AnimSteps.OnChange := AnimationChanged;

    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, List_AnimSteps.Top, 100, 20, 'StepID', fntMetal, taLeft);
      Edit_TextID := TKMNumericEdit.Create(P, Panel_CheckBoxScroll.Right + 20, List_AnimSteps.Top + 20, 0, high(Word));
      Edit_TextID.OnChange := AnimationChanged;
      Edit_TextID.Width := 100;

    Button_AddStep :=  TKMButton.Create(P, Panel_CheckBoxScroll.Right + 20, List_AnimSteps.Top + 45, 100, 25, 'Add/Del', bsGame);
    Button_AddStep.Hint := 'Shift + Click : Duplicate selected step';
    Button_AddStep.OnClickShift := AnimationChangedShift;

    Button_Clear :=  TKMButton.Create(P, Button_AddStep.Left, Button_AddStep.Bottom + 5, 100, 25, 'Clear', bsGame);
    //Button_Clear.Hint := 'Shift + Click : Duplicate selected step';
    Button_Clear.OnClickShift := AnimationChangedShift;

    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, Button_Clear.Bottom + 5, 100, 20, 'From ID', fntMetal, taLeft);
      Edit_StepStart := TKMNumericEdit.Create(P, Panel_CheckBoxScroll.Right + 20, Button_Clear.Bottom + 25, 0, high(Word));
      Edit_StepStart.OnChange := AnimationChanged;
      Edit_StepStart.Width := 100;

    TKMLabel.Create(P, Panel_CheckBoxScroll.Right + 25, Button_Clear.Bottom + 45, 100, 20, 'steps count', fntMetal, taLeft);
      Edit_StepsCount := TKMNumericEdit.Create(P, Panel_CheckBoxScroll.Right + 20, Button_Clear.Bottom + 65, 0, 100);
      Edit_StepsCount.OnChange := AnimationChanged;
      Edit_StepsCount.Width := 100;

    Button_SetAnim :=  TKMButton.Create(P, Button_AddStep.Left, Edit_StepsCount.Bottom + 5, 100, 25, 'Set anim', bsGame);
    Button_SetAnim.OnClickShift := AnimationChangedShift;
    Button_CopyAnim :=  TKMButton.Create(P, Button_AddStep.Left, Button_SetAnim.Bottom + 20, 100, 25, 'Copy', bsGame);
    Button_CopyAnim.OnClickShift := AnimationChangedShift;
    Button_PasteAnim :=  TKMButton.Create(P, Button_AddStep.Left, Button_CopyAnim.Bottom + 5, 100, 25, 'Paste', bsGame);
    Button_PasteAnim.OnClickShift := AnimationChangedShift;
    Button_PasteAnim.Enabled := false;


    P := AddNewPanel;//Pile offsets

    for I := Low(Pile_X) to High(Pile_X) do
    begin
      case I of
        0:TKMLabel.Create(P, 10, I * 70, 200, 20, 'Wood pile offset', fntMetal, taLeft);
        1:TKMLabel.Create(P, 10, I * 100, 200, 20, 'Stone pile offset', fntMetal, taLeft);
        2:TKMLabel.Create(P, 10, I * 100, 200, 20, 'Tile pile offset', fntMetal, taLeft);
      end;
      TKMLabel.Create(P, 10, I * 100 + 20, 20, 20, 'X:', fntMetal, taLeft);
        Pile_X[I] := TKMNumericEdit.Create(P, 30, I * 100 + 20, -200, 200);
        Pile_X[I].Width := 100;
        Pile_X[I].OnChange := PileOffsetChanged;
      TKMLabel.Create(P, 10, I * 100 + 45, 20, 20, 'Y:', fntMetal, taLeft);
        Pile_Y[I] := TKMNumericEdit.Create(P, 30, I * 100 + 45, -200, 200);
        Pile_Y[I].Width := 100;
        Pile_Y[I].OnChange := PileOffsetChanged;
    end;

    P := AddNewPanel;//development
      Dev_GuiIcon := TKMNumericEdit.Create(P, 30, 10, 0, 2000);
      Dev_GuiIcon.OnChange := TreeDevelopmentChange;
      Dev_GuiIcon.Width := 80;
      Dev_GuiIcon.Hint := 'Gui Icon';

      Position_X := TKMNumericEdit.Create(P, 30, 40, -1, 5);
      Position_X.OnChange := TreeDevelopmentChange;
      Position_X.Width := 80;
      Position_X.Hint := 'Position X';

      Cost := TKMNumericEdit.Create(P, 30, 70, 1, 64);
      Cost.OnChange := TreeDevelopmentChange;
      Cost.Width := 80;
      Cost.Hint := 'Cost';

      HintID := TKMNumericEdit.Create(P, 30, 100, 0, 300);
      HintID.OnChange := TreeDevelopmentChange;
      HintID.Width := 80;
      HintID.Hint := 'Hint';

      Dev_Special := TKMCheckBox.Create(P, 30, 130, 120, 20, 'Special', fntGame);
      Dev_Special.OnClick := TreeDevelopmentChange;
      Button_AddDev := TKMButton.Create(P, 30, 160, 75, 25, 'Add', bsGame);
      Button_AddDev.OnClick := TreeDevelopmentChange;
      Button_DelDev := TKMButton.Create(P, 30, 185, 75, 25, 'Del', bsGame);
      Button_DelDev.OnClick := TreeDevelopmentChange;
      Button_SavDev := TKMButton.Create(P, 30, 210, 75, 25, 'Save', bsGame);
      Button_SavDev.OnClick := TreeDevelopmentChange;
  SelectedTreeType := dttBuilder;
  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
  begin
    Button_SelectType[dtt] := TKMButton.Create(Panel_Debug, Panel_Debug.Width div 2 - 90 + byte(dtt) * 30, 50, 30, 30, TREE_TYPE_ICON[dtt], rxGui, bsGame);
    Button_SelectType[dtt].OnClickShift := TreeRepoButtonClicked;
    Button_SelectType[dtt].Tag := byte(dtt);

    Tree_Repo[dtt] := TKMDevRepoTree.Create(Panel_Debug, Panel_Debug.Width div 2 - 90, 80, 180, Panel_Debug.Height - 125);
    Tree_Repo[dtt].List := TKMDevRepoInfo.Create(Tree_Repo[dtt], TreeRepoButtonClicked, 2, 0);
    Tree_Repo[dtt].List.Button.Tag := Integer(Tree_Repo[dtt].List);
    Tree_Repo[dtt].List.Button.Down := true;
    Tree_Repo[dtt].Tag := Tree_Repo[dtt].List.Button.Tag;

    Tree_Repo[dtt].List.Dev := gRes.Development[SelectedTreeType].GetItem(Tree_Repo[dtt].List.Id);
    If Tree_Repo[dtt].List.Dev <> nil then
    begin
      Tree_Repo[dtt].List.Button.TexID := Tree_Repo[dtt].List.Dev.GuiIcon;
      Tree_Repo[dtt].List.Button.Hint := gRes.Development.GetText(Tree_Repo[dtt].List.Dev.HintID);
      Tree_Repo[dtt].List.Button.Caption := Tree_Repo[dtt].List.ID.ToString;
    end;
  end;
    P := AddNewPanel;//development  Repo
      Repo_Position_X := TKMNumericEdit.Create(P, 30, 20, 0, 4);
      Repo_Position_X.OnChange := TreeRepoChange;
      Repo_Position_X.Width := 80;
      Repo_Position_X.Hint := 'Position X';
      Repo_Position_X.Value := 2;
      Repo_Position_Y := TKMNumericEdit.Create(P, 30, 40, 0, 4);
      Repo_Position_Y.OnChange := TreeRepoChange;
      Repo_Position_Y.Width := 80;
      Repo_Position_Y.Hint := 'Position Y';
      Repo_Position_Y.Value := 0;

      Repo_DevID := TKMNumericEdit.Create(P, 30, 70, 0, 100);
      Repo_DevID.OnChange := TreeRepoChange;
      Repo_DevID.Width := 80;
      Repo_DevID.Hint := 'Dev ID';
      Repo_Button_Add := TKMButton.Create(P, 30, 160, 75, 25, 'Add', bsGame);
      Repo_Button_Add.OnClick := TreeRepoChange;
      Repo_Button_Del := TKMButton.Create(P, 30, 180, 75, 25, 'Del', bsGame);
      Repo_Button_Del.OnClick := TreeRepoChange;
      Button_SaveRepo := TKMButton.Create(P, 30, 220, 75, 25, 'Save', bsGame);
      Button_SaveRepo.OnClick := TreeRepoChange;

  Switch_Type.Selected := 0;
  SwitchType(nil);
end;

procedure TKMMenuDebug.Show;
begin
  Panel_Debug.Show;
end;

procedure TKMMenuDebug.Hide;
begin
  Panel_Debug.Hide;
end;


procedure TKMMenuDebug.BackClick(Sender: TObject);
begin
  fOnPageChange(gpMainMenu);
end;

procedure TKMMenuDebug.SaveClick(Sender: TObject);
begin
  gRes.Houses.SaveToJson(ExeDir + 'Export' + PathDelim + 'new_houses.json');
end;

procedure TKMMenuDebug.ListClicked(Sender: TObject);
begin
  SelectHouse(TKMHouseType(ColumnBox_Houses.SelectedItemTag));
end;

procedure TKMMenuDebug.SelectHouse(aHouseType : TKMHouseType);
begin
  House_Viewer.SetHouseType(aHouseType);
  fHouse := aHouseType;
  RefreshControls;
end;

procedure TKMMenuDebug.RefreshViewer;
var HA : TKMHouseActionType;
begin
  House_Viewer.WorkAnim := [];
  for HA := Low(TKMHouseActionType) to High(TKMHouseActionType) do
    if CheckBox_AnimType[HA].Checked then
      House_Viewer.WorkAnim := House_Viewer.WorkAnim + [HA];


end;

procedure TKMMenuDebug.RefreshControls;
var I : Integer;
begin
  for I := 0 to High(Panel_Type) do
    Panel_Type[I].Enabled := ((fHouse in HOUSES_VALID) and (Switch_Type.Selected < 2))
                             or (Switch_Type.Selected = 2)
                             or (Switch_Type.Selected = 3);

    Edit_X.Enabled := fSelectedAnim <> -1;
    Edit_Y.Enabled := fSelectedAnim <> -1;

  if not (fHouse in HOUSES_VALID) then
    Exit;

  for I := 0 to 2 do
  begin
    Pile_X[I].Value := Spec.BuildSupply[I + 1].MoveX;
    Pile_Y[I].Value := Spec.BuildSupply[I + 1].MoveY;
  end;
end;

procedure TKMMenuDebug.AnimationChanged(Sender: TObject);
  procedure RefreshAnimList(A : TKMAnimation);
  var I, J, T : integer;
  begin
    T := List_AnimSteps.TopIndex;
    J := List_AnimSteps.ItemIndex;
    List_AnimSteps.Clear;
    for I := 1 to A.Count do
      List_AnimSteps.Add(IntToStr(A.Step[I - 1]));
    List_AnimSteps.ItemIndex := J;
    List_AnimSteps.TopIndex := T;
  end;
var I : Integer;
  A : TKMAnimation;
  HA : TKMHouseActionType;
begin
  if not (fHouse In HOUSES_VALID) then
    Exit;

  HA := haWork1;
  If TKMControl(Sender).Tag2 = 0 then
  begin
    If (fSelectedAnim = -1) then
      Exit;
    HA := TKMHouseActionType(fSelectedAnim);
  end;

  If (Sender = Edit_X) or (Sender = Edit_Y) then
  begin
    EditAnimationPos;
  end else
  If (Sender = Edit_TextID) then
  begin
    If not List_AnimSteps.Selected then
      Exit;
    A := Spec.Anim[HA];
    A.Animation[List_AnimSteps.ItemIndex] := Edit_TextID.Value;
    List_AnimSteps.Items[List_AnimSteps.ItemIndex] := IntToStr(Edit_TextID.Value);
  end else
  If Sender = List_AnimSteps then
  begin
    If not List_AnimSteps.Selected then
      Exit;
    If TryStrToInt(List_AnimSteps.SelectedItem, I) then
      Edit_TextID.Value := I;
  end;

  RefreshControls;
end;

procedure TKMMenuDebug.AnimationChangedShift(Sender: TObject; Shift: TShiftState);
  procedure RefreshAnimList(A : TKMAnimation);
  var I, J, T : integer;
  begin
    T := List_AnimSteps.TopIndex;
    J := List_AnimSteps.ItemIndex;
    List_AnimSteps.Clear;
    for I := 1 to A.Count do
      List_AnimSteps.Add(IntToStr(A.Step[I - 1]));
    List_AnimSteps.ItemIndex := J;
    List_AnimSteps.TopIndex := T;
  end;
var HA : TKMHouseActionType;
  I : Integer;
  A : TKMAnimation;
begin
  if not (fHouse In HOUSES_VALID) then
    Exit;
  HA := haWork1;
  If TKMControl(Sender).Tag2 = 0 then
  begin
    If (fSelectedAnim = -1) then
      Exit;
    HA := TKMHouseActionType(fSelectedAnim);
  end;

  If sender = Button_PasteAnim then
  begin
    A := fCopiedAnim;
    Spec.DebugChangeAnimation(HA, A);
    RefreshAnimList(A);
  end else
  If sender = Button_CopyAnim then
  begin
    A := Spec.Anim[HA];
    IF A.Count > 0 then
    begin
      fCopiedAnim := A;
      Button_PasteAnim.Enabled := true;
    end;
  end;
  If Sender = Button_AddStep then
  begin
    If ssRight in Shift then
    begin
      A := Spec.Anim[TKMHouseActionType(fSelectedAnim)];
      A.Delete(List_AnimSteps.ItemIndex);
      RefreshAnimList(A);
      Spec.DebugChangeAnimation(HA, A);
    end else
    begin
      If (Edit_TextID.Value = 0) then
        Exit;
      A := Spec.Anim[TKMHouseActionType(fSelectedAnim)];
      A.Animation[A.Count] := Edit_TextID.Value;
      RefreshAnimList(A);
      List_AnimSteps.ItemIndex := List_AnimSteps.Count - 1;
      Spec.DebugChangeAnimation(HA, A);
    end;

  end else
  If Sender = Button_Clear then
  begin
    A.Clear;
    Spec.DebugChangeAnimation(HA, A);
    RefreshAnimList(A);
  end else
  If Sender = Button_SetAnim then
  begin
    If (Edit_StepsCount.Value = 0) or (Edit_StepStart.Value = 0) then
      Exit;
    A.Create(Edit_X.Value, Edit_Y.Value, Edit_StepStart.Value, Edit_StepsCount.Value);
    Spec.DebugChangeAnimation(HA, A);
    RefreshAnimList(A);
  end else
  begin
    fSelectedAnim := -1;
    for HA := Low(TKMHouseActionType) to High(TKMHouseActionType) do
    begin
      If (Sender = CheckBox_AnimType[HA]) then
      begin
        If ssShift in Shift then
          CheckBox_AnimType[HA].Checked := true;
        If CheckBox_AnimType[HA].Checked then
          fSelectedAnim := ord(HA);
      end;
      CheckBox_AnimType[HA].DrawOutline := (fSelectedAnim = ord(HA)) and CheckBox_AnimType[HA].Checked;
    end;
    If fSelectedAnim <> -1 then
    begin
      A := Spec.Anim[TKMHouseActionType(fSelectedAnim)];
      Edit_X.Value := A.X;
      Edit_Y.Value := A.Y;
      List_AnimSteps.Clear;
      for I := 1 to A.Count do
        List_AnimSteps.Add(IntToStr(A.Step[I - 1]));
      List_AnimSteps.ItemIndex := -1;
      List_AnimSteps.TopIndex := 0;
    end;
  end;

  RefreshViewer;
  RefreshControls;
end;

procedure TKMMenuDebug.PileOffsetChanged(Sender: TObject);
var I : Integer;
begin
  for I := 0 to 2 do
  begin
    Spec.DebugChangePileOffset(I + 1, Pile_X[I].Value, Pile_Y[I].Value)
  end;
end;

procedure TKMMenuDebug.SwitchType(Sender: TObject);
var I : Integer;
  dtt : TKMDevelopmentTreeType;
begin
  If Tree = nil then
    Exit;

  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
  begin
    Tree_Repo[dtt].Visible := (Switch_Type.Selected = 3) and (SelectedTreeType = dtt);
    Button_SelectType[dtt].Visible := (Switch_Type.Selected = 3);
  end;

  Tree.Visible := Switch_Type.Selected = 2;
  ColumnBox_Houses.Visible := Switch_Type.Selected < 2;
  House_Viewer.Visible := Switch_Type.Selected < 2;

  case Switch_Type.Selected of
    0 : Label_Type.Caption := 'Animations';
    1 : Label_Type.Caption := 'Supply Pile Offset';
    2 : Label_Type.Caption := 'Development tree';
  end;
  Button_SaveRes.Visible := Switch_Type.Selected < 2;
  for I := 0 to High(Panel_Type) do
    Panel_Type[I].Visible := Switch_Type.Selected = I;
  House_Viewer.ViewAsConstruction := Switch_Type.Selected = 1;
  RefreshControls;
end;

procedure TKMMenuDebug.SwitchButton(Sender: TObject);
begin
  If Sender = Switch_Left then
    Switch_Type.SelectPrevius
  else
    Switch_Type.SelectNext;
end;


function TKMMenuDebug.Spec: TKMHouseSpec;
begin
  Result := gRes.Houses[fHouse];
end;

procedure TKMMenuDebug.EditAnimationPos;
var A : TKMAnimation;
  HA : TKMHouseActionType;
begin
  If fSelectedAnim = -1 then
    Exit;
  HA := TKMHouseActionType(fSelectedAnim);
  A := Spec.Anim[HA];
  A.X := Edit_X.Value;
  A.Y := Edit_Y.Value;
  Spec.DebugChangeAnimation(HA, A);
end;


procedure  TKMMenuDebug.TreeButtonClicked(Sender: TObject; Shift : TShiftState);
var dev : PKMDevelopment;
begin
  dev := PKMDevelopment(TKMButtonFlatDevmenu(Sender).Dev);
  fSelectedDev := dev;
  Position_X.Value := dev.X;
  Dev_GuiIcon.Value := dev.GuiIcon;
  Cost.Value := dev.Cost;
  HintID.Value := dev.HintID;
  Dev_Special.Checked := dev.IsSpecial;
end;

procedure TKMMenuDebug.TreeDevelopmentChange(Sender : TObject);
var
  dev, devParent, next : PKMDevelopment;
  oldX : Byte;

  function GetPositionY : Byte;
  var tmp : PKMDevelopment;
  begin
    Result := 0;
    tmp := dev;
    while tmp.Parent <> nil do
    begin
      Inc(Result);
      tmp := tmp.Parent;
    end;
  end;

  function CheckCollision(aX : Byte) : Boolean;
  var I : Integer;
    takenPlaces : TKMByteArray;
  begin
    Result := false;
    takenPlaces := gRes.Development.GetAllTakenAtY(Tree.CurrentPage, GetPositionY);
    for I := 0 to high(takenPlaces) do
      If (takenPlaces[I] = aX)then
        Exit(true);
  end;

  procedure SetNextPos;
  var I : Integer;
    J, K: Integer;
  begin
    J := EnsureRange(Position_X.Value - oldX, -1, 1);
    K := oldX;
    IncLoop(K, 0, 4, J);
    If CheckCollision(K) then
      for I := 1 to 4 do
      begin
        //dev.X := (oldX + (I * J)) mod 5;
        IncLoop(K, 0, 4, J);
        If not CheckCollision(K) then
          Break;
      end;
    dev.X := K;
  end;

  function GetFreeSpace : Byte;
  var I, J, K : Integer;
    Collides : Boolean;
    takenPlaces : TKMByteArray;
  begin
    Result := 255;
    If length(dev.Next) = 5 then
      Exit;
    takenPlaces := gRes.Development.GetAllTakenAtY(Tree.CurrentPage, GetPositionY + 1);
    for J := 0 to 4 do
    begin
      K := (dev.X + J) mod 5;
      Collides := false;
      for I := 0 to high(takenPlaces) do
        If (K = takenPlaces[I]) then
          Collides := true;

      If not Collides then
        Exit(K);
    end;

  end;

var temp : Byte;
begin

  If sender = Button_SavDev then
  begin
    gRes.Development.SaveTOJson;
    Exit;
  end;



  dev := PKMDevelopment(fSelectedDev);
  If dev = nil then
    Exit;
  devParent := dev.Parent;
  If Sender = Dev_Special then
  begin
    dev.IsSpecial := Dev_Special.Checked;
    Tree.RefreshButton(dev);
  end else
  If Sender = HintID then
  begin
    dev.HintID := HintID.Value;
    Tree.RefreshButton(dev);
  end else
  If Sender = Cost then
  begin
    dev.Cost := Cost.Value;
    Tree.RefreshButton(dev);
  end else
  If Sender = Position_X then
  begin
    oldX := dev.X;

    SetNextPos;

    Position_X.Value := Dev.X;
    Tree.RefreshButton(dev);
  end else
  If sender = Dev_GuiIcon then
  begin
    dev.GuiIcon := Dev_GuiIcon.Value;
    Tree.RefreshButton(dev);
  end else
  If sender = Button_AddDev then
  begin
    temp := GetFreeSpace;
    If temp <> 255 then
    begin
      next := dev.AddNext(gRes.Development[Tree.CurrentPage].GetNewId, temp);
      next.GuiIcon := dev.GuiIcon + 1;
      next.HintID := dev.HintID + 1;
      next.Cost := 1;
      {SetLength(dev.Next, length(dev.Next) + 1);
      next := @dev.Next[high(dev.Next)];
      next.X := temp;
      next.Parent := dev;}

      Tree.ReloadTrees;
    end;
  end else
  If sender = Button_DelDev then
  begin
    If (devParent = nil) or (dev = nil) or (dev.Parent <> devParent) then
      Exit;
    {aIndex := -1;
    for I := 0 to High(devParent.Next) do
      If @devParent.Next[I] = dev then
        aIndex := I;
    If aIndex = -1 then
      Exit;

    devParent.Next[aIndex] := devParent.Next[high(devParent.Next)];
    SetLength(devParent.Next, high(devParent.Next)); }
    devParent.RemNext(dev);
    Tree.ReloadTrees;
  end;

end;

procedure TKMMenuDebug.TreeRepoButtonClicked(Sender: TObject; Shift: TShiftState);
var old : Integer;
var dev : TKMDevRepoInfo;
  dtt : TKMDevelopmentTreeType;
  IsType : Boolean;
begin
  If Sender = nil then
    Exit;
  IsType := false;
  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
    If Sender = Button_SelectType[dtt] then
    begin
      SelectedTreeType := dtt;
      IsType := true;
      Break
    end;
  If IsType then
  begin
    for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
        Tree_Repo[dtt].Visible := SelectedTreeType = dtt;
    Exit;
  end;

  TKMButtonFlat(Sender).Down := true;
  old := Tree_Repo[SelectedTreeType].Tag;
  Tree_Repo[SelectedTreeType].Tag := TKMButtonFlat(Sender).Tag;

  If old <> TKMButtonFlat(Sender).Tag then
  begin
    dev := TKMDevRepoInfo(Tree_Repo[SelectedTreeType].Tag);
    Repo_Position_X.Value := dev.X;
    Repo_Position_Y.Value := dev.Y;
    Repo_DevID.Value := dev.ID;
  end;

  Tree_Repo[SelectedTreeType].Refresh;
end;

procedure TKMMenuDebug.TreeRepoChange(Sender: TObject);
var dev, par : TKMDevRepoInfo;
begin
  If Sender = Button_SaveRepo then
  begin
    SaveRepo;
    Exit;
  end;
  If Tree_Repo[SelectedTreeType].Tag = 0 then
    Exit;
  dev := TKMDevRepoInfo(Tree_Repo[SelectedTreeType].Tag);
  par := dev.Parent;
  If Sender = Repo_Button_Del then
  begin
    If par <> nil then
    begin
      par.Delete(dev);
      TreeRepoButtonClicked(par.Button, []);
    end;
  end else
  If Sender = Repo_Button_Add then
  begin
    TreeRepoButtonClicked(dev.AddNext(Tree_Repo[SelectedTreeType], TreeRepoButtonClicked, -1, 0).Button, []);
  end
  else
  If Sender = Repo_Position_X then
    dev.X := Repo_Position_X.Value
  else
  If Sender = Repo_Position_Y then
    dev.Y := Repo_Position_Y.Value
  else
  If Sender = Repo_DevID then
  begin
    Dev.Id := Repo_DevID.Value;

    Dev.Button.TexID := 0;
    Dev.Button.Caption := '';
    Dev.Button.Hint := '';
    If (Dev.Id = -1) or (dev.ID >= gRes.Development[SelectedTreeType].Count) then
      Dev.Dev := nil
    else
      Dev.Dev := gRes.Development[SelectedTreeType].GetItem(Dev.Id);
    If Dev.Dev <> nil then
    begin
      Dev.Button.TexID := Dev.Dev.GuiIcon;
      Dev.Button.Hint := gRes.Development.GetText(Dev.Dev.HintID);
      Dev.Button.Caption := Dev.ID.ToString;
    end;
  end;

  Tree_Repo[SelectedTreeType].Refresh;

end;

procedure TKMMenuDebug.SaveRepo;
  procedure SaveDev(Json : TKMJsonObject; aDev : TKMDevRepoInfo);
  var arr : TKMJsonArrayNew;
    I : Integer;
  begin
    If JSON = nil then
      Exit;

    Json.Add('ID', aDev.Dev.ID);
    Json.Add('HintID', aDev.Dev.HintID);
    Json.Add('GuiIcon', aDev.Dev.GuiIcon);
    Json.Add('X', aDev.X);
    Json.Add('Y', aDev.Y);
    Json.Add('Cost', aDev.Dev.Cost);
    Json.Add('IsSpecial', aDev.Dev.IsSpecial);

    If Length(aDev.Next) > 0 then
    begin
      arr := Json.AddArray('Next');
      for I := 0 to high(aDev.Next) do
        SaveDev(arr.AddObject, aDev.Next[I]);
    end;
  end;

var nRoot, dev : TKMJsonObject;
  dtt : TKMDevelopmentTreeType;
begin
  nRoot := TKMJsonObject.Create;
  try
    for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
    begin
      SaveDev(nRoot.AddObject(TREE_TYPE_STRING[dtt]), Tree_Repo[dtt].List);
    end;
    nRoot.SaveToFile(ExeDir + 'Export' + PathDelim + 'DevelopmentTreeRepo.json');
  finally
    FreeAndNil(nRoot);
  end;
end;






constructor TKMHouseViewer.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  Inherited;
  fType := htNone;
  AnimStep := 0;
  ViewAsConstruction := false;
  DoAnimStep := true;
  WorkAnim := [haFlag1, haFlag2, haFlag3, haFlagPole, haSmoke, haIdle];
  fColor.SetColor(1, 1, 1);
end;

procedure TKMHouseViewer.SetHouseType(aHouse: TKMHouseType);
begin
  fType := aHouse;
  AnimStep := 0;
end;

procedure TKMHouseViewer.UpdateState(aGlobalTickCount: Cardinal);
begin
  Inherited;
  If DoAnimStep then
    Inc(AnimStep);
end;


procedure TKMHouseViewer.Paint;
var HA : TKMHouseActionType;
  cX, cY : Integer;
  spec : TKMHouseSpec;
  A : TKMAnimation;
begin
  Inherited;
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, fColor);
  If fType = htNone then
    Exit;
  TKMRenderUI.WriteText(AbsLeft, AbsBottom - 20, Width, Caption, fntOutline, taCenter);

  spec := gRes.Houses[fType];
  cX := AbsLeft + Width div 2;
  cY := AbsBottom - 100;

  If ViewAsConstruction then
  begin
    TKMRenderUI.WritePictureWithPivot(cX, cY, rxHouses, spec.WoodPic + 1, high(Cardinal));
    if spec.WoodCost > 0 then
    TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[1].MoveX, cY + spec.BuildSupply[1].MoveY,
                                      rxHouses, 260 + Min(spec.WoodCost, 6) - 1, high(Cardinal));
    if spec.StoneCost > 0 then
    TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[2].MoveX, cY + spec.BuildSupply[2].MoveY,
                                      rxHouses, 267 + Min(spec.StoneCost, 6) - 1, high(Cardinal));
    if spec.TileCost > 0 then
      TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[3].MoveX, cY + spec.BuildSupply[3].MoveY,
                                        rxHouses, 2288 + Min(spec.TileCost, 5) - 1, high(Cardinal));


    if spec.WoodCost > 6 then
    TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[1].MoveX + 9, cY + spec.BuildSupply[1].MoveY,
                                      rxHouses, 260 + Min(spec.WoodCost - 6, 6) - 1, high(Cardinal));
    if spec.StoneCost > 6 then
    TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[2].MoveX + 9, cY + spec.BuildSupply[2].MoveY + 8,
                                      rxHouses, 267 + Min(spec.StoneCost - 6, 6) - 1, high(Cardinal));
    if spec.TileCost > 5 then
      TKMRenderUI.WritePictureWithPivot(cX + spec.BuildSupply[3].MoveX + 9, cY + spec.BuildSupply[3].MoveY + 8,
                                        rxHouses, 2288 + Min(spec.TileCost - 5, 5) - 1, high(Cardinal));
  end else
  begin
    TKMRenderUI.WritePictureWithPivot(cX, cY, rxHouses, spec.StonePic + 1, high(Cardinal));

    for HA in WorkAnim do
      If spec.Anim[HA].Count > 0 then
      begin
        A := spec.Anim[HA];
        TKMRenderUI.WritePictureWithPivot(cX + A.X, cY + A.Y, rxHouses, A.Animation[AnimStep] + 1, $FF0000FF);
      end;

  end;

end;

function TKMGUIDevelopmentDebug.CreateButton(aParent : TKMPanel) : TKMButtonFlat;
begin
  Result := TKMButtonFlatDevMenu.Create(aParent, 0, 0, 0, 0, 0, rxGui);

end;

procedure TKMGUIDevelopmentDebug.SetUpButton(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevMenu(B).Dev := aDev;
  TKMButtonFlatDevMenu(B).Cost := aDev.Cost;
end;

procedure TKMGUIDevelopmentDebug.RefreshSingle(B : TKMButtonFlat; aDev : PKMDevelopment);
begin
  TKMButtonFlatDevMenu(B).Cost := aDev.Cost;
  TKMButtonFlatDevMenu(B).Hint := gRes.Development.GetText(aDev.HintID);
end;

procedure TKMButtonFlatDevMenu.Paint;
var textCol : Cardinal;
begin
  inherited;
  If Cost > 0 then
  begin
    textCol := IfThen(Enabled, CapColor, $FF888888);
    TKMRenderUI.WriteText(AbsLeft + 3, AbsTop - 3, Width, 'x' + Cost.ToString, fntGrey, taRight, textCol);

  end;
  TKMRenderUI.WriteText(AbsLeft, AbsTop + height - 13, Width, Dev.ID.ToString, fntMini, taCenter);
end;



procedure TKMDevRepoTree.Refresh;
  procedure Reposition(aDev : TKMDevRepoInfo; aY : Integer);
  var I : integer;
  begin
    aDev.Button.Top := 4 + aY * DISTANCE_BETWEEN_ROWS;
    aDev.Button.Left := aDev.X * 34;
    aDev.Button.Down := aDev.Button.Tag = self.Tag;

    for I := 0 to High(aDev.Next) do
      Reposition(aDev.Next[I], aY + 1 + aDev.Next[I].Y);

  end;
begin
  Reposition(List, 0);
end;


procedure TKMDevRepoTree.Paint;
  procedure MakeLine(aFrom : TKMPoint; aTo : TKMDevRepoInfo);
  var I : Integer;
    cent : TKMPoint;
  begin
    cent := aTo.Button.AbsCenter;
    //render a little dot behind first item
    TKMRenderUI.WriteLine(aFrom.X, aFrom.Y, aFrom.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2),
                          aTo.Button.DownColor, 65535, aTo.Button.LineWidth);//  \/
    TKMRenderUI.WriteLine(aFrom.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2),
                          cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2),
                          aTo.Button.DownColor, 65535, aTo.Button.LineWidth);//  <>

    TKMRenderUI.WriteLine(cent.X, cent.Y, cent.X, cent.Y - (DISTANCE_BETWEEN_ROWS div 2),
                          aTo.Button.DownColor, 65535, aTo.Button.LineWidth);//  /\

    for I := 0 to high(aTo.Next) do
        MakeLine(cent, aTo.Next[I]);
  end;
var I : integer;
begin
  If Visible then
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, 0.4);
    TKMRenderUI.SetupClipY(AbsTop, AbsTop + Height - 5);

    for I := 0 to high(List.Next) do
    begin
      MakeLine(List.Button.AbsCenter,
                List.Next[I]);
    end;
    TKMRenderUI.ReleaseClipY;
  end;

  Inherited;
end;

constructor TKMDevRepoInfo.Create(aParent: TKMPanel; aOnClickShift: TNotifyEventShift; aX: Integer; aY: Integer);
begin
  Inherited Create;
  Button := TKMButtonFlat.Create(aParent, aX * 34, 4 + aY * DISTANCE_BETWEEN_ROWS, 31, 31, 0);
  Button.BackBevelColor := $FF000000;
  Button.BackAlpha := 1;
  //Button.Tag := Integer(@self);
  Button.OnClickShift := aOnClickShift;
  X := aX;
  Id := -1;
  Dev := nil;
  SetLength(Next, 0);
  Parent := nil;
end;

function TKMDevRepoInfo.AddNext(aParent : TKMPanel; aOnClickShift : TNotifyEventShift; aX, aY : Integer) : TKMDevRepoInfo;
  function HasAt(X : Integer) : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to High(Next) do
      If Next[I].X = X then
        Exit(True);
  end;

  function GetX : Integer;
  var I, J : Integer;
  begin
    Result := -1;
    If aX = -1 then
      J := X
    else
      J := aX;
    I := 0;
    While HasAt(J) do
    begin
      IncLoop(J, 0, 4, 1);
      Inc(I);
      If I = 5 then
        Exit;
    end;
    Result := J;
  end;

var I, J : Integer;
begin
  Result := nil;
  //check for free space
  J := -1;
  for I := 0 to High(Next) do
    If Next[I] = nil then
    begin
      J := I;
      Break;
    end;

  I := length(Next);
  If I >= 5 then
    Exit;

  aX := GetX;
  If aX = -1 then //no free space
    Exit;

  If J = -1 then
  begin
    J := I;
    SetLength(Next, I + 1);
  end;

  Next[J] := TKMDevRepoInfo.Create(aParent, aOnClickShift, aX, aY);
  Next[J].Parent := self;
  Next[J].Button.Tag := Integer(Next[J]);
  Result := Next[J];
end;

destructor TKMDevRepoInfo.Destroy;
var I : integer;
begin
  Button.Parent.ChildRemove(Button);
  FreeAndNil(Button);
  for I := 0 to High(Next) do
      FreeAndNil(Next[I]);
  Inherited;
end;

procedure TKMDevRepoInfo.Delete(aNext: TKMDevRepoInfo);
var I : integer;
begin
  for I := 0 to High(Next) do
  If Next[I] = aNext then
  begin
    FreeAndNil(Next[I]);

    Next[I] := Next[high(Next)];
    SetLength(Next, high(Next));
    Exit;
  end;

end;

end.
