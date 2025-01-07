unit Unit_PathManager;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, StrUtils, SysUtils, Windows;


type
  // Scans folder and subfolders in search of .libx files
  TKMPathManager = class
  private
    fPaths: TStringList;
    function GetPath(aIndex: Integer): string;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Paths[aIndex: Integer]: string read GetPath; default;
    procedure Clear;
    procedure AddPath(const aBasePath, aSubPath: string);
  end;


implementation


{ TKMPathManager }
constructor TKMPathManager.Create;
begin
  inherited;
  fPaths := TStringList.Create;
end;


destructor TKMPathManager.Destroy;
begin
  fPaths.Free;
  inherited;
end;


function TKMPathManager.GetCount: Integer;
begin
  Result := fPaths.Count;
end;


function TKMPathManager.GetPath(aIndex: Integer): string;
begin
  Result := fPaths[aIndex];
end;


procedure TKMPathManager.Clear;
begin
  fPaths.Clear;
end;


procedure TKMPathManager.AddPath(const aBasePath, aSubPath: string);
var
  I: Integer;
  FileMask: string;
  SearchRec: TSearchRec;
  SubFolders: TStringList;
  PathAdded: Boolean;
begin
  SubFolders := TStringList.Create;
  SubFolders.Add(aBasePath + aSubPath);

  I := 0;
  repeat
    FindFirst(SubFolders[I] + '*', faAnyFile, SearchRec);
    repeat
      if (SearchRec.Name <> '') and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        if (SearchRec.Attr and faDirectory = faDirectory) then
          SubFolders.Add(SubFolders[I] + SearchRec.Name + '\')
        else
          if not PathAdded and SameText(RightStr(SearchRec.Name, 5), '.libx') then
          begin
            FileMask := LeftStr(SearchRec.Name, Length(SearchRec.Name) - 8) + '%s.libx';
            fPaths.Add(ExtractRelativePath(aBasePath, SubFolders[I]) + FileMask);
            PathAdded := True;
          end;
    until (FindNext(SearchRec) <> 0);
    Inc(I);
    PathAdded := False;
  until (I >= SubFolders.Count);
end;


end.
