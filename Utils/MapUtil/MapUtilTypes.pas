unit MapUtilTypes;
interface

type
  TFOWType = (ftRevealAll, ftRevealPlayers, ftMapSetting);

  TCLIParamRecord = record
    MapDatPath: string;
    Help: Boolean;
    FOWType: TFOWType;
    OutputFile: string;
  end;

implementation

end.
