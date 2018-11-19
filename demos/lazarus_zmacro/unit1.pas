unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, db, ZDataset, ZConnection, ZMacroQuery, rxdbgrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Label1: TLabel;
    ZConnection1: TZConnection;
    ZMacroQuery1: TZMacroQuery;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ZConnection1.Connect;
  ZMacroQuery1.MacroByName('Macro1').AsString:='where (1=1)';
  ZMacroQuery1.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ZMacroQuery1.Close;
  if Edit1.Text<>'' then
    ZMacroQuery1.MacroByName('Macro1').AsString:='where cast(pg_class.relkind as varchar(5))  = '''+Edit1.Text+''''
  else
    ZMacroQuery1.MacroByName('Macro1').AsString:='';
  ZMacroQuery1.Open;
end;

end.

