unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse;

type
  TStateMain = class(TUIState)
  private
    procedure SwitchState(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  GameStateSecond;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  //here we have a "normal state" with a button that switches to the new State: StateSecond
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  (UiOwner.FindRequiredComponent('Button1') as TCastleButton).OnClick := @SwitchState;
end;

procedure TStateMain.SwitchState(Sender: TObject);
begin
  //the simplest way here is just to grab a screenshot of our current view and give it to the next state
  StateSecond.Image := Container.SaveScreen;
  //And set the second state as current (this will Stop this state)
  TUiState.Current := StateSecond;
end;


end.
