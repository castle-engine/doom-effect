unit GameStateSecond;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleImages, CastleScreenEffects, X3dNodes, X3DFields;

type
  TStateSecond = class(TUIState)
  strict private
    EffectRoot: TX3DRootNode;
    EffectNode: TScreenEffectNode;
    Effect: TCastleScreenEffects;
    EffectUniform: TSFFloat;
    EffectStrength: Single;
  public
    Image: TCastleImage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateSecond: TStateSecond;

implementation

uses SysUtils,
  X3dLoad;

constructor TStateSecond.Create(AOwner: TComponent);
begin
  inherited;
  // Load the shader effect from x3dv file;
  // Doing that in Constructor so that we won't have to load it multiple times
  // Can be even done in some "separate place" to be reused in several TUiStates
  EffectRoot := LoadNode('castle-data:/effect.x3dv');
  // Find the TScreenEffectNode inside of it
  EffectNode := EffectRoot.FindNode('MyScreenEffect') as TScreenEffectNode;
  // And find the Uniform (writable variable) inside the TScreenEffectNode
  EffectUniform := EffectNode.FindNode('MyShader').Field('EffectStrength', true) as TSFFloat;
end;

destructor TStateSEcond.Destroy;
begin
  // Don't forget to free the RootNode
  FreeAndNil(EffectRoot);
  inherited;
end;

procedure TStateSecond.Start;
var
  UiOwner: TComponent;
  ImageControl: TCastleImageControl;
begin
  inherited;
  // Again, load and process the state nomrally
  InsertUserInterface('castle-data:/state_second.castle-user-interface', FreeAtStop, UiOwner);
  // Create an image control that takes the full screen
  // FreeAtStop as parent means the image will be freed when this state Stops
  // We can free it immediately after the effect is finished, but let's avoid that complication for now
  ImageControl := TCastleImageControl.Create(FreeAtStop);
  ImageControl.FullSize := true;
  // And feed it with the screenshot of our previous state
  // Note, that by default the image is "owned" (OwnsImage) and we won't have to free it manually
  ImageControl.Image := Image;
  // Next create TCastleScreenEffects, again FullSize to fill the entire parent TCastleUserInterface
  Effect := TCastleScreenEffects.Create(FreeAtStop);
  Effect.FullSize := true;
  // Add ImageControl as it's child. TCastleScreenEffects affects only its children
  Effect.InsertFront(ImageControl);
  // Add the Effect to the TCastleScreenEffects
  Effect.AddScreenEffect(EffectNode);
  // Turn on the EffectNode
  EffectNode.Enabled := true;
  // And insert the Effect as the front-most UI in our State - it'll cover all others
  InsertFront(Effect);
  // Finally, reset the effect strength to zero
  EffectStrength := 0;
end;

procedure TStateSecond.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  // If the effect is still visible on screen
  if EffectStrength < Container.Height then
  begin
    // Advance the effect progress at the rate of half of screen (container) per second
    EffectStrength += Container.Height / 2 * SecondsPassed;
    // And set the Uniform variable of our effect - it'll take effect next frame in the shader
    EffectUniform.Send(EffectStrength);
  end else
  // And a small hack to remove the effect from the screen to save FPS
  // I've set EffectStrength to some arbitrary large number as a marker that the effect has been removed
  if EffectStrength < MaxInt then
  begin
    // Remove effect from the TUiState to save FPS
    RemoveControl(Effect);
    // And make sure we don't remove the effect twice
    EffectStrength := MaxInt;
  end;
end;


end.
