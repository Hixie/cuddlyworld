{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics4;

interface

procedure TestMechanics4();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, cuddlycamp, world, threshold, testmechanics;

procedure TestMechanics4();

   function InitTestWorld: TWorld;
   var
      Land: TLocation;
   begin
      Result := TTestWorld.Create();
      Land := TGroundLocation.Create('Land', 'the land', 'a land', 'The land is a land.', CreateEarthSurface());
      Result.AddLocation(Land);
      (Result as TTestWorld).FStartLocation := Land.GetSurface();
   end;

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         Proxy.ExpectString('');
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

   begin
      RunCommand('look', ['Land', 'The land is a land.']);
      RunCommand('debug make ''new TDoorWay { name: "doorway"; pattern: "grand? doorway/doorways"; description: "The doorway is grand."; frontDirection: cdSouth; door: new TDoor { name: "door"; pattern: "(open:1 closed:2 wooden)* door/doors"; frontSide: new TDoorSide { name: "side"; pattern: "(varnished wooden front:1)* side/sides"; description: "The wooden door is varnished."; }; backSide: new TDoorSide { name: "side"; pattern: "(((scratched wooden back:1)* side/sides) scratch/scratches)@"; description: "The door has a big scratch, as if made by a wolf."; }; }; }''', ['Poof! There is a doorway here.']);
      RunCommand('debug things', ['Things:', ' - ground: the ground', ' - you: you', ' - bag of holding: the embroidered bag of holding labeled Tester', ' - rim: the rim of the bag of holding', ' - doorway: the grand doorway', ' - door: the closed wooden door', ' - side: the varnished wooden side of the door', ' - side: the scratched wooden side of the door']);
      RunCommand('inventory', ['You are carrying:', '  A bag of holding.', '  A doorway.']);
      RunCommand('debug make ''new TSign { name: "wooden sign"; pattern: "(burnt wooden)* (sign/signs writing/writings)@"; description: "The sign has some text burnt into it."; writing: "HELLO"; }''', ['The incantation fizzles as you hear a voice whisper "Required property "mass" not found".']);
      RunCommand('debug make ''new TSign { name: "wooden sign"; pattern: "(burnt wooden)* (sign/signs writing/writings)@"; description: "The sign has some text burnt into it."; writing: "HELLO"; mass: tmLight; size: tsSmall; }''', ['Poof! There is a wooden sign here.']);
      RunCommand('put sign on ground', ['Placed.']);
      RunCommand('read sign', ['On the wooden sign is written "HELLO".']);
      RunCommand('look', ['Land', 'The land is a land.', 'There is a wooden sign here.']);
      RunCommand('debug make ''new THole{description:"HOLE";size:tsSmall;pileClass: TSpade;}''', ['The incantation fizzles as you hear a voice whisper "Class specified for "pileClass" is not acceptable, must be a TPile".']);
      RunCommand('debug make ''new THole{description:"HOLE";size:tsSmall;pileClass: TPile;}''', ['Poof! There is a hole here.']);
      RunCommand('debug make ''new TGroundLocation { name: "Undisclosed location"; definiteName: "the undisclosed location"; indefiniteName: "an undisclosed location"; description: "This is the place where it happens, where secrets are discussed and plans made."; ground: new TEarthGround { name: "ground"; pattern: "(ground/grounds earth)@"; description: "The ground is a flat surface of earth."; }; }''', ['Hocus Pocus! The undisclosed location now exists.']);
      RunCommand('debug connect north to undisclosed location', ['Abracadabra!']);
      RunCommand('look', ['Land', 'The land is a land. To the north is the undisclosed location.', 'There is a wooden sign here.']);
      RunCommand('n', ['Undisclosed location', 'This is the place where it happens, where secrets are discussed and plans made.']);
      RunCommand('debug connect south to land with loPermissibleNavigationTarget loAutoDescribe loloVisibleFromFarAway', ['Flags must be a space-separated list of one or more of: loAutoDescribe, loPermissibleNavigationTarget, loThreshold, loVisibleFromFarAway, loNotVisibleFromBehind, loConsiderDirectionUnimportantWhenFindingChildren (default is "loAutoDescribe loPermissibleNavigationTarget").']);
      RunCommand('debug connect south to land with loPermissibleNavigationTarget loAutoDescribe loVisibleFromFarAway', ['Abracadabra!']);
      RunCommand('look', ['Undisclosed location', 'This is the place where it happens, where secrets are discussed and plans made. To the south is the land.']);
      RunCommand('find sign', ['I can''t find anything like a "sign" here.']);
      RunCommand('debug teleport land', ['Land', 'The land is a land. To the north is the undisclosed location.', 'There is a wooden sign here.']);
      RunCommand('debug teleport sign', ['On the wooden sign (at the land)', 'The sign has some text burnt into it.', 'The land is a land. To the north is the undisclosed location.']);
   end;

begin
   Writeln('MECHANICS IV (debug commands)');
   RunMechanicsHarness(@InitTestWorld, @RunTest, False);
end;

end.