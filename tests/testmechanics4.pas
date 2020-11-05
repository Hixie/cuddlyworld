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

   procedure RunTest1(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
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
      RunCommand('debug connect north to undisclosed location', ['Abracadabra! Going north from the Land now leads to the Undisclosed location.']);
      RunCommand('look', ['Land', 'The land is a land. To the north is the undisclosed location.', 'There is a wooden sign here.']);
      RunCommand('n', ['Undisclosed location', 'This is the place where it happens, where secrets are discussed and plans made.']);
      RunCommand('debug connect south to land with loPermissibleNavigationTarget loAutoDescribe loloVisibleFromFarAway', ['Flags must be a space-separated list of one or more of: loAutoDescribe, loPermissibleNavigationTarget, loThreshold, loVisibleFromFarAway, loNotVisibleFromBehind, loConsiderDirectionUnimportantWhenFindingChildren (default is "loAutoDescribe loPermissibleNavigationTarget").']);
      RunCommand('debug connect south to land with loPermissibleNavigationTarget loAutoDescribe loVisibleFromFarAway', ['Abracadabra! Going south from the Undisclosed location now leads to the Land.']);
      RunCommand('look', ['Undisclosed location', 'This is the place where it happens, where secrets are discussed and plans made. To the south is the land.']);
      RunCommand('find sign', ['I can''t find anything like a "sign" here.']);
      RunCommand('debug teleport land', ['Land', 'The land is a land. To the north is the undisclosed location.', 'There is a wooden sign here.']);
      RunCommand('debug teleport sign', ['On the wooden sign (at the land)', 'The sign has some text burnt into it.', 'The land is a land. To the north is the undisclosed location.']);
      RunCommand('debug make ''new TScenery { name: "test"; pattern: "test/tests"; description: "The test is a test."; }''', ['Poof! There is a test here.']);
      RunCommand('debug describe class TScenery', ['Properties available on TScenery:', ' - name: string', ' - pattern: pattern', ' - description: string', ' - underDescription: string', ' - findDescription: string', ' - cannotMoveExcuse: string', ' - opened: boolean', ' - mass: enum:TThingMass', ' - size: enum:TThingSize', ' - child: child*']);
      RunCommand('debug describe enum TThingMass', ['Enum values available on TThingMass:', ' - tmLight', ' - tmHeavy', ' - tmPonderous', ' - tmLudicrous']);
   end;

   procedure RunTest2(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         TestWorld.Perform(Command, TestPlayer);
         Proxy.ExpectDone();
      end;

   begin
      RunCommand(
         'debug make ''' +
         'new TBackdrop named skybox {' +
         '  source: new TScenery named sky {' +
         '    name: "sky";' +
         '    pattern: "blue? sky/skies";' +
         '    description: "The sky is clear.";' +
         '    mass: tmLudicrous;' +
         '    size: tsLudicrous;' +
         '    opened: true;' +
         '    child: tpEmbedded, new TScenery {' +
         '      name: "sun";' +
         '      pattern: "(sun/suns star/stars)@";' +
         '      description: "The sun is bright.";' +
         '    };' +
         '  };' +
         '  position: tpAtImplicit;' +
         '};' +
         'new TThresholdLocation named doorway {' +
         '  landmark: new TDoorWay {' +
         '    name: "doorway";' +
         '    pattern: "unnotable? doorway/doorways";' +
         '    description: "The doorway has no notable features.";' +
         '    frontDirection: cdSouth;' +
         '    door: new TDoor {' +
         '      name: "door";' +
         '      pattern: "wooden? (open:1 closed:2)* door/doors";' +
         '      frontSide: new TDoorSide {' +
         '        name: "side";' +
         '        pattern: "varnished? (front:1)? side/sides";' +
         '        description: "the door is varnished.";' +
         '      };' +
         '      backSide: new TDoorSide {' +
         '        name: "side";' +
         '        pattern: "((scratched? (back:1)? side/sides) scratch/scratches)@";' +
         '        description: "the door has a big scratch, as if made by a wolf.";' +
         '      };' +
         '    };' +
         '  };' +
         '  surface: new TThresholdSurface {' +
         '    name: "floor";' +
         '    pattern: "flat? (ground/grounds floor/floors)@";' +
         '    description: "The floor is flat.";' +
         '  };' +
         '};' +
         'new TGroundLocation named room1 {' +
         '  name: "Room";' +
         '  definiteName: "a room";' +
         '  indefiniteName: "the room";' +
         '  description: "Nothing is particularly noteworthy about this location.";' +
         '  ground: new TEarthGround {' +
         '    name: "ground";' +
         '    pattern: "(ground/grounds earth)@";' +
         '    description: "The ground is a flat surface of earth.";' +
         '  };' +
         '  landmark: cdUp, sky, loVisibleFromFarAway;' +
         '};' +
         'new TGroundLocation named room2 {' +
         '  name: "Back Room";' +
         '  definiteName: "a back room";' +
         '  indefiniteName: "the back room";' +
         '  description: "Nothing is particularly noteworthy about this second location.";' +
         '  ground: new TEarthGround {' +
         '    name: "ground";' +
         '    pattern: "(ground/grounds earth)@";' +
         '    description: "The ground is a flat surface of earth.";' +
         '  };' +
         '  landmark: cdUp, sky, loVisibleFromFarAway;' +
         '};' +
         'connect room1, cdSouth, doorway, loAutoDescribe loPermissibleNavigationTarget loNotVisibleFromBehind, bidirectional;' + // without loThreshold
         'connect room2, cdNorth, doorway, loAutoDescribe loPermissibleNavigationTarget loNotVisibleFromBehind loThreshold, bidirectional;' +
         '''',
         [
            'Hocus Pocus! The sky now exists.',
            'Hocus Pocus! The doorway now exists.',
            'Hocus Pocus! A room now exists.',
            'Hocus Pocus! A back room now exists.',
            'Abracadabra! Going south from the Room now leads to the doorway.',
            'Abracadabra! Going north from the doorway now leads to the Room.',
            'Abracadabra! Going north from the Back Room now leads to the doorway.',
            'Abracadabra! Going south from the doorway now leads to the Back Room.'
         ]
      );
      RunCommand('debug connect south and back to room with loAutoDescribe loPermissibleNavigationTarget', ['Abracadabra! Going south from the Land now leads to the Room.', 'Abracadabra! Going north from the Room now leads to the Land.']);
      RunCommand('south', ['Room', 'Nothing is particularly noteworthy about this location. To the south is a doorway. To the north is the land.']);
      RunCommand('find boogie', ['I can''t find anything like a "boogie" here.']); // verifies you can seek things when a door is around without loThreshold (used to crash)
      RunCommand('open door', ['Opened.']);
      RunCommand('move me to door', ['You can''t see the door anymore.']); // because of lacking loThreshold
      RunCommand('south', ['(through the doorway)', 'Back Room', 'Nothing is particularly noteworthy about this second location. To the north is a doorway.']);
      RunCommand('move me to door', ['On the door (installed in the doorway, between a room and a back room)', 'The door has two sides. On the front, the door is varnished. On the back, the door has a big scratch, as if made by a wolf. The door is open.']);
      RunCommand('find boogie', ['I can''t find anything like a "boogie" here.']);
      RunCommand('find ground', ['I can''t find anything like a "ground" here.']); // XXX should be able to find the ground
      RunCommand('down', ['You cannot go down. The floor has no discernible entrance.']); // XXX this is clearly dumb
      RunCommand('out', ['Doorway between a room and a back room', 'The doorway has no notable features. To the north is a room. To the south is a back room.']);
      RunCommand('south', ['Back Room', 'Nothing is particularly noteworthy about this second location. To the north is a doorway.']);
      RunCommand('move into doorway', ['(through the doorway)', 'Room', 'Nothing is particularly noteworthy about this location. To the south is a doorway. To the north is the land.']);
      RunCommand('say "hello"', ['You say "hello".']);
      RunCommand('say ''hello''', ['You say "hello".']);
      RunCommand('say "he\llo"', ['You say "he\llo".']);
      RunCommand('say ''he\llo''', ['You say "hello".']);
      RunCommand('say ''he\''', ['You say "he''".']);
      RunCommand('say ''he\', ['You say "he".']);
   end;

begin
   Writeln('MECHANICS IV (debug commands)');
   RunMechanicsHarness(@InitTestWorld, @RunTest1, False);
   RunMechanicsHarness(@InitTestWorld, @RunTest2, False);
end;

end.