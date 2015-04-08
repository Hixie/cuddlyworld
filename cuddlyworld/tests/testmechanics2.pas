{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics2;

interface

procedure TestMechanics2();

implementation

uses
   sysutils, storable, matcher, physics, player, locations, things, grammarian, cuddlycamp, world, threshold, testmechanics;

procedure TestMechanics2();

   function InitTest: TWorld;
   var
      World: TTestWorld;
      EastRoom, WestRoom, Doorway: TLocation;
      DoorFrame: TThresholdThing; // eventually we want the door frame to have a door...
   begin
      World := TTestWorld.Create();

      { Locations }
      EastRoom := TGroundLocation.Create('East Room', 'the East room', 'an East room', 'This is the room on the east side of the door frame.', CreateStoneSurface());
      WestRoom := TGroundLocation.Create('West Room', 'the West room', 'a West room', 'This is the room on the west side of the door frame.', CreateStoneSurface());

      { The Door }
      DoorFrame := TStaticThresholdThing.Create('door frame', '((door frame/frames?) door/doors)@', 'The door frame is a frame around where a door would go.', cdEast);
      Doorway := ConnectThreshold(EastRoom, WestRoom, DoorFrame, CreateStoneSurface());

      { World }
      World.AddLocation(EastRoom);
      World.AddLocation(WestRoom);
      World.AddLocation(Doorway);
      World.FStartLocation := DoorFrame;

      Result := World;
   end;

   procedure RunTest(TestPlayer: TPlayer; Proxy: TTestProxy);
   begin
      { and/then/etc tests }
      Proxy.Test('Looking at a threshold');
      Proxy.ExpectString('On the door frame (between the East room and the West room)');
      Proxy.ExpectString('The door frame is a frame around where a door would go.');
      Proxy.ExpectString('To the east is the East room. To the west is the West room.');
      Proxy.ExpectString('');
      TestPlayer.Perform('look');
      Proxy.ExpectDone();

      Proxy.ExpectString('Looking east, you see:');
      Proxy.ExpectString('East Room');
      Proxy.ExpectString('This is the room on the east side of the door frame.');
      Proxy.ExpectString('');
      TestPlayer.Perform('look east');
      Proxy.ExpectDone();

      Proxy.ExpectString('Door frame between the East room and the West room');
      Proxy.ExpectString('The door frame is a frame around where a door would go. To the east is the East room. To the west is the West room.');
      Proxy.ExpectString('');
      TestPlayer.Perform('exit');
      Proxy.ExpectDone();

      Proxy.ExpectString('West Room');
      Proxy.ExpectString('This is the room on the west side of the door frame. To the east is a door frame.');
      Proxy.ExpectString('');
      TestPlayer.Perform('west');
      Proxy.ExpectDone();

      Proxy.ExpectString('The west is to the west, obviously. I have a keen sense of direction that you can rely on.');
      Proxy.ExpectString('');
      TestPlayer.Perform('find west');
      Proxy.ExpectDone();

      Proxy.ExpectString('I don''t understand how to find things in a particular direction. It seems redundant.');
      Proxy.ExpectString('');
      TestPlayer.Perform('find west wing');
      Proxy.ExpectDone();

(*
      Proxy.ExpectString('The west room is to the west.');
      Proxy.ExpectString('');
      TestPlayer.Perform('find west room');
      Proxy.ExpectDone();

      Proxy.ExpectString('You are in the east room.');
      Proxy.ExpectString('');
      TestPlayer.Perform('find east room');
      Proxy.ExpectDone();
*)

(*
      Proxy.ExpectString('Door frame between the East room and the West room');
      Proxy.ExpectString('The door frame is a frame around where a door would go. To the east is the East room. To the west is the West room.');
      Proxy.ExpectString('');
      TestPlayer.Perform('go east and stop at door frame');
      Proxy.ExpectDone();

      Proxy.ExpectString('(by going east and stopping at the door frame)');
      Proxy.ExpectString('Door frame between the East room and the West room');
      Proxy.ExpectString('The door frame is a frame around where a door would go. To the east is the East room. To the west is the West room.');
      Proxy.ExpectString('');
      TestPlayer.Perform('go to door frame');
      Proxy.ExpectDone();
*)
   end;

begin
   Writeln('MECHANICS II');
   RunMechanicsHarness(@InitTest, @RunTest, {$IFDEF PLAY_IN_TEST_DOORLAND} True {$ELSE} False {$ENDIF});
end;

end.