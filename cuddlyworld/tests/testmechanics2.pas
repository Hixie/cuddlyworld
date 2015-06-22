{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics2;

//{$DEFINE PLAY_IN_TEST_EDEN}

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
      DoorFrame: TDoorWay;
   begin
      World := TTestWorld.Create();

      { Locations }
      EastRoom := TGroundLocation.Create('East Room', 'the East room', 'an East room', 'This is the room on the east side of the door frame.', CreateStoneSurface());
      WestRoom := TGroundLocation.Create('West Room', 'the West room', 'a West room', 'This is the room on the west side of the door frame.', CreateStoneSurface());

      { The Door }
      DoorFrame := TDoorWay.Create('door frame', '(door frame/frames)', 'The door frame is a frame around where a door would go.', cdEast,
                                   TDoor.Create('door', 'door/doors',
                                                TDoorSide.Create('side', 'front? side/sides', 'the front side of the door is flat.'),
                                                TDoorSide.Create('side', 'back? side/sides', 'the back side of the door is flat.')));
      DoorFrame.Door.Description := 'The door is flat.';
      Doorway := ConnectThreshold(EastRoom, WestRoom, DoorFrame);

      { World }
      World.AddLocation(EastRoom);
      World.AddLocation(WestRoom);
      World.AddLocation(Doorway);
      World.FStartLocation := EastRoom.GetSurface();

      Result := World;
   end;

   procedure RunTest(TestPlayer: TPlayer; Proxy: TTestProxy);

      procedure RunCommand(Command: UTF8String; Responses: array of UTF8String);
      var
         Response: UTF8String;
      begin
         for Response in Responses do
            Proxy.ExpectString(Response);
         Proxy.ExpectString('');
         TestPlayer.Perform(Command);
         Proxy.ExpectDone();
      end;

   begin
      { and/then/etc tests }

      // We start on the East side
      RunCommand('look', ['East Room', 'This is the room on the east side of the door frame. To the west is a door frame.']);
      RunCommand('west', ['You cannot go west. The door is closed.']);
      RunCommand('enter door', ['You cannot enter the door. The door is closed.']);
      RunCommand('enter door frame', ['You cannot enter the door frame. The door is closed.']);
      RunCommand('take door frame', ['The door frame cannot be moved.']);
      RunCommand('open door frame', ['Opened.']);
      RunCommand('close door frame', ['Closed.']);
      RunCommand('find door frame', ['The door frame is to the west.']);
      RunCommand('find door', ['The door is to the west, installed in the door frame.']);
      RunCommand('take door', ['Taken.']);
      RunCommand('take door frame', ['The door frame cannot be moved.']);
      RunCommand('enter door', ['That would prove rather challenging given where the door is relative to yourself.']);

      // Now we go over to the West side
      RunCommand('enter door frame', ['(through the door frame)', 'West Room', 'This is the room on the west side of the door frame. To the east is a door frame.']);
      RunCommand('open door', ['The door is being carried by you, so there is no way to open it.']);
      RunCommand('close door', ['The door is being carried by you, so there is no way to close it.']);
      RunCommand('open door frame', ['The door frame is wide open.']);
      RunCommand('close door frame', ['There is nothing in the door frame with which to close it.']);
      RunCommand('put door in door frame', ['You install a door in the door frame.']);
      RunCommand('open door', ['Opened.']);
      RunCommand('close door', ['Closed.']);
      RunCommand('take side', ['The side of the door is part of the door.']);
      RunCommand('look at door', ['The back side of the door is flat. The door is closed.']);
      RunCommand('look at door frame', ['The door frame is a frame around where a door would go.']);
      RunCommand('look in door', ['It is not clear how to get inside the door.']);
      RunCommand('look in door frame', ['Looking in the door frame, you see a door. The back side of the door is flat. The door is closed.', 'Other than the door, there is nothing in the door frame.']);
      RunCommand('look under door', ['Looking east, you see:', 'East Room', 'This is the room on the east side of the door frame.']);
      RunCommand('look under door frame', ['The door frame contains a door. The back side of the door is flat. The door is closed.', 'Other than the door, there is nothing in the door frame.']);
      RunCommand('look east', ['Looking east, you see a door. The back side of the door is flat. The door is closed.']);
      RunCommand('look west', ['You see nothing noteworthy when looking west.']);
      RunCommand('open door', ['Opened.']);
      RunCommand('take side', ['Which side do you mean, the front side of the door or the back side of the door?']);
      RunCommand('take any side', ['(the front side of the door)', 'The side of the door is part of the door.']);
      RunCommand('look at door', ['The door is flat. The door is open.']);
      RunCommand('look at door frame', ['The door frame is a frame around where a door would go.']);
      RunCommand('look in door', ['It is not clear how to get inside the door.']);
      RunCommand('look in door frame', ['Looking east, you see:', 'East Room', 'This is the room on the east side of the door frame.']);
      RunCommand('look under door', ['The door is open.']);
      RunCommand('look under door frame', ['The door frame contains a door. The door is flat. The door is open.', 'Other than the door, there is nothing in the door frame.']);
      RunCommand('look east', ['(looking through the open door)', 'Looking east, you see:', 'East Room', 'This is the room on the east side of the door frame.']);
      RunCommand('look west', ['You see nothing noteworthy when looking west.']);
      RunCommand('find door', ['The door is to the east, installed in the door frame.']);

      // ...and we go back to the East side
      RunCommand('enter door', ['(through the door frame)', 'East Room', 'This is the room on the east side of the door frame. To the west is a door frame.']);
      RunCommand('look at side', ['Which side do you mean, the front side of the door or the back side of the door?']);
      RunCommand('close door', ['Closed.']);
      RunCommand('look at side', ['The front side of the door is flat.']);
      RunCommand('west', ['You cannot go west. The door is closed.']);
      RunCommand('move door west', ['The door is installed in the door frame. It is not clear how to move it.']);

      // Back West.
      RunCommand('open door and go west', ['Opened.', '', '(through the door frame)', 'West Room', 'This is the room on the west side of the door frame. To the east is a door frame.']);
      RunCommand('move door', ['You shake the door.']);
      RunCommand('close', ['(the door)', 'Closed.']);
      RunCommand('put bag on door frame', ['You can''t put something on a door frame.']);
      RunCommand('put bag in door frame', ['The door is closed.']);
      RunCommand('put bag on door', ['The door is closed.']);
      RunCommand('put bag in door', ['It is not clear how to get inside the door.']);
      RunCommand('open', ['(the door)', 'Opened.']);
      RunCommand('put bag on door frame', ['You can''t put something on a door frame.']);
      RunCommand('put bag in door frame', ['Placed in the door frame.']);
      RunCommand('put bag in door', ['(first taking the bag of holding)', 'Taken.', 'It is not clear how to get inside the door.']);
      RunCommand('put bag on door', ['Placed on the door.']);
      RunCommand('enter door frame', ['(through the door frame)', 'The bag of holding falls onto the floor as you pass through the door, barely missing you on its way down.', 'East Room', 'This is the room on the east side of the door frame. To the west is a door frame.']);
      RunCommand('move me to bag', ['In the bag of holding (between the East room and the West room)', 'The bag has the name "Tester" embroidered around its rim.']);
      RunCommand('exit', ['Door frame between the East room and the West room', 'The door frame is a frame around where a door would go. To the east is the East room. To the west is the West room.', 'There is a bag of holding here.']);
      RunCommand('close', ['(the door)', 'The door cannot be closed; the bag of holding and you are in the way.']);
      RunCommand('exit', ['You can''t go out from here.']);
      RunCommand('east', ['East Room', 'This is the room on the east side of the door frame. To the west is a door frame.']);
      // XXX RunCommand('find bag', ['The bag of holding is between the East room and the West room.']);
      RunCommand('find bag', ['The bag of holding is to the west, on the flat ground.']);
      RunCommand('push bag west', ['Pushed.']);
      RunCommand('find bag', ['I can''t find anything like a "bag" here.']);
   end;

begin
   Writeln('MECHANICS II');
   RunMechanicsHarness(@InitTest, @RunTest, {$IFDEF PLAY_IN_TEST_DOORLAND} True {$ELSE} False {$ENDIF});
end;

end.