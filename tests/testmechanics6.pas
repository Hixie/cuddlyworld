{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit testmechanics6;

interface

//{$DEFINE PLAY_IN_TEST_STAIRLAND}

procedure TestMechanics6();

implementation

uses
   sysutils, storable, matcher, physics, thingdim, grammarian, player, locations, things, world, stairs, testmechanics;

procedure TestMechanics6();

   function InitTest: TWorld;
   var
      World: TTestWorld;
      BottomRoom, TopRoom: TLocation;
   begin
      World := TTestWorld.Create();

      { Locations }
      BottomRoom := TGroundLocation.Create('Bottom Room', 'the Bottom room', 'a Bottom room', 'This is the room at the bottom of the stairs.', CreateStoneSurface());
      TopRoom := TGroundLocation.Create('Top Room', 'the Top room', 'a Top room', 'This is the room at the top of the stairs.', CreateStoneSurface());

      { Things }
      TopRoom.GetSurface().Add(TDescribedPhysicalThing.Create('big block', 'big? block/blocks', 'It is a big block.', tmPonderous, tsBig), tpOn);
      
      { World }
      World.AddLocation(BottomRoom);
      World.AddLocation(TopRoom);
      World.AddLocation(ConnectStairs(BottomRoom, TopRoom));
      World.FStartLocation := BottomRoom.GetSurface();

      Result := World;
   end;

   procedure RunTest(TestWorld: TWorld; TestPlayer: TPlayer; Proxy: TTestProxy);

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
      RunCommand('look', ['Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('up', ['Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('down', ['(through the opening for the stairwell)', 'Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('look up', ['Looking up, you see:', 'Stairwell', 'The stairwell is a space for vertical movement. Above is the Top room.']);
      RunCommand('find stairs', ['The stairs are between the Bottom room and the Top room.']);
      RunCommand('x stairs', ['The stairs have steps.']);
      RunCommand('pick up stairs', ['The stairs are part of the overall structure and quite immovable.']);
      RunCommand('enter stairwell', ['You cannot enter the stairs. The stairs have no discernible entrance.']);
      RunCommand('u', ['Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('find opening', ['The opening for the stairwell is in the ground.']);
      RunCommand('x opening', ['The stairs lead down through an opening for the stairwell.']);
      RunCommand('look down', ['Looking down, you see a ground. The ground is a flat surface of stone. Beyond that, you can see a stairwell.']);
      RunCommand('find stairs', ['The stairs are between the Bottom room and the Top room.']);
      RunCommand('x stairs', ['The stairs have steps.']);
      RunCommand('pick up stairs', ['The stairs are part of the overall structure and quite immovable.']);
      RunCommand('enter stairwell', ['You cannot enter the stairs. The stairs have no discernible entrance.']);
      RunCommand('climb stairs', ['On the stairs (between the Bottom room and the Top room)', 'The stairs have steps.', 'The stairs are between the Bottom room and the Top room.']);
      RunCommand('debug things', ['Things:', ' - stairs: the stairs', ' - you: you', ' - bag of holding: the embroidered bag of holding labeled Tester', ' - rim: the rim of the bag of holding']);
      RunCommand('look up', ['Looking up, you see:', 'Top Room', 'This is the room at the top of the stairs.']);
      RunCommand('look down', ['Looking down, you see:', 'Bottom Room', 'This is the room at the bottom of the stairs.']);
      RunCommand('find stairs', ['The stairs are between the Bottom room and the Top room.']);
      RunCommand('x stairs', ['The stairs have steps.', 'You are on the stairs.']);
      RunCommand('take stairs', ['Given your current position, that would be quite difficult.']);
      RunCommand('enter stairwell', ['You cannot enter the stairs. The stairs have no discernible entrance.']);
      RunCommand('down', ['Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('put bag on stairs', ['Placed on the stairs.']);
      RunCommand('u', ['(past a bag of holding)', 'Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('d', ['(through the opening for the stairwell)', '(past a bag of holding)', 'Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('put bag in stairwell', ['(first taking the bag of holding)', 'Taken.', 'It is not clear how to get inside the stairs.']);
      RunCommand('put bag on stairs', ['Placed on the stairs.']);
      RunCommand('u', ['(past a bag of holding)', 'Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('put bag in stairwell', ['(first taking the bag of holding)', 'Taken.', 'It is not clear how to get inside the stairs.']);
      RunCommand('put bag on stairs', ['Placed on the stairs.']);
      RunCommand('move me to stairs', ['On the stairs (between the Bottom room and the Top room)', 'The stairs have steps.', 'The stairs are between the Bottom room and the Top room.', 'There is a bag of holding here.']);
      RunCommand('look out', ['Looking out past the stairs, you see the Bottom room.']);
      RunCommand('out', ['Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('up', ['(past a bag of holding)', 'Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('push bag into opening', ['(first taking the bag of holding)', 'Taken.', 'Placed in the opening for the stairwell.']);
      RunCommand('find bag', ['The bag of holding is on the stairs.']);
      RunCommand('push bag onto opening', ['(first taking the bag of holding)', 'Taken.', 'Placed on the opening for the stairwell.']);
      RunCommand('find bag', ['The bag of holding is on the stairs.']);
      RunCommand('enter opening', ['(through the opening for the stairwell)', '(past a bag of holding)', 'On the stairs (between the Bottom room and the Top room)', 'The stairs have steps.', 'The stairs are between the Bottom room and the Top room.', 'There is a bag of holding here.']);
      RunCommand('take all', ['(the embroidered bag of holding labeled Tester)', 'Taken.']);
      RunCommand('up', ['Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('put bag in opening', ['Placed in the opening for the stairwell.']);
      RunCommand('find bag', ['The bag of holding is on the stairs.']);
      RunCommand('put bag on opening', ['(first taking the bag of holding)', 'Taken.', 'Placed on the opening for the stairwell.']);
      RunCommand('find bag', ['The bag of holding is on the stairs.']);
      RunCommand('take stairs', ['(through the opening for the stairwell)', '(past a bag of holding)', 'Bottom Room', 'This is the room at the bottom of the stairs. Stairs lead up to the Top room.']);
      RunCommand('take stairs', ['(past a bag of holding)', 'Top Room', 'This is the room at the top of the stairs. Stairs lead down to the Bottom room.', 'There is a big block here.']);
      RunCommand('push block into opening', ['Moved into the opening for the stairwell.']);
      RunCommand('climb stairs', ['On the stairs (between the Bottom room and the Top room)', 'The stairs have steps.', 'The stairs are between the Bottom room and the Top room.', 'There is a bag of holding here.', 'There is a big block here.']);
      RunCommand('take block', ['Taken.']);
      RunCommand('take bag', ['Taken.', 'You fumble the big block.']);
      RunCommand('take block', ['Taken.', 'You fumble the big block.']);
   end;

begin
   Writeln('MECHANICS VI (stairs)');
   RunMechanicsHarness(@InitTest, @RunTest, {$IFDEF PLAY_IN_TEST_STAIRLAND} True {$ELSE} False {$ENDIF});
end;

end.