#!/usr/bin/perl -w
use strict;

sub regenRegistrations() {
    foreach my $filename (<*.pas>) {
        $filename =~ m/^(.+)\.pas$/os or die "$0: $filename: not a .pas file\n";
        my $include = "registrations/$1.inc";
        open(INFILE, '<', $filename) or die "$0: $filename: $!\n";
        my @registrations = ();
        my $foundInclude = 0;
        my $line = 0;
        foreach (<INFILE>) {
            $line += 1;
            m/\@(Register[A-Za-z0-9_]+)/os && do {
                my $func = $1;
                m/^ *(T[A-Za-z0-9_]+) *= *class\b/os or
                    m/^ *(T[A-Za-z0-9_]+) *= *specialize\b/os or
                        die "$0: $filename: $line: cannot find class name\n";
                my $class = $1;
                push(@registrations, "   $func($class);\n");
                next;
            };
            m/{\$INCLUDE $include}/s && do {
                $foundInclude = 1;
            };
        }
        if (not @registrations) {
            if (-f $include) {
                print "$0: $filename: registrations removed; deleting $include\n";
                unlink $include or die "$0: $include: $!\n";
            }
            next;
        }
        if (@registrations and not $foundInclude) {
            die "$0: $filename: wrong or missing registration include; expected {\$INCLUDE $include}\n";
        }
        open(OUTFILE, '>', $include);
        local $" = '';
        print OUTFILE "@registrations";
        close(OUTFILE);
    } continue {
        close(INFILE);
    }
}

sub regenAtomLists {
    open(ATOMLISTHELPER, '>', 'atomlisthelper.inc') or die $!;
    print ATOMLISTHELPER "// THIS FILE IS AUTOMATICALLY GENERATED FROM regen.pl -- DO NOT MODIFY DIRECTLY\n";
    foreach my $method (qw(Indefinite Definite LongDefinite)) {
        foreach my $class (qw(Atom Thing)) {
            print ATOMLISTHELPER atomlisthelper($class, $method);
        }
        print ATOMLISTHELPER atomarrayhelper($method);
    }
    close ATOMLISTHELPER;

    sub atomlisthelper {
        my($class, $method) = @_;
        return <<EOM;

function T${class}List.Get${method}String(Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
var
   Count: Cardinal;
   E: T${class}List.TEnumerator;
begin
   Result := '';
   Count := 0;
   E := GetEnumerator();
   try
      while (E.MoveNext()) do
      begin
         if (Count > 0) then
         begin
            if (E.HasMore()) then
               Result := Result + ', '
            else
            if (Count > 1) then
               Result := Result + ', ' + Conjunction + ' '
            else
               Result := Result + ' ' + Conjunction + ' ';
         end;
         Result := Result + E.Current.Get${method}Name(Perspective);
         Inc(Count);
      end;
   finally
      E.Free();
   end;
end;
EOM
    }

    sub atomarrayhelper {
        my($method) = @_;
        return <<EOM;

function Get${method}String(const List: array of TAtom; StartIndex, EndIndex: Cardinal; Perspective: TAvatar; const Conjunction: AnsiString): AnsiString;
var
   Index: Cardinal;
begin
   Assert(StartIndex >= Low(List));
   Assert(EndIndex <= High(List));
   Assert(StartIndex >= EndIndex);
   Result := '';
   for Index := StartIndex to EndIndex do
   begin
      if (Index-StartIndex > 0) then
      begin
         if (Index < EndIndex) then
            Result := Result + ', '
         else
         if (Index-StartIndex > 1) then
            Result := Result + ', ' + Conjunction + ' '
         else
            Result := Result + ' ' + Conjunction + ' ';
      end;
      Result := Result + List[Index].Get${method}Name(Perspective);
   end;
end;
EOM
    }

}

regenRegistrations();
regenAtomLists();
