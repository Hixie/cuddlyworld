#!/usr/bin/perl -wT
use strict;

my $error = 0;
while (<>) {
    chomp;
    next if m!^Hint: (?:Start|End) of reading config file /home/$ENV{USER}/\.fpc\.cfg$!os;
    next if m!^Free Pascal Compiler version 2\.5\.1 \[2010/08/22] for x86_64$!os;
    next if m!^Copyright \(c\) 1993-2010 by Florian Klaempfl$!os;
    next if m!^Target OS: Linux for x86-64$!os;
    next if m!^Compiling .+$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Parameter ".+" not used$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Value parameter ".+" is assigned but never used$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Conversion between ordinals and pointers is not portable$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Warning: Mixing signed expressions and longwords gives a 64bit result$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Converting the operands to "Int64" before doing the add could prevent overflow errors\.$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Warning: Type size mismatch, possible loss of data / range check error$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Warning: Inlining disabled$!os;
    $error = 1 if m!^Fatal: Compilation aborted$!os;
    next if m!^Error: .+ppcx64 returned an error exitcode \(normal if you did not specify a source file to be compiled\)$!os;
    next if m!^Linking ../bin/[a-z]+$!os;
    next if m!^[0-9]+ lines compiled, [0-9.]+ sec $!os;
    next if m!^[0-9]+ (?:hint|warning|note)\(s\) issued$!os;
    print "$_\n";
}
exit 1 if $error;
