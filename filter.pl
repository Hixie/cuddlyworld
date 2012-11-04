#!/usr/bin/perl -wT
use strict;

my $error = 0;
while (<>) {
    chomp;
    next if m!^Hint: (?:Start|End) of reading config file /home/$ENV{USER}/\.fpc\.cfg$!os;
    next if m!^Free Pascal Compiler version 2\.7\.1 \[[-/0-9]+] for x86_64$!os;
    next if m!^Copyright \(c\) 1993-.... by Florian Klaempfl and others$!os;
    next if m!^Target OS: Linux for x86-64$!os;
    next if m!^Compiling .+$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Parameter ".+" not used$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Value parameter ".+" is assigned but never used$!os;
    next if m!^.+\([0-9]+,[0-9]+\) Warning: Constructor should be public$!os; # i'll use whatever visibility i want, thanks
    next if m!^.+\([0-9]+,[0-9]+\) Hint: Inlining disabled$!os;
    next if m!^Error: .+ppcx64 returned an error exitcode \(normal if you did not specify a source file to be compiled\)$!os;
    next if m!^Linking ../bin/[-a-z]+$!os;
    next if m!^[0-9]+ lines compiled, [0-9.]+ sec $!os;
    next if m!^[0-9]+ (?:hint|warning|note)\(s\) issued$!os;

    next if m!^.+\([0-9]+,[0-9]+\) Hint: Local const "magic." is not used$!os;

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) Warning: (?:Comparison might be always true due to range of constant and expression|unreachable code)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        open(FILE, $file) or die "could not open $file: $!\n";
        local $_;
        <FILE> for (1..$line-1);
        my $statement = <FILE>;
        close(FILE);
        next if $statement =~ m/^ *Assert\(/os;
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) Warning: (?:Type size mismatch, possible loss of data / range check error)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        open(FILE, $file) or die "could not open $file: $!\n";
        my @lines = <FILE>;
        close(FILE);
        if ($lines[$line-2] =~ m!^ *if \(Length\((.+)\) > [0-9]+\) then\n$!os) {
            my $expression = $1;
            if ($lines[$line-1] =~ m!^ *for .+ := Low\(\Q$expression\E\) to High\(\Q$expression\E\) do\n$!s or
                $lines[$line-1] =~ m!^ *for .+ := High\(\Q$expression\E\) downto Low\(\Q$expression\E\) do\n$!s) {
                next;
            }
        }
    }

    if (m!^([^(]+)\(([0-9]+),([0-9]+)\) (Warning: .+)$!os) {
        my $file = $1;
        my $line = $2;
        # column is $3 but we don't care
        my $message = $4;
        open(FILE, $file) or die "could not open $file: $!\n";
        local $_;
        <FILE> for (1..$line-1);
        my $statement = <FILE>;
        close(FILE);
        next if $statement =~ m/ {BOGUS \Q$message\E}\n$/s;
    }

    $error = 1 if m!^Fatal: Compilation aborted$!os;
    print "$_\n";
}
exit 1 if $error;

#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Value parameter ".+" is assigned but never used$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Warning: Mixing signed expressions and longwords gives a 64bit result$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Hint: Converting the operands to "Int64" before doing the add could prevent overflow errors\.$!os;
#    next if m!^.+\([0-9]+,[0-9]+\) Warning: Type size mismatch, possible loss of data / range check error$!os;
