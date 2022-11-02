my $buffer;
sub next_line {
    $buffer = undef;
    while (<>) {
        if (/\S/) {
            return $_;
        }
        $buffer = "\n";
    }
    return undef;
}

my $line;
my $is_first = 1;
while (defined($line = next_line())) {
    print $buffer if !$is_first;
    $is_first = 0;
    $line =~ s/^\s+|\s+$//g;
    $line =~ s/\s{2,}/ /g;
    print $line . "\n";
}
