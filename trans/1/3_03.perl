my $hex_dig = '0-9a-fA-F';
my $gen_delims = ':/?#\[\]@';
my $sub_delims = '!$&\'()*+,;=';
my $alpha = 'a-zA-Z';
my $digit = '0-9';
my $unreserved = "$digit$alpha\-._~";
my $pct_encoded = "(?:%[$hex_dig]{2})";

my $reg_name = "(?:(?:[$unreserved$sub_delims]|$pct_encoded)*)";
my $dec_octet = "(?:[$digit]|[1-9][$digit]|1[$digit]{2}|2[0-4][$digit]|25[0-5])";
my $ipv4_address = "(?:$dec_octet\\.$dec_octet\\.$dec_octet\\.$dec_octet)";
my $h16 = "(?:[$hex_dig]{1,4})";
my $ls32 = "(?:$h16:$h16|$ipv4_address)";
my $ipv6_address = "(?:" .
    "(?:$h16:){6}$ls32|" .
    "::(?:$h16:){5}$ls32|" .
    "$h16?::(?:$h16:){4}$ls32|" .
    "(?:(?:$h16:){0,1}$h16)?(?:::)(?:$h16:){3}$ls32|" .
    "(?:(?:$h16:){0,2}$h16)?(?:::)(?:$h16:){2}$ls32|" .
    "(?:(?:$h16:){0,3}$h16)?(?:::)$h16:$ls32|" .
    "(?:(?:$h16:){0,4}$h16)?(?:::)$ls32|" .
    "(?:(?:$h16:){0,5}$h16)?(?:::)$h16|" .
    "(?:(?:$h16:){0,6}$h16)?(?:::)" .
    ")";

my $ipv_future = "(?:v[$hex_dig]+\\.[$unreserved$sub_delims:]+)";
my $ip_literal = "(?:\\[(?:$ipv6_address|$ipv_future)\\])";

my $scheme = "(?:[$alpha][$alpha$digit+\-.]*)";
my $userinfo = "(?:(?:[$unreserved]|$pct_encoded|[$sub_delims]|:)*)";
my $host = "(?:$ip_literal|$ipv4_address|$reg_name)";
my $port = "(?:[$digit]*)";
my $authority = "(?:(?:$userinfo@)?($host)(?::$port)?)";

my $pchar = "(?:[$unreserved$sub_delims@:]|$pct_encoded)";
my $segment = "(?:$pchar*)";
my $segment_nz = "(?:$pchar+)";
my $segment_nz_nc = "(?:(?:[$unreserved$sub_delims@]|$pct_encoded)+)";

my $path_abempty = "(?:(?:/$segment)*)";
my $path_absolute = "(?:/(?:$segment_nz(?:/$segment)*)?)";
my $path_noscheme = "(?:$segment_nz_nc(?:/$segment)*)";
my $path_rootless = "(?:$segment_nz(?:/$segment)*)";
my $path_empty = "";

my $hier_part_auth = "(?:$authority$path_abempty)";
my $hier_part_other = "(?:$path_absolute|$path_rootless|$path_empty)";
my $query = "(?:(?:$pchar|[/?])*)";
my $fragment = $query;

my $uri = "(?:" .
    "(?:(?:(?:$scheme://)|//)?$hier_part_auth|(?:$scheme:$hier_part_other))" .
    "(?:\\?$query)?(?:#$fragment)?" .
    ")";

my %set = ();

$_ = do { local $/; <> };
s/<!--([\s\S]*?)-->//g;
while (/<a[^>]*\shref\s*=\s*(["'])$uri\1/gi) {
    $set{$2} = 0;
}

print "$_\n" for sort keys %set;
